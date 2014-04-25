// This object will hold all exports.
var Haste = {};

/* Thunk
   Creates a thunk representing the given closure.
   Since we want automatic memoization of as many expressions as possible, we
   use a JS object as a sort of tagged pointer, where the member x denotes the
   object actually pointed to. If a "pointer" points to a thunk, it has a
   member 't' which is set to true; if it points to a value, be it a function,
   a value of an algebraic type of a primitive value, it has no member 't'.
*/

function T(f) {
    this.f = new F(f);
}

function F(f) {
    this.f = f;
}

/* Apply
   Applies the function f to the arguments args. If the application is under-
   saturated, a closure is returned, awaiting further arguments. If it is over-
   saturated, the function is fully applied, and the result (assumed to be a
   function) is then applied to the remaining arguments.
*/
function A(f, args) {
    if(f instanceof T) {
        f = E(f);
    }
    // Closure does some funny stuff with functions that occasionally
    // results in non-functions getting applied, so we have to deal with
    // it.
    if(!(f instanceof Function)) {
        return f;
    }

    if(f.arity === undefined) {
        f.arity = f.length;
    }
    if(args.length === f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return f(args[0]);
            default: return f.apply(null, args);
        }
    } else if(args.length > f.arity) {
        switch(f.arity) {
            case 0:  return f();
            case 1:  return A(f(args.shift()), args);
            default: return A(f.apply(null, args.splice(0, f.arity)), args);
        }
    } else {
        var g = function() {
            return A(f, args.concat(Array.prototype.slice.call(arguments)));
        };
        g.arity = f.arity - args.length;
        return g;
    }
}

/* Eval
   Evaluate the given thunk t into head normal form.
   If the "thunk" we get isn't actually a thunk, just return it.
*/
function E(t) {
    if(t instanceof T) {
        if(t.f instanceof F) {
            return t.f = t.f.f();
        } else {
            return t.f;
        }
    } else {
        return t;
    }
}

// Export Haste, A and E. Haste because we need to preserve exports, A and E
// because they're handy for Haste.Foreign.
if(!window) {
    var window = {};
}
window['Haste'] = Haste;
window['A'] = A;
window['E'] = E;


/* Throw an error.
   We need to be able to use throw as an exception so we wrap it in a function.
*/
function die(err) {
    throw err;
}

function quot(a, b) {
    return (a-a%b)/b;
}

function quotRemI(a, b) {
    return [0, (a-a%b)/b, a%b];
}

// 32 bit integer multiplication, with correct overflow behavior
// note that |0 or >>>0 needs to be applied to the result, for int and word
// respectively.
function imul(a, b) {
  // ignore high a * high a as the result will always be truncated
  var lows = (a & 0xffff) * (b & 0xffff); // low a * low b
  var aB = (a & 0xffff) * (b & 0xffff0000); // low a * high b
  var bA = (a & 0xffff0000) * (b & 0xffff); // low b * high a
  return lows + aB + bA; // sum will not exceed 52 bits, so it's safe
}

function addC(a, b) {
    var x = a+b;
    return [0, x & 0xffffffff, x > 0x7fffffff];
}

function subC(a, b) {
    var x = a-b;
    return [0, x & 0xffffffff, x < -2147483648];
}

function sinh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / 2;
}

function tanh (arg) {
    return (Math.exp(arg) - Math.exp(-arg)) / (Math.exp(arg) + Math.exp(-arg));
}

function cosh (arg) {
    return (Math.exp(arg) + Math.exp(-arg)) / 2;
}

// Scratch space for byte arrays.
var rts_scratchBuf = new ArrayBuffer(8);
var rts_scratchW32 = new Uint32Array(rts_scratchBuf);
var rts_scratchFloat = new Float32Array(rts_scratchBuf);
var rts_scratchDouble = new Float64Array(rts_scratchBuf);

function decodeFloat(x) {
    rts_scratchFloat[0] = x;
    var sign = x < 0 ? -1 : 1;
    var exp = ((rts_scratchW32[0] >> 23) & 0xff) - 150;
    var man = rts_scratchW32[0] & 0x7fffff;
    if(exp === 0) {
        ++exp;
    } else {
        man |= (1 << 23);
    }
    return [0, sign*man, exp];
}

function decodeDouble(x) {
    rts_scratchDouble[0] = x;
    var sign = x < 0 ? -1 : 1;
    var manHigh = rts_scratchW32[1] & 0xfffff;
    var manLow = rts_scratchW32[0];
    var exp = ((rts_scratchW32[1] >> 20) & 0x7ff) - 1075;
    if(exp === 0) {
        ++exp;
    } else {
        manHigh |= (1 << 20);
    }
    return [0, sign, manHigh, manLow, exp];
}

function err(str) {
    die(toJSStr(str));
}

/* unpackCString#
   NOTE: update constructor tags if the code generator starts munging them.
*/
function unCStr(str) {return unAppCStr(str, [0]);}

function unFoldrCStr(str, f, z) {
    var acc = z;
    for(var i = str.length-1; i >= 0; --i) {
        acc = A(f, [[0, str.charCodeAt(i)], acc]);
    }
    return acc;
}

function unAppCStr(str, chrs) {
    var i = arguments[2] ? arguments[2] : 0;
    if(i >= str.length) {
        return E(chrs);
    } else {
        return [1,[0,str.charCodeAt(i)],new T(function() {
            return unAppCStr(str,chrs,i+1);
        })];
    }
}

function charCodeAt(str, i) {return str.charCodeAt(i);}

function fromJSStr(str) {
    return unCStr(E(str));
}

function toJSStr(hsstr) {
    var s = '';
    for(var str = E(hsstr); str[0] == 1; str = E(str[2])) {
        s += String.fromCharCode(E(str[1])[1]);
    }
    return s;
}

// newMutVar
function nMV(val) {
    return ({x: val});
}

// readMutVar
function rMV(mv) {
    return mv.x;
}

// writeMutVar
function wMV(mv, val) {
    mv.x = val;
}

// atomicModifyMutVar
function mMV(mv, f) {
    var x = A(f, [mv.x]);
    mv.x = x[1];
    return x[2];
}

function localeEncoding() {
    var le = newByteArr(5);
    le['b']['i8'] = 'U'.charCodeAt(0);
    le['b']['i8'] = 'T'.charCodeAt(0);
    le['b']['i8'] = 'F'.charCodeAt(0);
    le['b']['i8'] = '-'.charCodeAt(0);
    le['b']['i8'] = '8'.charCodeAt(0);
    return le;
}

var isDoubleNaN = isNaN;
var isFloatNaN = isNaN;

function isDoubleInfinite(d) {
    return (d === Infinity);
}
var isFloatInfinite = isDoubleInfinite;

function isDoubleNegativeZero(x) {
    return (x===0 && (1/x)===-Infinity);
}
var isFloatNegativeZero = isDoubleNegativeZero;

function strEq(a, b) {
    return a == b;
}

function strOrd(a, b) {
    if(a < b) {
        return [0];
    } else if(a == b) {
        return [1];
    }
    return [2];
}

function jsCatch(act, handler) {
    try {
        return A(act,[0]);
    } catch(e) {
        return A(handler,[e, 0]);
    }
}

var coercionToken = undefined;

/* Haste represents constructors internally using 1 for the first constructor,
   2 for the second, etc.
   However, dataToTag should use 0, 1, 2, etc. Also, booleans might be unboxed.
 */
function dataToTag(x) {
    if(x instanceof Array) {
        return x[0];
    } else {
        return x;
    }
}

function __word_encodeDouble(d, e) {
    return d * Math.pow(2,e);
}

var __word_encodeFloat = __word_encodeDouble;
var jsRound = Math.round; // Stupid GHC doesn't like periods in FFI IDs...
var realWorld = undefined;
if(typeof _ == 'undefined') {
    var _ = undefined;
}

function popCnt(i) {
    i = i - ((i >> 1) & 0x55555555);
    i = (i & 0x33333333) + ((i >> 2) & 0x33333333);
    return (((i + (i >> 4)) & 0x0F0F0F0F) * 0x01010101) >> 24;
}

function jsAlert(val) {
    if(typeof alert != 'undefined') {
        alert(val);
    } else {
        print(val);
    }
}

function jsLog(val) {
    console.log(val);
}

function jsPrompt(str) {
    var val;
    if(typeof prompt != 'undefined') {
        val = prompt(str);
    } else {
        print(str);
        val = readline();
    }
    return val == undefined ? '' : val.toString();
}

function jsEval(str) {
    var x = eval(str);
    return x == undefined ? '' : x.toString();
}

function isNull(obj) {
    return obj === null;
}

function jsRead(str) {
    return Number(str);
}

function jsShowI(val) {return val.toString();}
function jsShow(val) {
    var ret = val.toString();
    return val == Math.round(val) ? ret + '.0' : ret;
}

function jsGetMouseCoords(e) {
    var posx = 0;
    var posy = 0;
    if (!e) var e = window.event;
    if (e.pageX || e.pageY) 	{
	posx = e.pageX;
	posy = e.pageY;
    }
    else if (e.clientX || e.clientY) 	{
	posx = e.clientX + document.body.scrollLeft
	    + document.documentElement.scrollLeft;
	posy = e.clientY + document.body.scrollTop
	    + document.documentElement.scrollTop;
    }
    return [posx - e.target.offsetLeft, posy - e.target.offsetTop];
}

function jsSetCB(elem, evt, cb) {
    // Count return press in single line text box as a change event.
    if(evt == 'change' && elem.type.toLowerCase() == 'text') {
        setCB(elem, 'keyup', function(k) {
            if(k == '\n'.charCodeAt(0)) {
                A(cb,[[0,k.keyCode],0]);
            }
        });
    }

    var fun;
    switch(evt) {
    case 'click':
    case 'dblclick':
    case 'mouseup':
    case 'mousedown':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,x.button],[0,mx,my],0]);
        };
        break;
    case 'mousemove':
    case 'mouseover':
        fun = function(x) {
            var mpos = jsGetMouseCoords(x);
            var mx = [0,mpos[0]];
            var my = [0,mpos[1]];
            A(cb,[[0,mx,my],0]);
        };
        break;
    case 'keypress':
    case 'keyup':
    case 'keydown':
        fun = function(x) {A(cb,[[0,x.keyCode],0]);};
        break;        
    default:
        fun = function() {A(cb,[0]);};
        break;
    }
    return setCB(elem, evt, fun);
}

function setCB(elem, evt, cb) {
    if(elem.addEventListener) {
        elem.addEventListener(evt, cb, false);
        return true;
    } else if(elem.attachEvent) {
        elem.attachEvent('on'+evt, cb);
        return true;
    }
    return false;
}

function jsSetTimeout(msecs, cb) {
    window.setTimeout(function() {A(cb,[0]);}, msecs);
}

function jsGet(elem, prop) {
    return elem[prop].toString();
}

function jsSet(elem, prop, val) {
    elem[prop] = val;
}

function jsGetAttr(elem, prop) {
    if(elem.hasAttribute(prop)) {
        return elem.getAttribute(prop).toString();
    } else {
        return "";
    }
}

function jsSetAttr(elem, prop, val) {
    elem.setAttribute(prop, val);
}

function jsGetStyle(elem, prop) {
    return elem.style[prop].toString();
}

function jsSetStyle(elem, prop, val) {
    elem.style[prop] = val;
}

function jsKillChild(child, parent) {
    parent.removeChild(child);
}

function jsClearChildren(elem) {
    while(elem.hasChildNodes()){
        elem.removeChild(elem.lastChild);
    }
}

function jsFind(elem) {
    var e = document.getElementById(elem)
    if(e) {
        return [1,[0,e]];
    }
    return [0];
}

function jsCreateElem(tag) {
    return document.createElement(tag);
}

function jsCreateTextNode(str) {
    return document.createTextNode(str);
}

function jsGetChildBefore(elem) {
    elem = elem.previousSibling;
    while(elem) {
        if(typeof elem.tagName != 'undefined') {
            return [1,[0,elem]];
        }
        elem = elem.previousSibling;
    }
    return [0];
}

function jsGetLastChild(elem) {
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            return [1,[0,elem.childNodes[i]]];
        }
    }
    return [0];
}

function jsGetChildren(elem) {
    var children = [0];
    var len = elem.childNodes.length;
    for(var i = len-1; i >= 0; --i) {
        if(typeof elem.childNodes[i].tagName != 'undefined') {
            children = [1, [0,elem.childNodes[i]], children];
        }
    }
    return children;
}

function jsSetChildren(elem, children) {
    children = E(children);
    jsClearChildren(elem, 0);
    while(children[0] === 1) {
        elem.appendChild(E(E(children[1])[1]));
        children = E(children[2]);
    }
}

function jsAppendChild(child, container) {
    container.appendChild(child);
}

function jsAddChildBefore(child, container, after) {
    container.insertBefore(child, after);
}

var jsRand = Math.random;

// Concatenate a Haskell list of JS strings
function jsCat(strs, sep) {
    var arr = [];
    strs = E(strs);
    while(strs[0]) {
        strs = E(strs);
        arr.push(E(strs[1])[1]);
        strs = E(strs[2]);
    }
    return arr.join(sep);
}

// JSON stringify a string
function jsStringify(str) {
    return JSON.stringify(str);
}

// Parse a JSON message into a Haste.JSON.JSON value.
// As this pokes around inside Haskell values, it'll need to be updated if:
// * Haste.JSON.JSON changes;
// * E() starts to choke on non-thunks;
// * data constructor code generation changes; or
// * Just and Nothing change tags.
function jsParseJSON(str) {
    try {
        var js = JSON.parse(str);
        var hs = toHS(js);
    } catch(_) {
        return [0];
    }
    return [1,hs];
}

function toHS(obj) {
    switch(typeof obj) {
    case 'number':
        return [0, [0, jsRead(obj)]];
    case 'string':
        return [1, [0, obj]];
        break;
    case 'boolean':
        return [2, obj]; // Booleans are special wrt constructor tags!
        break;
    case 'object':
        if(obj instanceof Array) {
            return [3, arr2lst(obj, 0)];
        } else {
            // Object type but not array - it's a dictionary.
            // The RFC doesn't say anything about the ordering of keys, but
            // considering that lots of people rely on keys being "in order" as
            // defined by "the same way someone put them in at the other end,"
            // it's probably a good idea to put some cycles into meeting their
            // misguided expectations.
            var ks = [];
            for(var k in obj) {
                ks.unshift(k);
            }
            var xs = [0];
            for(var i = 0; i < ks.length; i++) {
                xs = [1, [0, [0,ks[i]], toHS(obj[ks[i]])], xs];
            }
            return [4, xs];
        }
    }
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst(arr,elem+1);})]
}

function lst2arr(xs) {
    var arr = [];
    for(; xs[0]; xs = E(xs[2])) {
        arr.push(E(xs[1]));
    }
    return arr;
}

function ajaxReq(method, url, async, postdata, cb) {
    var xhr = new XMLHttpRequest();
    xhr.open(method, url, async);
    xhr.setRequestHeader('Cache-control', 'no-cache');
    xhr.onreadystatechange = function() {
        if(xhr.readyState == 4) {
            if(xhr.status == 200) {
                A(cb,[[1,[0,xhr.responseText]],0]);
            } else {
                A(cb,[[0],0]); // Nothing
            }
        }
    }
    xhr.send(postdata);
}

// Create a little endian ArrayBuffer representation of something.
function toABHost(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    return a;
}

function toABSwap(v, n, x) {
    var a = new ArrayBuffer(n);
    new window[v](a)[0] = x;
    var bs = new Uint8Array(a);
    for(var i = 0, j = n-1; i < j; ++i, --j) {
        var tmp = bs[i];
        bs[i] = bs[j];
        bs[j] = tmp;
    }
    return a;
}

window['toABle'] = toABHost;
window['toABbe'] = toABSwap;

// Swap byte order if host is not little endian.
var buffer = new ArrayBuffer(2);
new DataView(buffer).setInt16(0, 256, true);
if(new Int16Array(buffer)[0] !== 256) {
    window['toABle'] = toABSwap;
    window['toABbe'] = toABHost;
}

// MVar implementation.
// Since Haste isn't concurrent, takeMVar and putMVar don't block on empty
// and full MVars respectively, but terminate the program since they would
// otherwise be blocking forever.

function newMVar() {
    return ({empty: true});
}

function tryTakeMVar(mv) {
    if(mv.empty) {
        return [0, 0, undefined];
    } else {
        var val = mv.x;
        mv.empty = true;
        mv.x = null;
        return [0, 1, val];
    }
}

function takeMVar(mv) {
    if(mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to take empty MVar!");
    }
    var val = mv.x;
    mv.empty = true;
    mv.x = null;
    return val;
}

function putMVar(mv, val) {
    if(!mv.empty) {
        // TODO: real BlockedOnDeadMVar exception, perhaps?
        err("Attempted to put full MVar!");
    }
    mv.empty = false;
    mv.x = val;
}

function tryPutMVar(mv, val) {
    if(!mv.empty) {
        return 0;
    } else {
        mv.empty = false;
        mv.x = val;
        return 1;
    }
}

function sameMVar(a, b) {
    return (a == b);
}

function isEmptyMVar(mv) {
    return mv.empty ? 1 : 0;
}

// Implementation of stable names.
// Unlike native GHC, the garbage collector isn't going to move data around
// in a way that we can detect, so each object could serve as its own stable
// name if it weren't for the fact we can't turn a JS reference into an
// integer.
// So instead, each object has a unique integer attached to it, which serves
// as its stable name.

var __next_stable_name = 1;

function makeStableName(x) {
    if(!x.stableName) {
        x.stableName = __next_stable_name;
        __next_stable_name += 1;
    }
    return x.stableName;
}

function eqStableName(x, y) {
    return (x == y) ? 1 : 0;
}

var Integer = function(bits, sign) {
  this.bits_ = [];
  this.sign_ = sign;

  var top = true;
  for (var i = bits.length - 1; i >= 0; i--) {
    var val = bits[i] | 0;
    if (!top || val != sign) {
      this.bits_[i] = val;
      top = false;
    }
  }
};

Integer.IntCache_ = {};

var I_fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Integer.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Integer([value | 0], value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Integer.IntCache_[value] = obj;
  }
  return obj;
};

var I_fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Integer.ZERO;
  } else if (value < 0) {
    return I_negate(I_fromNumber(-value));
  } else {
    var bits = [];
    var pow = 1;
    for (var i = 0; value >= pow; i++) {
      bits[i] = (value / pow) | 0;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return new Integer(bits, 0);
  }
};

var I_fromBits = function(bits) {
  var high = bits[bits.length - 1];
  return new Integer(bits, high & (1 << 31) ? -1 : 0);
};

var I_fromString = function(str, opt_radix) {
  if (str.length == 0) {
    throw Error('number format error: empty string');
  }

  var radix = opt_radix || 10;
  if (radix < 2 || 36 < radix) {
    throw Error('radix out of range: ' + radix);
  }

  if (str.charAt(0) == '-') {
    return I_negate(I_fromString(str.substring(1), radix));
  } else if (str.indexOf('-') >= 0) {
    throw Error('number format error: interior "-" character');
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 8));

  var result = Integer.ZERO;
  for (var i = 0; i < str.length; i += 8) {
    var size = Math.min(8, str.length - i);
    var value = parseInt(str.substring(i, i + size), radix);
    if (size < 8) {
      var power = I_fromNumber(Math.pow(radix, size));
      result = I_add(I_mul(result, power), I_fromNumber(value));
    } else {
      result = I_mul(result, radixToPower);
      result = I_add(result, I_fromNumber(value));
    }
  }
  return result;
};


Integer.TWO_PWR_32_DBL_ = (1 << 16) * (1 << 16);
Integer.ZERO = I_fromInt(0);
Integer.ONE = I_fromInt(1);
Integer.TWO_PWR_24_ = I_fromInt(1 << 24);

var I_toInt = function(self) {
  return self.bits_.length > 0 ? self.bits_[0] : self.sign_;
};

var I_toWord = function(self) {
  return I_toInt(self) >>> 0;
};

var I_toNumber = function(self) {
  if (isNegative(self)) {
    return -I_toNumber(I_negate(self));
  } else {
    var val = 0;
    var pow = 1;
    for (var i = 0; i < self.bits_.length; i++) {
      val += I_getBitsUnsigned(self, i) * pow;
      pow *= Integer.TWO_PWR_32_DBL_;
    }
    return val;
  }
};

var I_getBits = function(self, index) {
  if (index < 0) {
    return 0;
  } else if (index < self.bits_.length) {
    return self.bits_[index];
  } else {
    return self.sign_;
  }
};

var I_getBitsUnsigned = function(self, index) {
  var val = I_getBits(self, index);
  return val >= 0 ? val : Integer.TWO_PWR_32_DBL_ + val;
};

var getSign = function(self) {
  return self.sign_;
};

var isZero = function(self) {
  if (self.sign_ != 0) {
    return false;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    if (self.bits_[i] != 0) {
      return false;
    }
  }
  return true;
};

var isNegative = function(self) {
  return self.sign_ == -1;
};

var isOdd = function(self) {
  return (self.bits_.length == 0) && (self.sign_ == -1) ||
         (self.bits_.length > 0) && ((self.bits_[0] & 1) != 0);
};

var I_equals = function(self, other) {
  if (self.sign_ != other.sign_) {
    return false;
  }
  var len = Math.max(self.bits_.length, other.bits_.length);
  for (var i = 0; i < len; i++) {
    if (I_getBits(self, i) != I_getBits(other, i)) {
      return false;
    }
  }
  return true;
};

var I_notEquals = function(self, other) {
  return !I_equals(self, other);
};

var I_greaterThan = function(self, other) {
  return I_compare(self, other) > 0;
};

var I_greaterThanOrEqual = function(self, other) {
  return I_compare(self, other) >= 0;
};

var I_lessThan = function(self, other) {
  return I_compare(self, other) < 0;
};

var I_lessThanOrEqual = function(self, other) {
  return I_compare(self, other) <= 0;
};

var I_compare = function(self, other) {
  var diff = I_sub(self, other);
  if (isNegative(diff)) {
    return -1;
  } else if (isZero(diff)) {
    return 0;
  } else {
    return +1;
  }
};

var I_compareInt = function(self, other) {
  return I_compare(self, I_fromInt(other));
}

var shorten = function(self, numBits) {
  var arr_index = (numBits - 1) >> 5;
  var bit_index = (numBits - 1) % 32;
  var bits = [];
  for (var i = 0; i < arr_index; i++) {
    bits[i] = I_getBits(self, i);
  }
  var sigBits = bit_index == 31 ? 0xFFFFFFFF : (1 << (bit_index + 1)) - 1;
  var val = I_getBits(self, arr_index) & sigBits;
  if (val & (1 << bit_index)) {
    val |= 0xFFFFFFFF - sigBits;
    bits[arr_index] = val;
    return new Integer(bits, -1);
  } else {
    bits[arr_index] = val;
    return new Integer(bits, 0);
  }
};

var I_negate = function(self) {
  return I_add(not(self), Integer.ONE);
};

var I_add = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  var carry = 0;

  for (var i = 0; i <= len; i++) {
    var a1 = I_getBits(self, i) >>> 16;
    var a0 = I_getBits(self, i) & 0xFFFF;

    var b1 = I_getBits(other, i) >>> 16;
    var b0 = I_getBits(other, i) & 0xFFFF;

    var c0 = carry + a0 + b0;
    var c1 = (c0 >>> 16) + a1 + b1;
    carry = c1 >>> 16;
    c0 &= 0xFFFF;
    c1 &= 0xFFFF;
    arr[i] = (c1 << 16) | c0;
  }
  return I_fromBits(arr);
};

var I_sub = function(self, other) {
  return I_add(self, I_negate(other));
};

var I_mul = function(self, other) {
  if (isZero(self)) {
    return Integer.ZERO;
  } else if (isZero(other)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_mul(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_mul(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_mul(self, I_negate(other)));
  }

  if (I_lessThan(self, Integer.TWO_PWR_24_) &&
      I_lessThan(other, Integer.TWO_PWR_24_)) {
    return I_fromNumber(I_toNumber(self) * I_toNumber(other));
  }

  var len = self.bits_.length + other.bits_.length;
  var arr = [];
  for (var i = 0; i < 2 * len; i++) {
    arr[i] = 0;
  }
  for (var i = 0; i < self.bits_.length; i++) {
    for (var j = 0; j < other.bits_.length; j++) {
      var a1 = I_getBits(self, i) >>> 16;
      var a0 = I_getBits(self, i) & 0xFFFF;

      var b1 = I_getBits(other, j) >>> 16;
      var b0 = I_getBits(other, j) & 0xFFFF;

      arr[2 * i + 2 * j] += a0 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j);
      arr[2 * i + 2 * j + 1] += a1 * b0;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 1] += a0 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 1);
      arr[2 * i + 2 * j + 2] += a1 * b1;
      Integer.carry16_(arr, 2 * i + 2 * j + 2);
    }
  }

  for (var i = 0; i < len; i++) {
    arr[i] = (arr[2 * i + 1] << 16) | arr[2 * i];
  }
  for (var i = len; i < 2 * len; i++) {
    arr[i] = 0;
  }
  return new Integer(arr, 0);
};

Integer.carry16_ = function(bits, index) {
  while ((bits[index] & 0xFFFF) != bits[index]) {
    bits[index + 1] += bits[index] >>> 16;
    bits[index] &= 0xFFFF;
  }
};

var I_mod = function(self, other) {
  return I_rem(I_add(other, I_rem(self, other)), other);
}

var I_div = function(self, other) {
  if(I_greaterThan(self, Integer.ZERO) != I_greaterThan(other, Integer.ZERO)) {
    if(I_rem(self, other) != Integer.ZERO) {
      return I_sub(I_quot(self, other), Integer.ONE);
    }
  }
  return I_quot(self, other);
}

var I_quotRem = function(self, other) {
  return [0, I_quot(self, other), I_rem(self, other)];
}

var I_divMod = function(self, other) {
  return [0, I_div(self, other), I_mod(self, other)];
}

var I_quot = function(self, other) {
  if (isZero(other)) {
    throw Error('division by zero');
  } else if (isZero(self)) {
    return Integer.ZERO;
  }

  if (isNegative(self)) {
    if (isNegative(other)) {
      return I_quot(I_negate(self), I_negate(other));
    } else {
      return I_negate(I_quot(I_negate(self), other));
    }
  } else if (isNegative(other)) {
    return I_negate(I_quot(self, I_negate(other)));
  }

  var res = Integer.ZERO;
  var rem = self;
  while (I_greaterThanOrEqual(rem, other)) {
    var approx = Math.max(1, Math.floor(I_toNumber(rem) / I_toNumber(other)));
    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);
    var approxRes = I_fromNumber(approx);
    var approxRem = I_mul(approxRes, other);
    while (isNegative(approxRem) || I_greaterThan(approxRem, rem)) {
      approx -= delta;
      approxRes = I_fromNumber(approx);
      approxRem = I_mul(approxRes, other);
    }

    if (isZero(approxRes)) {
      approxRes = Integer.ONE;
    }

    res = I_add(res, approxRes);
    rem = I_sub(rem, approxRem);
  }
  return res;
};

var I_rem = function(self, other) {
  return I_sub(self, I_mul(I_quot(self, other), other));
};

var not = function(self) {
  var len = self.bits_.length;
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = ~self.bits_[i];
  }
  return new Integer(arr, ~self.sign_);
};

var I_and = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) & I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ & other.sign_);
};

var I_or = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) | I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ | other.sign_);
};

var I_xor = function(self, other) {
  var len = Math.max(self.bits_.length, other.bits_.length);
  var arr = [];
  for (var i = 0; i < len; i++) {
    arr[i] = I_getBits(self, i) ^ I_getBits(other, i);
  }
  return new Integer(arr, self.sign_ ^ other.sign_);
};

var I_shiftLeft = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length + arr_delta + (bit_delta > 0 ? 1 : 0);
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i - arr_delta) << bit_delta) |
               (I_getBits(self, i - arr_delta - 1) >>> (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i - arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_shiftRight = function(self, numBits) {
  var arr_delta = numBits >> 5;
  var bit_delta = numBits % 32;
  var len = self.bits_.length - arr_delta;
  var arr = [];
  for (var i = 0; i < len; i++) {
    if (bit_delta > 0) {
      arr[i] = (I_getBits(self, i + arr_delta) >>> bit_delta) |
               (I_getBits(self, i + arr_delta + 1) << (32 - bit_delta));
    } else {
      arr[i] = I_getBits(self, i + arr_delta);
    }
  }
  return new Integer(arr, self.sign_);
};

var I_signum = function(self) {
  var cmp = I_compare(self, Integer.ZERO);
  if(cmp > 0) {
    return Integer.ONE
  }
  if(cmp < 0) {
    return I_sub(Integer.ZERO, Integer.ONE);
  }
  return Integer.ZERO;
};

var I_abs = function(self) {
  if(I_compare(self, Integer.ZERO) < 0) {
    return I_sub(Integer.ZERO, self);
  }
  return self;
};

var I_decodeDouble = function(x) {
  var dec = decodeDouble(x);
  var mantissa = I_fromBits([dec[3], dec[2]]);
  if(dec[1] < 0) {
    mantissa = I_negate(mantissa);
  }
  return [0, dec[4], mantissa];
}

var I_toString = function(self) {
  var radix = 10;

  if (isZero(self)) {
    return '0';
  } else if (isNegative(self)) {
    return '-' + I_toString(I_negate(self));
  }

  var radixToPower = I_fromNumber(Math.pow(radix, 6));

  var rem = self;
  var result = '';
  while (true) {
    var remDiv = I_div(rem, radixToPower);
    var intval = I_toInt(I_sub(rem, I_mul(remDiv, radixToPower)));
    var digits = intval.toString();

    rem = remDiv;
    if (isZero(rem)) {
      return digits + result;
    } else {
      while (digits.length < 6) {
        digits = '0' + digits;
      }
      result = '' + digits + result;
    }
  }
};

var I_fromRat = function(a, b) {
    return I_toNumber(a) / I_toNumber(b);
}

function I_fromInt64(x) {
    return I_fromBits([x.getLowBits(), x.getHighBits()]);
}

function I_toInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

function I_fromWord64(x) {
    return x;
}

function I_toWord64(x) {
    return I_rem(I_add(__w64_max, x), __w64_max);
}

// Copyright 2009 The Closure Library Authors. All Rights Reserved.
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
//      http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS-IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.

var Long = function(low, high) {
  this.low_ = low | 0;
  this.high_ = high | 0;
};

Long.IntCache_ = {};

Long.fromInt = function(value) {
  if (-128 <= value && value < 128) {
    var cachedObj = Long.IntCache_[value];
    if (cachedObj) {
      return cachedObj;
    }
  }

  var obj = new Long(value | 0, value < 0 ? -1 : 0);
  if (-128 <= value && value < 128) {
    Long.IntCache_[value] = obj;
  }
  return obj;
};

Long.fromNumber = function(value) {
  if (isNaN(value) || !isFinite(value)) {
    return Long.ZERO;
  } else if (value <= -Long.TWO_PWR_63_DBL_) {
    return Long.MIN_VALUE;
  } else if (value + 1 >= Long.TWO_PWR_63_DBL_) {
    return Long.MAX_VALUE;
  } else if (value < 0) {
    return Long.fromNumber(-value).negate();
  } else {
    return new Long(
        (value % Long.TWO_PWR_32_DBL_) | 0,
        (value / Long.TWO_PWR_32_DBL_) | 0);
  }
};

Long.fromBits = function(lowBits, highBits) {
  return new Long(lowBits, highBits);
};

Long.TWO_PWR_16_DBL_ = 1 << 16;
Long.TWO_PWR_24_DBL_ = 1 << 24;
Long.TWO_PWR_32_DBL_ =
    Long.TWO_PWR_16_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_31_DBL_ =
    Long.TWO_PWR_32_DBL_ / 2;
Long.TWO_PWR_48_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_16_DBL_;
Long.TWO_PWR_64_DBL_ =
    Long.TWO_PWR_32_DBL_ * Long.TWO_PWR_32_DBL_;
Long.TWO_PWR_63_DBL_ =
    Long.TWO_PWR_64_DBL_ / 2;
Long.ZERO = Long.fromInt(0);
Long.ONE = Long.fromInt(1);
Long.NEG_ONE = Long.fromInt(-1);
Long.MAX_VALUE =
    Long.fromBits(0xFFFFFFFF | 0, 0x7FFFFFFF | 0);
Long.MIN_VALUE = Long.fromBits(0, 0x80000000 | 0);
Long.TWO_PWR_24_ = Long.fromInt(1 << 24);

Long.prototype.toInt = function() {
  return this.low_;
};

Long.prototype.toNumber = function() {
  return this.high_ * Long.TWO_PWR_32_DBL_ +
         this.getLowBitsUnsigned();
};

Long.prototype.getHighBits = function() {
  return this.high_;
};

Long.prototype.getLowBits = function() {
  return this.low_;
};

Long.prototype.getLowBitsUnsigned = function() {
  return (this.low_ >= 0) ?
      this.low_ : Long.TWO_PWR_32_DBL_ + this.low_;
};

Long.prototype.isZero = function() {
  return this.high_ == 0 && this.low_ == 0;
};

Long.prototype.isNegative = function() {
  return this.high_ < 0;
};

Long.prototype.isOdd = function() {
  return (this.low_ & 1) == 1;
};

Long.prototype.equals = function(other) {
  return (this.high_ == other.high_) && (this.low_ == other.low_);
};

Long.prototype.notEquals = function(other) {
  return (this.high_ != other.high_) || (this.low_ != other.low_);
};

Long.prototype.lessThan = function(other) {
  return this.compare(other) < 0;
};

Long.prototype.lessThanOrEqual = function(other) {
  return this.compare(other) <= 0;
};

Long.prototype.greaterThan = function(other) {
  return this.compare(other) > 0;
};

Long.prototype.greaterThanOrEqual = function(other) {
  return this.compare(other) >= 0;
};

Long.prototype.compare = function(other) {
  if (this.equals(other)) {
    return 0;
  }

  var thisNeg = this.isNegative();
  var otherNeg = other.isNegative();
  if (thisNeg && !otherNeg) {
    return -1;
  }
  if (!thisNeg && otherNeg) {
    return 1;
  }

  if (this.subtract(other).isNegative()) {
    return -1;
  } else {
    return 1;
  }
};

Long.prototype.negate = function() {
  if (this.equals(Long.MIN_VALUE)) {
    return Long.MIN_VALUE;
  } else {
    return this.not().add(Long.ONE);
  }
};

Long.prototype.add = function(other) {
  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 + b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 + b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 + b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 + b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.subtract = function(other) {
  return this.add(other.negate());
};

Long.prototype.multiply = function(other) {
  if (this.isZero()) {
    return Long.ZERO;
  } else if (other.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    return other.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  } else if (other.equals(Long.MIN_VALUE)) {
    return this.isOdd() ? Long.MIN_VALUE : Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().multiply(other.negate());
    } else {
      return this.negate().multiply(other).negate();
    }
  } else if (other.isNegative()) {
    return this.multiply(other.negate()).negate();
  }

  if (this.lessThan(Long.TWO_PWR_24_) &&
      other.lessThan(Long.TWO_PWR_24_)) {
    return Long.fromNumber(this.toNumber() * other.toNumber());
  }

  var a48 = this.high_ >>> 16;
  var a32 = this.high_ & 0xFFFF;
  var a16 = this.low_ >>> 16;
  var a00 = this.low_ & 0xFFFF;

  var b48 = other.high_ >>> 16;
  var b32 = other.high_ & 0xFFFF;
  var b16 = other.low_ >>> 16;
  var b00 = other.low_ & 0xFFFF;

  var c48 = 0, c32 = 0, c16 = 0, c00 = 0;
  c00 += a00 * b00;
  c16 += c00 >>> 16;
  c00 &= 0xFFFF;
  c16 += a16 * b00;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c16 += a00 * b16;
  c32 += c16 >>> 16;
  c16 &= 0xFFFF;
  c32 += a32 * b00;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a16 * b16;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c32 += a00 * b32;
  c48 += c32 >>> 16;
  c32 &= 0xFFFF;
  c48 += a48 * b00 + a32 * b16 + a16 * b32 + a00 * b48;
  c48 &= 0xFFFF;
  return Long.fromBits((c16 << 16) | c00, (c48 << 16) | c32);
};

Long.prototype.div = function(other) {
  if (other.isZero()) {
    throw Error('division by zero');
  } else if (this.isZero()) {
    return Long.ZERO;
  }

  if (this.equals(Long.MIN_VALUE)) {
    if (other.equals(Long.ONE) ||
        other.equals(Long.NEG_ONE)) {
      return Long.MIN_VALUE;
    } else if (other.equals(Long.MIN_VALUE)) {
      return Long.ONE;
    } else {
      var halfThis = this.shiftRight(1);
      var approx = halfThis.div(other).shiftLeft(1);
      if (approx.equals(Long.ZERO)) {
        return other.isNegative() ? Long.ONE : Long.NEG_ONE;
      } else {
        var rem = this.subtract(other.multiply(approx));
        var result = approx.add(rem.div(other));
        return result;
      }
    }
  } else if (other.equals(Long.MIN_VALUE)) {
    return Long.ZERO;
  }

  if (this.isNegative()) {
    if (other.isNegative()) {
      return this.negate().div(other.negate());
    } else {
      return this.negate().div(other).negate();
    }
  } else if (other.isNegative()) {
    return this.div(other.negate()).negate();
  }

  var res = Long.ZERO;
  var rem = this;
  while (rem.greaterThanOrEqual(other)) {
    var approx = Math.max(1, Math.floor(rem.toNumber() / other.toNumber()));

    var log2 = Math.ceil(Math.log(approx) / Math.LN2);
    var delta = (log2 <= 48) ? 1 : Math.pow(2, log2 - 48);

    var approxRes = Long.fromNumber(approx);
    var approxRem = approxRes.multiply(other);
    while (approxRem.isNegative() || approxRem.greaterThan(rem)) {
      approx -= delta;
      approxRes = Long.fromNumber(approx);
      approxRem = approxRes.multiply(other);
    }

    if (approxRes.isZero()) {
      approxRes = Long.ONE;
    }

    res = res.add(approxRes);
    rem = rem.subtract(approxRem);
  }
  return res;
};

Long.prototype.modulo = function(other) {
  return this.subtract(this.div(other).multiply(other));
};

Long.prototype.not = function() {
  return Long.fromBits(~this.low_, ~this.high_);
};

Long.prototype.and = function(other) {
  return Long.fromBits(this.low_ & other.low_,
                                 this.high_ & other.high_);
};

Long.prototype.or = function(other) {
  return Long.fromBits(this.low_ | other.low_,
                                 this.high_ | other.high_);
};

Long.prototype.xor = function(other) {
  return Long.fromBits(this.low_ ^ other.low_,
                                 this.high_ ^ other.high_);
};

Long.prototype.shiftLeft = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var low = this.low_;
    if (numBits < 32) {
      var high = this.high_;
      return Long.fromBits(
          low << numBits,
          (high << numBits) | (low >>> (32 - numBits)));
    } else {
      return Long.fromBits(0, low << (numBits - 32));
    }
  }
};

Long.prototype.shiftRight = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >> numBits);
    } else {
      return Long.fromBits(
          high >> (numBits - 32),
          high >= 0 ? 0 : -1);
    }
  }
};

Long.prototype.shiftRightUnsigned = function(numBits) {
  numBits &= 63;
  if (numBits == 0) {
    return this;
  } else {
    var high = this.high_;
    if (numBits < 32) {
      var low = this.low_;
      return Long.fromBits(
          (low >>> numBits) | (high << (32 - numBits)),
          high >>> numBits);
    } else if (numBits == 32) {
      return Long.fromBits(high, 0);
    } else {
      return Long.fromBits(high >>> (numBits - 32), 0);
    }
  }
};



// Int64
function hs_eqInt64(x, y) {return x.equals(y);}
function hs_neInt64(x, y) {return !x.equals(y);}
function hs_ltInt64(x, y) {return x.compare(y) < 0;}
function hs_leInt64(x, y) {return x.compare(y) <= 0;}
function hs_gtInt64(x, y) {return x.compare(y) > 0;}
function hs_geInt64(x, y) {return x.compare(y) >= 0;}
function hs_quotInt64(x, y) {return x.div(y);}
function hs_remInt64(x, y) {return x.modulo(y);}
function hs_plusInt64(x, y) {return x.add(y);}
function hs_minusInt64(x, y) {return x.subtract(y);}
function hs_timesInt64(x, y) {return x.multiply(y);}
function hs_negateInt64(x) {return x.negate();}
function hs_uncheckedIShiftL64(x, bits) {x.shiftLeft(bits);}
function hs_uncheckedIShiftRA64(x, bits) {x.shiftRight(bits);}
function hs_uncheckedIShiftRL64(x, bits) {x.shiftRightUnsigned(bits);}
function hs_intToInt64(x) {return new Long(x, 0);}
function hs_int64ToInt(x) {return x.toInt();}



// Word64
function hs_wordToWord64(x) {
    return I_fromInt(x);
}
function hs_word64ToWord(x) {
    return I_toInt(x);
}
function hs_mkWord64(low, high) {
    return I_fromBits([low, high]);
}

var hs_and64 = I_and;
var hs_or64 = I_or;
var hs_xor64 = I_xor;
var __i64_all_ones = I_fromBits([0xffffffff, 0xffffffff]);
function hs_not64(x) {
    return I_xor(x, __i64_all_ones);
}
var hs_eqWord64 = I_equals;
var hs_neWord64 = I_notEquals;
var hs_ltWord64 = I_lessThan;
var hs_leWord64 = I_lessThanOrEqual;
var hs_gtWord64 = I_greaterThan;
var hs_geWord64 = I_greaterThanOrEqual;
var hs_quotWord64 = I_quot;
var hs_remWord64 = I_rem;
var __w64_max = I_fromBits([0,0,1]);
function hs_uncheckedShiftL64(x, bits) {
    return I_rem(I_shiftLeft(x, bits), __w64_max);
}
var hs_uncheckedShiftRL64 = I_shiftRight;
function hs_int64ToWord64(x) {
    var tmp = I_add(__w64_max, I_fromBits([x.getLowBits(), x.getHighBits()]));
    return I_rem(tmp, __w64_max);
}
function hs_word64ToInt64(x) {
    return Long.fromBits(I_getBits(x, 0), I_getBits(x, 1));
}

// Joseph Myers' MD5 implementation; used under the BSD license.

function md5cycle(x, k) {
var a = x[0], b = x[1], c = x[2], d = x[3];

a = ff(a, b, c, d, k[0], 7, -680876936);
d = ff(d, a, b, c, k[1], 12, -389564586);
c = ff(c, d, a, b, k[2], 17,  606105819);
b = ff(b, c, d, a, k[3], 22, -1044525330);
a = ff(a, b, c, d, k[4], 7, -176418897);
d = ff(d, a, b, c, k[5], 12,  1200080426);
c = ff(c, d, a, b, k[6], 17, -1473231341);
b = ff(b, c, d, a, k[7], 22, -45705983);
a = ff(a, b, c, d, k[8], 7,  1770035416);
d = ff(d, a, b, c, k[9], 12, -1958414417);
c = ff(c, d, a, b, k[10], 17, -42063);
b = ff(b, c, d, a, k[11], 22, -1990404162);
a = ff(a, b, c, d, k[12], 7,  1804603682);
d = ff(d, a, b, c, k[13], 12, -40341101);
c = ff(c, d, a, b, k[14], 17, -1502002290);
b = ff(b, c, d, a, k[15], 22,  1236535329);

a = gg(a, b, c, d, k[1], 5, -165796510);
d = gg(d, a, b, c, k[6], 9, -1069501632);
c = gg(c, d, a, b, k[11], 14,  643717713);
b = gg(b, c, d, a, k[0], 20, -373897302);
a = gg(a, b, c, d, k[5], 5, -701558691);
d = gg(d, a, b, c, k[10], 9,  38016083);
c = gg(c, d, a, b, k[15], 14, -660478335);
b = gg(b, c, d, a, k[4], 20, -405537848);
a = gg(a, b, c, d, k[9], 5,  568446438);
d = gg(d, a, b, c, k[14], 9, -1019803690);
c = gg(c, d, a, b, k[3], 14, -187363961);
b = gg(b, c, d, a, k[8], 20,  1163531501);
a = gg(a, b, c, d, k[13], 5, -1444681467);
d = gg(d, a, b, c, k[2], 9, -51403784);
c = gg(c, d, a, b, k[7], 14,  1735328473);
b = gg(b, c, d, a, k[12], 20, -1926607734);

a = hh(a, b, c, d, k[5], 4, -378558);
d = hh(d, a, b, c, k[8], 11, -2022574463);
c = hh(c, d, a, b, k[11], 16,  1839030562);
b = hh(b, c, d, a, k[14], 23, -35309556);
a = hh(a, b, c, d, k[1], 4, -1530992060);
d = hh(d, a, b, c, k[4], 11,  1272893353);
c = hh(c, d, a, b, k[7], 16, -155497632);
b = hh(b, c, d, a, k[10], 23, -1094730640);
a = hh(a, b, c, d, k[13], 4,  681279174);
d = hh(d, a, b, c, k[0], 11, -358537222);
c = hh(c, d, a, b, k[3], 16, -722521979);
b = hh(b, c, d, a, k[6], 23,  76029189);
a = hh(a, b, c, d, k[9], 4, -640364487);
d = hh(d, a, b, c, k[12], 11, -421815835);
c = hh(c, d, a, b, k[15], 16,  530742520);
b = hh(b, c, d, a, k[2], 23, -995338651);

a = ii(a, b, c, d, k[0], 6, -198630844);
d = ii(d, a, b, c, k[7], 10,  1126891415);
c = ii(c, d, a, b, k[14], 15, -1416354905);
b = ii(b, c, d, a, k[5], 21, -57434055);
a = ii(a, b, c, d, k[12], 6,  1700485571);
d = ii(d, a, b, c, k[3], 10, -1894986606);
c = ii(c, d, a, b, k[10], 15, -1051523);
b = ii(b, c, d, a, k[1], 21, -2054922799);
a = ii(a, b, c, d, k[8], 6,  1873313359);
d = ii(d, a, b, c, k[15], 10, -30611744);
c = ii(c, d, a, b, k[6], 15, -1560198380);
b = ii(b, c, d, a, k[13], 21,  1309151649);
a = ii(a, b, c, d, k[4], 6, -145523070);
d = ii(d, a, b, c, k[11], 10, -1120210379);
c = ii(c, d, a, b, k[2], 15,  718787259);
b = ii(b, c, d, a, k[9], 21, -343485551);

x[0] = add32(a, x[0]);
x[1] = add32(b, x[1]);
x[2] = add32(c, x[2]);
x[3] = add32(d, x[3]);

}

function cmn(q, a, b, x, s, t) {
a = add32(add32(a, q), add32(x, t));
return add32((a << s) | (a >>> (32 - s)), b);
}

function ff(a, b, c, d, x, s, t) {
return cmn((b & c) | ((~b) & d), a, b, x, s, t);
}

function gg(a, b, c, d, x, s, t) {
return cmn((b & d) | (c & (~d)), a, b, x, s, t);
}

function hh(a, b, c, d, x, s, t) {
return cmn(b ^ c ^ d, a, b, x, s, t);
}

function ii(a, b, c, d, x, s, t) {
return cmn(c ^ (b | (~d)), a, b, x, s, t);
}

function md51(s) {
var n = s.length,
state = [1732584193, -271733879, -1732584194, 271733878], i;
for (i=64; i<=s.length; i+=64) {
md5cycle(state, md5blk(s.substring(i-64, i)));
}
s = s.substring(i-64);
var tail = [0,0,0,0, 0,0,0,0, 0,0,0,0, 0,0,0,0];
for (i=0; i<s.length; i++)
tail[i>>2] |= s.charCodeAt(i) << ((i%4) << 3);
tail[i>>2] |= 0x80 << ((i%4) << 3);
if (i > 55) {
md5cycle(state, tail);
for (i=0; i<16; i++) tail[i] = 0;
}
tail[14] = n*8;
md5cycle(state, tail);
return state;
}

function md5blk(s) {
var md5blks = [], i;
for (i=0; i<64; i+=4) {
md5blks[i>>2] = s.charCodeAt(i)
+ (s.charCodeAt(i+1) << 8)
+ (s.charCodeAt(i+2) << 16)
+ (s.charCodeAt(i+3) << 24);
}
return md5blks;
}

var hex_chr = '0123456789abcdef'.split('');

function rhex(n)
{
var s='', j=0;
for(; j<4; j++)
s += hex_chr[(n >> (j * 8 + 4)) & 0x0F]
+ hex_chr[(n >> (j * 8)) & 0x0F];
return s;
}

function hex(x) {
for (var i=0; i<x.length; i++)
x[i] = rhex(x[i]);
return x.join('');
}

function md5(s) {
return hex(md51(s));
}

function add32(a, b) {
return (a + b) & 0xFFFFFFFF;
}

// Functions for dealing with arrays.

function newArr(n, x) {
    var arr = [];
    for(; n >= 0; --n) {
        arr.push(x);
    }
    return arr;
}

// Create all views at once; perhaps it's wasteful, but it's better than having
// to check for the right view at each read or write.
function newByteArr(n) {
    // Pad the thing to multiples of 8.
    var padding = 8 - n % 8;
    if(padding < 8) {
        n += padding;
    }
    var arr = {};
    var buffer = new ArrayBuffer(n);
    var views = {};
    views['i8']  = new Int8Array(buffer);
    views['i16'] = new Int16Array(buffer);
    views['i32'] = new Int32Array(buffer);
    views['w8']  = new Uint8Array(buffer);
    views['w16'] = new Uint16Array(buffer);
    views['w32'] = new Uint32Array(buffer);
    views['f32'] = new Float32Array(buffer);
    views['f64'] = new Float64Array(buffer);
    arr['b'] = buffer;
    arr['v'] = views;
    // ByteArray and Addr are the same thing, so keep an offset if we get
    // casted.
    arr['off'] = 0;
    return arr;
}

// An attempt at emulating pointers enough for ByteString and Text to be
// usable without patching the hell out of them.
// The general idea is that Addr# is a byte array with an associated offset.

function plusAddr(addr, off) {
    var newaddr = {};
    newaddr['off'] = addr['off'] + off;
    newaddr['b']   = addr['b'];
    newaddr['v']   = addr['v'];
    return newaddr;
}

function writeOffAddr(type, elemsize, addr, off, x) {
    addr['v'][type][addr.off/elemsize + off] = x;
}

function readOffAddr(type, elemsize, addr, off) {
    return addr['v'][type][addr.off/elemsize + off];
}

// Two addresses are equal if they point to the same buffer and have the same
// offset. For other comparisons, just use the offsets - nobody in their right
// mind would check if one pointer is less than another, completely unrelated,
// pointer and then act on that information anyway.
function addrEq(a, b) {
    if(a == b) {
        return true;
    }
    return a && b && a['b'] == b['b'] && a['off'] == b['off'];
}

function addrLT(a, b) {
    if(a) {
        return b && a['off'] < b['off'];
    } else {
        return (b != 0); 
    }
}

function addrGT(a, b) {
    if(b) {
        return a && a['off'] > b['off'];
    } else {
        return (a != 0);
    }
}

function withChar(f, charCode) {
    return f(String.fromCharCode(charCode)).charCodeAt(0);
}

function u_towlower(charCode) {
    return withChar(function(c) {return c.toLowerCase()}, charCode);
}

function u_towupper(charCode) {
    return withChar(function(c) {return c.toUpperCase()}, charCode);
}

var u_towtitle = u_towupper;

function u_iswupper(charCode) {
    var c = String.fromCharCode(charCode);
    return c == c.toUpperCase() && c != c.toLowerCase();
}

function u_iswlower(charCode) {
    var c = String.fromCharCode(charCode);
    return  c == c.toLowerCase() && c != c.toUpperCase();
}

function u_iswdigit(charCode) {
    return charCode >= 48 && charCode <= 57;
}

function u_iswcntrl(charCode) {
    return charCode <= 0x1f || charCode == 0x7f;
}

function u_iswspace(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(/\s/g,'') != c;
}

function u_iswalpha(charCode) {
    var c = String.fromCharCode(charCode);
    return c.replace(__hs_alphare, '') != c;
}

function u_iswalnum(charCode) {
    return u_iswdigit(charCode) || u_iswalpha(charCode);
}

function u_iswprint(charCode) {
    return !u_iswcntrl(charCode);
}

function u_gencat(c) {
    throw 'u_gencat is only supported with --full-unicode.';
}

// Regex that matches any alphabetic character in any language. Horrible thing.
var __hs_alphare = /[\u0041-\u005A\u0061-\u007A\u00AA\u00B5\u00BA\u00C0-\u00D6\u00D8-\u00F6\u00F8-\u02C1\u02C6-\u02D1\u02E0-\u02E4\u02EC\u02EE\u0370-\u0374\u0376\u0377\u037A-\u037D\u0386\u0388-\u038A\u038C\u038E-\u03A1\u03A3-\u03F5\u03F7-\u0481\u048A-\u0527\u0531-\u0556\u0559\u0561-\u0587\u05D0-\u05EA\u05F0-\u05F2\u0620-\u064A\u066E\u066F\u0671-\u06D3\u06D5\u06E5\u06E6\u06EE\u06EF\u06FA-\u06FC\u06FF\u0710\u0712-\u072F\u074D-\u07A5\u07B1\u07CA-\u07EA\u07F4\u07F5\u07FA\u0800-\u0815\u081A\u0824\u0828\u0840-\u0858\u08A0\u08A2-\u08AC\u0904-\u0939\u093D\u0950\u0958-\u0961\u0971-\u0977\u0979-\u097F\u0985-\u098C\u098F\u0990\u0993-\u09A8\u09AA-\u09B0\u09B2\u09B6-\u09B9\u09BD\u09CE\u09DC\u09DD\u09DF-\u09E1\u09F0\u09F1\u0A05-\u0A0A\u0A0F\u0A10\u0A13-\u0A28\u0A2A-\u0A30\u0A32\u0A33\u0A35\u0A36\u0A38\u0A39\u0A59-\u0A5C\u0A5E\u0A72-\u0A74\u0A85-\u0A8D\u0A8F-\u0A91\u0A93-\u0AA8\u0AAA-\u0AB0\u0AB2\u0AB3\u0AB5-\u0AB9\u0ABD\u0AD0\u0AE0\u0AE1\u0B05-\u0B0C\u0B0F\u0B10\u0B13-\u0B28\u0B2A-\u0B30\u0B32\u0B33\u0B35-\u0B39\u0B3D\u0B5C\u0B5D\u0B5F-\u0B61\u0B71\u0B83\u0B85-\u0B8A\u0B8E-\u0B90\u0B92-\u0B95\u0B99\u0B9A\u0B9C\u0B9E\u0B9F\u0BA3\u0BA4\u0BA8-\u0BAA\u0BAE-\u0BB9\u0BD0\u0C05-\u0C0C\u0C0E-\u0C10\u0C12-\u0C28\u0C2A-\u0C33\u0C35-\u0C39\u0C3D\u0C58\u0C59\u0C60\u0C61\u0C85-\u0C8C\u0C8E-\u0C90\u0C92-\u0CA8\u0CAA-\u0CB3\u0CB5-\u0CB9\u0CBD\u0CDE\u0CE0\u0CE1\u0CF1\u0CF2\u0D05-\u0D0C\u0D0E-\u0D10\u0D12-\u0D3A\u0D3D\u0D4E\u0D60\u0D61\u0D7A-\u0D7F\u0D85-\u0D96\u0D9A-\u0DB1\u0DB3-\u0DBB\u0DBD\u0DC0-\u0DC6\u0E01-\u0E30\u0E32\u0E33\u0E40-\u0E46\u0E81\u0E82\u0E84\u0E87\u0E88\u0E8A\u0E8D\u0E94-\u0E97\u0E99-\u0E9F\u0EA1-\u0EA3\u0EA5\u0EA7\u0EAA\u0EAB\u0EAD-\u0EB0\u0EB2\u0EB3\u0EBD\u0EC0-\u0EC4\u0EC6\u0EDC-\u0EDF\u0F00\u0F40-\u0F47\u0F49-\u0F6C\u0F88-\u0F8C\u1000-\u102A\u103F\u1050-\u1055\u105A-\u105D\u1061\u1065\u1066\u106E-\u1070\u1075-\u1081\u108E\u10A0-\u10C5\u10C7\u10CD\u10D0-\u10FA\u10FC-\u1248\u124A-\u124D\u1250-\u1256\u1258\u125A-\u125D\u1260-\u1288\u128A-\u128D\u1290-\u12B0\u12B2-\u12B5\u12B8-\u12BE\u12C0\u12C2-\u12C5\u12C8-\u12D6\u12D8-\u1310\u1312-\u1315\u1318-\u135A\u1380-\u138F\u13A0-\u13F4\u1401-\u166C\u166F-\u167F\u1681-\u169A\u16A0-\u16EA\u1700-\u170C\u170E-\u1711\u1720-\u1731\u1740-\u1751\u1760-\u176C\u176E-\u1770\u1780-\u17B3\u17D7\u17DC\u1820-\u1877\u1880-\u18A8\u18AA\u18B0-\u18F5\u1900-\u191C\u1950-\u196D\u1970-\u1974\u1980-\u19AB\u19C1-\u19C7\u1A00-\u1A16\u1A20-\u1A54\u1AA7\u1B05-\u1B33\u1B45-\u1B4B\u1B83-\u1BA0\u1BAE\u1BAF\u1BBA-\u1BE5\u1C00-\u1C23\u1C4D-\u1C4F\u1C5A-\u1C7D\u1CE9-\u1CEC\u1CEE-\u1CF1\u1CF5\u1CF6\u1D00-\u1DBF\u1E00-\u1F15\u1F18-\u1F1D\u1F20-\u1F45\u1F48-\u1F4D\u1F50-\u1F57\u1F59\u1F5B\u1F5D\u1F5F-\u1F7D\u1F80-\u1FB4\u1FB6-\u1FBC\u1FBE\u1FC2-\u1FC4\u1FC6-\u1FCC\u1FD0-\u1FD3\u1FD6-\u1FDB\u1FE0-\u1FEC\u1FF2-\u1FF4\u1FF6-\u1FFC\u2071\u207F\u2090-\u209C\u2102\u2107\u210A-\u2113\u2115\u2119-\u211D\u2124\u2126\u2128\u212A-\u212D\u212F-\u2139\u213C-\u213F\u2145-\u2149\u214E\u2183\u2184\u2C00-\u2C2E\u2C30-\u2C5E\u2C60-\u2CE4\u2CEB-\u2CEE\u2CF2\u2CF3\u2D00-\u2D25\u2D27\u2D2D\u2D30-\u2D67\u2D6F\u2D80-\u2D96\u2DA0-\u2DA6\u2DA8-\u2DAE\u2DB0-\u2DB6\u2DB8-\u2DBE\u2DC0-\u2DC6\u2DC8-\u2DCE\u2DD0-\u2DD6\u2DD8-\u2DDE\u2E2F\u3005\u3006\u3031-\u3035\u303B\u303C\u3041-\u3096\u309D-\u309F\u30A1-\u30FA\u30FC-\u30FF\u3105-\u312D\u3131-\u318E\u31A0-\u31BA\u31F0-\u31FF\u3400-\u4DB5\u4E00-\u9FCC\uA000-\uA48C\uA4D0-\uA4FD\uA500-\uA60C\uA610-\uA61F\uA62A\uA62B\uA640-\uA66E\uA67F-\uA697\uA6A0-\uA6E5\uA717-\uA71F\uA722-\uA788\uA78B-\uA78E\uA790-\uA793\uA7A0-\uA7AA\uA7F8-\uA801\uA803-\uA805\uA807-\uA80A\uA80C-\uA822\uA840-\uA873\uA882-\uA8B3\uA8F2-\uA8F7\uA8FB\uA90A-\uA925\uA930-\uA946\uA960-\uA97C\uA984-\uA9B2\uA9CF\uAA00-\uAA28\uAA40-\uAA42\uAA44-\uAA4B\uAA60-\uAA76\uAA7A\uAA80-\uAAAF\uAAB1\uAAB5\uAAB6\uAAB9-\uAABD\uAAC0\uAAC2\uAADB-\uAADD\uAAE0-\uAAEA\uAAF2-\uAAF4\uAB01-\uAB06\uAB09-\uAB0E\uAB11-\uAB16\uAB20-\uAB26\uAB28-\uAB2E\uABC0-\uABE2\uAC00-\uD7A3\uD7B0-\uD7C6\uD7CB-\uD7FB\uF900-\uFA6D\uFA70-\uFAD9\uFB00-\uFB06\uFB13-\uFB17\uFB1D\uFB1F-\uFB28\uFB2A-\uFB36\uFB38-\uFB3C\uFB3E\uFB40\uFB41\uFB43\uFB44\uFB46-\uFBB1\uFBD3-\uFD3D\uFD50-\uFD8F\uFD92-\uFDC7\uFDF0-\uFDFB\uFE70-\uFE74\uFE76-\uFEFC\uFF21-\uFF3A\uFF41-\uFF5A\uFF66-\uFFBE\uFFC2-\uFFC7\uFFCA-\uFFCF\uFFD2-\uFFD7\uFFDA-\uFFDC]/g;

// 2D Canvas drawing primitives.
function jsHasCtx2D(elem) {return !!elem.getContext;}
function jsGetCtx2D(elem) {return elem.getContext('2d');}
function jsBeginPath(ctx) {ctx.beginPath();}
function jsMoveTo(ctx, x, y) {ctx.moveTo(x, y);}
function jsLineTo(ctx, x, y) {ctx.lineTo(x, y);}
function jsStroke(ctx) {ctx.stroke();}
function jsFill(ctx) {ctx.fill();}
function jsRotate(ctx, radians) {ctx.rotate(radians);}
function jsTranslate(ctx, x, y) {ctx.translate(x, y);}
function jsScale(ctx, x, y) {ctx.scale(x, y);}
function jsPushState(ctx) {ctx.save();}
function jsPopState(ctx) {ctx.restore();}
function jsResetCanvas(el) {el.width = el.width;}
function jsDrawImage(ctx, img, x, y) {ctx.drawImage(img, x, y);}
function jsDrawImageClipped(ctx, img, x, y, cx, cy, cw, ch) {
    ctx.drawImage(img, cx, cy, cw, ch, x, y, cw, ch);
}
function jsDrawText(ctx, str, x, y) {ctx.fillText(str, x, y);}
function jsClip(ctx) {ctx.clip();}
function jsArc(ctx, x, y, radius, fromAngle, toAngle) {
    ctx.arc(x, y, radius, fromAngle, toAngle);
}
function jsCanvasToDataURL(el) {return el.toDataURL('image/png');}

// Simulate handles.
// When implementing new handles, remember that passed strings may be thunks,
// and so need to be evaluated before use.

function jsNewHandle(init, read, write, flush, close, seek, tell) {
    var h = {
        read: read || function() {},
        write: write || function() {},
        seek: seek || function() {},
        tell: tell || function() {},
        close: close || function() {},
        flush: flush || function() {}
    };
    init.call(h);
    return h;
}

function jsReadHandle(h, len) {return h.read(len);}
function jsWriteHandle(h, str) {return h.write(str);}
function jsFlushHandle(h) {return h.flush();}
function jsCloseHandle(h) {return h.close();}

function jsMkConWriter(op) {
    return function(str) {
        str = E(str);
        var lines = (this.buf + str).split('\n');
        for(var i = 0; i < lines.length-1; ++i) {
            op.call(console, lines[i]);
        }
        this.buf = lines[lines.length-1];
    }
}

function jsMkStdout() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.log),
        function() {console.log(this.buf); this.buf = '';}
    );
}

function jsMkStderr() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(_) {return '';},
        jsMkConWriter(console.warn),
        function() {console.warn(this.buf); this.buf = '';}
    );
}

function jsMkStdin() {
    return jsNewHandle(
        function() {this.buf = '';},
        function(len) {
            while(this.buf.length < len) {
                this.buf += prompt('[stdin]') + '\n';
            }
            var ret = this.buf.substr(0, len);
            this.buf = this.buf.substr(len);
            return ret;
        }
    );
}

var _0=function(_1){var _2=jsRound(_1);return [0,_2>>>0&255];},_3=[0,"(function(b,i){return b.getUint8(i);})"],_4=function(_5){var _6=A(_5,[_]);return E(_6);},_7=function(_8){return _4(function(_){var _=0;return eval(E(_8)[1]);});},_9=new T(function(){return _7(_3);}),_a=function(_b){var _c=new T(function(){return A(_9,[E(_b)]);});return function(_d,_){var _e=A(_c,[E(E(_d)[1]),_]);return new T(function(){return _0(_e);});};},_f=function(_g){var _h=jsRound(_g);return [0,_h];},_i=[0,"(function(b,i){return b.getInt32(i,true);})"],_j=new T(function(){return _7(_i);}),_k=function(_l){var _m=new T(function(){return A(_j,[E(_l)]);});return function(_n,_){var _o=A(_m,[E(E(_n)[1]),_]);return new T(function(){return _f(_o);});};},_p=function(_q,_r){return [1,[0,new T(function(){return [0,E(_r)[1]+4|0];}),new T(function(){return [0,_4(function(_){var _=0;return A(_k,[_q,_r,_]);})[1]];})]];},_s=function(_t,_u){var _v=E(_t);return _v[0]==0?E(_u):[1,_v[1],new T(function(){return _s(_v[2],_u);})];},_w=function(_x,_y){var _z=jsShowI(_x);return _s(fromJSStr(_z),_y);},_A=[0,41],_B=[0,40],_C=function(_D,_E,_F){return _E>=0?_w(_E,_F):_D<=6?_w(_E,_F):[1,_B,new T(function(){var _G=jsShowI(_E);return _s(fromJSStr(_G),[1,_A,_F]);})];},_H=[0],_I=function(_J){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _C(9,_J,_H);})));},_K=function(_L){var _M=E(_L)[1];return _M>>>0>1114111?_I(_M):[0,_M];},_N=function(_O,_P){var _Q=_p(_O,_P);return _Q[0]==0?[0,_Q[1]]:[1,new T(function(){var _R=E(_Q[1]);return [0,_R[1],new T(function(){return _K(_R[2]);})];})];},_S=[0,_N],_T=function(_U){return E(_U);},_V=[0,_T,_T],_W=0,_X=[0,"(function(a,x) {a.push(x);})"],_Y=new T(function(){return _7(_X);}),_Z=[0,4],_10=[0,"Int32Array"],_11=[0,"window[\'toABle\']"],_12=new T(function(){return _7(_11);}),_13=function(_14){return E(E(_14)[2]);},_15=function(_16,_17){var _18=new T(function(){return _13(_16);}),_19=new T(function(){return A(_12,[E(E(_17)[1])]);});return function(_1a){var _1b=new T(function(){return A(_19,[E(E(_1a)[1])]);});return function(_1c,_){return A(_1b,[A(_18,[_1c]),_]);};};},_1d=function(_1e){return E(E(_1e)[1]);},_1f=function(_1g){var _1h=new T(function(){return _4(function(_){var _=0;return A(_15,[_V,_10,_Z,new T(function(){return _1d(_1g);}),_]);});});return function(_1i,_){var _1j=A(_Y,[E(_1i),E(_1h),_]);return _W;};},_1k=function(_1l){return [0,_1f(new T(function(){return [0,E(_1l)[1]];}))];},_1m=[0,_S,_1k],_1n=function(_1o,_1p){return [1,[0,_1p,_H]];},_1q=function(_1r){return I_toInt(_1r)>>>0;},_1s=function(_1t){var _1u=E(_1t);return _1u[0]==0?_1u[1]>>>0:_1q(_1u[1]);},_1v=function(_1w){return [0,_1s(_1w)];},_1x=function(_1y,_1z){return [1,new T(function(){return _1v(_1y);}),_1z];},_1A=[0,1],_1B=function(_1C){return [0,_1C];},_1D=function(_1E){return [1,I_fromInt(_1E)];},_1F=function(_1G){var _1H=_1G&4294967295;return _1H<0?_1D(_1G):_1B(_1H);},_1I=[0,0],_1J=function(_1K,_1L){var _1M=E(_1K);if(!_1M[0]){var _1N=_1M[1],_1O=E(_1L);return _1O[0]==0?_1N>=_1O[1]:I_compareInt(_1O[1],_1N)<=0;}else{var _1P=_1M[1],_1Q=E(_1L);return _1Q[0]==0?I_compareInt(_1P,_1Q[1])>=0:I_compare(_1P,_1Q[1])>=0;}},_1R=function(_1S,_1T){var _1U=E(_1S);if(!_1U[0]){var _1V=_1U[1],_1W=E(_1T);return _1W[0]==0?_1V>_1W[1]:I_compareInt(_1W[1],_1V)<0;}else{var _1X=_1U[1],_1Y=E(_1T);return _1Y[0]==0?I_compareInt(_1X,_1Y[1])>0:I_compare(_1X,_1Y[1])>0;}},_1Z=function(_20,_21){var _22=E(_20);if(!_22[0]){var _23=_22[1],_24=E(_21);return _24[0]==0?_23<_24[1]:I_compareInt(_24[1],_23)>0;}else{var _25=_22[1],_26=E(_21);return _26[0]==0?I_compareInt(_25,_26[1])<0:I_compare(_25,_26[1])<0;}},_27=function(_28,_29){while(1){var _2a=E(_28);if(!_2a[0]){var _2b=_2a[1],_2c=E(_29);if(!_2c[0]){var _2d=_2c[1],_2e=addC(_2b,_2d);if(!E(_2e[2])){return [0,_2e[1]];}else{_28=[1,I_fromInt(_2b)];_29=[1,I_fromInt(_2d)];continue;}}else{_28=[1,I_fromInt(_2b)];_29=_2c;continue;}}else{var _2f=E(_29);if(!_2f[0]){_28=_2a;_29=[1,I_fromInt(_2f[1])];continue;}else{return [1,I_add(_2a[1],_2f[1])];}}}},_2g=function(_2h,_2i,_2j,_2k,_2l){if(!_1J(_2k,_1I)){var _2m=function(_2n){return !_1Z(_2n,_2l)?A(_2h,[_2n,new T(function(){return _2m(_27(_2n,_2k));})]):E(_2i);};return _2m(_2j);}else{var _2o=function(_2p){return !_1R(_2p,_2l)?A(_2h,[_2p,new T(function(){return _2o(_27(_2p,_2k));})]):E(_2i);};return _2o(_2j);}},_2q=function(_2r,_2s){return _2g(_1x,_H,_1F(_2r),_1A,_1F(_2s));},_2t=function(_2u){return E(E(_2u)[1]);},_2v=function(_2w){var _2x=jsRound(_2w);return [0,_2x>>>0];},_2y=[0,"(function(b,i){return b.getUint32(i,true);})"],_2z=new T(function(){return _7(_2y);}),_2A=function(_2B){var _2C=new T(function(){return A(_2z,[E(_2B)]);});return function(_2D,_){var _2E=A(_2C,[E(E(_2D)[1]),_]);return new T(function(){return _2v(_2E);});};},_2F=function(_2G){var _2H=new T(function(){return _2t(_2G);});return function(_2I,_2J){var _2K=_2q(1,_4(function(_){var _=0;return A(_2A,[_2I,_2J,_]);})[1]);if(!_2K[0]){return [1,[0,new T(function(){return [0,E(_2J)[1]+4|0];}),_H]];}else{var _2L=E(_2H)[1],_2M=A(_2L,[_2I,new T(function(){return [0,E(_2J)[1]+4|0];})]);if(!_2M[0]){return [0,_2M[1]];}else{var _2N=E(_2M[1]),_2O=function(_2P){var _2Q=E(_2P);if(!_2Q[0]){return _1n;}else{var _2R=new T(function(){return [0,_2O(_2Q[2])];});return function(_2S,_2T){var _2U=A(_2L,[_2S,_2T]);if(!_2U[0]){return [0,_2U[1]];}else{var _2V=E(_2U[1]),_2W=A(E(_2R)[1],[_2S,_2V[1]]);if(!_2W[0]){return E(_2W);}else{var _2X=E(_2W[1]);return [1,[0,_2X[1],[1,_2V[2],_2X[2]]]];}}};}},_2Y=A(_2O(_2K[2]),[_2I,_2N[1]]);if(!_2Y[0]){return E(_2Y);}else{var _2Z=E(_2Y[1]);return [1,[0,_2Z[1],[1,_2N[2],_2Z[2]]]];}}}};},_30=[0,function(_31,_32){var _33=A(_2F(_1m),[_31,_32]);return _33[0]==0?[0,_33[1]]:[1,new T(function(){var _34=E(_33[1]);return [0,_34[1],[0,_34[2]]];})];}],_35=unCStr("Wrong magic byte for ServerException"),_36=[0,_35],_37=function(_38,_39){return _4(function(_){var _=0;return A(_a,[_38,_39,_]);})[1]!=2?E(_36):A(E(_30)[1],[_38,new T(function(){return [0,E(_39)[1]+1|0];})]);},_3a=[0,_37],_3b=function(_3c,_3d){return [0,E(_3c)[1]+E(_3d)[1]|0];},_3e=function(_3f){var _3g=jsRound(_3f);return [0,_3g];},_3h=[0,"(function(b){return b.size;})"],_3i=new T(function(){return _7(_3h);}),_3j=function(_3k){return _4(function(_){var _=0,_3l=A(_3i,[E(_3k),_]);return new T(function(){return _3e(_3l);});});},_3m=[0,"(function(b){return new Blob([b]);})"],_3n=new T(function(){return _7(_3m);}),_3o=[0,0],_3p=[0,"(function(b,off,len){return b.slice(off,len);})"],_3q=function(_3r,_3s,_3t){var _3u=new T(function(){return _4(function(_){var _=0;return A(_3n,[E(_3t),_]);});}),_3v=E(_3r);if(!_3v){var _3w=E(_3s);return _3j(_3u)[1]<=_3w[1]?E(_3u):_4(function(_){var _=0;return A(_7,[_3p,E(_3u),E(E(_3o)[1]),E(E(_3w)[1]),_]);});}else{return _4(function(_){var _=0;return A(_7,[_3p,E(_3u),E(_3v),E(_3v+E(_3s)[1]|0),_]);});}},_3x=function(_3y,_3z){var _3A=_p(_3y,_3z);if(!_3A[0]){return [0,_3A[1]];}else{var _3B=E(_3A[1]),_3C=_3B[1],_3D=_3B[2];return [1,[0,new T(function(){return _3b(_3C,_3D);}),new T(function(){return _3q(E(_3C)[1],_3D,_3y);})]];}},_3E=[0,_3x],_3F=unCStr("Wrong magic byte for ServerReply"),_3G=[0,_3F],_3H=function(_3I,_3J){if(_4(function(_){var _=0;return A(_a,[_3I,_3J,_]);})[1]!=1){return E(_3G);}else{var _3K=new T(function(){return [0,E(_3J)[1]+1|0];}),_3L=A(E(_3E)[1],[_3I,new T(function(){return [0,E(_3K)[1]+4|0];})]);if(!_3L[0]){return [0,_3L[1]];}else{var _3M=E(_3L[1]);return [1,[0,_3M[1],[0,new T(function(){return [0,_4(function(_){var _=0;return A(_k,[_3I,_3K,_]);})[1]];}),_3M[2]]]];}}},_3N=[0,_3H],_3O=[0,0],_3P=[0,"(function(b,cb){var r=new FileReader();r.onload=function(){A(cb,[new DataView(r.result),0]);};r.readAsArrayBuffer(b);})"],_3Q=new T(function(){return _7(_3P);}),_3R=function(_3S){return [2];},_3T=function(_3U,_){while(1){var _3V=E(_3U);if(!_3V[0]){return _W;}else{var _3W=_3V[2],_3X=E(_3V[1]);switch(_3X[0]){case 0:var _3Y=A(_3X[1],[_]);_3U=_s(_3W,[1,_3Y,_H]);continue;case 1:_3U=_s(_3W,_3X[1]);continue;default:_3U=_3W;continue;}}}},_3Z=[2],_40=function(_41){return _3R(_41);},_42=function(_43,_44,_45){return [0,function(_){var _46=E(_43)[1],_47=rMV(_46),_48=E(_47);if(!_48[0]){var _=wMV(_46,[0,_48[1],new T(function(){var _49=new T(function(){return A(_45,[_W]);});return _s(_48[2],[1,[0,_44,function(_4a){return E(_49);}],_H]);})]);return _3Z;}else{var _4b=E(_48[1]);if(!_4b[0]){var _=wMV(_46,[0,_44,_H]);return new T(function(){return A(_45,[_W]);});}else{var _=wMV(_46,[1,_4b[2]]);return [1,[1,new T(function(){return A(_45,[_W]);}),[1,new T(function(){return A(_4b[1],[_44,_40]);}),_H]]];}}}];},_4c=[1,_H],_4d=function(_4e,_4f){return [0,function(_){var _4g=E(_4e)[1],_4h=rMV(_4g),_4i=E(_4h);if(!_4i[0]){var _4j=_4i[1],_4k=E(_4i[2]);if(!_4k[0]){var _=wMV(_4g,_4c);return new T(function(){return A(_4f,[_4j]);});}else{var _4l=E(_4k[1]),_=wMV(_4g,[0,_4l[1],_4k[2]]);return [1,[1,new T(function(){return A(_4f,[_4j]);}),[1,new T(function(){return A(_4l[2],[_40]);}),_H]]];}}else{var _=wMV(_4g,[1,new T(function(){return _s(_4i[1],[1,function(_4m){var _4n=new T(function(){return A(_4f,[_4m]);});return function(_4o){return E(_4n);};},_H]);})]);return _3Z;}}];},_4p=function(_4q){var _4r=new T(function(){return _3j(_4q);});return function(_4s){return [0,function(_){var _4t=nMV(_4c),_4u=[0,_4t];return [0,function(_){var _4v=A(_3Q,[E(_4q),function(_4w,_){return _3T([1,new T(function(){return _42(_4u,[0,_3O,_4r,_4w],_3R);}),_H],_);},_]);return new T(function(){return _4d(_4u,_4s);});}];}];};},_4x=function(_4y,_4z){var _4A=E(_4z);if(!_4A[0]){return [0,_H,_H];}else{var _4B=_4A[1];if(!A(_4y,[_4B])){return [0,_H,_4A];}else{var _4C=new T(function(){var _4D=_4x(_4y,_4A[2]);return [0,_4D[1],_4D[2]];});return [0,[1,_4B,new T(function(){return E(E(_4C)[1]);})],new T(function(){return E(E(_4C)[2]);})];}}},_4E=function(_4F){return A(_4F,[_W]);},_4G=function(_4H){return E(E(_4H)[1]);},_4I=[1,_W],_4J=unCStr("ServerException"),_4K=unCStr("Haste.App.Protocol"),_4L=unCStr("haste-lib-0.2.99"),_4M=new T(function(){var _4N=hs_wordToWord64(630183689),_4O=hs_wordToWord64(329570070);return [0,_4N,_4O,[0,_4N,_4O,_4L,_4K,_4J],_H];}),_4P=function(_4Q){return E(_4M);},_4R=function(_4S){return E(E(_4S)[1]);},_4T=unCStr("Maybe.fromJust: Nothing"),_4U=new T(function(){return err(_4T);}),_4V=function(_4W,_4X,_4Y){var _4Z=new T(function(){var _50=A(_4W,[_4Y]),_51=A(_4X,[new T(function(){var _52=E(_4Z);return _52[0]==0?E(_4U):E(_52[1]);})]),_53=hs_eqWord64(_50[1],_51[1]);if(!E(_53)){return [0];}else{var _54=hs_eqWord64(_50[2],_51[2]);return E(_54)==0?[0]:[1,_4Y];}});return E(_4Z);},_55=function(_56){var _57=E(_56);return _4V(_4R(_57[1]),_4P,_57[2]);},_58=[0,34],_59=unCStr("ServerException "),_5a=unCStr("Prelude.(!!): negative index\n"),_5b=new T(function(){return err(_5a);}),_5c=unCStr("Prelude.(!!): index too large\n"),_5d=new T(function(){return err(_5c);}),_5e=function(_5f,_5g){while(1){var _5h=E(_5f);if(!_5h[0]){return E(_5d);}else{var _5i=E(_5g);if(!_5i){return E(_5h[1]);}else{_5f=_5h[2];_5g=_5i-1|0;continue;}}}},_5j=unCStr("ACK"),_5k=unCStr("BEL"),_5l=unCStr("BS"),_5m=unCStr("SP"),_5n=[1,_5m,_H],_5o=unCStr("US"),_5p=[1,_5o,_5n],_5q=unCStr("RS"),_5r=[1,_5q,_5p],_5s=unCStr("GS"),_5t=[1,_5s,_5r],_5u=unCStr("FS"),_5v=[1,_5u,_5t],_5w=unCStr("ESC"),_5x=[1,_5w,_5v],_5y=unCStr("SUB"),_5z=[1,_5y,_5x],_5A=unCStr("EM"),_5B=[1,_5A,_5z],_5C=unCStr("CAN"),_5D=[1,_5C,_5B],_5E=unCStr("ETB"),_5F=[1,_5E,_5D],_5G=unCStr("SYN"),_5H=[1,_5G,_5F],_5I=unCStr("NAK"),_5J=[1,_5I,_5H],_5K=unCStr("DC4"),_5L=[1,_5K,_5J],_5M=unCStr("DC3"),_5N=[1,_5M,_5L],_5O=unCStr("DC2"),_5P=[1,_5O,_5N],_5Q=unCStr("DC1"),_5R=[1,_5Q,_5P],_5S=unCStr("DLE"),_5T=[1,_5S,_5R],_5U=unCStr("SI"),_5V=[1,_5U,_5T],_5W=unCStr("SO"),_5X=[1,_5W,_5V],_5Y=unCStr("CR"),_5Z=[1,_5Y,_5X],_60=unCStr("FF"),_61=[1,_60,_5Z],_62=unCStr("VT"),_63=[1,_62,_61],_64=unCStr("LF"),_65=[1,_64,_63],_66=unCStr("HT"),_67=[1,_66,_65],_68=[1,_5l,_67],_69=[1,_5k,_68],_6a=[1,_5j,_69],_6b=unCStr("ENQ"),_6c=[1,_6b,_6a],_6d=unCStr("EOT"),_6e=[1,_6d,_6c],_6f=unCStr("ETX"),_6g=[1,_6f,_6e],_6h=unCStr("STX"),_6i=[1,_6h,_6g],_6j=unCStr("SOH"),_6k=[1,_6j,_6i],_6l=unCStr("NUL"),_6m=[1,_6l,_6k],_6n=[0,92],_6o=unCStr("\\DEL"),_6p=unCStr("\\a"),_6q=unCStr("\\\\"),_6r=unCStr("\\SO"),_6s=unCStr("\\r"),_6t=unCStr("\\f"),_6u=unCStr("\\v"),_6v=unCStr("\\n"),_6w=unCStr("\\t"),_6x=unCStr("\\b"),_6y=function(_6z,_6A){if(_6z<=127){var _6B=E(_6z);switch(_6B){case 92:return _s(_6q,_6A);case 127:return _s(_6o,_6A);default:if(_6B<32){var _6C=E(_6B);switch(_6C){case 7:return _s(_6p,_6A);case 8:return _s(_6x,_6A);case 9:return _s(_6w,_6A);case 10:return _s(_6v,_6A);case 11:return _s(_6u,_6A);case 12:return _s(_6t,_6A);case 13:return _s(_6s,_6A);case 14:return _s(_6r,new T(function(){var _6D=E(_6A);return _6D[0]==0?[0]:E(E(_6D[1])[1])==72?unAppCStr("\\&",_6D):E(_6D);}));default:return _s([1,_6n,new T(function(){var _6E=_6C;return _6E>=0?_5e(_6m,_6E):E(_5b);})],_6A);}}else{return [1,[0,_6B],_6A];}}}else{return [1,_6n,new T(function(){var _6F=jsShowI(_6z);return _s(fromJSStr(_6F),new T(function(){var _6G=E(_6A);if(!_6G[0]){return [0];}else{var _6H=E(_6G[1])[1];return _6H<48?E(_6G):_6H>57?E(_6G):unAppCStr("\\&",_6G);}}));})];}},_6I=unCStr("\\\""),_6J=function(_6K,_6L){var _6M=E(_6K);if(!_6M[0]){return E(_6L);}else{var _6N=_6M[2],_6O=E(E(_6M[1])[1]);return _6O==34?_s(_6I,new T(function(){return _6J(_6N,_6L);})):_6y(_6O,new T(function(){return _6J(_6N,_6L);}));}},_6P=function(_6Q,_6R,_6S){return _6Q<11?_s(_59,[1,_58,new T(function(){return _6J(_6R,[1,_58,_6S]);})]):[1,_B,new T(function(){return _s(_59,[1,_58,new T(function(){return _6J(_6R,[1,_58,[1,_A,_6S]]);})]);})];},_6T=function(_6U){return _6P(0,E(_6U)[1],_H);},_6V=function(_6W,_6X){return _6P(0,E(_6W)[1],_6X);},_6Y=[0,44],_6Z=[0,93],_70=[0,91],_71=function(_72,_73,_74){var _75=E(_73);return _75[0]==0?unAppCStr("[]",_74):[1,_70,new T(function(){return A(_72,[_75[1],new T(function(){var _76=function(_77){var _78=E(_77);return _78[0]==0?E([1,_6Z,_74]):[1,_6Y,new T(function(){return A(_72,[_78[1],new T(function(){return _76(_78[2]);})]);})];};return _76(_75[2]);})]);})];},_79=function(_7a,_7b){return _71(_6V,_7a,_7b);},_7c=function(_7d,_7e,_7f){return _6P(E(_7d)[1],E(_7e)[1],_7f);},_7g=[0,_7c,_6T,_79],_7h=[0,_4P,_7g,_7i,_55],_7i=function(_7b){return [0,_7h,_7b];},_7j=function(_7k,_7l){return die(new T(function(){return A(_7l,[_7k]);}));},_7m=function(_7n){return _7j(_7n,_7i);},_7o=unCStr("WebSockets connection died for some reason!"),_7p=new T(function(){return err(_7o);}),_7q=[0,0],_7r=function(_7s,_7t){return E(_7s)[1]!=E(_7t)[1];},_7u=unCStr("Not enough data!"),_7v=[0,_7u],_7w=[0,"(function(url, cb, f, err) {var ws = new WebSocket(url);ws.binaryType = \'blob\';ws.onmessage = function(e) {A(cb,[ws,e.data,0]);};ws.onopen = function(e) {A(f,[ws,0]);};ws.onerror = function(e) {A(err,[0]);};return ws;})"],_7x=new T(function(){return _7(_7w);}),_7y=function(_7z,_7A,_7B,_7C,_7D){return [0,function(_){var _7E=nMV(_4c),_7F=[0,_7E],_7G=function(_7H){return _42(_7F,_7H,_3R);};return [0,function(_){var _7I=A(_7x,[E(toJSStr(E(_7z))),function(_7J,_7K,_){return _3T([1,new T(function(){return A(_7A,[_7J,_7K,_3R]);}),_H],_);},function(_7L,_){return _3T([1,new T(function(){return A(_7C,[_7L,_7G]);}),_H],_);},function(_){return _3T([1,new T(function(){return A(_7B,[_7G]);}),_H],_);},_]);return new T(function(){return _4d(_7F,_7D);});}];}];},_7M=function(_7N,_7O,_7P,_7Q,_){return [0,function(_){return _3T([1,[0,function(_){var _7R=nMV(_H);return [0,function(_){var _7S=nMV(_7q);return new T(function(){return _7y(new T(function(){return _4G(_7O);}),function(_7T,_7U){var _7V=new T(function(){return _4p(_7U);});return function(_7W){return A(_7V,[function(_7X){return [0,function(_){var _7Y=mMV(_7R,function(_7Z){if(!E(new T(function(){var _80=E(_7X),_81=A(E(_3a)[1],[_80[3],_80[1]]);if(!_81[0]){return E(_4I);}else{var _82=E(_81[1]);return E(_82[1])[1]>E(_80[2])[1]?E(_4I):_7m(_82[2]);}}))[0]){return [0,_7Z,_4E];}else{var _83=E(new T(function(){var _84=E(_7X),_85=A(E(_3N)[1],[_84[3],_84[1]]);if(!_85[0]){return [0,_85[1]];}else{var _86=E(_85[1]);return E(_86[1])[1]>E(_84[2])[1]?E(_7v):[1,_86[2]];}}));if(!_83[0]){return [0,_7Z,_4E];}else{var _87=E(_83[1]),_88=_4x(function(_89){return _7r(E(_89)[1],_87[1]);},_7Z),_8a=E(_88[2]);return _8a[0]==0?[0,_7Z,_4E]:[0,new T(function(){return _s(_88[1],_8a[2]);}),function(_8b){return _42(E(_8a[1])[2],_87[2],_8b);}];}}});return new T(function(){return A(_7Y,[_7W]);});}];}]);};},_7p,function(_8c){return A(_7N,[[0,_8c,[0,_7S],[0,_7R]]]);},_3R);});}];}],_H],_);},_7P,_7Q,_7O];},_8d=[0,1],_8e=[0,24601],_8f=unCStr("ws://localhost:24601"),_8g=[0,_8f,_8e,_H],_8h=function(_8i,_8j,_8k){return A(_8k,[_8i]);},_8l=function(_8m,_8n,_8o){var _8p=new T(function(){return A(_8m,[_8o]);});return function(_8q){return A(_8p,[function(_8r){return A(_8n,[_8r,_8o,_8q]);}]);};},_8s=function(_8t,_8u,_8v){var _8w=new T(function(){return A(_8t,[_8v]);});return function(_8x){var _8y=new T(function(){return A(_8u,[_8v,_8x]);});return A(_8w,[function(_8z){return E(_8y);}]);};},_8A=function(_8B,_8C,_8b){return _8s(_8B,_8C,_8b);},_8D=function(_8E){return err(_8E);},_8F=[0,_8l,_8A,_8h,_8D],_8G=function(_8H,_8I,_8J){return [0,function(_){var _8K=A(_8H,[_]);return new T(function(){return A(_8J,[_8K]);});}];},_8L=function(_8M,_8N){return A(_8M,[function(_){return jsFind(toJSStr(E(_8N)));}]);},_8O=function(_8P,_8Q){while(1){var _8R=E(_8Q);if(!_8R[0]){return false;}else{if(!A(_8P,[_8R[1]])){_8Q=_8R[2];continue;}else{return true;}}}},_8S=function(_8T){var _8U=E(_8T);return _8U[0]==0?E(_4U):E(_8U[1]);},_8V=function(_8W){return E(_8W)[0]==0?true:false;},_8X=function(_8Y,_8Z){var _90=E(_8Z);return _90[0]==0?[0]:[1,new T(function(){return A(_8Y,[_90[1]]);}),new T(function(){return _8X(_8Y,_90[2]);})];},_91=function(_92,_93){while(1){var _94=(function(_95,_96){var _97=E(_95);if(!_97[0]){return [0];}else{var _98=_97[2],_99=E(_96);if(!_99[0]){return [0];}else{var _9a=_99[2];if(!E(_99[1])[0]){return [1,_97[1],new T(function(){return _91(_98,_9a);})];}else{_92=_98;_93=_9a;return null;}}}})(_92,_93);if(_94!=null){return _94;}}},_9b=new T(function(){return unAppCStr("[]",_H);}),_9c=[1,_6Z,_H],_9d=function(_9e){var _9f=E(_9e);return _9f[0]==0?E(_9c):[1,_6Y,[1,_58,new T(function(){return _6J(_9f[1],[1,_58,new T(function(){return _9d(_9f[2]);})]);})]];},_9g=function(_9h,_9i){return err(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _9j=_91(_9i,_9h);return _9j[0]==0?E(_9b):[1,_70,[1,_58,new T(function(){return _6J(_9j[1],[1,_58,new T(function(){return _9d(_9j[2]);})]);})]];})));},_9k=function(_9l,_9m,_9n,_9o){var _9p=E(_9l),_9q=_9p[1],_9r=_9p[3];return A(_9q,[new T(function(){var _9s=new T(function(){return A(_9r,[_H]);}),_9t=function(_9u){var _9v=E(_9u);if(!_9v[0]){return E(_9s);}else{var _9w=new T(function(){return _9t(_9v[2]);});return A(_9q,[new T(function(){return _8L(_9m,_9v[1]);}),function(_9x){return A(_9q,[_9w,function(_9y){return A(_9r,[[1,_9x,_9y]]);}]);}]);}};return _9t(_9n);}),function(_9z){return !_8O(_8V,_9z)?A(_9o,[new T(function(){return _8X(_8S,_9z);})]):_9g(_9z,_9n);}]);},_9A=unCStr("output"),_9B=[1,_9A,_H],_9C=unCStr("submit"),_9D=[1,_9C,_9B],_9E=[8,coercionToken],_9F=function(_9G,_){return _W;},_9H=function(_9I,_9J){while(1){var _9K=E(_9I);if(!_9K[0]){return E(_9J);}else{_9I=_9K[2];var _9L=_9J+1|0;_9J=_9L;continue;}}},_9M=[0,"Uint32Array"],_9N=function(_9O){var _9P=new T(function(){return _4(function(_){var _=0;return A(_15,[_V,_9M,_Z,new T(function(){return _1d(_9O);}),_]);});});return function(_9Q,_){var _9R=A(_Y,[E(_9Q),E(_9P),_]);return _W;};},_9S=function(_9T){return E(E(_9T)[2]);},_9U=function(_9V,_9W){var _9X=new T(function(){return [0,_9H(_9W,0)>>>0];});return function(_9Y,_){var _9Z=A(_9N(_9X),[_9Y,_]);return A(E(new T(function(){var _a0=new T(function(){return _9S(_9V);}),_a1=function(_a2){var _a3=E(_a2);if(!_a3[0]){return _9F;}else{var _a4=new T(function(){return [0,_a1(_a3[2])];});return function(_a5,_){var _a6=A(A(_a0,[_a3[1]])[1],[_a5,_]);return A(E(_a4)[1],[_a5,_]);};}};return [0,_a1(_9W)];}))[1],[_9Y,_]);};},_a7=function(_a8){var _a9=new T(function(){return [0,_3j(_a8)[1]];});return [0,function(_aa,_){var _ab=A(_1f(_a9),[_aa,_]),_ac=A(_Y,[E(_aa),E(_a8),_]);return _W;}];},_ad=[0,_3E,_a7],_ae=[0,0],_af=[0,1],_ag=[0,"Uint8Array"],_ah=function(_ai){var _aj=new T(function(){return _4(function(_){var _=0;return A(_15,[_V,_ag,_af,new T(function(){return _1d(_ai);}),_]);});});return function(_ak,_){var _al=A(_Y,[E(_ak),E(_aj),_]);return _W;};},_am=new T(function(){return [0,_ah(_ae)];}),_an=function(_ao,_ap,_aq){var _ar=new T(function(){return [0,_1f(new T(function(){return [0,E(_ao)[1]];}))];}),_as=new T(function(){return [0,_1f(new T(function(){return [0,E(_ap)[1]];}))];}),_at=new T(function(){return [0,_9U(_ad,_aq)];});return function(_au,_){var _av=A(E(_am)[1],[_au,_]),_aw=A(E(_ar)[1],[_au,_]),_ax=A(E(_as)[1],[_au,_]);return A(E(_at)[1],[_au,_]);};},_ay=function(_az){return [0,new T(function(){return [0,E(_az)[1]+1|0];}),_az];},_aA=unCStr("Unable to decode return value!"),_aB=new T(function(){return err(_aA);}),_aC=function(_aD,_aE){while(1){var _aF=E(_aD);if(!_aF[0]){return E(_aE);}else{_aD=_aF[2];var _aG=[1,_aF[1],_aE];_aE=_aG;continue;}}},_aH=[0,"(function(){return [];})"],_aI=[0,"(function(parts){return new Blob(parts);})"],_aJ=new T(function(){return _7(_aI);}),_aK=function(_aL){return _4(function(_){var _=0,_aM=A(_7,[_aH,_]),_aN=A(_aL,[_aM,_]);return A(_aJ,[E(_aM),_]);});},_aO=[0,"(function(s, msg) {s.send(msg);})"],_aP=new T(function(){return _7(_aO);}),_aQ=function(_aR,_aS,_aT){var _aU=new T(function(){return _aC(_aT,_H);}),_aV=new T(function(){return _2t(_aR);});return function(_aW,_aX){return [0,function(_){var _aY=nMV(_4c),_aZ=[0,_aY];return [0,function(_){var _b0=E(_aW),_b1=mMV(E(_b0[2])[1],_ay);return [0,function(_){var _b2=mMV(E(_b0[3])[1],function(_b3){return [0,[1,[0,_b1,_aZ],_b3],_W];});return [0,function(_){var _b4=A(_aP,[E(_b0[1]),_aK(_an(_b1,_aS,_aU)),_]);return new T(function(){return _4d(_aZ,function(_b5){return A(_4p,[_b5,function(_b6){var _b7=E(_b6),_b8=A(E(_aV)[1],[_b7[3],_b7[1]]);if(!_b8[0]){return E(_aB);}else{var _b9=E(_b8[1]);return E(_b9[1])[1]>E(_b7[2])[1]?E(_aB):A(_aX,[_b9[2]]);}}]);});});}];}];}];}];};},_ba=function(_bb,_bc,_bd,_be){return A(_bb,[function(_){var _bf=jsSet(E(_bc)[1],toJSStr(E(_bd)),toJSStr(E(_be)));return _W;}]);},_bg=unCStr("innerHTML"),_bh=unCStr("Enter a message"),_bi=unCStr("Control.Exception.Base"),_bj=unCStr("base"),_bk=unCStr("PatternMatchFail"),_bl=new T(function(){var _bm=hs_wordToWord64(18445595),_bn=hs_wordToWord64(52003073);return [0,_bm,_bn,[0,_bm,_bn,_bj,_bi,_bk],_H];}),_bo=function(_bp){return E(_bl);},_bq=function(_br){var _bs=E(_br);return _4V(_4R(_bs[1]),_bo,_bs[2]);},_bt=function(_bu){return E(E(_bu)[1]);},_bv=function(_bw,_bx){return _s(E(_bw)[1],_bx);},_by=function(_bz,_bA){return _71(_bv,_bz,_bA);},_bB=function(_bC,_bD,_bE){return _s(E(_bD)[1],_bE);},_bF=[0,_bB,_bt,_by],_bG=[0,_bo,_bF,_bH,_bq],_bH=function(_bI){return [0,_bG,_bI];},_bJ=unCStr("Non-exhaustive patterns in"),_bK=[0,32],_bL=[0,10],_bM=[1,_bL,_H],_bN=function(_bO){return E(E(_bO)[1])==124?false:true;},_bP=function(_bQ,_bR){var _bS=_4x(_bN,unCStr(_bQ)),_bT=_bS[1],_bU=function(_bV,_bW){return _s(_bV,new T(function(){return unAppCStr(": ",new T(function(){return _s(_bR,new T(function(){return _s(_bW,_bM);}));}));}));},_bX=E(_bS[2]);return _bX[0]==0?_bU(_bT,_H):E(E(_bX[1])[1])==124?_bU(_bT,[1,_bK,_bX[2]]):_bU(_bT,_H);},_bY=function(_bZ){return _7j([0,new T(function(){return _bP(_bZ,_bJ);})],_bH);},_c0=new T(function(){return _bY("ping-pong.hs:(21,50)-(26,72)|lambda");}),_c1=function(_c2,_c3,_c4){var _c5=new T(function(){return A(_c3,[_c4]);});return function(_c6,_){return _3T([1,new T(function(){return A(_c5,[_c6,_c2,_3R]);}),_H],_);};},_c7=[0,10],_c8=[1,_c7,_H],_c9=function(_ca){return [0,_2F(_ca)];},_cb=function(_cc,_cd){return [0,_9U(_cc,_cd)];},_ce=function(_cf){return [0,new T(function(){return _c9(_cf);}),function(_cg){return _cb(_cf,_cg);}];},_ch=new T(function(){return _ce(_1m);}),_ci=[0,"keydown"],_cj=[0,"mousemove"],_ck=[0,"blur"],_cl=[0,"focus"],_cm=[0,"change"],_cn=[0,"unload"],_co=[0,"load"],_cp=[0,"keyup"],_cq=[0,"keypress"],_cr=[0,"mouseup"],_cs=[0,"mousedown"],_ct=[0,"dblclick"],_cu=[0,"click"],_cv=[0,"mouseout"],_cw=[0,"mouseover"],_cx=function(_cy){switch(E(_cy)[0]){case 0:return E(_co);case 1:return E(_cn);case 2:return E(_cm);case 3:return E(_cl);case 4:return E(_ck);case 5:return E(_cj);case 6:return E(_cw);case 7:return E(_cv);case 8:return E(_cu);case 9:return E(_ct);case 10:return E(_cs);case 11:return E(_cr);case 12:return E(_cq);case 13:return E(_cp);default:return E(_ci);}},_cz=function(_cA,_cB,_cC,_cD,_cE){var _cF=new T(function(){return _cx(_cC);}),_cG=new T(function(){return A(_cA,[_cE,_cD]);});return function(_cH){return [0,function(_){var _cI=jsSetCB(E(_cB)[1],E(_cF)[1],E(_cG));return new T(function(){return A(_cH,[_W]);});}];};},_cJ=[0,0],_cK=function(_cL){var _cM=E(_cL);if(!_cM[0]){return E(_c0);}else{var _cN=E(_cM[2]);return _cN[0]==0?E(_c0):E(_cN[2])[0]==0?function(_cO){return _cz(_c1,_cM[1],_9E,function(_cP,_cQ,_cR,_cS){return (function(_cT,_cU){return [0,function(_){var _cV=jsPrompt(toJSStr(E(_bh)));return new T(function(){return A(_aQ,[_ch,_cJ,[1,new T(function(){return _aK(_9U(_1m,new T(function(){return fromJSStr(_cV);})));}),_H],_cT,function(_cW){return [0,function(_){var _cX=E(_cN[1]),_cY=E(_bg),_cZ=jsGet(_cX[1],toJSStr(_cY));return new T(function(){return A(_ba,[_8G,_cX,_cY,new T(function(){return _s(fromJSStr(_cZ),new T(function(){return _s(fromJSStr(toJSStr(E(_cW))),_c8);}));}),_cT,_cU]);});}];}]);});}];})(_cR,_cS);},_cO);}:E(_c0);}},_d0=new T(function(){return _9k(_8F,_8G,_9D,_cK);}),_d1=unCStr("Prelude.undefined"),_d2=new T(function(){return err(_d1);}),_d3=function(_){var _d4=_7M(_d0,_8g,_8d,_d2,_);return A(E(_d4)[1],[_]);},_d5=function(_){return _d3(_);};
var hasteMain = function() {A(_d5, [0]);};window.onload = hasteMain;