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

function isFloatFinite(x) {
    return isFinite(x);
}

function isDoubleFinite(x) {
    return isFinite(x);
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
    return [posx - (e.target.offsetLeft || 0),
	    posy - (e.target.offsetTop || 0)];
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

var jsJSONParse = JSON.parse;

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
            return [3, arr2lst_json(obj, 0)];
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

function arr2lst_json(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, toHS(arr[elem]), new T(function() {return arr2lst_json(arr,elem+1);})]
}

function arr2lst(arr, elem) {
    if(elem >= arr.length) {
        return [0];
    }
    return [1, arr[elem], new T(function() {return arr2lst(arr,elem+1);})]
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

var _0=function(_1,_2){return A(_2,[_1]);},_3=function(_4){var _5=jsRound(_4);return [0,_5>>>0&255];},_6=new T(function(){return [0,"(function(b,i){return b.getUint8(i);})"];}),_7=function(_8){var _9=A(_8,[_]);return E(_9);},_a=function(_b){return _7(function(_){var _=0;return eval(E(_b)[1]);});},_c=new T(function(){return _a(_6);}),_d=function(_e){var _f=new T(function(){return A(_c,[E(_e)]);});return function(_g,_){var _h=A(_f,[E(E(_g)[1]),_]);return new T(function(){return _3(_h);});};},_i=function(_j){var _k=jsRound(_j);return [0,_k];},_l=new T(function(){return [0,"(function(b,i){return b.getInt32(i,true);})"];}),_m=new T(function(){return _a(_l);}),_n=function(_o){var _p=new T(function(){return A(_m,[E(_o)]);});return function(_q,_){var _r=A(_p,[E(E(_q)[1]),_]);return new T(function(){return _i(_r);});};},_s=function(_t,_u){return [1,[0,new T(function(){return [0,E(_u)[1]+4|0];}),new T(function(){return [0,_7(function(_){var _=0;return A(_n,[_t,_u,_]);})[1]];})]];},_v=function(_w,_x){var _y=E(_w);return _y[0]==0?E(_x):[1,_y[1],new T(function(){return _v(_y[2],_x);})];},_z=function(_A,_B){var _C=jsShowI(_A);return _v(fromJSStr(_C),_B);},_D=[0,41],_E=[0,40],_F=function(_G,_H,_I){return _H>=0?_z(_H,_I):_G<=6?_z(_H,_I):[1,_E,new T(function(){var _J=jsShowI(_H);return _v(fromJSStr(_J),[1,_D,_I]);})];},_K=[0],_L=function(_M){return err(unAppCStr("Prelude.chr: bad argument: ",new T(function(){return _F(9,_M,_K);})));},_N=function(_O){var _P=E(_O)[1];return _P>>>0>1114111?_L(_P):[0,_P];},_Q=function(_R,_S){var _T=_s(_R,_S);return _T[0]==0?[0,_T[1]]:[1,new T(function(){var _U=E(_T[1]);return [0,_U[1],new T(function(){return _N(_U[2]);})];})];},_V=[0,_Q],_W=0,_X=new T(function(){return [0,"(function(a,x) {a.push(x);})"];}),_Y=new T(function(){return _a(_X);}),_Z=function(_10,_11){return [0,_10,_11];},_12=function(_13){return E(_13);},_14=new T(function(){return _Z(_12,_12);}),_15=[0,4],_16=new T(function(){return [0,"Int32Array"];}),_17=function(_18){return E(E(_18)[2]);},_19=new T(function(){return [0,"window[\'toABle\']"];}),_1a=new T(function(){return _a(_19);}),_1b=function(_1c,_1d){var _1e=new T(function(){return A(_1a,[E(E(_1d)[1])]);});return function(_1f){var _1g=new T(function(){return A(_1e,[E(E(_1f)[1])]);});return function(_1h,_){return A(_1g,[A(_17,[_1c,_1h]),_]);};};},_1i=function(_1j){return E(E(_1j)[1]);},_1k=function(_1l){var _1m=new T(function(){return _7(function(_){var _=0;return A(_1b,[_14,_16,_15,new T(function(){return _1i(_1l);}),_]);});});return function(_1n,_){var _1o=A(_Y,[E(_1n),E(_1m),_]);return _W;};},_1p=function(_1q){return [0,_1k(new T(function(){return [0,E(_1q)[1]];}))];},_1r=[0,_V,_1p],_1s=function(_1t,_1u){return [1,[0,_1u,_K]];},_1v=function(_1w){return I_toInt(_1w)>>>0;},_1x=function(_1y){var _1z=E(_1y);return _1z[0]==0?_1z[1]>>>0:_1v(_1z[1]);},_1A=function(_1B){return [0,_1x(_1B)];},_1C=function(_1D,_1E){return [1,new T(function(){return _1A(_1D);}),_1E];},_1F=[0,1],_1G=function(_1H){return [0,_1H];},_1I=function(_1J){return [1,I_fromInt(_1J)];},_1K=function(_1L){var _1M=_1L&4294967295;return _1M<0?_1I(_1L):_1G(_1M);},_1N=[0,0],_1O=function(_1P,_1Q){var _1R=E(_1P);if(!_1R[0]){var _1S=_1R[1],_1T=E(_1Q);return _1T[0]==0?_1S>=_1T[1]:I_compareInt(_1T[1],_1S)<=0;}else{var _1U=_1R[1],_1V=E(_1Q);return _1V[0]==0?I_compareInt(_1U,_1V[1])>=0:I_compare(_1U,_1V[1])>=0;}},_1W=function(_1X,_1Y){var _1Z=E(_1X);if(!_1Z[0]){var _20=_1Z[1],_21=E(_1Y);return _21[0]==0?_20>_21[1]:I_compareInt(_21[1],_20)<0;}else{var _22=_1Z[1],_23=E(_1Y);return _23[0]==0?I_compareInt(_22,_23[1])>0:I_compare(_22,_23[1])>0;}},_24=function(_25,_26){var _27=E(_25);if(!_27[0]){var _28=_27[1],_29=E(_26);return _29[0]==0?_28<_29[1]:I_compareInt(_29[1],_28)>0;}else{var _2a=_27[1],_2b=E(_26);return _2b[0]==0?I_compareInt(_2a,_2b[1])<0:I_compare(_2a,_2b[1])<0;}},_2c=function(_2d,_2e){while(1){var _2f=E(_2d);if(!_2f[0]){var _2g=_2f[1],_2h=E(_2e);if(!_2h[0]){var _2i=_2h[1],_2j=addC(_2g,_2i);if(!E(_2j[2])){return [0,_2j[1]];}else{_2d=[1,I_fromInt(_2g)];_2e=[1,I_fromInt(_2i)];continue;}}else{_2d=[1,I_fromInt(_2g)];_2e=_2h;continue;}}else{var _2k=E(_2e);if(!_2k[0]){_2d=_2f;_2e=[1,I_fromInt(_2k[1])];continue;}else{return [1,I_add(_2f[1],_2k[1])];}}}},_2l=function(_2m,_2n,_2o,_2p,_2q){if(!_1O(_2p,_1N)){var _2r=function(_2s){return !_24(_2s,_2q)?A(_2m,[_2s,new T(function(){return _2r(_2c(_2s,_2p));})]):E(_2n);};return _2r(_2o);}else{var _2t=function(_2u){return !_1W(_2u,_2q)?A(_2m,[_2u,new T(function(){return _2t(_2c(_2u,_2p));})]):E(_2n);};return _2t(_2o);}},_2v=function(_2w,_2x){return _2l(_1C,_K,_1K(_2w),_1F,_1K(_2x));},_2y=function(_2z){return E(E(_2z)[1]);},_2A=function(_2B){var _2C=jsRound(_2B);return [0,_2C>>>0];},_2D=new T(function(){return [0,"(function(b,i){return b.getUint32(i,true);})"];}),_2E=new T(function(){return _a(_2D);}),_2F=function(_2G){var _2H=new T(function(){return A(_2E,[E(_2G)]);});return function(_2I,_){var _2J=A(_2H,[E(E(_2I)[1]),_]);return new T(function(){return _2A(_2J);});};},_2K=function(_2L){var _2M=new T(function(){return _2y(_2L);});return function(_2N,_2O){var _2P=_2v(1,_7(function(_){var _=0;return A(_2F,[_2N,_2O,_]);})[1]);if(!_2P[0]){return [1,[0,new T(function(){return [0,E(_2O)[1]+4|0];}),_K]];}else{var _2Q=E(_2M)[1],_2R=A(_2Q,[_2N,new T(function(){return [0,E(_2O)[1]+4|0];})]);if(!_2R[0]){return [0,_2R[1]];}else{var _2S=E(_2R[1]),_2T=function(_2U){var _2V=E(_2U);if(!_2V[0]){return _1s;}else{var _2W=new T(function(){return [0,_2T(_2V[2])];});return function(_2X,_2Y){var _2Z=A(_2Q,[_2X,_2Y]);if(!_2Z[0]){return [0,_2Z[1]];}else{var _30=E(_2Z[1]),_31=A(E(_2W)[1],[_2X,_30[1]]);if(!_31[0]){return E(_31);}else{var _32=E(_31[1]);return [1,[0,_32[1],[1,_30[2],_32[2]]]];}}};}},_33=A(_2T(_2P[2]),[_2N,_2S[1]]);if(!_33[0]){return E(_33);}else{var _34=E(_33[1]);return [1,[0,_34[1],[1,_2S[2],_34[2]]]];}}}};},_35=new T(function(){return [0,function(_36,_37){var _38=A(_2K(_1r),[_36,_37]);return _38[0]==0?[0,_38[1]]:[1,new T(function(){var _39=E(_38[1]);return [0,_39[1],[0,_39[2]]];})];}];}),_3a=unCStr("Wrong magic byte for ServerException"),_3b=[0,_3a],_3c=function(_3d,_3e){return _7(function(_){var _=0;return A(_d,[_3d,_3e,_]);})[1]!=2?E(_3b):A(E(_35)[1],[_3d,new T(function(){return [0,E(_3e)[1]+1|0];})]);},_3f=[0,_3c],_3g=function(_3h,_3i){return [0,E(_3h)[1]+E(_3i)[1]|0];},_3j=function(_3k){var _3l=jsRound(_3k);return [0,_3l];},_3m=new T(function(){return [0,"(function(b){return b.size;})"];}),_3n=new T(function(){return _a(_3m);}),_3o=function(_3p){return _7(function(_){var _=0,_3q=A(_3n,[E(_3p),_]);return new T(function(){return _3j(_3q);});});},_3r=new T(function(){return [0,"(function(b){try {return new Blob([b]);} catch (e) {return new Blob([b.buffer]);}})"];}),_3s=new T(function(){return _a(_3r);}),_3t=[0,0],_3u=new T(function(){return [0,"(function(b,off,len){return b.slice(off,len);})"];}),_3v=function(_3w,_3x,_3y){var _3z=new T(function(){return _7(function(_){var _=0;return A(_3s,[E(_3y),_]);});}),_3A=E(_3w);if(!_3A){var _3B=E(_3x);return _3o(_3z)[1]<=_3B[1]?E(_3z):_7(function(_){var _=0;return A(_a,[_3u,E(_3z),E(E(_3t)[1]),E(E(_3B)[1]),_]);});}else{return _7(function(_){var _=0;return A(_a,[_3u,E(_3z),E(_3A),E(_3A+E(_3x)[1]|0),_]);});}},_3C=function(_3D,_3E){var _3F=_s(_3D,_3E);if(!_3F[0]){return [0,_3F[1]];}else{var _3G=E(_3F[1]),_3H=_3G[1],_3I=_3G[2];return [1,[0,new T(function(){return _3g(_3H,_3I);}),new T(function(){return _3v(E(_3H)[1],_3I,_3D);})]];}},_3J=[0,_3C],_3K=unCStr("Wrong magic byte for ServerReply"),_3L=[0,_3K],_3M=function(_3N,_3O){if(_7(function(_){var _=0;return A(_d,[_3N,_3O,_]);})[1]!=1){return E(_3L);}else{var _3P=new T(function(){return [0,E(_3O)[1]+1|0];}),_3Q=A(E(_3J)[1],[_3N,new T(function(){return [0,E(_3P)[1]+4|0];})]);if(!_3Q[0]){return [0,_3Q[1]];}else{var _3R=E(_3Q[1]);return [1,[0,_3R[1],[0,new T(function(){return [0,_7(function(_){var _=0;return A(_n,[_3N,_3P,_]);})[1]];}),_3R[2]]]];}}},_3S=[0,_3M],_3T=[0,0],_3U=new T(function(){return [0,"(function(b,cb){var r=new FileReader();r.onload=function(){A(cb,[new DataView(r.result),0]);};r.readAsArrayBuffer(b);})"];}),_3V=new T(function(){return _a(_3U);}),_3W=function(_3X){return [2];},_3Y=function(_3Z,_){while(1){var _40=E(_3Z);if(!_40[0]){return _W;}else{var _41=_40[2],_42=E(_40[1]);switch(_42[0]){case 0:var _43=A(_42[1],[_]);_3Z=_v(_41,[1,_43,_K]);continue;case 1:_3Z=_v(_41,_42[1]);continue;default:_3Z=_41;continue;}}}},_44=[2],_45=function(_46){return _3W(_46);},_47=function(_48,_49,_4a){return [0,function(_){var _4b=E(_48)[1],_4c=rMV(_4b),_4d=E(_4c);if(!_4d[0]){var _=wMV(_4b,[0,_4d[1],new T(function(){var _4e=new T(function(){return A(_4a,[_W]);});return _v(_4d[2],[1,[0,_49,function(_4f){return E(_4e);}],_K]);})]);return _44;}else{var _4g=E(_4d[1]);if(!_4g[0]){var _=wMV(_4b,[0,_49,_K]);return new T(function(){return A(_4a,[_W]);});}else{var _=wMV(_4b,[1,_4g[2]]);return [1,[1,new T(function(){return A(_4a,[_W]);}),[1,new T(function(){return A(_4g[1],[_49,_45]);}),_K]]];}}}];},_4h=[1,_K],_4i=function(_4j,_4k){return [0,function(_){var _4l=E(_4j)[1],_4m=rMV(_4l),_4n=E(_4m);if(!_4n[0]){var _4o=_4n[1],_4p=E(_4n[2]);if(!_4p[0]){var _=wMV(_4l,_4h);return new T(function(){return A(_4k,[_4o]);});}else{var _4q=E(_4p[1]),_=wMV(_4l,[0,_4q[1],_4p[2]]);return [1,[1,new T(function(){return A(_4k,[_4o]);}),[1,new T(function(){return A(_4q[2],[_45]);}),_K]]];}}else{var _=wMV(_4l,[1,new T(function(){return _v(_4n[1],[1,function(_4r){var _4s=new T(function(){return A(_4k,[_4r]);});return function(_4t){return E(_4s);};},_K]);})]);return _44;}}];},_4u=function(_4v){var _4w=new T(function(){return _3o(_4v);});return function(_4x){return [0,function(_){var _4y=nMV(_4h),_4z=[0,_4y];return [0,function(_){var _4A=A(_3V,[E(_4v),function(_4B,_){return _3Y([1,new T(function(){return _47(_4z,[0,_3T,_4w,_4B],_3W);}),_K],_);},_]);return new T(function(){return _4i(_4z,_4x);});}];}];};},_4C=function(_4D,_4E){var _4F=E(_4E);if(!_4F[0]){return [0,_K,_K];}else{var _4G=_4F[1];if(!A(_4D,[_4G])){return [0,_K,_4F];}else{var _4H=new T(function(){var _4I=_4C(_4D,_4F[2]);return [0,_4I[1],_4I[2]];});return [0,[1,_4G,new T(function(){return E(E(_4H)[1]);})],new T(function(){return E(E(_4H)[2]);})];}}},_4J=function(_4K){return A(_4K,[_W]);},_4L=unCStr("WebSockets connection died for some reason!"),_4M=new T(function(){return err(_4L);}),_4N=[1,_W],_4O=unCStr("ServerException"),_4P=unCStr("Haste.App.Protocol"),_4Q=unCStr("haste-lib-0.3"),_4R=new T(function(){var _4S=hs_wordToWord64(3802377096),_4T=hs_wordToWord64(72817909);return [0,_4S,_4T,[0,_4S,_4T,_4Q,_4P,_4O],_K];}),_4U=function(_4V){return E(_4R);},_4W=function(_4X){return E(E(_4X)[1]);},_4Y=unCStr("Maybe.fromJust: Nothing"),_4Z=new T(function(){return err(_4Y);}),_50=function(_51,_52,_53){var _54=new T(function(){var _55=A(_51,[_53]),_56=A(_52,[new T(function(){var _57=E(_54);return _57[0]==0?E(_4Z):E(_57[1]);})]),_58=hs_eqWord64(_55[1],_56[1]);if(!E(_58)){return [0];}else{var _59=hs_eqWord64(_55[2],_56[2]);return E(_59)==0?[0]:[1,_53];}});return E(_54);},_5a=function(_5b){var _5c=E(_5b);return _50(_4W(_5c[1]),_4U,_5c[2]);},_5d=[0,34],_5e=unCStr("ServerException "),_5f=unCStr("Prelude.(!!): negative index\n"),_5g=new T(function(){return err(_5f);}),_5h=unCStr("Prelude.(!!): index too large\n"),_5i=new T(function(){return err(_5h);}),_5j=function(_5k,_5l){while(1){var _5m=E(_5k);if(!_5m[0]){return E(_5i);}else{var _5n=E(_5l);if(!_5n){return E(_5m[1]);}else{_5k=_5m[2];_5l=_5n-1|0;continue;}}}},_5o=unCStr("ACK"),_5p=unCStr("BEL"),_5q=unCStr("BS"),_5r=unCStr("SP"),_5s=[1,_5r,_K],_5t=unCStr("US"),_5u=[1,_5t,_5s],_5v=unCStr("RS"),_5w=[1,_5v,_5u],_5x=unCStr("GS"),_5y=[1,_5x,_5w],_5z=unCStr("FS"),_5A=[1,_5z,_5y],_5B=unCStr("ESC"),_5C=[1,_5B,_5A],_5D=unCStr("SUB"),_5E=[1,_5D,_5C],_5F=unCStr("EM"),_5G=[1,_5F,_5E],_5H=unCStr("CAN"),_5I=[1,_5H,_5G],_5J=unCStr("ETB"),_5K=[1,_5J,_5I],_5L=unCStr("SYN"),_5M=[1,_5L,_5K],_5N=unCStr("NAK"),_5O=[1,_5N,_5M],_5P=unCStr("DC4"),_5Q=[1,_5P,_5O],_5R=unCStr("DC3"),_5S=[1,_5R,_5Q],_5T=unCStr("DC2"),_5U=[1,_5T,_5S],_5V=unCStr("DC1"),_5W=[1,_5V,_5U],_5X=unCStr("DLE"),_5Y=[1,_5X,_5W],_5Z=unCStr("SI"),_60=[1,_5Z,_5Y],_61=unCStr("SO"),_62=[1,_61,_60],_63=unCStr("CR"),_64=[1,_63,_62],_65=unCStr("FF"),_66=[1,_65,_64],_67=unCStr("VT"),_68=[1,_67,_66],_69=unCStr("LF"),_6a=[1,_69,_68],_6b=unCStr("HT"),_6c=[1,_6b,_6a],_6d=[1,_5q,_6c],_6e=[1,_5p,_6d],_6f=[1,_5o,_6e],_6g=unCStr("ENQ"),_6h=[1,_6g,_6f],_6i=unCStr("EOT"),_6j=[1,_6i,_6h],_6k=unCStr("ETX"),_6l=[1,_6k,_6j],_6m=unCStr("STX"),_6n=[1,_6m,_6l],_6o=unCStr("SOH"),_6p=[1,_6o,_6n],_6q=unCStr("NUL"),_6r=[1,_6q,_6p],_6s=[0,92],_6t=unCStr("\\DEL"),_6u=unCStr("\\a"),_6v=unCStr("\\\\"),_6w=unCStr("\\SO"),_6x=unCStr("\\r"),_6y=unCStr("\\f"),_6z=unCStr("\\v"),_6A=unCStr("\\n"),_6B=unCStr("\\t"),_6C=unCStr("\\b"),_6D=function(_6E,_6F){if(_6E<=127){var _6G=E(_6E);switch(_6G){case 92:return _v(_6v,_6F);case 127:return _v(_6t,_6F);default:if(_6G<32){var _6H=E(_6G);switch(_6H){case 7:return _v(_6u,_6F);case 8:return _v(_6C,_6F);case 9:return _v(_6B,_6F);case 10:return _v(_6A,_6F);case 11:return _v(_6z,_6F);case 12:return _v(_6y,_6F);case 13:return _v(_6x,_6F);case 14:return _v(_6w,new T(function(){var _6I=E(_6F);return _6I[0]==0?[0]:E(E(_6I[1])[1])==72?unAppCStr("\\&",_6I):E(_6I);}));default:return _v([1,_6s,new T(function(){var _6J=_6H;return _6J>=0?_5j(_6r,_6J):E(_5g);})],_6F);}}else{return [1,[0,_6G],_6F];}}}else{return [1,_6s,new T(function(){var _6K=jsShowI(_6E);return _v(fromJSStr(_6K),new T(function(){var _6L=E(_6F);if(!_6L[0]){return [0];}else{var _6M=E(_6L[1])[1];return _6M<48?E(_6L):_6M>57?E(_6L):unAppCStr("\\&",_6L);}}));})];}},_6N=unCStr("\\\""),_6O=function(_6P,_6Q){var _6R=E(_6P);if(!_6R[0]){return E(_6Q);}else{var _6S=_6R[2],_6T=E(E(_6R[1])[1]);return _6T==34?_v(_6N,new T(function(){return _6O(_6S,_6Q);})):_6D(_6T,new T(function(){return _6O(_6S,_6Q);}));}},_6U=function(_6V,_6W,_6X){return _6V<11?_v(_5e,[1,_5d,new T(function(){return _6O(_6W,[1,_5d,_6X]);})]):[1,_E,new T(function(){return _v(_5e,[1,_5d,new T(function(){return _6O(_6W,[1,_5d,[1,_D,_6X]]);})]);})];},_6Y=function(_6Z){return _6U(0,E(_6Z)[1],_K);},_70=function(_71,_72){return _6U(0,E(_71)[1],_72);},_73=[0,44],_74=[0,93],_75=[0,91],_76=function(_77,_78,_79){var _7a=E(_78);return _7a[0]==0?unAppCStr("[]",_79):[1,_75,new T(function(){return A(_77,[_7a[1],new T(function(){var _7b=function(_7c){var _7d=E(_7c);return _7d[0]==0?E([1,_74,_79]):[1,_73,new T(function(){return A(_77,[_7d[1],new T(function(){return _7b(_7d[2]);})]);})];};return _7b(_7a[2]);})]);})];},_7e=function(_7f,_7g){return _76(_70,_7f,_7g);},_7h=function(_7i,_7j,_7k){return _6U(E(_7i)[1],E(_7j)[1],_7k);},_7l=[0,_7h,_6Y,_7e],_7m=new T(function(){return [0,_4U,_7l,_7n,_5a];}),_7n=function(_7g){return [0,_7m,_7g];},_7o=function(_7p,_7q){return die(new T(function(){return A(_7q,[_7p]);}));},_7r=function(_7s){return _7o(_7s,_7n);},_7t=[0,0],_7u=function(_7v,_7w){return E(_7v)[1]!=E(_7w)[1];},_7x=unCStr("Not enough data!"),_7y=[0,_7x],_7z=new T(function(){return [0,"(function(url, cb, f, err) {var ws = new WebSocket(url);ws.binaryType = \'blob\';ws.onmessage = function(e) {A(cb,[ws,e.data,0]);};ws.onopen = function(e) {A(f,[ws,0]);};ws.onerror = function(e) {A(err,[0]);};return ws;})"];}),_7A=new T(function(){return _a(_7z);}),_7B=function(_7C,_7D,_7E,_7F,_7G){return [0,function(_){var _7H=nMV(_4h),_7I=[0,_7H],_7J=function(_7K){return _47(_7I,_7K,_3W);};return [0,function(_){var _7L=A(_7A,[E(toJSStr(E(_7C))),function(_7M,_7N,_){return _3Y([1,new T(function(){return A(_7D,[_7M,_7N,_3W]);}),_K],_);},function(_7O,_){return _3Y([1,new T(function(){return A(_7F,[_7O,_7J]);}),_K],_);},function(_){return _3Y([1,new T(function(){return A(_7E,[_7J]);}),_K],_);},_]);return new T(function(){return _4i(_7I,_7G);});}];}];},_7P=new T(function(){return [0,"(function(s, msg) {s.send(msg);})"];}),_7Q=new T(function(){return _a(_7P);}),_7R=function(_7S,_7T,_7U,_7V,_){var _7W=new T(function(){return E(E(_7T)[1]);});return [0,function(_){return _3Y([1,[0,function(_){var _7X=nMV(_K);return [0,function(_){var _7Y=nMV(_7t);return [0,function(_){var _7Z=nMV([0,function(_80,_81,_82){var _83=new T(function(){return E(E(_81)[1]);});return _4i(_83,function(_84){return E(new T(function(){var _85=new T(function(){return A(_82,[_W]);});return _7B(_7W,function(_86,_87){return (function(_88,_89){return (function(_8a){var _8b=new T(function(){return _4u(_8a);});return function(_8c){return A(_8b,[function(_8d){return [0,function(_){var _8e=mMV(_7X,function(_8f){if(!E(new T(function(){var _8g=E(_8d),_8h=A(E(_3f)[1],[_8g[3],_8g[1]]);if(!_8h[0]){return E(_4N);}else{var _8i=E(_8h[1]);return E(_8i[1])[1]>E(_8g[2])[1]?E(_4N):_7r(_8i[2]);}}))[0]){return [0,_8f,_4J];}else{var _8j=E(new T(function(){var _8k=E(_8d),_8l=A(E(_3S)[1],[_8k[3],_8k[1]]);if(!_8l[0]){return [0,_8l[1]];}else{var _8m=E(_8l[1]);return E(_8m[1])[1]>E(_8k[2])[1]?E(_7y):[1,_8m[2]];}}));if(!_8j[0]){return [0,_8f,_4J];}else{var _8n=E(_8j[1]),_8o=_4C(function(_8p){return _7u(E(_8p)[1],_8n[1]);},_8f),_8q=E(_8o[2]);return _8q[0]==0?[0,_8f,_4J]:[0,new T(function(){return _v(_8o[1],_8q[2]);}),function(_87){return _47(E(_8q[1])[2],_8n[2],_87);}];}}});return new T(function(){return A(_8e,[_8c]);});}];}]);};})(_89);})(_86,_87);},_4M,_0,function(_8r){var _8s=new T(function(){return A(_7Q,[E(_8r)]);});return _47(_83,function(_8t,_8u,_8v){return [0,function(_){var _8w=A(_8s,[E(_8t),_]);return new T(function(){return A(_8v,[_W]);});}];},function(_8x){return E([0,function(_){var _8y=A(_8s,[E(_80),_]);return _85;}]);});});}));});},_K]);return new T(function(){return A(_7S,[[0,[0,_7Z],[0,_7Y],[0,_7X]],_3W]);});}];}];}],_K],_);},_7U,_7V,_7T];},_8z=[0,1],_8A=[0,24601],_8B=unCStr("ws://0.0.0.0:24601"),_8C=[0,_8B,_8A,_K],_8D=function(_8E,_8F,_8G){return A(_8G,[_8E]);},_8H=function(_8I,_8J,_8K){var _8L=new T(function(){return A(_8I,[_8K]);});return function(_8M){return A(_8L,[function(_8N){return A(_8J,[_8N,_8K,_8M]);}]);};},_8O=function(_8P,_8Q,_8R){var _8S=new T(function(){return A(_8P,[_8R]);});return function(_8T){var _8U=new T(function(){return A(_8Q,[_8R,_8T]);});return A(_8S,[function(_8V){return E(_8U);}]);};},_8W=function(_8X,_86,_87){return _8O(_8X,_86,_87);},_8Y=function(_8Z){return err(_8Z);},_90=[0,_8H,_8W,_8D,_8Y],_91=function(_92,_93,_94){return [0,function(_){var _95=A(_92,[_]);return new T(function(){return A(_94,[_95]);});}];},_96=function(_97,_98){return A(_97,[function(_){return jsFind(toJSStr(E(_98)));}]);},_99=function(_9a,_9b){while(1){var _9c=E(_9b);if(!_9c[0]){return false;}else{if(!A(_9a,[_9c[1]])){_9b=_9c[2];continue;}else{return true;}}}},_9d=function(_9e){var _9f=E(_9e);return _9f[0]==0?E(_4Z):E(_9f[1]);},_9g=function(_9h){return E(_9h)[0]==0?true:false;},_9i=function(_9j,_9k){var _9l=E(_9k);return _9l[0]==0?[0]:[1,new T(function(){return A(_9j,[_9l[1]]);}),new T(function(){return _9i(_9j,_9l[2]);})];},_9m=function(_9n,_9o){while(1){var _9p=(function(_9q,_9r){var _9s=E(_9q);if(!_9s[0]){return [0];}else{var _9t=_9s[2],_9u=E(_9r);if(!_9u[0]){return [0];}else{var _9v=_9u[2];if(!E(_9u[1])[0]){return [1,_9s[1],new T(function(){return _9m(_9t,_9v);})];}else{_9n=_9t;_9o=_9v;return null;}}}})(_9n,_9o);if(_9p!=null){return _9p;}}},_9w=new T(function(){return unAppCStr("[]",_K);}),_9x=[1,_74,_K],_9y=function(_9z){var _9A=E(_9z);return _9A[0]==0?E(_9x):[1,_73,[1,_5d,new T(function(){return _6O(_9A[1],[1,_5d,new T(function(){return _9y(_9A[2]);})]);})]];},_9B=function(_9C,_9D){return err(unAppCStr("Elements with the following IDs could not be found: ",new T(function(){var _9E=_9m(_9D,_9C);return _9E[0]==0?E(_9w):[1,_75,[1,_5d,new T(function(){return _6O(_9E[1],[1,_5d,new T(function(){return _9y(_9E[2]);})]);})]];})));},_9F=function(_9G,_9H,_9I,_9J){var _9K=E(_9G),_9L=_9K[1],_9M=_9K[3];return A(_9L,[new T(function(){var _9N=new T(function(){return A(_9M,[_K]);}),_9O=function(_9P){var _9Q=E(_9P);if(!_9Q[0]){return E(_9N);}else{var _9R=new T(function(){return _9O(_9Q[2]);});return A(_9L,[new T(function(){return _96(_9H,_9Q[1]);}),function(_9S){return A(_9L,[_9R,function(_9T){return A(_9M,[[1,_9S,_9T]]);}]);}]);}};return _9O(_9I);}),function(_9U){return !_99(_9g,_9U)?A(_9J,[new T(function(){return _9i(_9d,_9U);})]):_9B(_9U,_9I);}]);},_9V=unCStr("out"),_9W=[1,_9V,_K],_9X=unCStr("btn"),_9Y=[1,_9X,_9W],_9Z=[8,coercionToken],_a0=function(_a1){return _F(0,E(_a1)[1],_K);},_a2=function(_a3,_a4,_a5,_a6){return A(_a3,[new T(function(){return function(_){var _a7=jsSet(E(_a4)[1],toJSStr(E(_a5)),toJSStr(E(_a6)));return _W;};})]);},_a8=[0,_s],_a9=function(_aa){return [0,_1k(new T(function(){return [0,E(_aa)[1]];}))];},_ab=[0,_a8,_a9],_ac=function(_ad){var _ae=new T(function(){return [0,_3o(_ad)[1]];});return [0,function(_af,_){var _ag=A(_1k(_ae),[_af,_]),_ah=A(_Y,[E(_af),E(_ad),_]);return _W;}];},_ai=[0,_3J,_ac],_aj=[0,0],_ak=[0,1],_al=new T(function(){return [0,"Uint8Array"];}),_am=function(_an){var _ao=new T(function(){return _7(function(_){var _=0;return A(_1b,[_14,_al,_ak,new T(function(){return _1i(_an);}),_]);});});return function(_ap,_){var _aq=A(_Y,[E(_ap),E(_ao),_]);return _W;};},_ar=new T(function(){return [0,_am(_aj)];}),_as=function(_at,_){return _W;},_au=function(_av,_aw){while(1){var _ax=E(_av);if(!_ax[0]){return E(_aw);}else{_av=_ax[2];var _ay=_aw+1|0;_aw=_ay;continue;}}},_az=new T(function(){return [0,"Uint32Array"];}),_aA=function(_aB){var _aC=new T(function(){return _7(function(_){var _=0;return A(_1b,[_14,_az,_15,new T(function(){return _1i(_aB);}),_]);});});return function(_aD,_){var _aE=A(_Y,[E(_aD),E(_aC),_]);return _W;};},_aF=function(_aG){return E(E(_aG)[2]);},_aH=function(_aI,_aJ){var _aK=new T(function(){return [0,_au(_aJ,0)>>>0];});return function(_aL,_){var _aM=A(_aA(_aK),[_aL,_]);return A(E(new T(function(){var _aN=new T(function(){return _aF(_aI);}),_aO=function(_aP){var _aQ=E(_aP);if(!_aQ[0]){return _as;}else{var _aR=new T(function(){return [0,_aO(_aQ[2])];});return function(_aS,_){var _aT=A(A(_aN,[_aQ[1]])[1],[_aS,_]);return A(E(_aR)[1],[_aS,_]);};}};return [0,_aO(_aJ)];}))[1],[_aL,_]);};},_aU=function(_aV,_aW,_aX){var _aY=new T(function(){return [0,_1k(new T(function(){return [0,E(_aV)[1]];}))];}),_aZ=new T(function(){return [0,_1k(new T(function(){return [0,E(_aW)[1]];}))];}),_b0=new T(function(){return [0,_aH(_ai,_aX)];});return function(_b1,_){var _b2=A(E(_ar)[1],[_b1,_]),_b3=A(E(_aY)[1],[_b1,_]),_b4=A(E(_aZ)[1],[_b1,_]);return A(E(_b0)[1],[_b1,_]);};},_b5=function(_b6){return [0,new T(function(){return [0,E(_b6)[1]+1|0];}),_b6];},_b7=unCStr("Unable to decode return value!"),_b8=new T(function(){return err(_b7);}),_b9=function(_ba,_bb){while(1){var _bc=E(_ba);if(!_bc[0]){return E(_bb);}else{_ba=_bc[2];var _bd=[1,_bc[1],_bb];_bb=_bd;continue;}}},_be=new T(function(){return [0,"(function(){return [];})"];}),_bf=new T(function(){return [0,"(function(parts){return new Blob(parts);})"];}),_bg=new T(function(){return _a(_bf);}),_bh=function(_bi){return _7(function(_){var _=0,_bj=A(_a,[_be,_]),_bk=A(_bi,[_bj,_]);return A(_bg,[E(_bj),_]);});},_bl=function(_bm,_bn,_bo){var _bp=new T(function(){return _b9(_bo,_K);}),_bq=new T(function(){return _2y(_bm);});return function(_br,_bs){var _bt=new T(function(){return E(E(_br)[1]);});return _4i(_bt,function(_bu){return _47(_bt,_bu,function(_bv){return E([0,function(_){var _bw=nMV(_4h),_bx=[0,_bw],_by=new T(function(){return _4i(_bx,function(_bz){return A(_4u,[_bz,function(_bA){var _bB=E(_bA),_bC=A(E(_bq)[1],[_bB[3],_bB[1]]);if(!_bC[0]){return E(_b8);}else{var _bD=E(_bC[1]);return E(_bD[1])[1]>E(_bB[2])[1]?E(_b8):A(_bs,[_bD[2]]);}}]);});});return [0,function(_){var _bE=E(_br),_bF=mMV(E(_bE[2])[1],_b5);return [0,function(_){var _bG=mMV(E(_bE[3])[1],function(_bH){return [0,[1,[0,_bF,_bx],_bH],_W];});return new T(function(){return A(_bu,[new T(function(){return _bh(_aU(_bF,_bn,_bp));}),_bE,function(_bI){return E(_by);}]);});}];}];}]);});});};},_bJ=[0,0],_bK=new T(function(){return _bl(_ab,_bJ,_K);}),_bL=unCStr("Control.Exception.Base"),_bM=unCStr("base"),_bN=unCStr("PatternMatchFail"),_bO=new T(function(){var _bP=hs_wordToWord64(18445595),_bQ=hs_wordToWord64(52003073);return [0,_bP,_bQ,[0,_bP,_bQ,_bM,_bL,_bN],_K];}),_bR=function(_bS){return E(_bO);},_bT=function(_bU){var _bV=E(_bU);return _50(_4W(_bV[1]),_bR,_bV[2]);},_bW=function(_bX){return E(E(_bX)[1]);},_bY=function(_bZ,_c0){return _v(E(_bZ)[1],_c0);},_c1=function(_c2,_c3){return _76(_bY,_c2,_c3);},_c4=function(_c5,_c6,_c7){return _v(E(_c6)[1],_c7);},_c8=[0,_c4,_bW,_c1],_c9=new T(function(){return [0,_bR,_c8,_ca,_bT];}),_ca=function(_cb){return [0,_c9,_cb];},_cc=unCStr("Non-exhaustive patterns in"),_cd=[0,32],_ce=[0,10],_cf=[1,_ce,_K],_cg=function(_ch){return E(E(_ch)[1])==124?false:true;},_ci=function(_cj,_ck){var _cl=_4C(_cg,unCStr(_cj)),_cm=_cl[1],_cn=function(_co,_cp){return _v(_co,new T(function(){return unAppCStr(": ",new T(function(){return _v(_ck,new T(function(){return _v(_cp,_cf);}));}));}));},_cq=E(_cl[2]);return _cq[0]==0?_cn(_cm,_K):E(E(_cq[1])[1])==124?_cn(_cm,[1,_cd,_cq[2]]):_cn(_cm,_K);},_cr=function(_cs){return _7o([0,new T(function(){return _ci(_cs,_cc);})],_ca);},_ct=new T(function(){return _cr("client-server.hs:(8,42)-(11,38)|lambda");}),_cu=function(_cv,_cw,_cx){var _cy=new T(function(){return A(_cw,[_cx]);});return function(_cz,_){return _3Y([1,new T(function(){return A(_cy,[_cz,_cv,_3W]);}),_K],_);};},_cA=unCStr("innerHTML"),_cB=new T(function(){return [0,"keydown"];}),_cC=new T(function(){return [0,"mousemove"];}),_cD=new T(function(){return [0,"blur"];}),_cE=new T(function(){return [0,"focus"];}),_cF=new T(function(){return [0,"change"];}),_cG=new T(function(){return [0,"unload"];}),_cH=new T(function(){return [0,"load"];}),_cI=new T(function(){return [0,"keyup"];}),_cJ=new T(function(){return [0,"keypress"];}),_cK=new T(function(){return [0,"mouseup"];}),_cL=new T(function(){return [0,"mousedown"];}),_cM=new T(function(){return [0,"dblclick"];}),_cN=new T(function(){return [0,"click"];}),_cO=new T(function(){return [0,"mouseout"];}),_cP=new T(function(){return [0,"mouseover"];}),_cQ=function(_cR){switch(E(_cR)[0]){case 0:return E(_cH);case 1:return E(_cG);case 2:return E(_cF);case 3:return E(_cE);case 4:return E(_cD);case 5:return E(_cC);case 6:return E(_cP);case 7:return E(_cO);case 8:return E(_cN);case 9:return E(_cM);case 10:return E(_cL);case 11:return E(_cK);case 12:return E(_cJ);case 13:return E(_cI);default:return E(_cB);}},_cS=function(_cT,_cU,_cV,_cW,_cX){var _cY=new T(function(){return _cQ(_cV);}),_cZ=new T(function(){return A(_cT,[_cX,_cW]);});return function(_d0){return [0,function(_){var _d1=jsSetCB(E(_cU)[1],E(_cY)[1],E(_cZ));return new T(function(){return A(_d0,[_W]);});}];};},_d2=function(_d3){var _d4=E(_d3);if(!_d4[0]){return E(_ct);}else{var _d5=E(_d4[2]);return _d5[0]==0?E(_ct):E(_d5[2])[0]==0?function(_d6){return _cS(_cu,_d4[1],_9Z,function(_d7,_d8,_d9){return (function(_da){var _db=new T(function(){return A(_bK,[_da]);});return function(_dc){return A(_db,[function(_dd){return A(_a2,[_91,_d5[1],_cA,new T(function(){return _a0(_dd);}),_da,_dc]);}]);};})(_d9);},_d6);}:E(_ct);}},_de=new T(function(){return _9F(_90,_91,_9Y,_d2);}),_df=unCStr("Prelude.undefined"),_dg=new T(function(){return err(_df);}),_dh=function(_){var _di=_7R(_de,_8C,_8z,_dg,_);return A(E(_di)[1],[_]);},_dj=function(_){return _dh(_);};
var hasteMain = function() {A(_dj, [0]);};window.onload = hasteMain;