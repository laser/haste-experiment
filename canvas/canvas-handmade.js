window.onload = function main() {
  var canvas = document.getElementById("canvas");
  var ctx    = canvas.getContext("2d");
  animate(canvas, ctx, 0);
};

function square(ctx) {
  ctx.rect(-40, -40, 80, 80);
  ctx.rect(-10, -10, 20, 20);
}

function animate(canvas, ctx, angle) {
  canvas.width = canvas.width;
  ctx.translate(70, 70);
  ctx.rotate(angle);

  ctx.beginPath();
  square(ctx);
  ctx.stroke();
  ctx.closePath();

  setTimeout(function() {
    animate(canvas, ctx, angle + 0.01);
  }, 10);
}
