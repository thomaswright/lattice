export default function downloadSvgAsPng(svg) {
  var serializer = new XMLSerializer();
  var svgString = serializer.serializeToString(svg);

  var img = new Image();

  var svgBlob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
  var url = URL.createObjectURL(svgBlob);

  img.onload = function () {
    var canvas = document.createElement("canvas");
    canvas.width = svg.clientWidth;
    canvas.height = svg.clientHeight;

    var ctx = canvas.getContext("2d");
    ctx.drawImage(img, 0, 0);

    var pngLink = document.createElement("a");
    pngLink.href = canvas.toDataURL("image/png");
    pngLink.download = "lattice.png";

    pngLink.click();

    URL.revokeObjectURL(url);
  };

  img.src = url;
}
