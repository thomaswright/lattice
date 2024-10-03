export default function downloadSvg(svg) {
  var serializer = new XMLSerializer();
  var svgString = serializer.serializeToString(svg);
  var blob = new Blob([svgString], { type: "image/svg+xml;charset=utf-8" });
  var link = document.createElement("a");
  link.href = URL.createObjectURL(blob);
  link.download = "lattice.svg";
  link.click();

  URL.revokeObjectURL(link.href);
}
