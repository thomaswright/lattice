let floatToColor = x =>
  switch x {
  | x if x < 0.25 => "red"
  | x if x < 0.50 => "blue"
  | x if x < 0.75 => "yellow"
  | _ => "black"
  }

let its = Int.toString

module Texel = {
  type triple = (float, float, float)
  type texelType

  @module("@texel/color") external okhsv: texelType = "OKHSV"
  @module("@texel/color") external okhsl: texelType = "OKHSL"
  @module("@texel/color") external oklch: texelType = "OKLCH"
  @module("@texel/color") external srgb: texelType = "sRGB"

  @module("@texel/color") external rgbToHex: triple => string = "RGBToHex"
  @module("@texel/color") external hexToRgb: string => triple = "hexToRGB"

  @module("@texel/color") external convert: (triple, texelType, texelType) => triple = "convert"
  @module("@texel/color") external isRGBInGamut: triple => bool = "isRGBInGamut"
}

module Pixels = {
  @react.component
  let make = () => {
    let gridNum = 40
    let empty =
      Array.make(~length=gridNum, false)->Array.map(_ => Array.make(~length=gridNum, false))

    let withVals = empty->Array.map(a => a->Array.map(_ => Math.random()))

    let reflected = withVals->Array.mapWithIndex((a, i) =>
      a->Array.mapWithIndex((_, j) => {
        let oI = i > gridNum / 2 - 1 ? gridNum - 1 - i : i
        let oJ = j > gridNum / 2 - 1 ? gridNum - 1 - j : j
        let flip = oI > oJ

        withVals->Array.getUnsafe(flip ? oI : oJ)->Array.getUnsafe(flip ? oJ : oI)
      })
    )

    <div className="relative">
      {reflected
      ->Array.mapWithIndex((a, i) =>
        a
        ->Array.mapWithIndex((v, j) => {
          <div
            className=" absolute"
            style={{
              top: (i * 10)->its ++ "px",
              left: (j * 10)->its ++ "px",
              width: "9px",
              height: "9px",
              backgroundColor: v->floatToColor,
            }}
          />
        })
        ->React.array
      )
      ->React.array}
    </div>
  }
}

let random = (a, b) => {
  Math.random() *. (b -. a) +. a
}

let randomInt = (a, b) => {
  (Math.random() *. (b->Int.toFloat -. a->Int.toFloat) +. a->Int.toFloat)->Float.toInt
}

let intAbs = (a, b) => {
  a > b ? a - b : b - a
}

let dist = ((x1, y1), (x2, y2)) => {
  let dx = (x2 - x1)->Int.toFloat
  let dy = (y2 - y1)->Int.toFloat
  Math.sqrt(dx *. dx +. dy *. dy)->Float.toInt
}

let getShapeFilter = gridNum => {
  let cutoff = Math.random()

  (i, j) => {
    let inCircle = dist((gridNum / 2, gridNum / 2), (i, j)) > gridNum / 2
    let inDiamond = intAbs(gridNum / 2, i) + intAbs(gridNum / 2, j) > gridNum / 2
    switch cutoff {
    | x if x < 0.1 => !inCircle
    | x if x < 0.2 => !inDiamond
    | x if x < 0.4 => inCircle
    | x if x < 0.6 => inDiamond
    | _ => false
    }
  }
}

let getColorPair = () => {
  let h1 = random(0., 360.)
  let h2 = random(0., 360.)

  let flipD = Math.random() > 0.5

  let v1 = random(flipD ? 0.6 : 0.8, 1.0)
  let v2 = random(0.0, flipD ? 0.2 : 0.4)

  let c1 = random(0.8, 1.0)
  let c2 = random(0.8, 1.0)

  let flipV = Math.random() > 0.5
  let flipSpace = Math.random() > 0.5

  let color1 =
    Texel.convert(
      (h1, c1, flipV ? v1 : v2),
      flipSpace ? Texel.okhsl : Texel.okhsv,
      Texel.srgb,
    )->Texel.rgbToHex
  let color2 =
    Texel.convert(
      (h2, c2, flipV ? v2 : v1),
      flipSpace ? Texel.okhsl : Texel.okhsv,
      Texel.srgb,
    )->Texel.rgbToHex

  (color1, color2, flipV)
}

module Lattice = {
  @react.component
  let make = () => {
    let gridNum = randomInt(8, 80)
    let cutoff = random(0.1, 0.5)
    let strokeWidth = (random(0.4, 1.8) *. (1000. /. gridNum->Int.toFloat))->Float.toInt
    let length = 2000 / gridNum
    let empty =
      Array.make(~length=gridNum, false)->Array.map(_ => Array.make(~length=gridNum, false))

    let withVals = empty->Array.map(a => a->Array.map(_ => Math.random()))
    let shapeFilter = getShapeFilter(gridNum)
    let id =
      withVals
      ->Array.mapWithIndex((a, i) =>
        a->Array.mapWithIndex((v, j) => {
          mod(i + j, 2) == 0 ||
          v > cutoff ||
          (i > gridNum / 2 || j > gridNum / 2 || i > j) ||
          shapeFilter(i, j)
            ? None
            : Some((i, j))
        })
      )
      ->Belt.Array.concatMany

    let paths =
      withVals
      ->Array.mapWithIndex((a, i) =>
        a->Array.mapWithIndex((v, j) => {
          mod(i + j, 2) == 0 ||
          v > cutoff ||
          (i > gridNum / 2 || j > gridNum / 2 || i > j) ||
          shapeFilter(i, j)
            ? None
            : Some({
                let direction = mod(j, 2) == 0 ? "h" : "v"
                let x = mod(j, 2) == 0 ? (i - 1) / 2 : i / 2
                let y = mod(i, 2) == 0 ? (j - 1) / 2 : j / 2
                `M ${(x * length - (mod(j, 2) == 0 ? strokeWidth / 2 : 0))->its},${(y * length - (
                      mod(j, 2) == 1 ? strokeWidth / 2 : 0
                    ))->its} ${direction} ${(length + strokeWidth)->its}`
              })
        })
      )
      ->Belt.Array.concatMany
      ->Belt.Array.keepMap(x => x)
      ->Array.join(" ")

    let (color, bgColor, _isDark) = getColorPair()

    let padding = (random(0.2, 0.5) *. 1000.)->Float.toInt
    let viewBoxSize = (1000 + strokeWidth + padding)->its
    let viewBoxAdjustment = ((strokeWidth + padding) / 2)->its

    paths == ""
      ? React.null
      : <div className="w-40 m-6">
          // <svg viewBox={`0 0 1040 1040`} xmlns="http://www.w3.org/2000/svg">
          //   <g transform={`translate(20, 20)`}>

          <svg viewBox={`0 0 ${viewBoxSize} ${viewBoxSize}`} xmlns="http://www.w3.org/2000/svg">
            <rect x="0" y="0" width={viewBoxSize} height={viewBoxSize} fill=bgColor />
            <g transform={`translate(${viewBoxAdjustment}, ${viewBoxAdjustment})`}>
              {[
                "",
                "rotate(90, 500, 500)",
                "rotate(180, 500, 500)",
                "rotate(270, 500, 500)",
                "rotate(90, 0, 0) scale(1,-1)",
                "rotate(90, 0, 0) scale(1,-1) rotate(90, 500, 500)",
                "rotate(90, 0, 0) scale(1,-1) rotate(180, 500, 500)",
                "rotate(90, 0, 0) scale(1,-1) rotate(270, 500, 500)",
              ]
              ->Array.map(transform => {
                <path fill="none" stroke=color strokeWidth={strokeWidth->its} d={paths} transform />
              })
              ->React.array}
            </g>
          </svg>
        </div>
  }
}

@react.component
let make = () => {
  <div>
    <div
      className="flex flex-col items-center justify-center text-gray-900 border-gray-900 bg-gray-100 py-8 ">
      <div
        className="font-thin font-serif uppercase text-5xl mb-4 border-4 border-gray-900 w-fit px-8 py-4"
        style={{letterSpacing: "0.2em"}}>
        {"Lattice"->React.string}
      </div>
      <div className="uppercase text-sm text-gray-900">
        {"A generative art project by "->React.string}
        <a className={" font-black text-gray-900"} href={"https://github.com/thomaswright/lattice"}>
          {"Thomas Wright"->React.string}
        </a>
      </div>
    </div>
    <div className="p-6 flex flex-row flex-wrap bg-gray-100 justify-center">
      {Array.make(~length=1000, false)
      ->Array.map(_ => {
        <Lattice />
      })
      ->React.array}
    </div>
  </div>
}
