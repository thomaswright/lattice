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
    switch cutoff {
    // Circles / regular dist
    | x if x < 0.3 => dist((gridNum / 2, gridNum / 2), (i, j)) > gridNum / 2
    // Diamonds / Taxi cab dist
    | x if x < 0.6 => intAbs(gridNum / 2, i) + intAbs(gridNum / 2, j) > gridNum / 2
    // Squares
    | _ => false
    }
  }
}

module Lattice = {
  @react.component
  let make = () => {
    let gridNum = randomInt(12, 12)
    let cutoff = random(0.1, 0.5)
    let strokeWidth = (random(0.4, 1.8) *. (1000. /. gridNum->Int.toFloat))->Float.toInt
    let length = 2000 / gridNum
    let empty =
      Array.make(~length=gridNum, false)->Array.map(_ => Array.make(~length=gridNum, false))

    let withVals = empty->Array.map(a => a->Array.map(_ => Math.random()))
    let shapeFilter = getShapeFilter(gridNum)
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

    let l = randomInt(30, 90)
    let c = randomInt(50, 100)->its
    let h = randomInt(0, 360)
    let h2 = randomInt(0, 360)

    let bg_c = randomInt(50, 100)->its
    let bg_l = l < 50 ? randomInt(70, 95) : randomInt(10, 30)
    let bg_h = randomInt(0, 360)->its

    let color = `oklch(${l->its}% ${c}% ${h})`
    let bgColor = `oklch(${bg_l->its}% ${bg_c}% ${bg_h})`

    Texel.convert()

    let padding = (0.4 *. 1000.)->Float.toInt
    let viewBoxSize = (1000 + strokeWidth + padding)->its
    let viewBoxAdjustment = ((strokeWidth + padding) / 2)->its

    <div className="w-40 p-4">
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
  <div className="p-6 flex flex-row flex-wrap bg-black">
    {Array.make(~length=1000, false)
    ->Array.map(_ => {
      <Lattice />
    })
    ->React.array}
  </div>
}
