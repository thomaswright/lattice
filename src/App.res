let floatToColor = x =>
  switch x {
  | x if x < 0.25 => "red"
  | x if x < 0.50 => "blue"
  | x if x < 0.75 => "yellow"
  | _ => "black"
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
              top: (i * 10)->Int.toString ++ "px",
              left: (j * 10)->Int.toString ++ "px",
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

module Lattice = {
  @react.component
  let make = () => {
    let gridNum = randomInt(16, 80)
    let cutoff = random(0.1, 0.5)
    let strokeWidth = (1.3 *. (1000. /. gridNum->Int.toFloat))->Float.toInt
    let length = 2000 / gridNum
    let empty =
      Array.make(~length=gridNum, false)->Array.map(_ => Array.make(~length=gridNum, false))

    let withVals = empty->Array.map(a => a->Array.map(_ => Math.random()))
    let paths =
      withVals
      ->Array.mapWithIndex((a, i) =>
        a->Array.mapWithIndex((v, j) => {
          mod(i + j, 2) == 0 || v > cutoff || (i > gridNum / 2 || j > gridNum / 2 || i > j)
            ? None
            : Some({
                let direction = mod(j, 2) == 0 ? "h" : "v"
                let x = mod(j, 2) == 0 ? (i - 1) / 2 : i / 2
                let y = mod(i, 2) == 0 ? (j - 1) / 2 : j / 2
                `M ${(x * length - (mod(j, 2) == 0 ? strokeWidth / 2 : 0))->Int.toString},${(y *
                  length - (mod(j, 2) == 1 ? strokeWidth / 2 : 0))
                    ->Int.toString} ${direction} ${(length + strokeWidth)->Int.toString}`
              })
        })
      )
      ->Belt.Array.concatMany
      ->Belt.Array.keepMap(x => x)
      ->Array.join(" ")

    <div className="w-40 p-4">
      // <svg viewBox={`0 0 1040 1040`} xmlns="http://www.w3.org/2000/svg">
      //   <g transform={`translate(20, 20)`}>

      <svg
        viewBox={`0 0 ${(1000 + strokeWidth)->Int.toString} ${(1000 + strokeWidth)->Int.toString}`}
        xmlns="http://www.w3.org/2000/svg">
        <g
          transform={`translate(${(strokeWidth / 2)->Int.toString}, ${(strokeWidth / 2)
              ->Int.toString})`}>
          {
            let color = `oklch( ${randomInt(30, 90)->Int.toString}%  ${randomInt(
                50,
                100,
              )->Int.toString}% ${randomInt(0, 360)->Int.toString})`

            [
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
              <path
                fill="none" stroke=color strokeWidth={strokeWidth->Int.toString} d={paths} transform
              />
            })
            ->React.array
          }
        </g>
      </svg>
    </div>
  }
}

@react.component
let make = () => {
  <div className="p-6 flex flex-row flex-wrap">
    {Array.make(~length=100, false)
    ->Array.map(_ => {
      <Lattice />
    })
    ->React.array}
  </div>
}
