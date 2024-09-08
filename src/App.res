let intToColor = x =>
  switch x {
  | x if x < 0.25 => "red"
  | x if x < 0.50 => "blue"
  | x if x < 0.75 => "yellow"
  | _ => "black"
  }

@react.component
let make = () => {
  let gridNum = 40
  let empty = Array.make(~length=gridNum, false)->Array.map(_ => Array.make(~length=gridNum, false))

  let withVals = empty->Array.map(a => a->Array.map(_ => Math.random()->intToColor))

  let reflected = withVals->Array.mapWithIndex((a, i) =>
    a->Array.mapWithIndex((_, j) => {
      let oI = i > gridNum / 2 - 1 ? gridNum - 1 - i : i
      let oJ = j > gridNum / 2 - 1 ? gridNum - 1 - j : j
      let flip = oI > oJ

      withVals->Array.getUnsafe(flip ? oI : oJ)->Array.getUnsafe(flip ? oJ : oI)
    })
  )
  let strokeWidth = 20

  let paths =
    reflected
    ->Array.mapWithIndex((a, i) =>
      a->Array.mapWithIndex((v, j) => {
        mod(i + j, 2) == 0 ||
        (v == "red" || v == "blue") ||
        (i > gridNum / 2 ||
        j > gridNum / 2 ||
        i > j)
          ? None
          : Some({
              let direction = mod(j, 2) == 0 ? "h" : "v"
              let x = mod(j, 2) == 0 ? (i - 1) / 2 : i / 2
              let y = mod(i, 2) == 0 ? (j - 1) / 2 : j / 2
              `M ${(x * 50 - (mod(j, 2) == 0 ? strokeWidth / 2 : 0))->Int.toString},${(y * 50 - (
                    mod(j, 2) == 1 ? strokeWidth / 2 : 0
                  ))->Int.toString} ${direction} ${(50 + strokeWidth)->Int.toString}`
            })
      })
    )
    ->Belt.Array.concatMany
    ->Belt.Array.keepMap(x => x)
    ->Array.join(" ")

  <div className="p-6 ">
    <div className="w-80 p-2">
      <svg viewBox="0 0 1020 1020" xmlns="http://www.w3.org/2000/svg">
        <g transform={"translate(10, 10)"}>
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
            <path
              fill="none" stroke="red" strokeWidth={strokeWidth->Int.toString} d={paths} transform
            />
          })
          ->React.array}
        </g>
      </svg>
    </div>
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
              backgroundColor: v,
            }}
          />
        })
        ->React.array
      )
      ->React.array}
    </div>
  </div>
}
