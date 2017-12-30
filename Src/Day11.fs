module Day11

// For reference: https://www.redblobgames.com/grids/hexagons/#distances-cube
let walk (directions: string) =
  let step history = function
    | "n"  -> history@[(0, 1)]
    | "ne" -> history@[(1, 0)]
    | "se" -> history@[(1, -1)]
    | "s"  -> history@[(0, -1)]
    | "sw" -> history@[(-1, 0)]
    | "nw" -> history@[(-1, 1)]
    | c -> failwith (sprintf "unknown command %s" c)

  Array.fold step [] (directions.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries))

let distance (x, y) =
  (abs(x) + abs(y) + abs((x + y))) / 2

let accumulate (x, y) (xx, yy) =
  (x + xx, y + yy)

let calculateDistance (input: string) =
  List.fold accumulate (0, 0) (walk input)
  |> distance

let calculateFurthest (input: string) =
  walk input
  |> List.fold (fun (currentCoords::history: (int * int) list) coords ->
    [(accumulate currentCoords coords)]@[currentCoords]@history
  ) [(0, 0)]
  |> List.map distance
  |> List.max
