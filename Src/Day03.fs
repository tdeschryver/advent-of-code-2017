module Day03

let manhattanDistance (input : int) =
  match input with
    | 1 -> 0
    | _ ->
      Seq.initInfinite (fun i -> i * 2 + 1)
      |> Seq.map (fun layer -> (layer, layer * layer))
      |> Seq.find (fun (_, size) -> size >= input)
      |> (fun (layer, size) ->
        let offset = size - input
        let steps = offset % (layer - 1)
        (layer - 1) / 2 + abs((layer / 2) - steps)
      )

let part2 (input : int) =
  let next (x, y) =
    match (x, y) with
      | (x, y) when y > -x && x > y -> (x, y + 1)
      | (x, y) when y > -x && y >= x -> (x - 1, y)
      | (x, y) when y <= -x && x < y -> (x, y - 1)
      | _ -> (x + 1, y)

  let rec createSpiral (spiral: Map<(int * int), int>, currentCoords, currentvalue)  =
    match (currentCoords, currentvalue) with
      | (_, value) when value > input -> value
      | ((x, y), _) ->
        let (nextX, nextY) =
          next(x, y)
        let nextValue =
          Seq.allPairs [| 1; 0; -1 |]  [| 1; 0; -1 |]
          |> Seq.sumBy (fun (xx, yy) ->
            match spiral.TryFind((xx + nextX, yy + nextY)) with
              | Some(value) -> value
              | None -> 0
          )
        createSpiral(spiral.Add((nextX, nextY), nextValue), (nextX, nextY), nextValue)

  createSpiral((Map<(int * int), int>)[(0, 0), 1], (0, 0), 1)
