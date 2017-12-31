module Day13

let cycle items =
  Seq.initInfinite (fun _ -> items)
  |> Seq.concat

let parseLine (line : string) =
  match line.Split([| ": " |], System.StringSplitOptions.None) with
  | [| x; y; |] -> (int x, int y)
  | _ -> failwith (sprintf "Bad input: %s" line)

let getPositionAtDepth (depth, range) delay =
  [1..range-1]@[range-2..-1..0]
  |> cycle
  |> Seq.skip (depth - 1 + delay)
  |> Seq.take 2
  |> Seq.toList

let calculateSeverity delay cb lines =
  lines
  |> Array.sumBy (fun (depth, range) ->
    match getPositionAtDepth (depth, range) delay with
    | [0; 1] -> cb depth range
    | _ -> 0
  )

let severityTotal (lines: string array) =
  Array.map parseLine lines
  |> calculateSeverity 0 (*)

let savePassageBruteForce (lines: string array) =
  let parsed =
    Array.map parseLine lines

  Seq.initInfinite (fun i -> i + 1)
  |> Seq.find (fun i -> (calculateSeverity i (+) parsed) = 0)

let savePassage (lines: string array) =
  let parsed =
    Array.map parseLine lines

  let severity delay (tick, depth) =
    if (tick + delay) % (2 * (depth - 1)) = 0
    then depth * (tick + delay)
    else 0

  let rec findWhereNotCaught =
    let rec findWhereNotCaught' delay =
      match Seq.sumBy (severity delay) parsed with
      | 0 -> delay
      | _ -> findWhereNotCaught' (delay + 1)
    findWhereNotCaught' 1

  findWhereNotCaught
