module Day05

let countSteps (lines: string array) up =
  let rec count (maze: array<int>) (index: int) (totalSteps: int) =
    match index < maze.Length with
      | true ->
        let current = maze.[index]
        maze.[index] <- up current
        count maze (index + current) (totalSteps + 1)
      | false -> totalSteps

  let maze =
    lines
    |> Seq.map int
    |> Seq.toArray

  count maze 0 0

let countStepsPart1 (lines: string array) =
  countSteps lines ((+) 1)

let countStepsPart2 (lines: string array) =
  countSteps lines (fun p -> if p >= 3 then p - 1 else p + 1)

// this is extremely slow... :)
let countStepsWithHistory (lines: string array) =
  let rec count (maze: int seq) (counter: Map<int, int seq>) (index: int) (totalSteps: int) =
    match maze |> Seq.tryItem index with
    | Some(item) ->
      match counter.TryFind(index) with
      | Some(history) ->
        count maze (counter.Add(index, Seq.append [1] history)) (index + item + (Seq.sum history)) (totalSteps + 1)
      | None ->
        count maze (counter.Add(index, [1])) (index + item) (totalSteps + 1)
    | None -> totalSteps

  let init =
    Seq.map int lines

  count init Map.empty 0 0
