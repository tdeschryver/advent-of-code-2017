module Day05

open System

let countSteps (input: string) up =
  let rec count (maze: array<int>) (index: int) (totalSteps: int) =
    match index < maze.Length with
      | true ->
        let current = maze.[index]
        maze.[index] <- up current
        count maze (index + current) (totalSteps + 1)
      | false -> totalSteps

  let maze =
    input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
      |> Seq.map (fun index -> index |> int)
      |> Seq.toArray

  count maze 0 0

let countStepsPart1 (input: string) =
  countSteps input ((+) 1)

let countStepsPart2 (input: string) =
  countSteps input (fun p -> if p >= 3 then p - 1 else p + 1)

// this is extremely slow... :)
let countStepsWithHistory (input: string) =
  let rec count (maze: seq<int>) (counter: Map<int, seq<int>>) (index: int) (totalSteps: int) =
    match maze |> Seq.tryItem index with
      | Some(item) ->
        match counter.TryFind(index) with
        | Some(history) ->
          count maze (counter.Add(index, history |> Seq.append [1])) (index + item + (history |> Seq.sum)) (totalSteps + 1)
        | None ->
          count maze (counter.Add(index, [1])) (index + item) (totalSteps + 1)
      | None -> totalSteps

  let init =
    input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
      |> Seq.map (fun line -> line |> int)

  count init Map.empty 0 0
