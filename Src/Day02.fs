module Day02

open System

let private matrixify (input: string) =
  input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.map (fun line ->
      line.Split([|' '; '\t'|])
        |> Seq.map(fun c -> c |> int)
      )

let checksumMinMax input =
  input
    |> matrixify
    |> Seq.map (fun line -> (Seq.min(line), Seq.max(line)))
    |> Seq.sumBy (fun (min, max) -> max - min)

let checksumDivisible input =
  let rec divisble (list : List<int>) =
    let isEvenlyDivisible number elem = number % elem  = 0

    match list with
    | [] -> 0
    | head :: tail ->
      match tail |> Seq.tryFindBack (isEvenlyDivisible head) with
      | Some(elem) -> head/elem
      | _ -> divisble tail

  input
    |> matrixify
    |> Seq.map (Seq.distinct >> Seq.sortDescending >> Seq.toList)
    |> Seq.sumBy divisble
