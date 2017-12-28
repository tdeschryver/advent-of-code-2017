module Day02

open System

let private matrixify (input: string) =
  input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
  |> Seq.map (fun line -> Seq.map int (line.Split([|' '; '\t'|], StringSplitOptions.RemoveEmptyEntries)))

let checksumMinMax input =
  input
  |> matrixify
  |> Seq.map (fun line -> (Seq.min(line), Seq.max(line)))
  |> Seq.sumBy (fun (min, max) -> max - min)

let checksumDivisible input =
  let rec divisible (list : seq<int>) =
    let isEvenlyDivisible number elem = number % elem  = 0
    match Seq.toList (Seq.sortDescending list) with
    | [] -> 0
    | head::tail ->
      match Seq.tryFindBack (isEvenlyDivisible head) tail with
      | Some(elem) -> head/elem
      | _ -> divisible tail

  input
  |> matrixify
  |> Seq.map Seq.distinct
  |> Seq.sumBy divisible
