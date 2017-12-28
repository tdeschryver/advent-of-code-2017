module Day07

open System
open System.IO

let lines fileName =
  File.ReadAllLines(
    Path.Combine(__SOURCE_DIRECTORY__, "inputs", fileName + ".txt"))

let findBottom fileName =
  lines fileName
    |> Array.Parallel.collect (fun line ->
      line
      |> Seq.filter (fun c -> Char.IsLetter(c) || c = ' ')
      |> String.Concat
      |> (fun p -> p.Split(' '))
    )
    |> Seq.groupBy (id)
    |> Seq.find (fun (_, v) -> Seq.length v = 1)
    |> fst

let findWeight fileName =
  let parseInput =
    lines fileName
      |> Seq.map (fun line ->
        let parts = line.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)

        let key = parts.[0]
        let weight = abs (Int32.Parse(parts.[1], Globalization.NumberStyles.AllowParentheses))

        match line.IndexOf "->" with
          | -1 ->
            (key, (weight, List.empty))
          | i ->
            (key, (weight, line.[i+3..].Split([| ", " |], StringSplitOptions.RemoveEmptyEntries) |> Array.toList))
      )
      |> Map.ofSeq

  let rec createTree (tree: Map<string, int*(string list)>) currentBranch =
    let branchWeight, branchChildren =
      Map.find currentBranch tree
    let result =
      List.map (createTree tree) branchChildren
    match List.tryFind (fun (k, _) -> (not k)) result with
      | Some v ->
        v
      | None ->
        let children =
          List.map (fun (_, c) -> c) result
        let sum =
          List.map (fun (w, c) -> w + c) children

        match List.length (List.distinct sum) with
          | 2 ->
            let weights =
              sum
              |> List.groupBy id
              |> List.map (fun (k, v) -> (v |> List.length) <> 1, k)
              |> Map.ofList

            let rightWeight = weights.Item true
            let wrongWeight = weights.Item false

            let diff =
              (List.max sum) - (List.min sum)
            let wrongChildWeight =
              children
              |> List.pick (fun (k, v) ->
                if k + v = wrongWeight
                then Some(k)
                else None
              )

            if rightWeight > wrongWeight
            then (false, (branchWeight, wrongChildWeight + diff))
            else (false, (branchWeight, wrongChildWeight - diff))
          | _ -> (true, (branchWeight, List.sum sum))

  createTree parseInput (findBottom fileName) |> snd |> snd
