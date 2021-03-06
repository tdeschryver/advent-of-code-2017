module Day06

let redistrubate (input: array<int>) =
  let cycle items =
    Seq.concat (Seq.initInfinite (fun _ -> items))

  let rec redistrubateMe (banks: array<int>) (history: Map<array<int>, int>) =
    let (maxIndex, maxValue) =
      Seq.mapi (fun i p -> (i, p)) banks
      |> Seq.maxBy snd

    let newBanks =
      Seq.initInfinite (id)
      |> Seq.takeWhile (fun i -> i <= maxIndex + maxValue || i % banks.Length <> 0)
      |> Seq.map (fun i -> if i > maxIndex && i <= (maxIndex + maxValue) then 1 else 0)
      |> Seq.chunkBySize banks.Length
      |> Seq.fold (fun acc chunk ->
        Seq.zip chunk acc
        |> Seq.map (fun (x, y) -> x + y)
      ) (Seq.mapi (fun i p -> if i = maxIndex then 0 else p) banks)
      |> Seq.toArray

    if history.ContainsKey newBanks then
      let index = history.[newBanks]
      let length = Seq.length history
      (length + 1, length - index)
    else
      redistrubateMe newBanks (history.Add(newBanks, Seq.length history))

  redistrubateMe input Map.empty
