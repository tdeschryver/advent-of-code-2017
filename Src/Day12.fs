module Day12

let solve (lines: string array) (part: int) =
  let nodes =
    Array.map ((fun (line: string) ->
      line.Replace(" <-> ", ", "))
      >> (fun line -> Array.map int (line.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries)))
      >> (fun parts -> (parts.[0], Set.ofArray parts.[1..]))
    ) lines
    |> Map.ofArray

  let rec traverse seen currentNode =
    if Set.contains currentNode seen
    then seen
    else Set.fold traverse (Set.union seen (Set.singleton currentNode)) (Map.find currentNode nodes)

  match part with
  | 1 -> Set.count (traverse Set.empty 0)
  | 2 ->
    nodes
    |> Map.fold (fun (count, seen) node _  ->
      if Set.contains node seen
      then (count, seen)
      else (count + 1, traverse seen node)
    ) (0, Set.empty)
    |> fst
  | _ -> invalidArg "part" (sprintf "Part passed in was %d." part)
