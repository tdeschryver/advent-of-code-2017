module Day10

let cycle items =
  Seq.concat (Seq.initInfinite (fun _ -> items))

let rec knotHash (elements: int list) (sequence: int list) currentPosition skip =
  match sequence with
  | [] -> elements
  | stepsToTake::tail ->
    let reversed =
      elements
      |> cycle
      |> Seq.skip currentPosition
      |> Seq.take stepsToTake
      |> Seq.rev
      |> Seq.mapi (fun i p -> ((currentPosition + i) % elements.Length, p))
      |> Map.ofSeq

    let newElements =
      elements
      |> List.mapi (fun i p ->
        match reversed.TryFind i with
        | Some(item) -> item
        | None -> p
      )

    knotHash newElements tail ((elements.Length + currentPosition + stepsToTake + skip) % elements.Length) (skip + 1)

let solvePart1 (elements: int list) (sequence: int list) =
  let x::y::_ =
   knotHash elements sequence 0 0
  x*y

let asHexadecimal (input: string) =
  let asci =
    List.ofSeq (Seq.map int input) @ [17; 31; 73; 47; 23;]

  knotHash [0..255] (List.collect (id) (List.replicate 64 asci)) 0 0
  |> List.chunkBySize 16
  |> List.map (List.fold (^^^) 0)
  |> List.fold (fun hex p -> hex + sprintf "%02x" p) ""
