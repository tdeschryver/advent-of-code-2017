module Day16

type Command =
  | Spin of int
  | Exchange of int * int
  | Partner of char * char

let shutUpAndDance (programs: string, moves: string) cycles =
  let parse (command: string) =
    match command.[0] with
    | 's' -> Spin (int command.[1..])
    | 'x' -> Exchange (command.[1..].Split('/') |> (fun p -> (int p.[0], int p.[1])))
    | 'p' -> Partner (command.[1], command.[3])
    |  c  -> failwith (sprintf "unknown command %c" c)

  let swap (i, j) (line: char list) =
    List.mapi (fun i' j' -> if i' = i then line.[j] elif i' = j then line.[i] else j') line

  let makeMove line = function
    | Spin p ->
      (List.skip (programs.Length - p) line) @ (List.take (programs.Length - p) line)
    | Exchange (a, b) ->
      swap(a, b) line
    | Partner (a, b) ->
      swap((List.findIndex ((=) a) line), (List.findIndex ((=)b) line)) line

  let rec dance line moves cycles =
    let rec dance' line' seen =
      match List.length seen with
      | length when length = cycles -> line'
      | length when List.contains line' seen -> List.item (cycles % (cycles - cycles - length)) seen
      | _ -> dance' (Seq.fold makeMove line' moves) (seen@[line'])
    dance' line List.empty

  let movesParsed =
    Array.map parse (moves.Split([| ',' |], System.StringSplitOptions.RemoveEmptyEntries))

  dance (List.ofSeq programs) movesParsed cycles
  |> Seq.map string
  |> String.concat ""
