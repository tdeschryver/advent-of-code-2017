module Day04

open System

let validPassphrase (input: string) key =
  input.Split([|' '|], StringSplitOptions.RemoveEmptyEntries)
    |> Seq.groupBy (key)
    |> Seq.exists (fun (_, value) -> (Seq.length value) > 1)
    |> not

let countValidPassphrases (input: string) key =
  input.Split([|'\n'|], StringSplitOptions.RemoveEmptyEntries)
    |> Array.Parallel.choose (fun line ->
        match validPassphrase line key with
          | true -> Some(line)
          | false -> None
      )
    |> Array.length

let countValidPassphrasesPart1 input =
  countValidPassphrases input (id)

let countValidPassphrasesPart2 input =
  countValidPassphrases input (Seq.sort >> String.Concat)
