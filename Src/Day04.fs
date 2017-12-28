module Day04

let private validPassphrase (input: string) key =
  input.Split([| ' '|], System.StringSplitOptions.RemoveEmptyEntries)
  |> Seq.groupBy key
  |> Seq.exists (fun (_, value) -> (Seq.length value) > 1)
  |> not

let private countValidPassphrases (input: string) key =
  input.Split([| '\n' |], System.StringSplitOptions.RemoveEmptyEntries)
  |> Array.Parallel.choose (fun line ->
      match validPassphrase line key with
      | true -> Some(line)
      | false -> None
    )
  |> Array.length

let countValidPassphrasesPart1 input =
  countValidPassphrases input (id)

let countValidPassphrasesPart2 input =
  countValidPassphrases input (Seq.sort >> System.String.Concat)
