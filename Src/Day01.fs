module Day01

let private inverseCaptcha offset (input : string) =
  input
  |> Seq.mapi (fun i c -> c, input.[(i + offset) % input.Length])
  |> Seq.filter (fun (a, b) -> a = b)
  |> Seq.sumBy (fst >> System.Char.GetNumericValue >> int)

let inverseCaptchaPart1 =
  inverseCaptcha 1

let inverseCaptchaPart2 (input : string) =
  inverseCaptcha (input.Length / 2) input
