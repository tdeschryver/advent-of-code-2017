module Day08

type Instruction =
  {
    Register: string;
    Command: string;
    Operand: int;
    CompareRegister: string;
    Comparison: string;
    Amount: int;
  }

let valueOrDefault defaultValue = function
  | Some v -> v
  | _ -> defaultValue

let compare = function
  | "==" -> (=)
  | "!=" -> (<>)
  | "<" -> (<)
  | "<=" -> (<=)
  | ">" -> (>)
  | ">=" -> (>=)
  | c -> failwith (sprintf "unknown comparison %s" c)

let command = function
  | "inc" -> (+)
  | "dec" -> (-)
  | c -> failwith (sprintf "unknown command %s" c)

let register (lines: string array) =
  lines
  |> Seq.map (fun line ->
    let parts = line.Split([| ' ' |], System.StringSplitOptions.RemoveEmptyEntries)
    {
      Register = parts.[0]
      Command = parts.[1]
      Operand = int parts.[2]
      CompareRegister = parts.[4]
      Comparison = parts.[5]
      Amount = int parts.[6]
    }
  )
  |> Seq.fold (fun (registers, history) instruction  ->
    let currentValue =
      Map.tryFind instruction.Register registers |> valueOrDefault 0
    let checkerValue =
      Map.tryFind instruction.CompareRegister registers |> valueOrDefault 0

    let newValue =
      if (compare instruction.Comparison) checkerValue instruction.Amount
      then (command instruction.Command) currentValue instruction.Operand
      else currentValue

    Map.add instruction.Register newValue registers, newValue::history
  ) (Map.empty, [])

let solvePart1 (lines: string array) =
  register lines
  |> fst
  |> Map.toSeq
  |> Seq.maxBy snd
  |> snd

let solvePart2 (lines: string array) =
  Seq.max (snd (register lines))
