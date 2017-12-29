module Day09

let rec parseList (characters: char list) (level, score, garbageCount) =
  let rec garbage (garbageCharacters: char list) (level, score, garbageCount) =
    match garbageCharacters with
    | '!'::_::tail -> garbage tail (level, score, garbageCount)
    | '>'::tail -> parseList tail (level, score, garbageCount)
    | _::tail -> garbage tail (level, score, garbageCount + 1)
    | [] -> (score, garbageCount)

  match characters with
  | [] -> (score, garbageCount)
  | '{'::tail -> parseList tail (level + 1, score + level, garbageCount)
  | '}'::tail -> parseList tail (level - 1, score, garbageCount)
  | '<'::tail -> garbage tail (level, score, garbageCount)
  | ','::tail -> parseList tail (level, score, garbageCount)
  | _::tail -> parseList tail (level, score, garbageCount)

let (|Prefix|_|) (prefix:string) (tail:string) =
  if tail.StartsWith(prefix) then Some(tail.Substring(prefix.Length))
  else None

let rec parseString input (level, score, garbageCount) =
  let rec garbage input (level, score, garbageCount) =
    match input with
    | Prefix "!" tail -> garbage tail.[1..] (level, score, garbageCount)
    | Prefix ">" tail -> parseString tail (level, score, garbageCount)
    | other -> garbage other.[1..] (level, score, garbageCount + 1)

  match input with
  | "" -> (score, garbageCount)
  | Prefix "{" tail -> parseString tail (level + 1, score + level, garbageCount)
  | Prefix "}" tail -> parseString tail (level - 1, score, garbageCount)
  | Prefix "<" tail -> garbage tail (level, score, garbageCount)
  | Prefix "," tail -> parseString tail (level, score, garbageCount)
  | other -> parseString other.[1..] (level, score, garbageCount)

let solveList (input: string) =
  parseList (Array.toList (input.ToCharArray())) (1, 0, 0)

let solveString (input: string) =
  parseString input (1, 0, 0)
