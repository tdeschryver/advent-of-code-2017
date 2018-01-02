module Day14

let toBinary (value: int) =
  System.Convert.ToString(value, 2).PadLeft(8, '0')

let toHash value =
  Day10.sparseHash value
  |> List.fold (fun hash p -> hash + (toBinary p)) ""

let grid input =
  Seq.map ((fun p -> sprintf "%s-%i" input p) >> toHash) [0..127]

let usedSquares input =
  grid input
  |> Seq.collect (id)
  |> Seq.filter ((=) '1')
  |> Seq.length

let regions input =
  let neighbours (x, y) =
    [ (x - 1, y); (x + 1, y); (x, y - 1); (x, y + 1); ]

  let onlyOnes (value, coords) =
    if value = '1'
    then Some(coords)
    else None

  let coordinates =
    grid input
    |> Seq.mapi (fun y line ->
      Seq.mapi (fun x value -> (value, (x, y))) line
      |> Seq.choose onlyOnes
    )
    |> Seq.collect (id)
    |> Set.ofSeq

  let rec removeGroup values start =
    start
    |> neighbours
    |> List.filter (fun p -> Set.contains p values)
    |> List.fold removeGroup (Set.remove start values)

  let rec groups values =
    if Set.isEmpty values
    then 0
    else 1 + groups (removeGroup values (Set.minElement values))

  groups coordinates
