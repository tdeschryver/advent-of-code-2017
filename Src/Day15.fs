module Day15

let rec generate doYield factor previous =
  let next =
    previous * factor % 2147483647UL

  seq {
    if doYield next then yield next
    yield! generate doYield factor next
  }

let ``match`` (a, b) =
  int16 a = int16 b

let finalCount (initA, initB) =
  let generatorA =
    generate (fun _ -> true) 16807UL initA
  let generatorB =
    generate (fun _ -> true) 48271UL initB

  Seq.zip generatorA generatorB
  |> Seq.take 40_000_000
  |> Seq.filter ``match``
  |> Seq.length

let finalCountPart2 (initA, initB) =
  let generatorA =
    generate (fun n -> n % 4UL = 0UL) 16807UL initA
  let generatorB =
    generate (fun n -> n % 8UL = 0UL) 48271UL initB

  Seq.zip generatorA generatorB
  |> Seq.take 5_000_000
  |> Seq.filter ``match``
  |> Seq.length
