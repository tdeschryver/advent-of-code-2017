module Day17

type State =
  { CurrentPosition: int
    Buffer: int list }
let initial =
  { CurrentPosition = 0
    Buffer = [0] }

let spinlock stepSize times findValueAfter =
  let next stepsForward { CurrentPosition = currentPosition; Buffer = buffer} value =
    let nextPosition =
      (currentPosition + stepsForward) % buffer.Length + 1
    let head, tail =
      List.splitAt nextPosition buffer
    { CurrentPosition = nextPosition; Buffer = head@[value]@tail }

  List.fold (next stepSize) initial [1..times]
  |> (fun p -> p.Buffer)
  |> List.pairwise
  |> List.find (fst >> (=) findValueAfter)
  |> snd

type StatePart2 =
  { CurrentPosition: int
    ValueAfterZero: int }
let initialPart2 =
  { CurrentPosition = 0
    ValueAfterZero = 0 }

let spinlockPart2 stepSize =
  let next stepsForward { CurrentPosition = currentPosition; ValueAfterZero = afterZero} value =
    let nextPosition = (currentPosition + stepsForward) % value
    { CurrentPosition = nextPosition + 1; ValueAfterZero = if nextPosition = 0 then value else afterZero }

  (List.fold (next stepSize) initialPart2 [1..50_000_000]).ValueAfterZero

type StatePart2Steroids =
  { Index: int
    CurrentPosition: int
    ValueAfterZero: int }
let initialPart2Steroids =
  { Index = 0
    CurrentPosition = 0
    ValueAfterZero = 0 }

let spinlockPart2OnSteroids stepSize =
  let rec spinlockOnSteroids { Index = index; CurrentPosition = position; ValueAfterZero = valueAfterZero } =
    match index with
    | i when i >= 50_000_000 -> valueAfterZero
    | i ->
      let fits = // How many steps fit between position and the next index to wrap?
        (i - position) / stepSize
      let newValue =
        i + fits + 1
      let newPosition =
        (position + (fits + 1) * (stepSize + 1) - 1) % newValue + 1
      let newValueAfterZero =
        if position = 1
        then i
        else valueAfterZero
      spinlockOnSteroids { Index = newValue; CurrentPosition = newPosition; ValueAfterZero = newValueAfterZero }

  spinlockOnSteroids initialPart2Steroids
