module Tests.Util

let createPath fileName =
  System.IO.Path.Combine(__SOURCE_DIRECTORY__, "inputs", fileName + ".txt")

let readText fileName =
  System.IO.File.ReadAllText(createPath fileName)

let readAllLines fileName =
  System.IO.File.ReadAllLines(createPath fileName)
