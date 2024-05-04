module Tests

open System
open Xunit
open rtmpoop.ChunkStream

let assertError x =
    match x with
    | Ok _ -> Assert.Fail()
    | Error _ -> ()

[<Fact>]
let ``1 is a deprecated version`` () =
    let actual = createVersion 1uy
    assertError actual

[<Fact>]
let ``3 is good version`` () =
    let ver = 3uy
    let (Ok actual) = createVersion ver
    let expected = Version ver
    Assert.Equal(expected, actual)

[<Fact>]
let ``printable version is not allowed`` () = 55uy |> createVersion |> assertError

[<Fact>]
let ``C1 looks good`` () =
    let C1 = createMessage1
    Assert.Equal(1528, C1.random_data.Length)
