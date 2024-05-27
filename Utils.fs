module Utils

let combineOn f mx my =
    mx |> Result.bind (fun x -> my |> Result.bind (fun y -> Ok(f x y)))
