module Utils

let combineOn f mx my =
    mx |> Result.bind (fun x -> my |> Result.bind (fun y -> f x y))

let appendOk x y = Array.append x y |> Ok
