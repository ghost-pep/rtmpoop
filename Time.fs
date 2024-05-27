module Time

type Time = uint32

let timeSize = 4

let epochStart: Time = 0u

let calculateCurTime (epoch_start: System.DateTime) : Time =
    let cur = System.DateTime.Now
    let diff = cur.Subtract(epoch_start)
    diff.TotalMilliseconds |> uint32
