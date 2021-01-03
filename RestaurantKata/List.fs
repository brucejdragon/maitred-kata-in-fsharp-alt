module BitsForThought.List

let sumUntil n l =
    let consume (xs, n) x =
        if n >= x then (xs, n - x)
        else (x::xs, 0)

    let (rest, remainder) = l |> List.fold consume ([], n)
    (List.rev rest, remainder)