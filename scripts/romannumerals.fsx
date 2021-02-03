let charValue ch =
    match ch with
    | 'I' -> 1
    | 'V' -> 5
    | 'X' -> 10
    | 'L' -> 50
    | 'C' -> 100
    | 'D' -> 500
    | 'M' -> 1000
    | _ -> failwith $"Unsupported character {ch}"

let rec calculate values =
    match values with
    | [ x ] -> x
    | [ x; y ] when x < y -> y - x
    | [ x; y ] -> x + y
    | x :: y :: rest when x < y -> y - x + (calculate rest)
    | x :: y :: rest -> x + (calculate ([ y ] @ rest))
    | _ -> failwith "Defect in calculate pattern"

let romanNumeralToInt roman =
    roman
    |> Seq.toList
    |> List.map charValue
    |> calculate
