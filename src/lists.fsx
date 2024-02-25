module lists

    // Problem 1
    let rec last items =
        match items with
        | [] -> None
        | [item] -> Some(item)
        | _ :: rest -> last rest

    // Problem 2
    let rec lastTwo items =
        match items with
        | [] -> None
        | [_] -> None
        | [first; second] -> Some(first, second)
        | _ :: rest -> lastTwo rest
