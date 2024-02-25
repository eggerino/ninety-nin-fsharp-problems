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

    // Problem 3
    let rec at index items =
        match index, items with
        | _, [] -> None
        | 1, item :: _ -> Some(item)
        | _, _ :: rest -> at (index - 1) rest
