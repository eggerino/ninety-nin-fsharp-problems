module lists

    // Problem 1
    let rec last items =
        match items with
        | [] -> None
        | [item] -> Some(item)
        | _ :: rest -> last rest
    