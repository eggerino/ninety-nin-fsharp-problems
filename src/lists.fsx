module lists

// Problem 1
let rec last items =
    match items with
    | [] -> None
    | [ item ] -> Some(item)
    | _ :: rest -> last rest

// Problem 2
let rec lastTwo items =
    match items with
    | [] -> None
    | [ _ ] -> None
    | [ first; second ] -> Some(first, second)
    | _ :: rest -> lastTwo rest

// Problem 3
let rec at index items =
    match index, items with
    | _, [] -> None
    | 1, item :: _ -> Some(item)
    | _, _ :: rest -> at (index - 1) rest

// Problem 4
let length items =
    let rec recLength current items =
        match items with
        | [] -> current
        | _ :: rest -> recLength (current + 1) rest

    recLength 0 items

// Problem 5
let rev items =
    let rec aux acc =
        function
        | [] -> acc
        | item :: rest -> aux (item :: acc) rest

    aux [] items

// Problem 6
let isPalindrome items = items = rev items

// Problem 7
type 'a Node =
    | One of 'a
    | Many of 'a Node list

let flatten items =
    let rec aux acc =
        function
        | [] -> acc
        | One item :: rest -> item :: aux acc rest
        | Many children :: rest -> aux (aux acc rest) children

    aux [] items

// Problem 8
let rec compress =
    function
    | a :: (b :: _ as rest) -> if a = b then compress rest else a :: compress rest
    | x -> x

// Problem 9
let pack items =
    let rec consume item acc =
        function
        | a :: rest when a = item -> consume item (a :: acc) rest
        | x -> acc, x

    let rec aux acc =
        function
        | [] -> acc
        | a :: _ as items ->
            let block, consumedItems = consume a [] items
            aux (block :: acc) consumedItems

    items |> aux [] |> rev

// Problem 10
let encode items =
    let rec aux count acc =
        function
        | [] -> acc
        | [ x ] -> (count + 1, x) :: acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux (count + 1) acc rest
            else
                aux 0 ((count + 1, a) :: acc) rest

    rev (aux 0 [] items)

// Problem 11
type 'a Rle =
    | One of 'a
    | Many of int * 'a

let encode2 items =
    let prepend count item acc =
        match count with
        | 1 -> One(item) :: acc
        | _ -> Many(count, item) :: acc

    let rec aux count acc =
        function
        | [] -> acc
        | [ x ] -> prepend (count + 1) x acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux (count + 1) acc rest
            else
                aux 0 (prepend (count + 1) a acc) rest

    rev (aux 0 [] items)

// Problem 12
let decode items =
    let rec many acc n x =
        if n = 0 then acc else many (x :: acc) (n - 1) x

    let rec aux acc =
        function
        | [] -> acc
        | One x :: rest -> aux (x :: acc) rest
        | Many(n, x) :: rest -> aux (many acc n x) rest

    aux [] (rev items)

// Problem 13
let encode3 items =
    let createRle n x = if n = 1 then One(x) else Many(n, x)

    let rec aux acc count =
        function
        | [] -> acc
        | [ x ] -> (createRle (count + 1) x) :: acc
        | a :: (b :: _ as rest) ->
            if a = b then
                aux acc (count + 1) rest
            else
                aux ((createRle (count + 1) a) :: acc) 0 rest

    items |> aux [] 0 |> rev
