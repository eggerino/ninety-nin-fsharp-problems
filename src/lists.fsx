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

// Problem 14
let rec duplicate =
    function
    | [] -> []
    | a :: t -> a :: a :: duplicate t

// Problem 15
let rec replicate items count =
    let rec many acc n x =
        if n = 0 then acc else many (x :: acc) (n - 1) x

    match items with
    | [] -> []
    | a :: t -> many (replicate t count) count a

// Problem 16
let drop items n =
    let rec aux i =
        function
        | [] -> []
        | _ :: t when i = n -> aux 1 t
        | a :: t -> a :: aux (i + 1) t

    aux 1 items

// Problem 17
let rec split items count =
    match items, count with
    | _, 0 -> [], items
    | [], _ -> [], []
    | a :: t, _ ->
        let take, rest = split t (count - 1)
        a :: take, rest

// Problem 18
let slice items i k =
    fst (split (snd (split items i)) (k - i + 1))

// Problem 19
let rotate items n =
    let len = length items
    let count = if len = 0 then 0 else ((n % len) + len) % len
    let first, second = split items count
    second @ first

// Problem 20
let rec removeAt i =
    function
    | [] -> []
    | _ :: t when i = 0 -> t
    | a :: t -> a :: removeAt (i - 1) t

// Problem 21
let rec insertAt item i =
    function
    | [] -> [ item ]
    | x when i = 0 -> item :: x
    | a :: t -> a :: insertAt item (i - 1) t

// Problem 22
let rec range a b =
    if a = b then [ a ]
    elif a < b then a :: range (a + 1) b
    else a :: range (a - 1) b

// Problem 23
let randSelect items count =
    let rng = new System.Random()
    let len = length items

    let getRandomItem () =
        at (1 + rng.Next len) items |> Option.get

    let rec aux acc =
        function
        | 0 -> acc
        | i -> aux (getRandomItem () :: acc) (i - 1)

    aux [] count

// Problem 24
let lottoSelect n m =
    let rng = new System.Random()

    let rec aux =
        function
        | 0 -> []
        | i -> rng.Next(1, m) :: aux (i - 1)

    aux n

// Problem 25
let rec permutation items =
    let i = (new System.Random()).Next (length items)

    match items with
    | [] -> []
    | x -> Option.get (at (i + 1) items) :: permutation (removeAt i items)
