(*******************************************************************************
   Filename: lists.ml

     Author: David C. Drake (https://davidcdrake.com)

Description: A collection of list-manipulating functions written in OCaml.
*******************************************************************************)

open Core.Std

let rec sumList a =
    match a with
    | [] -> 0
    | x :: xs -> x + sumList xs

let sumList2 = List.fold ~init:0 ~f:(+)

let productList a = List.fold_right ~init:1 ~f:(fun x y -> x * y) a

let myMap f = List.fold_right ~init:[] ~f:(fun x y -> (f x) :: y)

let rec quicksort = function
    | [] -> []
    | pivot::others ->
        let is_less x = x < pivot in
        let less, greater_or_equal = List.partition_tf ~f:is_less others in
        quicksort less @ [pivot] @ quicksort greater_or_equal

let dropN a n =
    let rec helper x = function
        | [] -> []
        | head :: tail -> if x = n then helper 1 tail
                          else head :: helper (x + 1) tail in
    helper 1 a

let compress a =
    let rec helper num b = function
        | [] -> []
        | [element] -> (element, num + 1) :: b
        | x :: (y :: _ as tail) -> if x = y then helper (num + 1) b tail
                                   else helper 0 ((x, num + 1) :: b) tail in
    List.rev (helper 0 [] a)

let decompress a =
    let rec many b n x =
        if n = 0 then b else many (x :: b) x (n - 1) in
    let rec helper b = function
        | [] -> b
        | x :: tail -> helper (x :: b) tail
        | (x, n) :: tail -> helper (many b x n) tail in
    helper [] (List.rev list)
