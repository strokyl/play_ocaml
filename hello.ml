module Array : sig
  type t

  val empty: t
  val get: t -> int -> char
  val set: t -> int -> char -> t

end = struct

  type t = char array

  let empty = Array.make 30000 '\000'

  let get arr idx =
    arr.(idx)

  let set arr idx value =
    arr.(idx) <- value;
    arr
end

module BrainfuckImplem : sig
  type t

  val init: t
  val incPos: t -> t
  val decPos: t -> t
  val incVal: t -> t
  val decVal: t -> t
  val get: t -> char
  val set: t -> char -> t
end = struct
  type t = {pos: int;  buffer: Array.t}
  let init = {pos = 0; buffer= Array.empty}
  let addToChar i c = Char.chr @@ Char.code c + i

  let addToPos i buffer = {buffer with pos = buffer.pos + i}
  let incPos = addToPos 1
  let decPos = addToPos (-1)

  let addToVal i buffer = {buffer with buffer = Array.set buffer.buffer buffer.pos (addToChar i (Array.get buffer.buffer buffer.pos))}
  let incVal = addToVal 1
  let decVal = addToVal (-1)
  let get buffer = Array.get buffer.buffer buffer.pos
  let set buffer c = {buffer with buffer = Array.set buffer.buffer buffer.pos c}
end

let eval prog pos implem = match prog.[pos] with
  | '>' -> (BrainfuckImplem.incPos implem, pos + 1)
  | '<' -> (BrainfuckImplem.decPos implem, pos + 1)
  | '+' -> (BrainfuckImplem.incVal implem, pos + 1)
  | '-' -> (BrainfuckImplem.decVal implem, pos + 1)
  | '.' -> ((print_endline @@ Char.escaped @@ BrainfuckImplem.get implem ; implem), pos + 1)
  | ',' -> (BrainfuckImplem.set implem @@ get_char, pos + 1)





(* Exemple de pattern matching:
    type t = Monday | Tuesday | Friday

    let f = function
       | Monday -> print_int 3
       | Tuesday -> print_int 2
       | _ -> Printf.printf "All other cases"

*)

(* Syntaxe de 'functional updates' pour les records:

    type t = { filename: string; lnum: int; cnum: int }

    let i0 = { filename = "toto.c"; lnum = 0; cnum = 0 }
    let i1 = { i0 with lnum = 1 }

*)

