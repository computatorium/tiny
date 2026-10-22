(* 1.6.1 | pc:110101 | uva:100 *)

(*
The 3n + 1 Problem (the Collatz conjecture)

For any integer n:
    1. If n is even, divide it by 2.
    2. If n is odd, multiply it by 3 and add 1.
    3. If n != 1, goto step 1.
    4. Done.

The *cycle length* of n is the number of steps taken to reach 1.

This problem gives pairs of two integers i and j, and asks 
for the maximum cycle length of all integers between i and j,
inclusive (both endpoints included).
*)

let collatz n =
  let next m = if m mod 2 = 0 then m/2 else 3*m+1 in
  let rec cycles curn count =
    if curn = 1 then count else cycles (next curn) (count+1)
  in cycles n 1

let max_cycles a b = 
  let lo, hi = min a b, max a b in
  let rec go n best =
    if n > hi then best
    else go (n + 1) (max best (collatz n))
  in go lo 0

let split line = String.trim line |> String.split_on_char ' '

let parse = function
  | [f; s] -> int_of_string f, int_of_string s
  | words -> failwith @@ Format.sprintf "bad line: [%s]" @@ String.concat " " words

let pair_and_cycle (l, h) = max_cycles l h |> Format.sprintf "%d %d %d\n" l h

let solve (input : in_channel) (output : out_channel) : unit =
  In_channel.input_lines input 
  |> List.to_seq
  |> Seq.map split
  |> Seq.map parse
  |> Seq.map pair_and_cycle
  |> Seq.iter @@ Out_channel.output_string output

  
