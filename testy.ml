open PMap
open Topol

let check list topo =
  let rec make_order l i m =
    match l with
    | [] -> m
    | h :: t -> make_order t (i+1) (add h i m)
  in
  let order = make_order topo 0 empty in
  let rec check_ l =
    match l with
    | [] -> ()
    | (a, list) :: t ->
      let a_val = find a order in
      let new_list = List.map (fun x -> find x order) list in
      assert(List.filter (fun x -> x <= a_val ) new_list = []);
      check_ t
  in
  check_ list;;

let test1 = [ (1,[2;3;4]) ];;
(*check test1 (topol test1);;*)
print_string("test1 OK\n");;
let test2 = [(1,[2]); (2,[4]); (3,[4])];;
(*check test2 (topol test2);;*)
print_string("test2 OK\n");;


(* pusty graf *)
let test3 = [];;
check test3 (topol test3);;
assert( topol test3 = [] );;
print_string("test4 OK\n");;
let test4 = [ (1,[]); (2,[]); (3,[]); (4,[]); (5,[]) ];;
check test4 (topol test4);;
assert( List.length (topol test4) = 5 );;
let rec gen n list =
  if n = 0 then list else gen (n-1) ( (n,[])::list );;
let test5 = gen 100000 [];;
let ans5 = topol test5;;
check test5 ans5;;
assert( List.length test5 = 100000);;
print_string("test5 OK\n");;

Random.init(123);;

(* długa ścieżka z losowymi krawędziami *)
let m = 100000;;
let rec gen n list =
  if n = 0 then list else gen(n-1) ((n, [n+1; Random.int (m-n+1) + n + 2]) :: list);;
let test6 = gen m [];;
check test6 (topol test6);;
print_string("test6 OK\n");;

(* pełny *)
let m = 1000;;
let rec range n m list =
  if n > m then list else range (n+1) m (n :: list);;
 
let rec gen n list =
  if n > m then list else gen (n+1) ( (n, range (n+1) m []) :: list ) 

let test7 = gen 1 [];;
check test7 (topol test7);;
print_string("test7 OK\n");;

(* nie spójny *)
let test8 = [ (1,[2]); (3,[4;5]); (4,[5]); (6,[]); (7,[10]); (8,[10]); (9,[10]); ];;
check test8 (topol test8);;
print_string("test8 OK\n");;

let test8a = [ ("asd",["vsa"]); ("hsu",["qke"]); ("vsa",["vsa";"oep"]); ("oep",["qke";"skt"]); ("qke",["skt"]) ];;
check test8a (topol test8a);;
assert( List.length (topol test8a) = 6 );;
print_string("test8a OK\n");;

let m = 100000;;
let rec gen n list =
  if n = m then list else gen (n+1) ((n, [n+1]) :: list);;
let test9 = (m,[1]) :: (gen 1 []);;
topol test9

print_string("OK");;

