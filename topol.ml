(* autor: Artur Matyjasek *)
(* code review: Michał Borowski *)

open PMap

exception Cykliczne

let topol l =
  (* mapa zawierająca listy sąsiadów *)
  let rec make_graph l map =
    match l with
    | [] -> map
    | (a, list) :: t -> make_graph t (add a list map)
  in
  let graph = make_graph l empty in

  (* mapa zawierająca stopnie wieżchołków *)
  let rec make_deg l map =
    match l with
    | [] -> map
    | (a, list) :: t ->
      let update m x =
        let old = if mem x m then find x m else 0 in
        add x (old + 1) m
      in
      let n = if mem a map then map else add a 0 map in
      make_deg t (List.fold_left update n list)
  in
  let deg = make_deg l empty in

  let f k v a =
    if v = 0 then k :: a else a
  in
  let queue = foldi f deg [] in (* wieżchołki o stopniu 0 *)
  
  let rec sort q ans d =
    match q with
    | [] -> List.rev ans, d
    | h :: t ->
      let f (l, m) x = (* aktualizuje stopnie i zwraca wieżchołki o stopniu 0 *)
        let old = find x m in (* na pewno występuje w mapie *)
        ((if old = 1 then x :: l else l), add x (old - 1) m)
      in
      let n = if mem h graph then (find h graph) else [] in
      let (a, b) = List.fold_left f (t, d) n in
      sort a (h :: ans) b
  in
  let (ans, deg) = sort queue [] deg in
  iter (fun x v -> if v <> 0 then raise Cykliczne) deg;
  ans

