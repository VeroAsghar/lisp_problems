let pack list = 
    let rec aux acc inner = function
        | [] -> acc
        | a :: [] -> aux ((a::inner)::acc) [] [] 
        | a :: (b :: _ as t) -> 
                if a = b then 
                    aux acc (a::inner) t 
                else
                    aux ((a::inner)::acc) [] t
    in
    List.rev (aux [] [] list)
;;

let pack2 list =
    let rec aux current acc = function
      | [] -> []    (* Can only be reached if original list is empty *)
      | [x] -> (x :: current) :: acc
      | a :: (b :: _ as t) ->
         if a = b then aux (a :: current) acc t
         else aux [] ((a :: current) :: acc) t  in
    List.rev (aux [] [] list);;
pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"];;


let encode list = 
    let countup letter count =
        (count, letter)
    in
    let rec aux acc count = function
        | [] -> acc
        | a :: [] -> aux ((countup a (count+1))::acc) 0 []
        | a :: (b :: _ as t) -> 
                if a = b then 
                    aux acc (count+1) t
                else
                    aux ((countup a (count+1))::acc) 0 t
    in
    List.rev (aux [] 0 list)
;;
let encode2 list =
    let rec aux count acc = function
      | [] -> [] (* Can only be reached if original list is empty *)
      | [x] -> (count + 1, x) :: acc
      | a :: (b :: _ as t) -> if a = b then aux (count + 1) acc t
                              else aux 0 ((count + 1, a) :: acc) t in
    List.rev (aux 0 [] list);;

let encode3 list = 
    let rec aux acc count = function
        | [] -> acc
        | a :: [] -> aux ((count+1, a)::acc) 0 []
        | a :: (b :: _ as t) -> 
                if a = b then 
                    aux acc (count+1) t
                else
                    aux ((count+1, a)::acc) 0 t
    in
    List.rev (aux [] 0 list);;

encode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
encode3 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;



type 'a rle =
  | One of 'a
  | Many of int * 'a

let modencode list = 
    let split count letter =
        if count = 1 then One letter
        else Many (count, letter)
    in
    let rec aux acc count = function
        | [] -> acc
        | [a] -> aux ((split (count+1) a)::acc) 0 []
        | a :: (b :: _ as t) -> 
                if a = b then aux acc (count+1) t
                else aux ((split (count+1) a)::acc) 0 t
    in
    List.rev (aux [] 0 list);;

let modencode2 l =
    let create_tuple cnt elem =
      if cnt = 1 then One elem
      else Many (cnt, elem) in
    let rec aux count acc = function
      | [] -> []
      | [x] -> (create_tuple (count + 1) x) :: acc
      | hd :: (snd :: _ as tl) ->
          if hd = snd then aux (count + 1) acc tl
          else aux 0 ((create_tuple (count + 1) hd) :: acc) tl in
      List.rev (aux 0 [] l);;

modencode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
modencode2 ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;


let decode list =
    let rec create_chunk acc cnt letter = 
        if cnt = 0 then acc
        else create_chunk (letter::acc) (cnt-1) letter
    in
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> 
                match hd with
                | One a -> aux (a::acc) tl
                | Many (cnt, a) -> aux ((create_chunk acc cnt a)) tl
    in
    List.rev (aux [] list);;

let decode2 list =
    let rec many acc n x =
      if n = 0 then acc else many (x :: acc) (n - 1) x
    in
    let rec aux acc = function
      | [] -> acc
      | One x :: t -> aux (x :: acc) t
      | Many (n, x) :: t -> aux (many acc n x) t
    in
      aux [] (List.rev list);;

decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
decode2 [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;

let duplicate list =
    let rec aux acc = function 
        | [] -> acc
        | hd :: tl -> aux (hd::(hd::acc)) tl
    in
    List.rev (aux [] list);;

duplicate ["a"; "b"; "c"; "c"; "d"];;

let replicate list cnt = 
    let rec rep acc letter cnt =
        if cnt = 0 then acc
        else rep (letter::acc) letter (cnt-1) 
    in
    let rec aux acc = function
        | [] -> acc
        | hd :: tl -> aux (rep acc hd cnt) tl
    in
    List.rev (aux [] list);;

let replicate2 list n =
    let rec prepend n acc x =
      if n = 0 then acc else prepend (n-1) (x :: acc) x in
    let rec aux acc = function
      | [] -> acc
      | h :: t -> aux (prepend n acc h) t in
    (* This could also be written as:
       List.fold_left (prepend n) [] (List.rev list) *)
    aux [] (List.rev list);;

replicate ["a"; "b"; "c"] 3;;
replicate2 ["a"; "b"; "c"] 3;;


let drop list cnt = 
    let rec aux acc cnt = function
        | [] -> acc
        | hd::tl -> 
                if cnt = 1 then aux acc 3 tl
                else aux (hd::acc) (cnt-1) tl 
    in
    List.rev (aux [] cnt list);;

let drop2 list n =
    let rec aux i = function
      | [] -> []
      | h :: t -> if i = n then aux 1 t else h :: aux (i + 1) t  in
    aux 1 list;;

drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
drop2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;


let split list cnt = 
    let rec aux tup cnt list = 
        let (x, y) = tup in
        match list with
        | [] -> (List.rev y, List.rev x)
        | hd::tl -> 
            if cnt = 0 then aux ((hd::x), y) (cnt) tl
            else aux (x, (hd::y)) (cnt-1) tl
    in
    aux ([], []) cnt (list);;

let split2 list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t 
    in
      aux n [] list;;

split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
split2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;

split ["a"; "b"; "c"; "d"] 5;;
split2 ["a"; "b"; "c"; "d"] 5;;


let slice list first last = 
    let rec aux acc cnt = function
        | [] -> List.rev acc
        | hd::tl -> 
                if cnt >= first && cnt <= last then aux (hd::acc) (cnt+1) tl
                else aux acc (cnt+1) tl
    in
    aux [] 0 list;;

let slice2 list i k =
    let rec take n = function
      | [] -> []
      | h :: t -> if n = 0 then [] else h :: take (n - 1) t
    in
    let rec drop n = function
      | [] -> []
      | _ :: t as l -> if n = 0 then l else drop (n - 1) t
    in
    take (k - i + 1) (drop i list);;

let slice3 list first last = 
    let rec aux acc cnt = function
        | [] -> List.rev acc
        | hd::tl -> 
                if cnt >= last then (aux [@tailcall]) (hd::acc) (cnt+1) []
                else if cnt >= first then (aux [@tailcall]) (hd::acc) (cnt+1) tl
                else (aux [@tailcall]) acc (cnt+1) tl
    in
    aux [] 0 list;;

slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
slice2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
slice3 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;







let rotate list cnt = 
    let rec aux acc acc2 cnt = function 
        | [] -> List.rev acc2 @ List.rev acc
        | hd::tl -> 
                if cnt = 0 then aux acc (hd::acc2) cnt tl
                else aux (hd::acc) acc2 (cnt-1) tl
    in
    aux [] [] cnt list;;

let split list n =
    let rec aux i acc = function
      | [] -> List.rev acc, []
      | h :: t as l -> if i = 0 then List.rev acc, l
                       else aux (i - 1) (h :: acc) t  in
    aux n [] list;;

let rotate2 list n =
    let len = List.length list in
    (* Compute a rotation value between 0 and len - 1 *)
    let n = if len = 0 then 0 else (n mod len + len) mod len in
    if n = 0 then list
    else let a, b = split list n in b @ a;;

rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;
rotate2 ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3;;






let remove_at k list = 
    let rec aux acc cnt = function
        | [] -> acc
        | hd::tl -> 
                if cnt = 0 then List.rev acc @ tl
                else aux (hd::acc) (cnt-1) tl
    in
    aux [] k list;;

let rec remove_at2 n = function
    | [] -> []
    | h :: t -> if n = 0 then t else h :: remove_at2 (n - 1) t;;

remove_at 1 ["a"; "b"; "c"; "d"];;
remove_at2 1 ["a"; "b"; "c"; "d"];;
remove_at 3 ["a"; "b"; "c"; "d"];;
remove_at2 3 ["a"; "b"; "c"; "d"];;







let rec insert_at str n = function
    | [] -> [str]
    | hd::tl as l -> if n = 0 then str::l else hd :: insert_at str (n - 1) tl;;

let rec insert_at2 x n = function
    | [] -> [x]
    | h :: t as l -> if n = 0 then x :: l else h :: insert_at2 x (n - 1) t;;

insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
insert_at2 "alfa" 1 ["a"; "b"; "c"; "d"];;







let range f e = 
    let rec aux acc start cnt =
        if cnt > 0 then aux ((start + cnt)::acc) start (cnt-1)
        else if cnt < 0 then aux ((start + cnt)::acc) start (cnt+1)
        else (start + cnt)::acc
    in
    aux [] f (e-f);;

let range2 a b =
    let rec aux acc high low =
      if high >= low then
        aux (high :: acc) (high - 1) low
      else acc
    in
      if a < b then aux [] b a else List.rev (aux [] a b);;

range 4 9;;
range2 4 9;;
range 9 4;;
range2 9 4;;






let wrong_extract k list = 
    let rec traverse_tail acc k letter = function
        | [] -> acc
        | hd::tl -> 
                if k = 0 then ([letter; hd])::acc
                else traverse_tail (([letter; hd])::acc) (k-1) letter tl
    in
    let rec aux acc k = function
        | [] -> List.rev acc
        | hd::tl -> aux ((traverse_tail acc k hd tl)) k tl
    in
    aux [] k list;;

let extract k list = 
    let len = List.length list in
    let n = len - k in
    if n <= 0 then [[]]
    else let rec aux acc k = function
        | [] -> []
        | hd::tl -> 



let rec extract2 k list =
    if k <= 0 then [[]]
    else match list with
         | [] -> []
         | h :: tl ->
            let with_h = List.map (fun l -> h :: l) (extract2 (k - 1) tl) in
            let without_h = extract2 k tl in
            with_h @ without_h;;

extract 2 ["a"; "b"; "c"; "d"];;
extract2 2 ["a"; "b"; "c"; "d"];;
extract 3 ["a"; "b"; "c"; "d"];;
extract2 3 ["a"; "b"; "c"; "d"];;
