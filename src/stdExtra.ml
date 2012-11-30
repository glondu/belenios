let ( |> ) x f = f x
let ( =% ) = Z.equal

let array_forall2 f a b =
  let n = Array.length a in
  n = Array.length b &&
  (let rec check i =
     if i >= 0 then f a.(i) b.(i) && check (pred i)
     else true
   in check (pred n))

let array_foralli f x =
  let rec loop i =
    if i >= 0 then f i x.(i) && loop (pred i)
    else true
  in loop (pred (Array.length x))
