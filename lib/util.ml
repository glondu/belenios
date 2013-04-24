let ( |> ) x f = f x
let ( =% ) = Z.equal

module Array = struct
  include Array

  let forall f a =
    let n = Array.length a in
    (let rec check i =
       if i >= 0 then f a.(i) && check (pred i)
       else true
     in check (pred n))

  let forall2 f a b =
    let n = Array.length a in
    n = Array.length b &&
    (let rec check i =
       if i >= 0 then f a.(i) b.(i) && check (pred i)
       else true
     in check (pred n))

  let foralli f x =
    let rec loop i =
      if i >= 0 then f i x.(i) && loop (pred i)
      else true
    in loop (pred (Array.length x))

  let fforall f xs =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) in
        let n = Array.length x in
        let rec loop_inner j =
          if j >= 0 then f x.(j) && loop_inner (pred j)
          else true
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    loop_outer (pred n)

  let fforall2 f xs ys =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) and y = ys.(i) in
        let n = Array.length x in
        n = Array.length y &&
        let rec loop_inner j =
          if j >= 0 then f x.(j) y.(j) && loop_inner (pred j)
          else true
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    n = Array.length ys &&
    loop_outer (pred n)

  let fforall3 f xs ys zs =
    let rec loop_outer i =
      if i >= 0 then
        let x = xs.(i) and y = ys.(i) and z = zs.(i) in
        let n = Array.length x in
        n = Array.length y &&
        n = Array.length z &&
        let rec loop_inner j =
          if j >= 0 then f x.(j) y.(j) z.(j) && loop_inner (pred j)
          else true
        in loop_inner (pred n)
      else true
    in
    let n = Array.length xs in
    n = Array.length ys &&
    n = Array.length zs &&
    loop_outer (pred n)

  let map2 f a b =
    Array.mapi (fun i ai -> f ai b.(i)) a

  let map2i f a b =
    Array.mapi (fun i ai -> f i ai b.(i)) a

  let map3 f a b c =
    Array.mapi (fun i ai -> f ai b.(i) c.(i)) a

  let mmap f a =
    Array.map (fun ai ->
      Array.map f ai
    ) a

  let mmap2 f a b =
    Array.mapi (fun i ai ->
      let bi = b.(i) in
      Array.mapi (fun j aj ->
        f aj bi.(j)
      ) ai
    ) a

  let mmap3 f a b c =
    Array.mapi (fun i ai ->
      let bi = b.(i) and ci = c.(i) in
      Array.mapi (fun j aj ->
        f aj bi.(j) ci.(j)
      ) ai
    ) a

  let ssplit a =
    mmap fst a, mmap snd a
end

module List = struct
  include List

  let iteri f xs =
    let rec loop i = function
      | [] -> []
      | x :: xs -> f i x :: loop (succ i) xs
    in List.flatten (loop 0 xs)
end

module String = struct
  include String

  let map f s =
    let n = String.length s in
    let res = String.create n in
    for i = 0 to n-1 do res.[i] <- f s.[i] done;
    res

  let startswith x s =
    let xn = String.length x and sn = String.length s in
    xn >= sn && String.sub x 0 sn = s
end

let hashB x = Cryptokit.(x |>
  hash_string (Hash.sha256 ()) |>
  transform_string (Base64.encode_compact ())
)

let load_from_file read fname =
  let i = open_in fname in
  let buf = Lexing.from_channel i in
  let lex = Yojson.init_lexer ~fname () in
  let result = read lex buf in
  close_in i;
  result

let non_empty_lines_of_file fname =
  Lwt_io.lines_of_file fname |>
  Lwt_stream.filter (fun s -> s <> "") |>
  Lwt_stream.to_list

let seed = lazy (Cryptokit.Random.(string secure_rng 32))

let prng = Cryptokit.Random.(pseudo_rng (Lazy.force seed))

let random q =
  let size = Z.size q * Sys.word_size / 8 in
  let r = Cryptokit.Random.string prng size in
  Z.(of_bits r mod q)
