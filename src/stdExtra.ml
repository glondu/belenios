let ( |> ) x f = f x
let ( =% ) = Z.equal

module Array = struct
  include Array

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

let random q = assert false
