let identity x = x

let ( >> ) f g x = g (f x)

let rec compose f n x =
  if n <= 0 then x
  else if n land 1 = 0 then compose (f >> f) (n / 2) x
  else compose f (n - 1) (f x)

let flip f x y = f y x

let maybe f x = try Some (f x) with _ -> None
let default x = function None -> x | Some y -> y
let may_default x f = function None -> x | Some y -> (f y)

let init f n =
  let rec aux i acc = if i >= n then acc else aux (i + 1) (f i :: acc) in
  aux 0 []

let rec find_map f = function
  | [] -> None
  | x :: xs -> match f x with
    | None -> find_map f xs
    | Some y -> Some (x, y)

let rec filter_map f = function
  | [] -> []
  | x :: xs -> match f x with
    | None -> filter_map f xs
    | Some y -> y :: filter_map f xs

let mapi2 f =
  let rec aux i xs ys = match xs, ys with
    | [], [] -> []
    | x :: xs, y :: ys -> f i x y :: aux (i + 1) xs ys
    | _ -> failwith "mapi2"
  in
  aux 0

let cfold = List.fold_left

(** [cfold2 f init [x1; x2; ...; xn] [y1; y2; ...; yn]] is
    [f (... (f (f (... (f (f init x1 y1) x1 y2) ...) x2 y1) x2 y2) ...) xn yn].
*)
let cfold2 f init xs ys =
  cfold (fun acc x -> cfold (flip f x) acc ys) init xs

(** [cfold3 f init [x1; x2; ...; xn] [y1; y2; ...; yn] [z1; z2; ...; zn]] is
    [f (... (f (f init x1 y1 z1) x1 y1 z2) ...) xn yn zn]. *)
let cfold3 f init xs ys zs =
  cfold (fun acc x -> cfold2 (flip f x) acc ys zs) init xs

(** [cfoldN] is a generalized-version of [cfold], [cfold2], and [cfold3].
    - [cfoldN f init [xs]] is [cfold (fun acc [x] -> f acc x) init xs],
    - [cfoldN f init [xs; ys]] is
      [cfold2 (fun acc [x; y] -> f acc x y) init xs ys],
    - [cfoldN f init [xs; ys; zs]] is
      [cfold3 (fun acc [x; y; z] -> f acc x y z) init xs ys zs]. *)
let cfoldN f =
  let rec aux arg acc = function
    | [] -> f acc arg
    | xs :: ll -> List.fold_left (fun acc' x -> aux (x :: arg) acc' ll) acc xs
  in
  aux []

let max_snd_option x = function
  | None -> Some x
  | Some y -> if snd x > snd y then Some x else Some y

let get_option = function
  | None -> failwith "Misc.get_option"
  | Some x -> x

let csum f = cfold (fun acc x -> acc +. f x) 0.

let csum2 f = cfold2 (fun acc x y -> acc +. f x y) 0.

let csum3 f = cfold3 (fun acc x y z -> acc +. f x y z) 0.

let cmax f xs =
  cfold (fun acc x -> max_snd_option (f x) acc) None xs
  |> get_option

let cmax2 f xs ys =
  cfold2 (fun acc x y -> max_snd_option (f x y) acc) None xs ys
  |> get_option

let cmax3 f xs ys zs =
  cfold3 (fun acc x y z -> max_snd_option (f x y z) acc) None xs ys zs
  |> get_option

let cmaxN f xss =
  cfoldN (fun acc xs -> max_snd_option (f xs) acc) None xss
  |> get_option

(** [unique_int_list rng n m] generates a list of integers with no duplication.
    [n] is the length of a list. Each element is less than [m]. *)
(*let unique_int_list rng n m =
  let rec aux i indices =
    if i >= n then indices else begin
      let j = Gsl.Rng.uniform_int rng m in
      if List.mem j indices then aux i indices else aux (i + 1) (j :: indices)
    end
  in
  assert(m >= n);
  aux 0 []

let sampling rng n xs =
  let m = List.length xs in
  if m <= n then xs else List.map (List.nth xs) (unique_int_list rng n m)*)

let pp_int_list =
  let open Format in
  pp_print_list
    ~pp_sep:(fun ppf () -> pp_print_string ppf " ") pp_print_int
