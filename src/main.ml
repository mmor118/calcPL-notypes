open Ast

(** [parse s] parses [s] into an AST. *)
let parse (s : string) : expr =
  let lexbuf = Lexing.from_string s in
  let ast = Parser.prog Lexer.read lexbuf in
  ast

let string_of_val (e: expr) : string =
  match e with
  | Int i -> string_of_int i
  | Bool b -> string_of_bool b
  | Binop _ -> failwith "precondition violated"
  | Var v -> v
  | Let _ -> failwith "precondition violated"
  | If _ -> failwith "precondition violated"
  | Error s -> "Error: " ^ s

let is_value : expr -> bool = function
  | Int _ | Bool _ -> true
  | Binop _ | Var _ | Let _ | If _ | Error _ -> false

let rec step : expr -> expr = function
  | Int _ | Bool _ | Error _ -> failwith "Does not step"
  | Binop (bop, e1, e2) when is_value e1 && is_value e2 -> step_bop bop e1 e2
  | Binop (bop, e1, e2) when is_value e1 -> Binop (bop, e1, step e2)
  | Binop (bop, e1, e2) -> Binop(bop, step e1, e2)
  | Let (id, e1, e2) when is_value e1 -> substitute e2 id e1
  | Let (id, e1, e2) -> Let (id, step e1, e2)
  | If (Bool true, t, _) -> t
  | If (Bool false, _, f) -> f
  | If (c, t, f) -> If (step c, t, f)

and step_bop bop e1 e2 = match bop, e1, e2 with
  | Add, Int a, Int b -> Int (a + b)
  | Mult, Int a, Int b -> Int (a * b)
  | LEQ, Int a, Int b -> Bool (a <= b)
  | _ -> failwith "precondition violated"

and substitute e id v =
  match e with
  | Int _ -> e
  | Bool _ -> e
  | Var x when x = id -> v
  | Var _ -> e
  | Binop (bop, e1, e2) -> Binop (bop, substitute e1 id v, substitute e2 id v)
  | Let (x, e1, e2) when x = id -> Let (x, substitute e1 id v, e2)
  | Let (x, e1, e2) -> Let (x, substitute e1 id v, substitute e2 id v)
  | If (c, t, f) -> If (substitute c id v, substitute t id v, substitute f id v)
  | Error _ -> e

let rec eval (e: expr) : expr = 
  if is_value e then e else
    e |> step |> eval

(** [interp s] interprets [s] by lexing and parsing,
    evaluating, and converting the result to a string. *)
let interp (s : string) : string =
    s |> parse |> eval |> string_of_val
  
  