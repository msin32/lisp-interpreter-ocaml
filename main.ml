(* new type for stream so we can read file. Also fields for linenum and char buffer (list of chars) *)
type stream =
  { mutable line_num: int; mutable chr: char list; chan: in_channel };;

(* read stream char by char, update line_num and chr *)
let read_char stm =
    match stm.chr with
      | [] -> (* if no char in buffer*)
              let c = input_char stm.chan in (* call input_char function from in_channel lib that reads char from chan *)
              if c = '\n' then let _ = stm.line_num <- stm.line_num + 1 in c (* if newline, increment linenum and return c *)
              else c (*otherwise return the char *)
      | c::rest -> (* if there are chars in buffer *)
         let _ = stm.chr <- rest in (* remove first char from buffer *)
         if c = '\n' then stm.line_num <- stm.line_num + 1; (* if newline, increment linenum *)
         c;;
(* opposite of read_char, undoes read_char *)
let unread_char stm c =
  if c = '\n' then stm.line_num  <- stm.line_num - 1; (* if newline, unread another char and decrement linenum *)
    stm.chr <- c :: stm.chr;;

(* whitespace handling *)
let is_white c =
  c = ' ' || c = '\t' || c = '\n';;
let isalpha = function | 'A'..'Z'|'a'..'z' -> true
                         | _ -> false;;
let literalQuote = String.get "\"" 0;;
let is_delimiter = function | '('|')'|'{'|'}'|';' -> true
                              | c when c=literalQuote -> true
                              | c -> is_white c;;
let rec eat_whitespace stm =
  let c = read_char stm in
  if is_white c then
    eat_whitespace stm
  else
    unread_char stm c;
  ();;

(* lisp typesystem *)
type lobject =
  | Fixnum of int
    | Boolean of bool
    | Symbol of string
    | Nil
    | Pair of lobject * lobject


(* exception types*)
exception SyntaxError of string;;
exception ThisCan'tHappenError;;

let rec pair_to_list pr =
  match pr with
  | Nil -> []
  | Pair(a, b) -> a::(pair_to_list b)
  | _ -> raise ThisCan'tHappenError;;

(* Read Expresions *)
let stringOfChar c = String.make 1 c;; (* faster than Char.escaped and easier to read *)
let rec read_sexp stm =
  let is_digit c =
    let code = Char.code c in
    code >= Char.code('0') && code <= Char.code('9')
  in
  let rec read_fixnum acc =
    let nc = read_char stm in
    if is_digit nc
    then read_fixnum (acc ^ (Char.escaped nc))
    else
      let _ = unread_char stm nc in
      Fixnum(int_of_string acc)
  in
  let is_lone stm c =
    let nc = read_char stm in
    let _ = unread_char stm nc in
    if is_white nc || is_delimiter nc then
      true
    else false
  in
  let is_bool stm c =
    match c with
    |'t'|'f' -> is_lone stm c
    | _ -> false
  in
  let is_symstartchar =
    function | '*'|'/'|'>'|'<'|'='|'?'|'!'|'-'|'+' -> true
             | c -> isalpha c
  in
  let rec read_symbol () =
    let literalQuote = String.get "\"" 0 in
    let nc = read_char stm in
    if is_delimiter nc
    then let _ = unread_char stm nc in ""
    else stringOfChar nc ^ read_symbol ()
  in
  let rec read_list stm =
    eat_whitespace stm;
    let c = read_char stm in
    if c = ')'
    then Nil
    else
      let _ = unread_char stm c in
      let car = read_sexp stm in
      let cdr = read_list stm in
      Pair(car, cdr)
  in
  eat_whitespace stm;
  let c = read_char stm in
  if is_bool stm c then
    if c = 't' then Boolean(true)
    else Boolean(false)
  else if is_symstartchar c then
    Symbol(stringOfChar c ^ read_symbol ())
  else if is_digit c then read_fixnum (Char.escaped c) (* positive numbers only*)
  else if c = '~' || c = '-' then read_fixnum (Char.escaped '-')
  else if c = '#' then (* bools either #t or #f *)
    match (read_char stm) with
    | 't' -> Boolean(true)
    | 'f' -> Boolean(false)
    | x -> raise (SyntaxError ("Invalid boolean literal " ^ (Char.escaped x)))
  else if c = '('
  then read_list stm
  else raise (SyntaxError ("Unexpected char " ^ (Char.escaped c)));;

let rec print_sexp e =
  let rec print_list l =
    match l with
    | Pair(a, Nil) -> print_sexp a;
    | Pair(a, b) -> print_sexp a; print_string " "; print_list b
    | _ -> raise ThisCan'tHappenError
   in
   let print_pair p =
     match p with
     | Pair(a,b) -> print_sexp a; print_string " . "; print_sexp b
     | _ -> raise ThisCan'tHappenError
   in
  let rec is_list e =
    match e with
    | Nil -> true
    | Pair(a, b) -> is_list b
    | _ -> false
  in
  match e with
  | Fixnum(v) -> print_int v
  | Boolean(b) -> print_string (if b then "#t" else "#f")
  | Symbol(s) -> print_string s
  | Nil -> print_string "nil"
  | Pair (a, b) ->
     print_string "(";
     if is_list e
     then print_list e
     else print_pair e;
     print_string ")";;

let rec repl stm =
  print_string "> ";
  flush stdout;
  let sexp = read_sexp stm in
  print_sexp sexp;
  print_newline ();
  repl stm;;

let main =
  let stm = { chr=[]; line_num=1; chan=stdin } in
  repl stm;;


(* OLD STUFF *)
(* else if c = 't' then Boolean(true) *) (* simple t and f bool support, replaced with checks inside symbol parsing *)
  (* else if c = 'f' then  Boolean(false)   *)
(* old def *)
  (* let print_pair (Pair(a, b)) = *)
  (*   print_sexp a; print_string " . "; print_sexp b *)
  (* in *)
(*fail booleans 2 *)
    (* if !(read_symbol stm) then (\* if a lone char, check if t or f *\) *)
    (*   if c = 't' then Boolean(true) *)
    (*   else if c = 'f' then Boolean(false) *)
    (*   else *)
    (*     begin *)
    (*       (\* unread_char stm c; *\) *)
    (*       Symbol(stringofChar c ^ read_symbol ()) end *)
(* fail booleans *)
    (* if c = 't' && !(read_char stm) then Boolean(true) (\* first check if not 't' or 'f' alone *\) *)
    (* else if c = 'f' && !(read_char stm) then Boolean(false) *)
(* else Symbol(stringOfChar c ^ read_symbol ()) *)

(* before adding symbols *)
(* exception SyntaxError of string;; *)
(* let rec read_sexp stm = *)
(*   let is_digit c = *)
(*     let code = Char.code c in *)
(*     code >= Char.code('0') && code <= Char.code('9') *)
(*   in *)
(*   let rec read_fixnum acc = *)
(*     let nc = read_char stm in *)
(*     if is_digit nc *)
(*     then read_fixnum (acc ^ (Char.escaped nc)) *)
(*     else *)
(*       let _ = unread_char stm nc in *)
(*       Fixnum(int_of_string acc) *)
(*   in *)
(*   eat_whitespace stm; *)
(*   let c = read_char stm in *)
(*   if is_digit c then read_fixnum (Char.escaped c) (\* positive numbers only*\) *)
(*   else if c = '~' || c = '-' then read_fixnum (Char.escaped '-') *)
(*   else if c = '#' then (\* bools either #t or #f *\) *)
(*     match (read_char stm) with *)
(*     | 't' -> Boolean(true) *)
(*     | 'f' -> Boolean(false) *)
(*     | x -> raise (SyntaxError ("Invalid boolean literal " ^ (Char.escaped x))) *)
(*   else if c = 't' then Boolean(true) (\* t and f bool support*\) *)
(*   else if c = 'f' then  Boolean(false)   *)
(*   else raise (SyntaxError ("Unexpected char " ^ (Char.escaped c)));; *)

(* let rec print_sexp e = *)
(*   match e with *)
(*   | Fixnum(v) -> print_int v *)
(*   | Boolean(b) -> print_string (if b then "#t" else "#f") *)

(* let rec repl stm = *)
(*   print_string "> "; *)
(*   flush stdout; *)
(*   let sexp = read_sexp stm in *)
(*   print_sexp sexp; *)
(*   print_newline (); *)
(*   repl stm;; *)

(* let main = *)
(*   let stm = { chr=[]; line_num=1; chan=stdin } in *)
(*   repl stm;; *)

(* old repl that can only return Fixnum*)
(* let rec repl stm = *)
(*   print_string "> "; (\* write to internal buffer *\) *)
(*   flush stdout; (\* write internal buffer to stdout for display *\) *)
(*   let Fixnum(v) = read_sexp stm in (\* match v as fixnum (else exception) *\) *)
(*   print_int v; *)
(*   print_newline (); *)
(*   repl stm;; *)

(* one shot char evaluator - replaced with repl *)
(* let main = *)
(*   let stm = { chr=[]; line_num=1; chan=stdin } in *)
(*   print_string "> "; *)
(*   flush stdout; *)
(*   let Fixnum(v) = read_sexp stm in *)
(*   print_string "Your int: "; *)
(*   print_int v; *)
(*   print_newline ();; *)
