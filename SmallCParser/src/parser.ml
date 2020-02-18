open SmallCTypes
open Utils
open TokenTypes

type stmt_result = token list * stmt
type expr_result = token list * expr

exception ParseError of string

let lookahead tok_list =
  match tok_list with
    [] -> raise (ParseError "no tokens")
  | (h::t) -> h

(* Provided helper function - takes a token list and an expected token.
 * Handles error cases and returns the tail of the list *)
let match_token (toks : token list) (tok : token) : token list =
  match toks with
  
  | [] -> raise (InvalidInputException(string_of_token tok))
  | h::t when h = tok -> t 
  | h::_ -> raise (InvalidInputException(
      Printf.sprintf "Expected %s from input %s, got %s"
        (string_of_token tok)
        (string_of_list string_of_token toks)
        (string_of_token h)
    ))

    (* start your code here *)


(*
Expr -> OrExpr
OrExpr -> BExpr || AExpr | BExpr
AndExpr -> AndExpr && AndExpr | EqualityExpr
EqualityExpr -> EqualityExpr EqualityOperator EqualityExpr | RelationalExpr
EqualityOperator -> == | !=
RelationalExpr -> RelationalExpr RelationalOperator RelationalExpr | AdditiveExpr
RelationalOperator -> < | > | <= | >=
AdditiveExpr -> AdditiveExpr AdditiveOperator AdditiveExpr | MultiplicativeExpr
AdditiveOperator -> + | -
MultiplicativeExpr -> MultiplicativeExpr MultiplicativeOperator MultiplicativeExpr | PowerExpr
MultiplicativeOperator -> * | /
PowerExpr -> PowerExpr ^ PowerExpr | UnaryExpr
UnaryExpr -> ! UnaryExpr | PrimaryExpr
PrimaryExpr -> Tok_Int | Tok_Bool | Tok_ID | ( Expr )
*)

let rec parse_A tok_list = 
  let (l, a1) = parse_B tok_list in
  let t = lookahead l in
  match t with 
    Tok_Or -> 
    let tok_list_2 =  match_token l Tok_Or in
    let (l2, a2) = parse_A tok_list_2 in
    (l2, Or(a1, a2))
    | _ -> (l, a1)


and parse_B tok_list = 
  let (l, a1) = parse_C tok_list in
  let t = lookahead l in
  match t with 
    Tok_And -> 
    let tok_list_2 =  match_token l Tok_And in
    let (l2, a2) = parse_B tok_list_2 in
    (l2, And(a1, a2))
    | _ -> (l, a1)

and parse_C tok_list = 
  let (l, a1) = parse_D tok_list in
  let t = lookahead l in
  match t with 
    Tok_Equal -> 
    let tok_list_2 =  match_token l Tok_Equal in
    let (l2, a2) = parse_C tok_list_2 in
    (l2, Equal(a1, a2))
    | Tok_NotEqual -> 
    let tok_list_2 =  match_token l Tok_NotEqual in
    let (l2, a2) = parse_C tok_list_2 in
    (l2, NotEqual(a1, a2))
    | _ -> (l, a1)

and parse_D tok_list = 
  let (l, a1) = parse_E tok_list in
  let t = lookahead l in
  match t with 
    Tok_Less -> 
    let tok_list_2 =  match_token l Tok_Less in
    let (l2, a2) = parse_D tok_list_2 in
    (l2, Less(a1, a2))

    | Tok_Greater -> 
    let tok_list_2 =  match_token l Tok_Greater in
    let (l2, a2) = parse_D tok_list_2 in
    (l2, Greater(a1, a2))

    | Tok_LessEqual -> 
    let tok_list_2 =  match_token l Tok_LessEqual in
    let (l2, a2) = parse_D tok_list_2 in
    (l2, LessEqual(a1, a2))

    | Tok_GreaterEqual -> 
    let tok_list_2 =  match_token l Tok_GreaterEqual in
    let (l2, a2) = parse_D tok_list_2 in
    (l2, GreaterEqual(a1, a2))

    | _ -> (l, a1)

and parse_E tok_list =
  let (l, a1) = parse_F tok_list in
  let t = lookahead l in
  match t with
    Tok_Add ->
    let tok_list_2 =  match_token l Tok_Add in
    let (l2, a2) = parse_E tok_list_2 in
    (l2, Add(a1, a2))

  | Tok_Sub ->
    let tok_list_2 =  match_token l Tok_Sub in
    let (l2, a2) = parse_E tok_list_2 in
    (l2, Sub(a1, a2))

  | _ -> (l, a1)     (* E -> T *)

and parse_F tok_list =
  let (l, a1) = parse_G tok_list in
  let t = lookahead l in
  match t with
    Tok_Mult ->
    let tok_list_2 =  match_token l Tok_Mult in
    let (l2, a2) = parse_F tok_list_2 in
    (l2, Mult(a1, a2))

  | Tok_Div ->
    let tok_list_2 =  match_token l Tok_Div in
    let (l2, a2) = parse_F tok_list_2 in
    (l2, Div(a1, a2))

  | _ -> (l, a1)     (* E -> T *)

and parse_G tok_list =
  let (l, a1) = parse_H tok_list in
  let t = lookahead l in
  match t with
    Tok_Pow ->
    let tok_list_2 =  match_token l Tok_Pow in
    let (l2, a2) = parse_G tok_list_2 in
    (l2, Pow(a1, a2))
    | _ -> (l, a1) 

and parse_H tok_list = 
  let (l, a1) = parse_I tok_list in
  let t = lookahead l in
  match t with 
    Tok_Not ->
    let tok_list_2 =  match_token l Tok_Not in
    (tok_list_2, Not(a1))
    | _ -> (l, a1) 

and parse_I tok_list =
  let t = lookahead tok_list in
  match t with
    Tok_Int(c) ->
    let  l = match_token tok_list (Tok_Int c)  in
    (l, Int c)

    | Tok_Bool c -> 
    let  l = match_token tok_list (Tok_Bool c) in
    (l, Bool c)

    | Tok_ID c -> 
    let  l = match_token tok_list (Tok_ID c) in
    (l, ID c)

    | Tok_LParen -> 
    let  l = match_token tok_list Tok_LParen in
    let (l2, a2) = parse_A l in
    let t2 = lookahead l2 in 
    (match t2 with 
      | Tok_RParen -> 
      let tok_list_2 = match_token l2 Tok_RParen in 
      (tok_list_2, a2) (* HELP *)
      | _ -> raise (ParseError "parse_I right paren failed")
    )
    | _ -> raise (ParseError "parse_I failed to match anything")

let rec parse_expr (toks : token list) : token list * expr = 
  parse_A toks 

(*
Stmt -> Stmt Stmt | DeclareStmt | AssignStmt | PrintStmt | IfStmt | DoWhileStmt | WhileStmt
DeclareStmt -> BasicType ID ;
BasicType -> int | bool
AssignStmt -> ID = Expr ;
PrintStmt -> printf ( Expr ) ;
IfStmt -> if ( Expr ) { Stmt } ElseBranch
ElseBranch -> else { Stmt } | ε
DoWhileStmt -> do { Stmt } while ( Expr ) ;
WhileStmt -> while ( Expr ) { Stmt }
*)

let rec parse_AA tok_list = 
  let t = lookahead tok_list in
  match t with 
  | Tok_Int_Type -> 
  let tok_list_2 = match_token tok_list Tok_Int_Type in
  let id = lookahead tok_list_2 in
  (match id with 
    | Tok_ID c -> 
     let tok_list_3 = match_token tok_list_2 (Tok_ID c) in 
    let tok_list_4 = match_token tok_list_3 Tok_Semi in 
    let (l, seq) = parse_AA tok_list_4 in
    (l, Seq(Declare(Int_Type, c), seq))

  )

  | Tok_Bool_Type -> 
    let tok_list_2 = match_token tok_list Tok_Bool_Type in
  let id = lookahead tok_list_2 in
  (match id with 
    | Tok_ID c -> 
    let tok_list_3 = match_token tok_list_2 (Tok_ID c) in 
    let tok_list_4 = match_token tok_list_3 Tok_Semi in 
    let (l, seq) = parse_AA tok_list_4 in
    (l, Seq(Declare(Bool_Type, c), seq))
  )

  | Tok_ID c ->
  let t = match_token tok_list (Tok_ID c) in 
  let tok_list_2 = match_token t Tok_Assign in 
  let (l, a) = parse_expr tok_list_2 in
  let tok_list_3 = match_token l Tok_Semi in
  let (l2, seq) = parse_AA tok_list_3 in
  (l2, Seq(Assign( c, a), seq))

  | Tok_Print -> 
    let tok_list_2 = match_token tok_list Tok_Print in 
    let tok_list_3 = match_token tok_list_2 Tok_LParen in 
    let (l, a) = parse_expr tok_list_3 in
    let tok_list_4 = match_token l Tok_RParen in 
    let tok_list_5 = match_token tok_list_4 Tok_Semi in 
    let (l2, seq) = parse_AA tok_list_5 in
    (l2, Seq(Print(a), seq))

  | Tok_If ->
    let tok_list_2 = match_token tok_list Tok_If in 
    let tok_list_3 = match_token tok_list_2 Tok_LParen in 
    let (l, a) = parse_expr tok_list_3 in
    let tok_list_4 = match_token l Tok_RParen in 
    let tok_list_5 = match_token tok_list_4 Tok_LBrace in 
    let (l2, seq) = parse_AA tok_list_5 in 
    let tok_list_6 = match_token l2 Tok_RBrace in
    let t = lookahead tok_list_6 in
    (match t with 
      |Tok_Else ->
      let tok_list_7 = match_token tok_list_6 Tok_Else in 
      let tok_list_8 = match_token tok_list_7 Tok_LBrace in 
      let (l3, seq2) = parse_AA tok_list_8 in 
      let tok_list_9 = match_token l3 Tok_RBrace in 
      let (l4, seq3) = parse_AA tok_list_9 in
      (l4, Seq(If(a, seq, seq2 ), seq3))
      
      | _ -> let (l3, seq2) = parse_AA tok_list_6 in 
      (l3, Seq(If(a, seq, NoOp), seq2))
    )

    (* DoWhileStmt -> do { Stmt } while ( Expr ) ;*)

    | Tok_Do -> 
    let tok_list_2 = match_token tok_list Tok_Do in 
    let tok_list_3 = match_token tok_list_2 Tok_LBrace in 
    let (l, seq) = parse_AA tok_list_3 in 
    let tok_list_4 = match_token l Tok_RBrace in 
    let tok_list_5 = match_token tok_list_4 Tok_While in 
    let tok_list_6 = match_token tok_list_5 Tok_LParen in 
    let (l2, a) = parse_expr tok_list_6 in
    let tok_list_7 = match_token l2 Tok_RParen in 
    let tok_list_8 = match_token tok_list_7 Tok_Semi in 
    let (l3, seq2) = parse_AA tok_list_8 in 
    (l3, Seq(DoWhile(seq, a), seq2))

    | Tok_While -> 
    let tok_list_2 = match_token tok_list Tok_While in 
    let tok_list_3 = match_token tok_list_2 Tok_LParen in 
    let (l, a) = parse_expr tok_list_3 in
    let tok_list_4 = match_token l Tok_RParen in 
    let tok_list_5 = match_token tok_list_4 Tok_LBrace in 
    let (l2, seq) = parse_AA tok_list_5 in 
    let tok_list_6 = match_token l2 Tok_RBrace in
    let (l3, seq2) = parse_AA tok_list_6 in 
    (l3, Seq(While(a, seq), seq2))

    | _ -> (tok_list, NoOp)

let rec parse_stmt toks = 
  parse_AA toks

(* 
Main ::= int main ( ) { Statement } EOF
*)

let parse_main toks = 
  let tok_list = match_token toks Tok_Int_Type in
  let tok_list_2 = match_token tok_list Tok_Main in  
  let tok_list_3 = match_token tok_list_2 Tok_LParen in 
  let tok_list_4 = match_token tok_list_3 Tok_RParen in 
  let tok_list_5 = match_token tok_list_4 Tok_LBrace in
  let (l, seq) = parse_AA tok_list_5 in
  let tok_list_6 = match_token l Tok_RBrace in 
  let tok_list_7 = match_token tok_list_6 EOF in 
  seq

