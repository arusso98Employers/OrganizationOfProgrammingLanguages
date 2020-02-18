open SmallCTypes
open EvalUtils

exception TypeError of string
exception DeclareError of string
exception DivByZeroError

let rec lookup env x =
  match env with
    [] -> raise (DeclareError("no variable here"))
  	| (y,v)::env_1 -> if x = y then v else lookup env_1 x

let rec eval_expr env e = 


	match e with 
		| ID(string_id) -> lookup env string_id

  		| Int(int_val) -> Int_Val int_val

  		| Bool(bool_val) -> Bool_Val bool_val

  		| Add(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Int_Val (value_1 + value_2)
  			| _ -> raise (TypeError("no variable here")))



  		| Sub(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Int_Val (value_1 - value_2)
  			| _ -> raise (TypeError("no variable here")))


  		| Mult(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Int_Val(value_1 * value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| Div(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val(0)  -> raise (DivByZeroError)
  			| Int_Val value_1, Int_Val value_2 -> Int_Val(value_1 / value_2)
  			| _ -> raise (TypeError("no variable here")))


  		| Pow(expr_1, expr_2) ->

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Int_Val(int_of_float(float_of_int value_1 ** float_of_int value_2))
  			| _ -> raise (TypeError("no variable here")))

  		| Greater(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Bool_Val(value_1 > value_2)
  			| _ -> raise (TypeError("no variable here")))


  		| Less(expr_1, expr_2) -> 
  			
  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val value_1, Int_Val value_2 -> Bool_Val(value_1 < value_2)
  			| _ -> raise (TypeError("no variable here")))


  		| GreaterEqual(expr_1, expr_2) ->
  			
  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val(value_1), Int_Val(value_2) -> Bool_Val(value_1 >= value_2)
  			| _ -> raise (TypeError("no variable here")))


  		| LessEqual(expr_1, expr_2) ->
  			
  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val(value_1), Int_Val(value_2) -> Bool_Val(value_1 <= value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| Equal(expr_1, expr_2) ->

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val(value_1), Int_Val(value_2) -> Bool_Val(value_1 = value_2)
  			| Bool_Val(value_1), Bool_Val(value_2) -> Bool_Val(value_1 = value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| NotEqual(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Int_Val(value_1), Int_Val(value_2) -> Bool_Val(value_1 != value_2)
  			| Bool_Val(value_1), Bool_Val(value_2) -> Bool_Val(value_1 != value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| Or(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Bool_Val(value_1), Bool_Val(value_2) -> Bool_Val(value_1 || value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| And(expr_1, expr_2) -> 

  			(match (eval_expr env expr_1, eval_expr env expr_2) with
  			| Bool_Val(value_1), Bool_Val(value_2) -> Bool_Val(value_1 && value_2)
  			| _ -> raise (TypeError("no variable here")))

  		| Not(expr) -> 

  			(match eval_expr env expr with
  			| Bool_Val(true) -> Bool_Val(false)
  			| Bool_Val(false) -> Bool_Val(true)
  			| _ -> raise (TypeError("no variable here")))

let rec eval_stmt env s = 
	
	match s with

	| NoOp -> env

	| Seq(stmt_1, stmt_2) -> 
		let env_1 = eval_stmt env stmt_1 in
		eval_stmt env_1 stmt_2

	| Declare(d_type, string_id) ->
		let exists = List.mem_assoc string_id env in

		if exists = true then raise (DeclareError("no variable here")) else 
			(match d_type with
			| Int_Type ->  (string_id, Int_Val(0)) :: env 
			| Bool_Type -> (string_id, Bool_Val(false)) :: env)
			

	| Assign(string_id, expr) ->
		let outp = eval_expr env expr in
		let exists = List.mem_assoc string_id env in

		if exists = false then raise (DeclareError("no variable here")) else 
			let cur_val = List.assoc string_id env in
			(match (outp, cur_val) with 
				| (Int_Val(new_val), Int_Val(_)) ->  (string_id, Int_Val(new_val)) :: env
				| (Bool_Val(new_val), Bool_Val(_)) ->  (string_id, Bool_Val(new_val)) :: env
				| _ -> raise (TypeError("no variable here")))
			

	| If(guard_expr, body_a, body_b) -> 
		let guard = eval_expr env guard_expr in
		(match guard with
    	| Bool_Val(true) ->  eval_stmt env body_a 
   	 	| Bool_Val(false) -> eval_stmt env body_b
    	| _ -> raise (TypeError("no variable here")))
		

	| While(guard_expr, body) -> 
    	let guard = eval_expr env guard_expr in
    	(match guard with
    	| Bool_Val(true) -> eval_stmt (eval_stmt env body) s
   	 	| Bool_Val(false) -> env
    	| _ -> raise (TypeError("no variable here")))
		

	| DoWhile(body, guard_expr) -> 
    	
    	let env_1 = eval_stmt env body in

    	let guard = eval_expr env_1 guard_expr in
		(match guard with
    		| Bool_Val(true) -> eval_stmt (eval_stmt env_1 body) s
   	 		| Bool_Val(false) -> env_1
    		| _ -> raise (TypeError("no variable here")))
		
    	

	| Print(expr) -> 
		let outp = eval_expr env expr in
		(match outp with 
		| Int_Val(x) -> print_output_int x ; print_output_newline() ; env
		| Bool_Val(x) -> print_output_bool x ; print_output_newline() ; env)
		
	

