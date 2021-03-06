(*
Darpan Choudhary || dc3292
Xiao Lim || xl2669
Zion Lin || zel2109
Katherine Patton || kyp2106 
*)

(* Code generation: translate takes a semantically checked AST and
produces LLVM IR
*)

(* We'll refer to Llvm and Ast constructs with module names *)
module L = Llvm
module A = Ast
open Sast 

module StringMap = Map.Make(String)

(* Code Generation from the SAST. Returns an LLVM module if successful,
   throws an exception if something is wrong. *)
let translate (globals, functions, structs) =
	let context    = L.global_context () in
	(* Add types to the context so we can use them in our LLVM code *)
	let i32_t      = L.i32_type    context
	and i8_t       = L.i8_type     context
	and i1_t       = L.i1_type     context
	and float_t    = L.double_type context
	and void_t     = L.void_type   context 
	and p_t        = L.pointer_type (L.i8_type (context))
	(* and array_t    = L.array_type  context *)
	(* Create an LLVM module -- this is a "container" into which we'll 
	   generate actual code *)
	and the_module = L.create_module context "Statxt" in

	let struct_type_table:(string, L.lltype) Hashtbl.t = Hashtbl.create 8 in

	let make_struct_type sdecl =
		(* this creates a LLVM struct type for EACH individual struct and adds to 
		hashtable as key = name, value = struct type *)
		let struct_t = L.named_struct_type context sdecl.ssname in
		Hashtbl.add struct_type_table sdecl.ssname struct_t in 
		let _  = List.map make_struct_type structs 
	in
	let lookup_struct_type ssname = try Hashtbl.find struct_type_table ssname
		with Not_found -> raise(Failure("struct " ^ ssname ^ " not found"))
	in 
	(* Convert MicroC types to LLVM types *)
	let rec ltype_of_typ = function
		  A.Int    -> i32_t
		| A.Bool   -> i1_t
		| A.Float  -> float_t
		| A.Void   -> void_t
		| A.String -> p_t
		| A.Char   -> i8_t
		| A.Struct(ssname) -> lookup_struct_type ssname
		| A.Array(typ, size) -> L.array_type (ltype_of_typ typ) size
		(*| t -> raise (Failure ("Type " ^ A.string_of_typ t ^ " not implemented yet1"))*)
	in

	(* Define struct body *)

	let make_struct_body sdecl =
		let struct_typ : L.lltype = try Hashtbl.find struct_type_table sdecl.ssname
			with Not_found -> raise(Failure("struct type not defined")) in
		(* retrieve each struct member and its OCaml type *)
		let smembers_types = List.map (fun (t, _) -> t) sdecl.smembers in
		(* convert OCaml types into LLVM types and convert the list to an array *)
		let smembers_lltypes = Array.of_list (List.map ltype_of_typ smembers_types) in
		(* set the body of each named struct by passing through the members of the struct *)
		L.struct_set_body struct_typ smembers_lltypes true
	in ignore(List.map make_struct_body structs); (* this section is how we know what types are in a struct for allocation *)

	(* Each struct member has an index, for Sretrieve to access members *)
	let struct_element_index =
		let handles m each_struct = 
		(* get a list of member names for each struct*)
		let struct_element_names = List.map (fun (_, n) -> n) each_struct.smembers in
		let add_one n = n + 1 in
		(* function that returns a tuple of (map, index) *)
		let add_element_index (m, i) element_name =
			(StringMap.add element_name (add_one i) m, add_one i) in
		let struct_element_map (* map * index *) = 
			List.fold_left add_element_index (StringMap.empty, -1) struct_element_names
		in
		StringMap.add each_struct.ssname (fst struct_element_map) m  
	in
	List.fold_left handles StringMap.empty structs  
	in

	(* Declare each global variable; remember its value in a map *)
	let global_vars : L.llvalue StringMap.t =
		let global_var m (t, n) = 
			let init = match t with
				  A.Float -> L.const_float (ltype_of_typ t) 0.0
				| A.Array(typ, size) -> 
					L.const_null (L.array_type (ltype_of_typ typ) size)
				| A.String -> 
					let l = L.define_global "" (L.const_stringz context n) the_module in
					L.const_bitcast (L.const_gep l [|L.const_int i32_t 0|]) p_t
				| A.Struct _ -> raise(Failure("struct declaration in global not allowed"))
				| _ -> L.const_int (ltype_of_typ t) 0
			in StringMap.add n (L.define_global n init the_module) m in
	List.fold_left global_var StringMap.empty globals in

	let printf_t : L.lltype = 
		L.var_arg_function_type i32_t [| L.pointer_type i8_t |] in
	let printf_func : L.llvalue = 
		L.declare_function "printf" printf_t the_module in
	
	(* Declare the built-in atoi() function *)
  	let atoi_t = L.function_type i32_t [| p_t |] in
  	let atoi_func = L.declare_function "atoi" atoi_t the_module in

  	(* Declare the built-in itoc() function *)
  	let itoc_t = L.function_type i8_t [| i32_t |] in
  	let itoc_func = L.declare_function "int_to_char" itoc_t the_module in

	(* Declare the built-in open() function *)
  	let open_t = L.function_type p_t [| L.pointer_type i8_t; L.pointer_type i8_t |] in
  	let open_func = L.declare_function "open_file" open_t the_module in

  	(* Declare the built-in close() function *)
  	let close_t = L.function_type i32_t [| p_t |] in
  	let close_func = L.declare_function "close_file" close_t the_module in
   
  	(* Declare the built-in fputs() function as write() *)
  	let write_t = L.function_type i32_t [| L.pointer_type i8_t; i32_t; i32_t; p_t |] in 
  	let write_func = L.declare_function "write_file" write_t the_module in

  	(* Declare the built-in read() function *)
  	let read_t = L.function_type i32_t [| p_t; i32_t; i32_t; p_t |] in 
  	let read_func = L.declare_function "read_file" read_t the_module in

	(* Declare the built-in putstr() function *)
  	let putstr_t = L.function_type i32_t [| p_t; p_t |] in 
  	let putstr_func = L.declare_function "put_in_file" putstr_t the_module in

  	(* Declare the built-in strlen() function *)
  	let strlen_t = L.function_type i32_t [| p_t |] in 
  	let strlen_func = L.declare_function "strlen" strlen_t the_module in

  	(* Declare the built-in strcmp() function *)
  	let strcmp_t = L.function_type i32_t [| p_t; p_t|] in 
  	let strcmp_func = L.declare_function "strcmp" strcmp_t the_module in

  	(* Declare the built-in strcpy() function *)
  	let strcpy_t = L.function_type p_t [| p_t; p_t|] in 
  	let strcpy_func = L.declare_function "strcpy" strcpy_t the_module in

  	(* Declare the built-in strcat() function *)
  	let strcat_t = L.function_type p_t [| p_t; p_t|] in 
  	let strcat_func = L.declare_function "str_concat" strcat_t the_module in

  	(* Declare the built-in strget() function *)
  	let strget_t = L.function_type i8_t [| p_t; i32_t|] in 
  	let strget_func = L.declare_function "strget" strget_t the_module in

  	(* Declare string_lower() function *)
  	let lower_t = L.function_type p_t [| p_t |] in 
  	let lower_func = L.declare_function "string_lower" lower_t the_module in

    (* Declare heap storage function *)
    let calloc_t = L.function_type p_t [| i32_t ; i32_t|] in 
    let calloc_func = L.declare_function "calloc" calloc_t the_module in

    (* Declare free() from heap *)
    let free_t = L.function_type i32_t [| p_t |] in 
    let free_func = L.declare_function "free" free_t the_module in

	(* Declare isletter() function *)
	let isvalid_t = L.function_type i1_t [| i8_t |] in
	let isvalid_func = L.declare_function "is_valid_letter" isvalid_t the_module in

	(* Declare strappend() function *)
	let strappend_t = L.function_type i8_t [| p_t; i32_t; i8_t |] in
	let strappend_func = L.declare_function "string_append" strappend_t the_module in

	(* Declare moveptr() function *)
	let moveptr_t = L.function_type p_t [| p_t; i32_t |] in
	let moveptr_func = L.declare_function "ith_pointer" moveptr_t the_module in

	(* Declare substring() function *)
	let substring_t = L.function_type p_t [| p_t; i32_t; i32_t |] in
	let substring_func = L.declare_function "substring" substring_t the_module in

	(* Define each function (arguments and return type) so we can 
	 * define it's body and call it later *)
	let function_decls : (L.llvalue * sfunc_decl) StringMap.t =
		let function_decl m fdecl =
			let name = fdecl.sfname
			and formal_types = 
				Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
			in let ftype = L.function_type (ltype_of_typ fdecl.styp) formal_types in
		StringMap.add name (L.define_function name ftype the_module, fdecl) m in
	List.fold_left function_decl StringMap.empty functions in
  
	(* Fill in the body of the given function *)
	let build_function_body fdecl =
		let (the_function, _) = StringMap.find fdecl.sfname function_decls in
		let builder = L.builder_at_end context (L.entry_block the_function) in

		let int_format_str 		= L.build_global_stringptr "%d\n" "fmt" builder in
		let float_format_str 	= L.build_global_stringptr "%g\n" "fmt" builder in
		let string_format_str	= L.build_global_stringptr "%s\n" "fmt" builder in
		let char_format_str		= L.build_global_stringptr "%c\n" "fmt" builder in

		(* Construct the function's "locals": formal arguments and locally
		   declared variables.  Allocate each on the stack, initialize their
		   value, if appropriate, and remember their values in the "locals" map *)
		let local_vars =
			let add_formal m (t, n) p = 
			let () = L.set_value_name n p in
			let local = L.build_alloca (ltype_of_typ t) n builder in
			let _  = L.build_store p local builder in
			StringMap.add n local m 
		in

		(* Allocate space for any locally declared variables and add the
		 * resulting registers to our map *)
		let add_local m (t, n) =
			let local_var = L.build_alloca (ltype_of_typ t) n builder
			in StringMap.add n local_var m 
		in

		let formals = List.fold_left2 add_formal StringMap.empty fdecl.sformals
			(Array.to_list (L.params the_function)) in
			List.fold_left add_local formals fdecl.slocals 
		in

		(* Return the value for a variable or formal argument. First check
		 * locals, then globals *)
		let lookup n = try StringMap.find n local_vars
			with Not_found -> StringMap.find n global_vars
		in
		(* Construct code for an expression; return its value *)
		let rec expr builder ((_, e) : sexpr) = match e with
			  SIntlit i -> L.const_int i32_t i 
			| SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
			| SFliteral l -> L.const_float_of_string float_t l
			| SStrlit s -> L.build_global_stringptr s "tmp" builder
			| SCharlit c -> L.const_int i8_t (Char.code c)
			| SArraylit (sexprs, size)-> 	let ltype_of_arr = ltype_of_typ (fst(List.hd sexprs)) in
											let all_emements = List.map (fun x -> expr builder x) sexprs in
											let this_array = L.build_alloca (L.array_type ltype_of_arr size) "tmp" builder in
											let rec range i j = if i >= j then [] else i :: (range (i+1) j) in
											let index_list = range 0 size in
											List.iter (fun x ->
												let where = L.build_in_bounds_gep this_array [| L.const_int i32_t 0; L.const_int i32_t x |] "tmp" builder in
												let what = List.nth all_emements x in
												ignore (L.build_store what where builder)
											) index_list; L.build_load this_array "tmp" builder
			| SArraccess(exp0, exp) ->
				(match exp0 with
					  (_, SId s) -> let array_llvalue = lookup s in
							let where = L.build_in_bounds_gep array_llvalue [| L.const_int i32_t 0;(expr builder exp) |] "tmp" builder in
							let array_load = L.build_load where "tmp" builder in array_load
					| (_, SSretrieve (str, element)) -> (let s_llvalue = 
							(match str with
								  (_, SId s) ->
									let etype = fst( 
										let fdecl_locals = List.map (fun (t, n) -> (t, n)) fdecl.slocals in
										let fdecl_formals = List.map (fun (t, n) -> (t, n)) fdecl.sformals in
										try List.find (fun n -> snd(n) = s) fdecl_locals
										with Not_found -> try List.find (fun n -> snd(n) = s) fdecl_formals
												with Not_found -> raise (Failure("Unable to find " ^ s )))
									in
									(try match etype with
										  A.Struct _ -> let struct_llvalue = lookup s in struct_llvalue
										| _ -> raise (Failure("not found"))
									with Not_found -> raise (Failure(s ^ "not found")))
								| _ -> raise (Failure("lhs not found")))
							in
							let the_element = expr builder str in
							let the_element_lltype = L.type_of the_element in
							let the_element_opt = L.struct_name the_element_lltype in
							let the_element_name = (match the_element_opt with 
													  None -> ""
													| Some(s) -> s)
							in 
							let indices = StringMap.find the_element_name struct_element_index in
							let index = StringMap.find element indices in
							let array_llvalue = L.build_struct_gep s_llvalue index "tmp" builder in
							let where = L.build_in_bounds_gep array_llvalue [| L.const_int i32_t 0;(expr builder exp) |] "tmp" builder in
							let array_load = L.build_load where "tmp" builder in array_load)
					| _ -> raise(Failure("not an array")))
			| SNoexpr -> L.const_int i32_t 0
			| SId s -> L.build_load (lookup s) s builder
			| SAssign (e1, e2) -> 	let e1' = (match e1 with
												  (_, SId s) -> lookup s
												| (_, SSretrieve (str,element)) -> 
													(match str with
														  (_, SId s) ->
															let etype = fst( 
																let fdecl_locals = List.map (fun (t, n) -> (t, n)) fdecl.slocals in
																let fdecl_formals = List.map (fun (t, n) -> (t, n)) fdecl.sformals in
																try List.find (fun n -> snd(n) = s) fdecl_locals
																with Not_found -> try List.find (fun n -> snd(n) = s) fdecl_formals
																	with Not_found -> raise (Failure("Unable to find " ^ s )))
															in
															(try match etype with
																  A.Struct t->
																	let index_number_list = StringMap.find t struct_element_index in
																	let index_number = StringMap.find element index_number_list in
																	let struct_llvalue = lookup s in
																	let access_llvalue = L.build_struct_gep struct_llvalue index_number "tmp" builder in
																	access_llvalue
																| _ -> raise (Failure("not found"))
															with Not_found -> raise (Failure(s ^ "not found")))
														| _ -> raise (Failure("lhs not found")))
												| (_, SArraccess (exp0, exp)) -> 
																	(match exp0 with
																	  (_, SId s) -> let array_llvalue = lookup s in
																			let where = L.build_in_bounds_gep array_llvalue [| L.const_int i32_t 0;(expr builder exp) |] "tmp" builder in
																			where
																	| (_, SSretrieve (str, element)) -> (let s_llvalue = 
																			(match str with
																				  (_, SId s) ->
																					let etype = fst( 
																						let fdecl_locals = List.map (fun (t, n) -> (t, n)) fdecl.slocals in
																						let fdecl_formals = List.map (fun (t, n) -> (t, n)) fdecl.sformals in
																						try List.find (fun n -> snd(n) = s) fdecl_locals
																						with Not_found -> try List.find (fun n -> snd(n) = s) fdecl_formals
																							with Not_found -> raise (Failure("Unable to find " ^ s )))
																					in
																					(try match etype with
																						  A.Struct _ -> let struct_llvalue = lookup s in struct_llvalue
																						| _ -> raise (Failure("not found"))
																					with Not_found -> raise (Failure(s ^ "not found")))
																				| _ -> raise (Failure("lhs not found")))
																			in
																			let the_element = expr builder str in
																			let the_element_lltype = L.type_of the_element in
																			let the_element_opt = L.struct_name the_element_lltype in
																			let the_element_name = (match the_element_opt with 
																									  None -> ""
																									| Some(s) -> s)
																			in 
																			let indices = StringMap.find the_element_name struct_element_index in
																			let index = StringMap.find element indices in
																			let array_llvalue = L.build_struct_gep s_llvalue index "tmp" builder in
																			let where = L.build_in_bounds_gep array_llvalue [| L.const_int i32_t 0;(expr builder exp) |] "tmp" builder in
																			where)
																	| _ -> raise(Failure("not an array")))
												| _ -> raise (Failure "fudgesicles")
											)
									and e2' = expr builder e2 in
									let _ = L.build_store e2' e1' builder in e2'
			| SSretrieve (str, element) ->
				let s_llvalue = 
				(match str with
					  (_, SId s) ->
						let etype = fst( 
							let fdecl_locals = List.map (fun (t, n) -> (t, n)) fdecl.slocals in
							let fdecl_formals = List.map (fun (t, n) -> (t, n)) fdecl.sformals in
							try List.find (fun n -> snd(n) = s) fdecl_locals
							with Not_found -> try List.find (fun n -> snd(n) = s) fdecl_formals
								with Not_found -> raise (Failure("Unable to find " ^ s )))
						in
						(try match etype with
							  A.Struct _ -> let struct_llvalue = lookup s in struct_llvalue
							| _ -> raise (Failure("not found"))
						with Not_found -> raise (Failure(s ^ "not found")))
					| _ -> raise (Failure("lhs not found")))
				in
				let the_element = expr builder str in
				let the_element_lltype = L.type_of the_element in
				let the_element_opt = L.struct_name the_element_lltype in
				let the_element_name = (match the_element_opt with 
										  None -> ""
										| Some(s) -> s)
				in 
				let indices = StringMap.find the_element_name struct_element_index in
				let index = StringMap.find element indices in
				let access_llvalue = L.build_struct_gep s_llvalue index "tmp" builder in
					L.build_load access_llvalue "tmp" builder

			| SBinop (e1, op, e2) ->
				let (t, _) = e1
				and e1' = expr builder e1
				and e2' = expr builder e2 in
				if t = A.Float then (match op with 
					  A.Add     -> L.build_fadd
					| A.Sub     -> L.build_fsub
					| A.Mult    -> L.build_fmul
					| A.Div     -> L.build_fdiv 
					| A.Equal   -> L.build_fcmp L.Fcmp.Oeq
					| A.Neq     -> L.build_fcmp L.Fcmp.One
					| A.Less    -> L.build_fcmp L.Fcmp.Olt
					| A.Leq     -> L.build_fcmp L.Fcmp.Ole
					| A.Greater -> L.build_fcmp L.Fcmp.Ogt
					| A.Geq     -> L.build_fcmp L.Fcmp.Oge
					| A.And | A.Or ->
						raise (Failure "internal error: semant should have rejected and/or on float")
				) e1' e2' "tmp" builder 
				else (match op with
					  A.Add     -> L.build_add
					| A.Sub     -> L.build_sub
					| A.Mult    -> L.build_mul
					| A.Div     -> L.build_sdiv
					| A.And     -> L.build_and
					| A.Or      -> L.build_or
					| A.Equal   -> L.build_icmp L.Icmp.Eq
					| A.Neq     -> L.build_icmp L.Icmp.Ne
					| A.Less    -> L.build_icmp L.Icmp.Slt
					| A.Leq     -> L.build_icmp L.Icmp.Sle
					| A.Greater -> L.build_icmp L.Icmp.Sgt
					| A.Geq     -> L.build_icmp L.Icmp.Sge
				) e1' e2' "tmp" builder
      		| SUnop(op, e) ->
				let (t, _) = e in
					let e' = expr builder e in
						(match op with
							  A.Neg when t = A.Float -> L.build_fneg 
							| A.Neg                  -> L.build_neg
							| A.Not                  -> L.build_not) e' "tmp" builder
			| SIncDec(e, op) -> 
				let e' = expr builder e in
					(match op with 
					 | A.Inc -> ignore(expr builder (A.Int,(SAssign(e, (A.Int, SBinop(e, A.Add, (A.Int, SIntlit(1)))))))); e'                 
       				 | A.Dec -> ignore(expr builder (A.Int,(SAssign(e, (A.Int, SBinop(e, A.Sub, (A.Int, SIntlit(1)))))))); e'
       				) 
			| SCall ("print", [e]) | SCall ("printb", [e]) ->
				L.build_call printf_func [| int_format_str ; (expr builder e) |]
					"printf" builder
			| SCall ("printstr", [e]) -> 
				L.build_call printf_func [| string_format_str ; (expr builder e) |]
					"printf" builder
			| SCall ("printchar", [e]) -> 
				L.build_call printf_func [| char_format_str ; (expr builder e) |]
					"printf" builder
			| SCall ("printf", [e]) -> 
				L.build_call printf_func [| float_format_str ; (expr builder e) |]
					"printf" builder
			| SCall("atoi", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call atoi_func (Array.of_list x) "atoi" builder
            | SCall("itoc", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call itoc_func (Array.of_list x) "int_to_char" builder          		
			| SCall("open", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call open_func (Array.of_list x) "open_file" builder
      	 	| SCall("close", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call close_func (Array.of_list x) "close_file" builder
      	 	| SCall ("read", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call read_func (Array.of_list x) "read_file" builder
            | SCall ("putstr", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call putstr_func (Array.of_list x) "put_in_file" builder
      		| SCall("write", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call write_func (Array.of_list x) "fputs" builder
      		| SCall("strlen", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strlen_func (Array.of_list x) "strlen" builder
      		| SCall("strcmp", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strcmp_func (Array.of_list x) "strcmp" builder
            | SCall("strcpy", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strcpy_func (Array.of_list x) "strcpy" builder
      		| SCall("strcat", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strcat_func (Array.of_list x) "str_concat" builder
      		| SCall("strget", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strget_func (Array.of_list x) "strget" builder
      		| SCall("lower", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call lower_func (Array.of_list x) "string_lower" builder
			| SCall("calloc", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call calloc_func (Array.of_list x) "calloc" builder
      		| SCall("free", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call free_func (Array.of_list x) "free" builder
            | SCall("isletter", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call isvalid_func (Array.of_list x) "is_valid_letter" builder
            | SCall("strappend", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call strappend_func (Array.of_list x) "string_append" builder
            | SCall("moveptr", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call moveptr_func (Array.of_list x) "ith_pointer" builder
            | SCall("substring", e) -> let x = List.rev (List.map (expr builder) (List.rev e)) in
            	L.build_call substring_func (Array.of_list x) "substring" builder

			| SCall (f, args) ->
				let (fdef, fdecl) = StringMap.find f function_decls in
				let llargs = List.rev (List.map (expr builder) (List.rev args)) in
				let result = (match fdecl.styp with 
					  A.Void -> ""
					| _ -> f ^ "_result") in
				L.build_call fdef (Array.of_list llargs) result builder
		in
    
	(* Each basic block in a program ends with a "terminator" instruction i.e.
	   one that ends the basic block. By definition, these instructions must
	   indicate which basic block comes next -- they typically yield "void" value
	   and produce control flow, not values *)
	(* Invoke "instr builder" if the current block doesn't already
	   have a terminator (e.g., a branch). *)
	let add_terminal builder instr =
		(* The current block where we're inserting instr *)
		match L.block_terminator (L.insertion_block builder) with
			  Some _ -> ()
			| None -> ignore (instr builder) in

				(* Build the code for the given statement; return the builder for
				   the statement's successor (i.e., the next instruction will be built
				   after the one generated by this call) *)
				(* Imperative nature of statement processing entails imperative OCaml *)
				let rec stmt builder = function
					  SBlock (_,sl) -> List.fold_left stmt builder sl
					(* Generate code for this expression, return resulting builder *)
					| SExpr e -> let _ = expr builder e in builder 
					| SReturn e -> let _ = match fdecl.styp with
							(* Special "return nothing" instr *)
							  A.Void -> L.build_ret_void builder 
							(* Build return statement *)
							| _ -> L.build_ret (expr builder e) builder 
						in builder
					(* The order that we create and add the basic blocks for an If statement
					doesnt 'really' matter (seemingly). What hooks them up in the right order
					are the build_br functions used at the end of the then and else blocks (if
					they don't already have a terminator) and the build_cond_br function at
					the end, which adds jump instructions to the "then" and "else" basic blocks *)
					| SIf (predicate, then_stmt, else_stmt) ->
						let bool_val = expr builder predicate in
						(* Add "merge" basic block to our function's list of blocks *)
						let merge_bb = L.append_block context "merge" the_function in
						(* Partial function used to generate branch to merge block *) 
						let branch_instr = L.build_br merge_bb in

						(* Same for "then" basic block *)
						let then_bb = L.append_block context "then" the_function in
						(* Position builder in "then" block and build the statement *)
						let then_builder = stmt (L.builder_at_end context then_bb) then_stmt in
						(* Add a branch to the "then" block (to the merge block) 
						   if a terminator doesn't already exist for the "then" block *)
						let () = add_terminal then_builder branch_instr in

						(* Identical to stuff we did for "then" *)
						let else_bb = L.append_block context "else" the_function in
						let else_builder = stmt (L.builder_at_end context else_bb) else_stmt in
						let () = add_terminal else_builder branch_instr in

						(* Generate initial branch instruction perform the selection of "then"
						or "else". Note we're using the builder we had access to at the start
						of this alternative. *)
						let _ = L.build_cond_br bool_val then_bb else_bb builder in
						(* Move to the merge block for further instruction building *)
						L.builder_at_end context merge_bb

					| SWhile (predicate, body) ->
						(* First create basic block for condition instructions -- this will
						serve as destination in the case of a loop *)
						let pred_bb = L.append_block context "while" the_function in
						(* In current block, branch to predicate to execute the condition *)
						let _ = L.build_br pred_bb builder in

						(* Create the body's block, generate the code for it, and add a branch
						back to the predicate block (we always jump back at the end of a while
						loop's body, unless we returned or something) *)
						let body_bb = L.append_block context "while_body" the_function in
						let while_builder = stmt (L.builder_at_end context body_bb) body in
						let () = add_terminal while_builder (L.build_br pred_bb) in

						(* Generate the predicate code in the predicate block *)
						let pred_builder = L.builder_at_end context pred_bb in
						let bool_val = expr pred_builder predicate in

						(* Hook everything up *)
						let merge_bb = L.append_block context "merge" the_function in
						let _ = L.build_cond_br bool_val body_bb merge_bb pred_builder in
						L.builder_at_end context merge_bb

					(* Implement for loops as while loops! *)
				| SFor (e1, e2, e3, body) -> stmt builder
					( SBlock ([], [SExpr e1 ; SWhile (e2, SBlock ([], [body ; SExpr e3])) ] ))
				in

		(* Build the code for each statement in the function *)
		let builder = stmt builder (SBlock ([], fdecl.sbody)) in
			(* Add a return if the last block falls off the end *)
			add_terminal builder (match fdecl.styp with
				  A.Void -> L.build_ret_void
				| A.Float -> L.build_ret (L.const_float float_t 0.0)
				| t -> L.build_ret (L.const_int (ltype_of_typ t) 0))
		in

List.iter build_function_body functions;
the_module
