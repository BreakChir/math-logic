open Grammar
open Checker
open Utils

module H  = Hashtbl

let rec deduct assump e =
    match assump with
    | [] -> e
    | x :: xs -> deduct xs (x --> e)

let get_vars expr =
    let vars = (H.create 3 : (string, int) H.t) in
    let var_list = ref [] in
    let rec get_vars' = function
        | Var s -> if not (H.mem vars s) then begin H.add vars s 0; var_list := !var_list @ [s]; end
        | Not a -> get_vars' a
        | Binary (_, a, b) -> get_vars' a;
                              get_vars' b
    in
    get_vars' expr;
    !var_list

class prover assump expr oc =
    let e' = deduct assump expr in
    let var_list' = get_vars e' in
    object(self)
    
    val e = e'
    val str_e = string_of_expr e'
    val vars = (H.create 3 : (string, expr) H.t)
    val var_list = var_list'
    
    method get_lemma file_name a b =
        List.map (parse_expr a b) (read_file ("proofs/" ^ file_name ^ ".txt"))
        
    method get_proof =
        let apply_op = function
        | Conj -> (&&)
        | Disj -> (||)
        | Impl -> fun a b -> (not a) || b
        in
        let get_name a b = function
        | Conj -> "conj_" ^ string_of_bool a ^ string_of_bool b
        | Disj -> "disj_" ^ string_of_bool a ^ string_of_bool b
        | Impl -> "impl_" ^ string_of_bool a ^ string_of_bool b
        in
        let rec get_proof' = function
        | Var s -> (let value = H.find vars s in
                   match value with
                   | Not a -> (false, [value])
                   | _ -> (true, [value]))
        | Not a -> let (is_pr, pr) = get_proof' a in
                   (not is_pr, pr @ (self#get_lemma ("not_" ^ string_of_bool is_pr)
                   (string_of_expr a) ""))
        | Binary (_ as op, a, b) -> 
                    let (is_pr_a, pr_a) = get_proof' a in
                    let (is_pr_b, pr_b) = get_proof' b in
                    (apply_op op is_pr_a is_pr_b,
                    pr_a @ pr_b @ (self#get_lemma (get_name is_pr_a is_pr_b op)
                    (string_of_expr a) (string_of_expr b)))
        in
        get_proof' e

    method deduce assump' proof' var' =
        let worker = new checker var' in
        worker#fill_assump assump' 1;
        let rec read_proof pr pos =
           match pr with
           | [] -> []
           | x :: xs -> (* Expr -> String -> Expr 
                        если не пропарсить заново, то высказывания, которые должны быть одинаковыми, будут разными
                        хз почему, где то криво пишу*)
                        let exx = parse_ex (string_of_expr x) in
                        let a = worker#check exx pos in
                        a @ (read_proof xs (pos + 1))
        in
        read_proof proof' 0

    
    method get_ans_str variables =
        let rec get_str = function
            | x :: [] -> (match x with
                            | Not (Var (var)) -> var ^ "=Л"
                            | Var (var)       -> var ^ "=И"
                            | _ -> "")
            | x :: xs -> (match x with
                            | Not (Var (var)) -> var ^ "=Л, " ^ (get_str xs)
                            | Var (var)       -> var ^ "=И, " ^ (get_str xs)
                            | _ -> "")
            | [] -> ""                
        in
        "Высказывание ложно при " ^ get_str variables

    method prove =
        let wrong = ref [] in
        let rec down assump' = function
            | [] -> let (is_proved, proof) = self#get_proof in
                   if (not is_proved) then wrong := assump';
                   (proof, is_proved) 
            | x :: xs -> let assump_false = assump' @ [Not(Var (x))] in
                         let assump_true = assump' @ [Var (x)] in
                         H.replace vars x (Not(Var (x)));
                         let (pr1, is_go1) = down assump_false xs in
                         if (not is_go1) then ([], false)
                         else begin
                         H.replace vars x (Var (x));
                         let (pr2, is_go2) = down assump_true xs in
                         if (not is_go2) then ([], false)
                         else begin
                         let d_pr1 = self#deduce assump' pr1 (Not (Var (x))) in                        
                         let d_pr2 = self#deduce assump' pr2 (Var (x)) in
                         let merge = self#get_lemma "_excluded" str_e x in
                         (d_pr1 @ d_pr2 @ merge, true) end end
        in
        let (pr, ok) = down [] (var_list) in
        if (not ok) then (pr, e, self#get_ans_str !wrong)
        else (pr, e, "")   
    end