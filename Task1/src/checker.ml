open Utils
open Grammar
open Equaler

module H  = Hashtbl

class checker =
    object(self)
    
    val assump    = (H.create 1024 : (expr, int) H.t)
    val pos_expr  = (H.create 1024 : (int, expr) H.t)
    val mp        = (H.create 1024 : (expr, int * int) H.t)
    val impl      = (H.create 1024 : (expr, int list) H.t)
    val proved    = (H.create 1024 : (expr, int) H.t)
    
    method fill_assump assump' pos =
        match assump' with
        | []      -> ()
        | x :: xs -> 
            H.add assump x pos;
            self#fill_assump xs (pos + 1)
 
    method check_axioms e axioms' pos = 
        match axioms' with
        | []      -> None 
        | x :: xs -> if equal x e then Some pos
                     else self#check_axioms e xs (pos + 1)
    
    method check_assumptions e = if H.mem assump e then Some (H.find assump e) else None
    
    method check_modus_ponens e = if H.mem mp e then Some (H.find mp e) else None
    
    method get_proof e = 
        match self#check_assumptions e with
        | Some num -> Assumption num
        | None -> match self#check_axioms e axioms 1 with
            | Some num -> Axiom num
            | None -> match self#check_modus_ponens e with
                | Some (a, b) -> ModusPonens (a, b)
                | None -> NoProof
       
    method upd_old_impl list' pos = 
        match list' with
        | []      -> ()
        | x :: xs -> begin 
                     match H.find pos_expr x with
                     | Binary (Impl, _, b) -> H.replace mp b (x + 1, pos + 1);
                     | _ -> ();
                     self#upd_old_impl xs pos
                     end
       
    method upd_mp e pos = 
        if H.mem impl e then begin
            self#upd_old_impl (H.find impl e) pos;
            H.remove impl e
            end
                            
    method set_impl e pos =
        match e with
        | Binary (Impl, eL, eR) -> 
            if H.mem proved eL then H.replace mp eR (pos + 1, H.find proved eL + 1)
                else if H.mem impl eL then H.replace impl eL (pos :: H.find impl eL)
                    else H.add impl eL [pos]
        | _ -> ()            

    method check e pos =
        let ann = self#get_proof e in 
        H.add pos_expr pos e;
        self#upd_mp e pos;
        H.replace proved e pos;
        self#set_impl e pos;
        ann
    end
        