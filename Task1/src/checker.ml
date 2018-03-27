open Utils
open Grammar
open Equaler

module H = Hashtbl

class checker assump exprs'=
    object(self)
    
    val assump = assump
    val exprs  = Array.of_list exprs'
    val anns   = Array.make (List.length exprs') NoProof
    val mp     = (H.create 1024 : (expr, int * int) H.t)
    val impl   = (H.create 1024 : (expr, int list) H.t)
    val proved = (H.create 1024 : (expr, int) H.t)
 
    method checkBasic op e list' pos = 
        match list' with
        | []      -> None 
        | x :: xs -> if op x e then Some pos
                     else self#checkBasic op e xs (pos + 1)
    
    method checkAxioms e = self#checkBasic equal e axioms 1
    
    method checkAssumptions e = self#checkBasic (=) e assump 1
    
    method checkModusPonens e = if H.mem mp e then Some (H.find mp e) else None
    
    method getProof e = 
        match self#checkAssumptions e with
        | Some num -> Assumption num
        | None -> match self#checkAxioms e with
            | Some num -> Axiom num
            | None -> match self#checkModusPonens e with
                | Some (a, b) -> ModusPonens (a, b)
                | None -> NoProof
       
    method updOldImpl list' pos = 
        match list' with
        | []      -> ()
        | x :: xs -> begin 
                     match exprs.(x) with
                     | Binary (Impl, _, b) -> H.replace mp b (x + 1, pos + 1);
                     | _ -> ();
                     self#updOldImpl xs pos
                     end
       
    method updMP e pos = if H.mem impl e then begin
                self#updOldImpl (H.find impl e) pos;
                H.remove impl e
                end
                            
    method setImpl e pos =
        match e with
        | Binary (Impl, eL, eR) -> 
            if H.mem proved eL then H.replace mp eR (pos + 1, H.find proved eL + 1)
                else if H.mem impl eL then H.replace impl eL (pos :: H.find impl eL)
                    else H.add impl eL [pos]
        | _ -> ()            

    method check =
        for pos = 0 to Array.length exprs - 1 do
            let e = exprs.(pos) in
            anns.(pos) <- self#getProof e;
            self#updMP e pos;
            H.replace proved e pos;
            self#setImpl e pos;
        done;
        anns
    end

let checkProof exprs assump = (new checker assump exprs)#check
        