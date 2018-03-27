open Grammar

class equaler = 
    object (self)
    
    val hmap = (Hashtbl.create 1024 : (string, expr) Hashtbl.t)
    
    method equals a b =
        match (a, b) with
        | (Binary (Impl, a1, a2), Binary (Impl, b1, b2))
        | (Binary (Conj, a1, a2), Binary (Conj, b1, b2))
        | (Binary (Disj, a1, a2), Binary (Disj, b1, b2)) -> self#equals a1 b1 && self#equals a2 b2
        | (Not a, Not b)               -> self#equals a b
        | (Var a, b)                   -> 
            if Hashtbl.mem hmap a then
                Hashtbl.find hmap a = b
            else begin
                Hashtbl.add hmap a b;
                true
                end
        | (_, _)                       -> false        
    end

let equal a b = let eq = new equaler in
    eq#equals a b
