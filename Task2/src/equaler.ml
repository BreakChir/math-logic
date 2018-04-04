open Grammar

let hmap = ref (Hashtbl.create 1024 : (string, expr) Hashtbl.t)
    
let rec equals a b =
    match (a, b) with
    | (Binary (Impl, a1, a2), Binary (Impl, b1, b2))
    | (Binary (Conj, a1, a2), Binary (Conj, b1, b2))
    | (Binary (Disj, a1, a2), Binary (Disj, b1, b2)) -> equals a1 b1 && equals a2 b2
    | (Not a, Not b)               -> equals a b
    | (Var a, b)                   -> 
        if Hashtbl.mem !hmap a then
            Hashtbl.find !hmap a = b
        else begin
            Hashtbl.add !hmap a b;
            true
            end
    | (_, _)                       -> false