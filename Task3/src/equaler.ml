open Grammar
    
let check_axioms' e = 
    match e with
    | Binary (Impl, a1, Binary (Impl, b1, a2)) 
	when (a1 = a2) -> Some (1)
	| Binary (Impl, Binary (Impl, a1, b1), Binary (Impl, Binary (Impl, a2, Binary (Impl, b2, c1)), Binary (Impl, a3, c2)))
	when (a1 = a2 && a2 = a3 && b1 = b2 && c1 = c2) -> Some (2)
	| Binary (Impl, a1, Binary (Impl, b1, Binary (Conj, a2, b2)))
	when (a1 = a2 && b1 = b2) -> Some (3)
	| Binary (Impl, Binary (Conj, a1, b1), a2)
	when (a1 = a2) -> Some (4)
	| Binary (Impl, Binary (Conj, a1, b1), b2)
	when (b1 = b2) -> Some (5)
	| Binary (Impl, a1, Binary (Disj, a2, b1))
	when (a1 = a2) -> Some (6)
	| Binary (Impl, b1, Binary (Disj, a1, b2))
	when (b1 = b2) -> Some (7)
	| Binary (Impl, Binary (Impl, a1, c1), Binary (Impl, Binary (Impl, b1, c2), Binary (Impl, Binary (Disj, a2, b2), c3)))
	when (a1 = a2 && b1 = b2 && c1 = c2 && c2 = c3) -> Some (8)
	| Binary (Impl, Binary (Impl, a1, b1), Binary (Impl, Binary(Impl, a2, Not (b2)), Not (a3))) 
	when (a1 = a2 && a2 = a3 && b1 = b2) -> Some (9)
	| Binary (Impl, Not (Not (a1)), a2)
	when (a1 = a2) -> Some (10)
	| _ -> None