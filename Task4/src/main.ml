open Printf
open Str

module H = Hashtbl
module A = Array

let (>>) x f = f x

let (ic, oc) = (open_in "input.txt", open_out "output.txt")
let n = ic >> input_line >> int_of_string

exception Sum of int * int
exception Mul of int * int
exception Distr of int * int * int
exception BoolAlg of int

let child  = A.make n ([] : int list)
let parent = A.make n (-1)

let big    = A.init n (fun _ -> (H.create 50 : (int, int) H.t))
let small  = A.init n (fun _ -> (H.create 50 : (int, int) H.t))

let sum    = A.init n (fun _ -> (A.make n 0))
let mul    = A.init n (fun _ -> (A.make n 0))
let pseudo = A.make n 0
let rel    = A.init n (fun _ -> (A.make n false))


let read =
    for v = 0 to n - 1 do
        let line = input_line ic in
        let ch = List.map int_of_string (Str.split (Str.regexp " +") line) in
        List.iter (fun u -> if ((u - 1)!= v && u <= n) then begin 
            child.(v) <- (u - 1) :: child.(v);
            parent.(u - 1) <- v end) ch   
    done;
    close_in ic

let set_values = 
    let rec dfs v root =
        H.replace big.(root) v v;
        H.replace small.(v) root root;
        rel.(v).(root) <- true;
        rel.(root).(v) <- true;
        List.iter (fun u -> dfs u root) child.(v)
    in
    for i = 0 to n - 1 do
        dfs i i
    done
    
let checking =
    let basic big' small' arr' exc' =
        let rai_e exc a b =
            match exc with
            | "Sum" -> Sum (a, b)
            | "Mul" -> Mul (a, b)
        in  
        for a = 0 to n - 1 do
            for b = a to n - 1 do
                let exist = ref false in
                let min_c = ref 0 in
                let cc = ref ([] : int list) in
            
                let peg = fun c v -> if (H.mem big'.(b) c) then 
                    if !exist then begin
                        if (H.mem small'.(!min_c) c) then 
                            min_c := c
                        else if (rel.(!min_c).(c) = false) then
                            cc := c :: !cc
                    end        
                    else begin
                        exist := true;
                        min_c := c;
                    end
                in
            
                H.iter peg big'.(a);
            
                List.iter (fun c -> if (rel.(!min_c).(c) = false) then exist := false) !cc;
            
                if !exist then begin
                    arr'.(a).(b) <- !min_c;
                    arr'.(b).(a) <- !min_c
                    end
                else raise (rai_e exc' (a + 1) (b + 1))   
            done
        done
    in    

    try 
    basic big small sum "Sum";
    basic small big mul "Mul";
    
    for a = 0 to n - 1 do
        for b = 0 to n - 1 do
            for c = 0 to n - 1 do
                if (mul.(a).(sum.(b).(c)) != sum.(mul.(a).(b)).(mul.(a).(c))) then raise (Distr (a + 1, b + 1, c + 1))  
            done
        done
    done;
        
    let rec find_zero v =
        if (parent.(v) == -1) then v
        else find_zero parent.(v)
    in
    
    let zero = find_zero 0 in
    for a = 0 to n - 1 do
        let max_c = ref (-1) in
        for c = 0 to n - 1 do
            if (mul.(a).(c) = zero) then 
                if !max_c != (-1) then begin
                    if (H.mem big.(!max_c) c) then 
                        max_c := c
                end        
                else max_c := c;
        done;
        pseudo.(a) <- !max_c
    done;

    let one = pseudo.(zero) in
    for a = 0 to n - 1 do
        if (sum.(a).(pseudo.(a)) != one) then raise (BoolAlg (a + 1)) 
    done;
    
    fprintf oc "Булева алгебра";
    with 
    | Sum (a, b)      -> fprintf oc "Операция '+' не определена: %d+%d" a b
    | Mul (a, b)      -> fprintf oc "Операция '*' не определена: %d*%d" a b
    | Distr (a, b, c) -> fprintf oc "Нарушается дистрибутивность: %d*(%d+%d)" a b c
    | BoolAlg a       -> fprintf oc "Не булева алгебра: %d+~%d" a a;
    close_out oc
    