open OUnit2

module IntOrdered = struct
  type t = int 

  let eq = (=)
  let lt = (<)
  let leq = (<=)
end

module IntSet = OSet.UnbalancedSet(IntOrdered)

let empty ctxt = 
  assert_bool "No member in empty set" (not (IntSet.member 7 IntSet.empty))

let s1 = IntSet.(empty |> insert 10 |> insert 3 |> insert 18 |> insert 7 |> insert 15)

let id x = x

let members ctxt = 
  let results = List.map (fun x -> IntSet.member x s1) [15; 10; 3; 18] in
  assert_bool "members" (List.for_all id results)

let nonmembers ctxt = 
  let results = List.map (fun x -> IntSet.member x s1) [8; 1; 9; 11; 22] in
  assert_bool "non members" (not (List.for_all id results))

let suite = 
  "Set test suite" >:::
    ["empty set" >:: empty;
     "members" >:: members;
     "non members" >:: nonmembers]

let () = 
  run_test_tt_main suite
