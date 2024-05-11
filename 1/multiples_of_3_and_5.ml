(* If we list all the natural numbers below 10 that are multiples of 3 or 5,
 we get 3, 5, 6 and 9. The sum of these multiples is 23.

Find the sum of all the multiples of 3 or 5 below 1000. *)

module Answer = struct
    let nums = List.init 999 (fun i -> i + 1)
    let is_divisible n = n mod 3 = 0 || n mod 5 = 0
    let matches = List.filter is_divisible nums 
    let rec sum = function
        | [] -> 0
        | h :: t -> h + sum t
    let () = print_int (sum matches)
end

module UpdatedAnswer = struct
  let rec f n = 
    match n with
      | n when n < 3 -> 0
      | n when n mod 3 = 0 || n mod 5 = 0 -> n + f (n-1)
      | _ -> f (n-1);;
  let () = print_int (f 999);;
end
