(* The prime factors of 13195 are 5, 7, 13 and 29.

What is the largest prime factor of the number 600851475143? *)

module Answer = struct
    let limit = Float.to_int(Float.sqrt 13195.0)
    let is_prime n =
        let rec aux num next = 
            next <= 1 || ((num mod next) <> 0) && aux num (next - 1)
        in
        aux n (n - 1)

    let () = Printf.printf "28: %b\n29: %b\n" (is_prime 28) (is_prime 29)
end
