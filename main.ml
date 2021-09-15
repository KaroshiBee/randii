module R = Randii.Threefry

let _ = ()
(* let x0 = [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |] in
 * let x1 = [| 0.; 1.; 2.; 3.; 400.; 5.; 6.; 7.; 8.; 9. |] in
 * let y0 = Array.map (fun y -> 2. *. y) (Array.copy x0) in
 * let s0 = 570. in
 * let index = 4 in
 * let value = x1.(index) in
 * let coord = B.X in
 * let d0 = B.Dot (B.Dot_data.init ~xarr:(Some x0) ~yarr:(Some y0)) in
 * let d1 = B.update d0 ~index ~value ~coord in
 * let s1 = match d1 with
 *   | B.Dot dot_data ->
 *     B.Dot_data.result dot_data
 *     |> Option.value ~default:0.
 *   | _ -> raise (Sys_error "Unknown Blas op")
 * in
 * let ss = Printf.sprintf "s0: %f, s1: %f, 3738." s0 s1 in
 * print_endline ss *)
