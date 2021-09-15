
(* module B = Blasso.Blas
 * module S = Blasso.Stencil
 * (\* module Z = Blasso.Zipper *\)
 *
 * let x0 = [| 0.; 1.; 2.; 3.; 4.; 5.; 6.; 7.; 8.; 9. |];;
 * let x1 = [| 0.; 1.; 2.; 3.; 400.; 5.; 6.; 7.; 8.; 9. |];;
 * let y0 = Array.map (fun y -> 2. *. y) (Array.copy x0);;
 * let s0 = 570.;;
 * let index = 4;;
 * let value = x1.(index);;
 *
 * let res =
 *   (\* 1. +. 2. +. 3. = 6. *\)
 *   let xs = [|1.; -2.; 3.;|] in
 *   let u0 = B.Abs_sum.make 3 in
 *   let u1 =
 *     u0
 *     |> B.Abs_sum.update ~stencil:{coord=X; index=1; value=30.0} (\* 1. +. 30. +. 3.  = 34. *\)
 *     |> B.Abs_sum.update ~stencil:{coord=Y; index=2; value=(-3.0)} in
 *   let u2 =
 *     u1
 *     |> B.Abs_sum.update ~stencil:{coord=Y; index=2; value=(-3.0)}
 *     |> B.Abs_sum.update ~stencil:{coord=X; index=0; value=20.0} in (\* 20. +. 30. +. 3. = 25. *\)
 *   u0.calc xs, u1.calc xs, u2.calc xs, xs
 * ;; *)
