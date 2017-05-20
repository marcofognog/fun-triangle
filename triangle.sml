(**)
fun append (one : 'a list, two : 'a list) =
    if null one
    then two
    else (hd one) :: append(tl one, two)

fun invert (xs : 'a list list) =
  if null xs
  then []
  else append(invert(tl xs), [(hd xs)])

fun gr (p : int*int) =
  if #1 p > #2 p
  then #1 p
  else #2 p

fun solve_simple (l : int*int list list) =
  gr(hd (tl l)) + hd (hd l)

fun to_pairs(l : int list) =
  if null l
  then []
  else ((hd l), (hd (tl l))) :: to_pairs(tl (tl l))

                                        (*
fun solve(l : int list list) =
  val base = hd to_pairs(hd invert(l))
  val top = hd tl(invert(l))
*)
