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

fun to_pairs (l : int list) =
  if null l
  then []
  else ((hd l), (hd (tl l))) :: to_pairs(tl (tl l))

fun solve_simple (p : int*int, el : int) =
  gr(p) + el

fun solve_lines (line1 : int*int list, line2 : int list) =
  if null line2
  then []
  else solve_simple(hd(line1), hd(line2)) :: solve_lines(tl(line1), tl(line2))

                                                      (*
fun resolve (l : int list list) =
  if only_one_element(line[0])
  then hd line[0]
  else resolve(solve_lines(to_pairs(lines[0]), lines[1]) :: tl lines)

fun triangle (l : int list list) =
  resolve(invert(l))
*)