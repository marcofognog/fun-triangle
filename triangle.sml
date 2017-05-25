CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun append (one : 'a list, two : 'a list) =
    if null one
    then two
    else (hd one) :: append(tl one, two)

fun invert (xs : 'a list list) =
  case xs of
  [] => []
| x::xs => append(invert(xs), [(x)])

fun gr (p1,p2) = Int.max(p1, p1)

fun to_pairs (l : int list) =
  case l of
      [] => []
    | [x] => []
    | x::xs => (x, hd(xs)) :: to_pairs(xs)

fun solve_simple (p : int*int, el : int) =
  gr(p) + el

fun solve_lines (line1 : (int*int) list, line2 : int list) =
  if null(line1)
  then []
  else solve_simple(hd(line1), hd(line2)) :: solve_lines(tl(line1), tl(line2))

fun first (lines : (int list) list) =
  hd(lines)

fun second (lines : (int list) list) =
  hd(tl(lines))

fun generate_new_tree (lines : (int list) list) =
  solve_lines(to_pairs(first(lines)), second(lines)) :: tl(tl(lines))

fun resolve (lines : (int list) list) =
  if null(tl(lines))
  then hd(lines)
  else resolve(generate_new_tree(lines))

fun triangle (l : int list list) =
  resolve(invert(l))
