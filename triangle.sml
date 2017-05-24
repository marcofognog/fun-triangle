CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun append (one : 'a list, two : 'a list) =
    if null one
    then two
    else (hd one) :: append(tl one, two)

fun invert (xs : 'a list list) =
  if null xs
  then []
  else append(invert(tl xs), [(hd xs)])

fun gr (p : int*int) =
  Int.max(#1 p, #2 p)

fun to_pairs (l : int list) =
  if null(l) orelse null(tl(l)) 
  then []
  else ((hd l), (hd (tl l))) :: to_pairs(tl(l))

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
