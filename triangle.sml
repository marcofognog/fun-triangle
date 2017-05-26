CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun append (one : 'a list, two : 'a list) =
  case one of
      [] => two
    | x::xs => x :: append(xs, two)

fun invert (xs : 'a list list) =
  case xs of
      [] => []
    | x::xs => append(invert(xs), [(x)])

fun gr (p1,p2) = Int.max(p1, p2)

fun to_pairs (l : int list) =
  case l of
      [] => []
    | [x] => []
    | x::xs => (x, hd(xs)) :: to_pairs(xs)

fun solve_simple (p : int*int, el : int) =
  gr(p) + el

fun solve_lines (line1 : (int*int) list, line2 : int list) =
  case line1 of
      [] => []
    | x::xs => solve_simple(x, hd(line2)) :: solve_lines(xs, tl(line2))

fun first (lines : (int list) list) = hd(lines)

fun second (lines : (int list) list) = hd(tl(lines))

fun generate_new_tree (lines : (int list) list) =
  solve_lines(to_pairs(first(lines)), second(lines)) :: tl(tl(lines))

fun resolve (lines : (int list) list) =
  case lines of
      [] => []
    | [i] => i
    | x::xs => resolve(generate_new_tree(x::xs))

fun triangle (l : int list list) = resolve(invert(l));

