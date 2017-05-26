CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun append (one : 'a list, two : 'a list) =
  case one of
      [] => two
    | x::xs => x :: append(xs, two)

fun to_pairs (l : int list) =
  case l of
      [] => []
    | [x] => []
    | x::xs => (x, hd(xs)) :: to_pairs(xs)

fun solve_lines (line1 : (int*int) list, line2 : int list) =
  let
      fun solve_simple ((p1,p2), el) = Int.max(p1, p2) + el
  in
      case line1 of
          [] => []
        | x::xs => solve_simple(x, hd(line2)) :: solve_lines(xs, tl(line2))
  end

fun new_tree (lines : (int list) list) =
  let
      fun first (lines : (int list) list) = hd(lines)
      fun second (lines : (int list) list) = hd(tl(lines))
      fun the_rest (lines : (int list) list) = tl(tl(lines))
  in
      solve_lines(to_pairs(first(lines)), second(lines)) :: the_rest(lines)
  end

fun resolve (lines : (int list) list) =
  case lines of
      [] => []
    | [i] => i
    | x::xs => resolve(new_tree(x::xs))

fun triangle (l : int list list) =
  let
      fun invert (xs : 'a list list) =
        case xs of
            [] => []
          | x::xs => append(invert(xs), [x])
  in
      hd(resolve(invert(l)))
  end;

(* Tests *)
val res1 = triangle([[6],[3,5],[9,7,1],[4,6,8,4]]) = 26;
val res2 = triangle([[1],[1,1],[1,1,1],[1,1,1,1]]) = 4;

val res3 = triangle([[1],[2,1],[3,1,1],[1,1,1,100]]) = 103;
val res3 = triangle([[1],[2,1],[3,1,1],[1,1,100,1]]) = 104;
val res3 = triangle([[1],[2,1],[3,1,1],[1,100,1,1]]) = 106;
val res3 = triangle([[1],[2,1],[3,1,1],[100,1,1,1]]) = 106;

val res4 = to_pairs([1,2]) = [(1,2)];
val res5 = to_pairs([1,2,3]) = [(1,2),(2,3)];
val res6 = to_pairs([1,2,3,4]) = [(1,2),(2,3),(3,4)];
val res4 = to_pairs([1]) = [];
val res4 = to_pairs([]) = [];
