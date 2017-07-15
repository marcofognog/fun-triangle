CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun to_pairs (l) =
  case l of
      [] => []
    | [x] => []
    | x::xs => (x, hd(xs)) :: to_pairs(xs)

fun solve_simple ((p1,p2), el) = Int.max(p1, p2) + el

fun solve_lines (f, line1, line2) =
      case line1 of
          [] => []
        | x::xs => f(x, hd(line2)) :: solve_lines(f, xs, tl(line2))

fun traverse (f, lines) =
  case lines of
      [] => []
    | [i] => i
    | first::second::the_rest => traverse(f, solve_lines(f, to_pairs(first), second) :: the_rest)

fun triangle (l) =
  hd(traverse(solve_simple, List.rev(l)));

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
