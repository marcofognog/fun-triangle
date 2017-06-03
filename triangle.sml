CM.make "$smlnj-tdp/back-trace.cm";
SMLofNJ.Internals.TDP.mode := true;

fun to_pairs (l) =
  case l of
      [] => []
    | [x] => []
    | x::xs => (x, hd(xs)) :: to_pairs(xs)

fun solve_lines (line1, line2) =
  let
      fun solve_simple ((p1,p2), el) = Int.max(p1, p2) + el
  in
      case line1 of
          [] => []
        | x::xs => solve_simple(x, hd(line2)) :: solve_lines(xs, tl(line2))
  end

fun new_tree (first::second::the_rest) =
  solve_lines(to_pairs(first), second) :: the_rest

fun resolve (lines) =
  case lines of
      [] => []
    | [i] => i
    | x::xs => resolve(new_tree(x::xs))

fun triangle (l) =
  let
      fun invert (xs) =
        let fun aux(n, acc) =
        case n of
            [] => acc
          | x::xs' => aux(xs', x::acc)
        in
            aux(xs, [])
        end
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
