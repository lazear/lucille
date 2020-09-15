(*fun $ (f, g) = f g
infixr 2 $
fun |> (f, g) = g o f
infix 5 |>*)

functor Mailbox (B : BOARD) =
struct
  structure Board = B
  structure Parser = Fen(B);

  (* open B.Piece *)
  open Piece
  exception Invalid = B.Invalid

  exception Violation of string
  exception IllegalMove of string * B.position * B.position

  datatype move = Standard of B.position * B.position | Castle of side

  fun findRanks board p = 
    let fun loop (pos, White p', acc) = if p = p' then pos :: acc else acc
	  | loop (pos, Black p', acc) = if p = p' then pos :: acc else acc
	  | loop (_, _, acc) = acc
    in B.foldli loop [] end  

  fun domove invariant b src dst = 
    let val s = B.sub b src
        val d = B.sub b dst 
        val check = invariant (s, d) handle Violation s => raise IllegalMove (s, src, dst)
    in B.update (B.update b src Empty) dst s end


  fun canMove invariant b src dst = 
    let val s = B.sub b src
        val d = B.sub b dst 
        val check = (invariant (s, d); true) handle Violation s => false
    in check end


  fun mkInvariant f reason (x: piece*piece) = if f x then x else raise (Violation reason)
  fun mkOr a b x = (a x handle _ => b x) 

  val noCaptureKing = mkInvariant (not o isRank King o #2) "Can't capture a King"
  val ckEnemies = mkInvariant (fn (s,d) => enemies s d) "Can't capture piece of same color"
  val ckDstEmpty = mkInvariant (fn x => #2 x = Empty) "Can't move to an occupied square"
  val ckSrcOccup = mkInvariant (fn x => #1 x <> Empty) "No piece in source square"

  (* Move a piece from src to dst
  * src must not be empty
  * dst must not have a piece *)
  val move = domove (ckSrcOccup o (mkOr ckDstEmpty ckEnemies))
  (* Attack a piece *) 
  val attack = domove (ckEnemies o ckSrcOccup) 

  fun mv b s d = 
    let val src = B.fromAlg s  
        val dst = B.fromAlg d
    in move b src dst end
  
  fun mv' ((s,d), b) = mv b s d
  
(*
  fun search board = print o (fn x => (x ^ "\n")) o (String.concatWith "\n")
     o rev o (map B.toAlg) o (findRanks board) *)
  
  val fromFen = Parser.fromFen

  fun fmtErr (reason, src, dst) = "Illegal move! " ^ reason ^ ": " ^ B.toAlg src ^ "->" ^ B.toAlg dst

  fun runMoves board s = (List.foldl mv' board) s handle IllegalMove s => raise Fail (fmtErr s)

  (* structure Moves = X88Moves *)
  val psuedoLegal = canMove (mkOr ckEnemies ckDstEmpty)
  fun generate board pos = 
    let val piece = B.sub board pos
        val mvs = B.moves piece pos false (* no enpasssant *)
        val plegal = List.filter (fn x => psuedoLegal board pos x) mvs 
    in plegal end
end

structure M2 = Mailbox(ImmX88)

val moves = [("e2", "e4"), ("c7", "c5"), ("g1", "f3"), ("e4", "c5")]

(* val b = M2.runMoves (M2.Board.init ()) moves  *)
val () = print "hello"
