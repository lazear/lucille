(*fun $ (f, g) = f g
infixr 2 $
fun |> (f, g) = g o f
infix 5 |>*)

functor Mailbox (B : BOARD) =
struct
  structure Board = B
  structure Parser = Fen(B);

  open B.Piece
  exception Invalid = B.Invalid
  exception IllegalMove of string

  datatype move = Standard of B.position * B.position | Castle of side

  fun findRanks board p = 
    let fun loop (pos, White p', acc) = if p = p' then pos :: acc else acc
	  | loop (pos, Black p', acc) = if p = p' then pos :: acc else acc
	  | loop (_, _, acc) = acc
    in B.foldli loop [] end  

  fun domove invariant b src dst = 
    let val s = B.sub b src
        val d = B.sub b dst 
        val check = invariant (s, d)
    in B.update (B.update b src Empty) dst s end

  fun mkInvariant f reason (x: piece*piece) = if f x then x else raise IllegalMove reason 

  val noCaptureKing = mkInvariant (not o isRank King o #2) "Can't capture a King"
  val ckEnemies = mkInvariant (fn (s,d) => enemies s d) "Can't capture piece of same color"
  val ckDstEmpty = mkInvariant (fn x => #2 x = Empty) "Can't move to an occupied square"
  val ckSrcOccup = mkInvariant (fn x => #1 x <> Empty) "No piece in source square"

  (* Move a piece from src to dst
  * src must not be empty
  * dst must not have a piece *)
  val move = domove (ckDstEmpty)
  (* Attack a piece *) 
  val attack = domove (ckEnemies) 

  fun mv b s d = 
    let val src = B.fromAlg s  
        val dst = B.fromAlg d
    in move b src dst end
  
  fun mv' ((s,d), b) = mv b s d handle IllegalMove s => raise IllegalMove s
  
(*
  fun search board = print o (fn x => (x ^ "\n")) o (String.concatWith "\n")
     o rev o (map B.toAlg) o (findRanks board) *)
  
  val fromFen = Parser.fromFen

  fun runMoves board = (List.foldl mv' board) handle IllegalMove s => raise Fail ("Illegal Move: " ^ s)
  val runMoves = runMoves handle IllegalMove s => raise Fail s
end

structure M2 = Mailbox(ImmX88)

val moves = [("e2", "e4"), ("c7", "c5"), ("g1", "f3"), ("e4", "c5")]

val b = M2.runMoves (M2.Board.init ()) moves handle M2.IllegalMove s => raise Fail
s
val () = print "hello"
