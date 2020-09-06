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
  exception IllegalMove

  fun domove invariant b src dst = 
    let val s = B.sub b src
        val d = B.sub b dst 
        val check =
          if invariant (s, d) then () else raise IllegalMove
    in B.update (B.update b src Empty) dst s end
 
  (* Move a piece from src to dst
  * src must not be empty
  * dst must not have a piece *)
  val move = domove (fn (s, d) => s <> Empty andalso d = Empty) 
  (* Attack a piece *) 
  val attack = domove (fn (s,d) => s <> Empty andalso d <> Empty)

  fun mv b s d = 
    let val src = B.fromAlg s  
        val dst = B.fromAlg d
    in move b src dst end

  fun search board = print o (fn x => (x ^ "\n")) o (String.concatWith "\n")
     o rev o (map B.toAlg) o (B.findRanks board)
  
  val fromFen = Parser.fromFen
end

structure M2 = Mailbox(ImmX88)

