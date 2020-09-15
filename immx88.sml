structure ImmX88 : BOARD = 
struct
  open X88
  open X88Moves
  exception Invalid = X88.Invalid 

  type position = Word8.word
  type board = Piece.piece vector; 
  
  fun foldli f acc b = X88.foldl (fn (i, acc) => f (i, Vector.sub(b, Word8.toInt i), acc)) acc

  fun empty () = Vector.tabulate (0x80, fn _ => Piece.Empty)
  fun copy src = src 
  fun sub b p = Vector.sub (b, Word8.toInt p)
  fun update b p v = Vector.update (b, Word8.toInt p, v)
  
  fun init () = 
    let open Piece 
        val rankify = 
         fn 0 => Rook
          | 1 => Knight
          | 2 => Bishop
          | 3 => Queen
          | 4 => King
          | 5 => Bishop
          | 6 => Knight
          | 7 => Rook
          | _ => raise Fail "unreachable"  
        fun color (0,x) = White (rankify x)
          | color (1,_) = White Pawn
          | color (6,_) = Black Pawn
          | color (7,x) = Black (rankify x)
          | color _ = Empty
	fun run w = if valid w then (color o toRankFile) w else Empty	 
    in Vector.tabulate (0x80, run o Word8.fromInt)  end
end
