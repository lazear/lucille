structure MutX88 : BOARD =  
struct
  structure Piece = Piece
  open X88
  open X88Moves
  exception Invalid = X88.Invalid 

  type position = Word8.word
  type board = Piece.piece Array.array; 
 
  (* 
  fun foldli f = 
    let fun inner (i, p, acc) =
      let val w = Word8.fromInt i
	  val v = Word8.andb (w, 0wx88) = 0wx0
      in if v then f (w, p, acc) else acc end 
    in Array.foldli inner end
  *) 
  fun foldli f acc b = X88.foldl (fn (i, acc) => f (i, Array.sub (b, Word8.toInt i), acc)) acc 

  fun empty () = Array.array (0x80, Piece.Empty)
  fun copy src = let val dst = empty () in Array.copy {src=src, dst=dst, di=0}; dst end
  fun sub  b p = Array.sub (b, Word8.toInt p)
  fun update b p v = (Array.update (b, Word8.toInt p, v); b)
 
  fun init () = 
    let open Piece 
        val board = empty () 
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
    in X88.foldl (fn (i, acc) => Array.update (board, Word8.toInt i, color (toRankFile i))) (); board end
end
