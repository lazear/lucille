structure ImmX88 : BOARD = 
struct
  structure Piece = Piece
  open X88
  exception Invalid = X88.Invalid 
  exception Iter = X88.Iter

  type position = Word8.word
  type board = Piece.piece vector; 
  
  fun empty () = Vector.tabulate (0x80, fn _ => Piece.Empty)
  fun copy src = src 
  fun sub  b p = Vector.sub (b, Word8.toInt p)
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
    in Vector.tabulate (0x80, fn i => color (deidx i) handle Invalid => Empty) end

  fun findRanks board p = 
    let fun loop ix acc =
          let val xs = 
              case sub board ix 
                of Piece.White p' => if p = p' then ix :: acc else acc
                 | Piece.Black p' => if p = p' then ix :: acc else acc
                 | Piece.Empty => acc
          in loop (iter ix) xs handle Iter => xs end 
    in loop 0wx0 [] end 
  
  fun fmt x = Option.getOpt ((Char.fromString o Int.toString) x, #"0") 

  fun fold (7, Piece.Empty, (mt, ps)) = (0, #"/" :: fmt (mt+1) :: ps)
    | fold (c, Piece.Empty, (mt, ps)) = (mt+1, ps)
    | fold (c, x, (0, ps)) = (0, emit c x ps)
    | fold (c, x, (y, ps)) = (0, emit c x (fmt y ::ps))
  and emit 7 x xs = #"/" :: Piece.toChar x :: xs
    | emit _ x xs = Piece.toChar x :: xs

  fun fold' (i, p, acc) = 
    let fun inner (_, f) = fold (f, p, acc)
    in (inner o deidx) i handle _ => acc end 

  val toFen = implode o tl o #2 o (Vector.foldli fold' (0, [])) 
  fun unfold (ch, (row, acc)) =
    if ch = #"/" then ([], row::acc) else  
    if Char.isDigit ch then (List.tabulate(ord ch - ord #"0", fn _ =>Piece.Empty) @
    row, acc) else (Piece.fromChar ch :: row, acc) 
   
   fun fromFen _ = raise Fail "unimplemented"
end
