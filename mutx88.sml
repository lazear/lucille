structure MutX88 =  
struct
  structure Piece = Piece
  open X88
  exception Invalid = X88.Invalid 
  exception Iter = X88.Iter

  type position = Word8.word
  type board = Piece.piece Array.array; 
  
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
    in Array.modifyi (fn (i,_) => color (deidx i) handle Invalid => Empty) board; board end

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
    
  fun fold'' (i, p, (acc, acc')) = 
    if not (valid (Word8.fromInt i)) then (acc, acc') else 
    if length acc = 7 then ([], (rev (p::acc)) :: acc')
    else (p::acc, acc') 

  val toFen = fn x => raise Fail "not yet"
(*
  val toFen =  #2 o (Array.foldli fold'' ([], [])) 
*)
  fun unfold (ch, (row, acc)) =
    if ch = #"/" then ([], row::acc) else  
    if Char.isDigit ch then (List.tabulate(ord ch - ord #"0", fn _ =>Piece.Empty) @
    row, acc) else (Piece.fromChar ch :: row, acc) 
 
  (* Kinda complicated. Unfold splits the char list into 8 lists of 8 chars
   * and we can think perform a nested foldr over the lists, updating the 
   * mutable array inplace as we go. Odd-indexed lists need to be reversed 
   * from FEN *)
  fun doit b = 
    (List.foldri (fn (i, x, _) => 
      List.foldri (fn (j, p, _) => 
        (update b (rf(i,j)) p; ())) () (if i mod 2 = 0 then x else rev x)) ())
    o op:: o (foldl unfold ([], [])) o explode 
   
  fun fromFen s = let val b = empty () in doit b s; b end
end
