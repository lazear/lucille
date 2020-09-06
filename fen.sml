
signature FEN = sig
  structure Board : BOARD
  structure Piece : PIECE
  sharing Board.Piece = Piece
  val fromFen : string -> Board.board
  val toFen : Board.board -> string
end

functor Fen(B : BOARD) :> FEN = struct
  structure Board = B;
  structure Piece = B.Piece;

  fun foldli f acc xs  = 
    let fun loop i f acc (x::xs) = f (i, x, loop (i+1) f acc xs)
	  | loop _ _ acc [] = acc
    in loop 0 f acc xs end

  fun fmt x = Option.getOpt ((Char.fromString o Int.toString) x, #"0") 

  fun serialize (7, Piece.Empty, (mt, ps)) = (0, fmt (mt+1) :: ps)
    | serialize (_, Piece.Empty, (mt, ps)) = (mt+1, ps)
    | serialize (_, x, (0, ps)) = (0, Piece.toChar x :: ps) 
    | serialize (_, x, (y, ps)) = (0, Piece.toChar x :: fmt y ::ps)
   
  fun serialize' (i, p, (acc, acc')) = 
      if length acc = 7
	then 
	  let val (_, xs) = foldli serialize (0, []) (p::acc)
	      val s = implode xs 
	  in ([], ( s :: acc')) end
	else (p::acc, acc') 
  val toFen = (String.concatWith "/") o #2 o (B.foldli serialize' ([], []))

  fun deserialize s = 
    let 
      fun inner (ch, (row, acc)) =
	if ch = #"/" 
	  then ([], row::acc) 
	  else if Char.isDigit ch 
	    then (List.tabulate(ord ch - ord #"0", fn _ =>Piece.Empty) @ row, acc) 
	    else (Piece.fromChar ch :: row, acc) 
      val tks = String.tokens (fn x => x = #"/") s
      val chrs = map ((foldr inner ([], [])) o explode) tks
    in map #1 chrs end

  fun deserialize' b = 
    (foldli (fn (i, x, b) => 
      foldli (fn (j, p, b ) => 
        (B.update b (B.rf (i,j)) p)) b x) b)
    o rev o deserialize 
  
  val fromFen = deserialize' (B.empty ()) 
end
