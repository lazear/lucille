
signature PIECES = sig
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside

  exception Parse
  type algebraic = 
    { piece: rank
    , start: (int * int)
    , final: (int * int) 
    }

  val parse : string -> algebraic
  val toChar : piece -> char
  val fromChar : char -> piece
end

structure Pieces = struct
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside

  type algebraic = 
    { piece: rank
    , start: (int * int) 
    , final: (int * int) 
    }

  exception Parse
  fun toChar (Black Pawn) = #"p"
    | toChar (Black Knight) = #"n"
    | toChar (Black Bishop) = #"b"
    | toChar (Black Rook) = #"r"
    | toChar (Black Queen) = #"q"
    | toChar (Black King) = #"k"
    | toChar (White Pawn) = #"P"
    | toChar (White Knight) = #"N"
    | toChar (White Bishop) = #"B"
    | toChar (White Rook) = #"R"
    | toChar (White Queen) = #"Q"
    | toChar (White King) = #"K"
    | toChar Empty = #"_"
  
  fun fromChar #"p" = (Black Pawn)
    | fromChar #"n" = (Black Knight)
    | fromChar #"b" = (Black Bishop)
    | fromChar #"r" = (Black Rook)
    | fromChar #"q" = (Black Queen)
    | fromChar #"k" = (Black King)
    | fromChar #"P" = (White Pawn)
    | fromChar #"N" = (White Knight)
    | fromChar #"B" = (White Bishop)
    | fromChar #"R" = (White Rook)
    | fromChar #"Q" = (White Queen)
    | fromChar #"K" = (White King)
    | fromChar _ = Empty

  fun strip (White x) = x
    | strip (Black x) = x 
    | strip (Empty)   = raise Fail "no rank" 

  fun parseRank [] = raise Parse
    | parseRank (c::cs) = 
      let val (piece, xs) = case Char.isUpper c
	   of true => (strip (fromChar c), cs)
	    | false => (Pawn, c::cs)
      in (piece, xs) end
  
  fun parseTgt [] = raise Parse
    | parseTgt (c::r::xs) = 
      let val col = if Char.isAlpha c then ord c - ord #"a" else raise Parse
	  val row = if Char.isDigit r then ord r - ord #"0" else raise Parse
      in ((row, col), xs) end 

  fun parseCheck (#"+" :: xs) = (true, xs)
    | parseCheck xs = (false, xs)
  fun parse xs = 
    let val (piece, xs) = parseRank xs
	val (start, xs) = parseTgt xs
	val (final, xs) = parseTgt xs
    in {piece=piece, start=start, final=final} end
  val parse = parse o explode
end

