
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

signature BOARD = sig
  structure P : PIECES
  type board
  type position
  datatype move = Standard of position * position | Castle of Pieces.side
  
  exception InvalidMove
  
  val new : unit -> board
  val sub : board * position -> Pieces.piece
  val fromFEN : string -> board
  val toFEN   : board -> string
end

structure Mailbox = 
struct
  structure P = Pieces;
  open P;
  type position = int * int
  datatype move = Standard of position * position | Castle of P.side

  type board = piece Array2.array

  exception InvalidMove 
  
  val empty : piece list = List.tabulate (8, fn _ => Empty) 
  fun pawns color = List.tabulate (8, fn _ => color Pawn)
  fun back color  = map color [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  
  fun new () = Array2.fromList [back Black, pawns Black, empty, empty, empty, empty, pawns White, back White]  

  fun sub (board, (x, y)) = Array2.sub (board, x, y)

  fun fmt x = Option.getOpt ((Char.fromString o Int.toString) x, #"0") 
  fun fold (_, 7, Empty, (mt, ps)) = (0, #"/" :: fmt (mt+1) :: ps)
    | fold (_, c, Empty, (mt, ps)) = (mt+1, ps)
    | fold (_, c, x, (0, ps)) = (0, emit c x ps)
    | fold (_, c, x, (y, ps)) = (0, emit c x (fmt y ::ps))
  and emit 7 x xs = #"/" :: toChar x :: xs
    | emit _ x xs = toChar x :: xs

  fun region b : piece Array2.region = {base=b, col=0, row=0, ncols=NONE, nrows=NONE}
  fun fenify board = Array2.foldi Array2.RowMajor fold (0, []) (region board)
  val toFEN : board -> string = implode o rev o tl o #2 o fenify
 
  fun fromFEN s = raise Fail s

  fun adj (row,col) = (8-row, col)
  val parse = (fn {piece,start,final} => {piece=piece, start=adj start, final=adj final}) o parse

  fun distance {piece, start, final} = 
    let val (row, col) = start
	val (row', col') = final
    in (row' - row, col' - col) end 
  
  (* (row, col) -> en_passant -> rank *)
  fun valid_move (r, c) ep (White Pawn) = 
    if r > 0
    then InvalidMove
    else if c <> 0 andalso not ep
    then InvalidMove
    else true 
    



  fun move board {piece, start, final} = 
    let val x  = sub (board, start)
	val p  = strip x handle Fail _ => raise InvalidMove
	val x' = sub (board, final)
	(* handle attacking? *)
	val _ = if x' <> Empty then () else ()
	fun run () = 
	  (Array2.update (board, #1 start, #2 start, Empty);
	   Array2.update (board, #1 final, #2 final, x))
    in if piece <> p then raise InvalidMove else run () end
end
