
signature BOARD = sig
  type board
  type position
  
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside
  and move = Standard of position * position | Castle of side

  exception InvalidMove
  
  val new : unit -> board
  val sub : board * position -> piece
  val fromFEN : string -> board
  val toFEN   : board -> string
end

structure Mailbox = 
struct
  type position = int * int
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside
  and move = Standard of position * position | Castle of side

  type board = piece Array2.array

  exception InvalidMove
  
  val empty : piece list = List.tabulate (8, fn _ => Empty) 
  fun pawns color = List.tabulate (8, fn _ => color Pawn)
  fun back color  = map color [Rook, Knight, Bishop, Queen, King, Bishop, Knight, Rook]
  
  fun new () = Array2.fromList [back Black, pawns Black, empty, empty, empty, empty, pawns White, back White]  

  fun sub (board, (x, y)) = Array2.sub (board, x, y)

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
  
  fun fold (_, 7, Empty, (mt, ps)) = (0, chr (mt+1) :: ps)
    | fold (_, c, Empty, (mt, ps)) = (mt+1, ps)
    | fold (_, c, x, (0, ps)) = (0, emit c x ps)
    | fold (_, c, x, (y, ps)) = (0, chr y :: emit c x ps) 
  and emit 7 x xs = toChar x :: #"/" :: xs
    | emit _ x xs = toChar x :: xs

  val fenify = Array2.foldi Array2.RowMajor fold (0, []) 
  val toFEN : board -> string = implode o #2 o fenify
  fun fromFEN s = raise Fail s
end
