(*fun $ (f, g) = f g
infixr 2 $
fun |> (f, g) = g o f
infix 5 |>*)
signature PIECE = sig
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside

  type algebraic = 
    { piece: rank
    , start: (int * int)
    , final: (int * int) 
    }
 
  val rank : piece -> rank 
  val parse : string -> algebraic
  val toChar : piece -> char
  val fromChar : char -> piece
end

structure Piece :> PIECE  = struct
  datatype rank = Pawn | Knight | Bishop | Rook | Queen | King
  and piece = Black of rank | White of rank | Empty
  and side = Kingside | Queenside

  type algebraic = 
    { piece: rank
    , start: (int * int) 
    , final: (int * int) 
    }

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

  fun rank (White x) = x
    | rank (Black x) = x 
    | rank (Empty)   = raise Fail "no rank" 

  fun parseRank [] = raise Fail "no rank"
    | parseRank (c::cs) = 
      let val (piece, xs) = case Char.isUpper c
           of true => (rank (fromChar c), cs)
            | false => (Pawn, c::cs)
      in (piece, xs) end
  
  fun parseTgt [] = raise Fail "no target"
    | parseTgt (c::r::xs) = 
      let val col = if Char.isAlpha c then ord c - ord #"a" else raise Fail
      "invalid col"
          val row = if Char.isDigit r then ord r - ord #"0" else raise Fail
          "invalid row"
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

signature REPRESENTATION =
sig
  exception Invalid
  exception Iter
  
  structure Piece : PIECE  
  type board
  type position
  
  val new : unit -> board
  val initBoard : board -> unit
  val copy : board -> board -> unit
  val sub : board -> position -> Piece.piece
  val iter : position -> position
  val update : board -> position -> Piece.piece  -> unit
  val valid : position -> bool

  val findRanks : board -> Piece.rank -> position list

  (* from 0-based rank and file to positon *)
  val rf : int * int -> position

  val diff : position * position -> int * int

  (* convert to algebraic notation *)
  val toAlg : position -> string
  (* convert from algebraic notation *)
  val fromAlg : string -> position 
  
  val toFen   : board -> string
  val fromFen : string -> board
end



structure OhX88 : REPRESENTATION = 
struct
  
  structure Piece = Piece
  exception Invalid
  exception Iter
  
  local
    open Word8
  in
    val >> = >>
    val << = <<
    val && = andb
    val || = orb
    val ^^ = xorb
    infix 4 >> <<
    infix 3 && || ^^
  end

  type position = Word8.word
  type board = Piece.piece Array.array; 
  
  fun new () = Array.array (0x80, Piece.Empty)
  fun copy src dst = Array.copy {src=src, dst=dst, di=0}  

  fun sub  b p = Array.sub (b, Word8.toInt p)

  fun update b p v = Array.update (b, Word8.toInt p, v)
  val valid = fn x => (x && 0wx88) = 0wx0
  fun iter pos =
    let val x = Word8.+ (pos, 0wx1)
    in if x < 0wx80 then x else raise Iter end
  
  fun rf (rank,file) =
    let val r = Word8.fromInt rank
        val f = Word8.fromInt file
        val w = (r << 0wx4) || f
    in if valid w then w else raise Invalid end

  fun toRankFile w = 
    let val r = Word8.toInt (w >> 0wx4)
        val f = Word8.toInt (w && 0wx7)
    in (r, f) end
  
  fun findRanks board p = 
    let fun loop ix acc =
          let val xs = 
              case sub board ix 
                of Piece.White p' => if p = p' then ix :: acc else acc
                 | Piece.Black p' => if p = p' then ix :: acc else acc
                 | Piece.Empty => acc
          in loop (iter ix) xs handle Iter => xs end 
    in loop 0wx0 [] end 

  fun fromAlg (f::r::nil) = 
    let val r = ord r - ord #"1"
        val f = ord f - ord #"a"
    in rf (r, f) end

  val fromAlg = fromAlg o explode

  fun toAlg w = 
    let val (r, f) = toRankFile w
        val r = chr (r + ord #"1")
        val f = chr (f + ord #"a")
    in implode [f,r] end
  
  fun diff (a, b) = 
    let val x = Word8.- (a, b) in if valid x then toRankFile x else
        toRankFile (Word8.notb x) end
 
  fun deidx x = 
    let val w = Word8.fromInt x
        val (r,f) = if valid w then toRankFile w else raise Invalid 
   in (r,f) end

 
  fun initBoard b = 
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
    in Array.modifyi (fn (i,_) => color (deidx i) handle Invalid => Empty) b end

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

  val toFen = implode o tl o #2 o (Array.foldli fold' (0, [])) 
  fun unfold (ch, (row, acc)) =
    if ch = #"/" then ([], row::acc) else  
    if Char.isDigit ch then (List.tabulate(ord ch - ord #"0", fn _ =>Piece.Empty) @
    row, acc) else (Piece.fromChar ch :: row, acc) 
 
  fun doit b = (List.foldri (fn (i, x, xs) => List.foldri (fn (j, x, xs) => update b
    (rf(i,j)) x) () x) ()) o rev o op:: o (foldl unfold ([], [])) o explode 
   
  fun fromFen s = let val b = new () in doit b s; b end
end


  functor Mailbox (R : REPRESENTATION) =
struct
  open R.Piece
  exception Invalid = R.Invalid
  exception IllegalMove

  val board = let val b = R.new () in R.initBoard b; b end 
  fun copy () = let val b' = R.new () in R.copy board b'; b' end 
 
  fun domove invariant b src dst = 
    let val s = R.sub b src
        val d = R.sub b dst 
        val check =
          if invariant (s, d) then () else raise IllegalMove
        val up = R.update b handle Subscript => raise Invalid
    in up src Empty; up dst s end
 
  (* Move a piece from src to dst
  * src must not be empty
  * dst must not have a piece *)
  val move = domove (fn (s, d) => s <> Empty andalso d = Empty) 
  (* Attack a piece *) 
  val attack = domove (fn (s,d) => s <> Empty andalso d <> Empty)

  fun mv s d = 
    let val src = R.fromAlg s  
        val dst = R.fromAlg d
    in move board src dst end


end

structure M = Mailbox(OhX88)

structure O = OhX88;
val b = O.new ();
val _ = O.initBoard b;
val f = O.toFen b;

fun search board = print o (fn x => (x ^ "\n")) o (String.concatWith "\n") o rev o (map
OhX88.toAlg) o (OhX88.findRanks board)

(*
*
fun diag w = 
  let val rank = Word8.>> (w, 0wx4)
      val file = Word8.andb (w, 0wx7)
l
*)
