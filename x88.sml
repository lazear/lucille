signature X88_SIG =
sig
  exception Invalid

  (* is a position on the board? *)
  val valid : Word8.word -> bool
  
  (* Convert a 64-square based integer to an internal position. This is guaranteed to be O(1) *)
  val position : int -> Word8.word
  
  (* Foldl over board positions *)
  val foldl : (Word8.word * 'a -> 'a) -> 'a -> 'a

  (* from 0-based rank and file to positon *)
  val rf : int * int -> Word8.word
  val toRankFile : Word8.word -> int * int
  val diff : Word8.word * Word8.word -> int * int

  (* convert to algebraic notation *)
  val toAlg : Word8.word -> string
  (* convert from algebraic notation *)
  val fromAlg : string -> Word8.word 
end

structure X88 :> X88_SIG =
struct
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

  exception Invalid
  fun $ (f, g) = f g
  infixr 4 $

  val valid = fn x => (x && 0wx88) = 0wx0
  
  val squares = Vector.fromList $ List.filter valid $ List.tabulate (0x80, Word8.fromInt)

  fun position pos = Vector.sub (squares, pos) handle Subscript => raise Invalid
  
  fun foldl f acc = Vector.foldl f acc squares

  fun rf (rank,file) =
    let val r = Word8.fromInt rank
        val f = Word8.fromInt file
        val w = (r << 0wx4) || f
    in if valid w then w else raise Invalid end
  
  fun toRankFile w = 
    let val r = Word8.toInt (w >> 0wx4)
        val f = Word8.toInt (w && 0wx7)
    in (r, f) end
  
  fun fromAlg (f::r::nil) = 
    let val r = ord r - ord #"1"
        val f = ord f - ord #"a"
    in rf (r, f) end
    | fromAlg _ = raise Fail "invalid algebraic notation"
  val fromAlg = fromAlg o explode

  fun toAlg w = 
    let val (r, f) = toRankFile w
        val r = chr (r + ord #"1")
        val f = chr (f + ord #"a")
    in implode [f,r] end
  
  fun diff (a, b) = 
    let val x = Word8.- (a, b) in if valid x then toRankFile x else
        toRankFile (Word8.notb x) end
end


signature Moves =
sig
  structure Piece : PIECE
  datatype dir = N | S | W | E | NW | NE | SW | SE

  val sliding : dir list -> Word8.word -> Word8.word list
  val stepping: dir list -> Word8.word -> Word8.word list
  val knight: Word8.word -> Word8.word list

  val moves : Piece.piece -> Word8.word -> bool -> Word8.word list
end

functor X88Moves(P : PIECE) :> Moves =
struct
  structure Piece = P
  datatype dir = N | S | W | E | NW | NE | SW | SE

  val dirToOrd = 
    fn N => 16
     | S => ~16
     | W => ~1
     | E => 1
     | NW => 15
     | NE => 17
     | SW => ~17
     | SE => ~15

  val dirToOrd = Word8.fromInt o dirToOrd

  fun validate pos = (List.filter X88.valid) o (map (fn x => Word8.+ (pos, x)))

  fun sliding dirs pos = 
    let 
      fun inner dir pos xs=
        let
          val x = Word8.+ (dir, pos)
        in
          case X88.valid x
            of true  => inner dir x (x::xs)
             | false => xs
        end
    in foldl (fn (x, xs) => inner (dirToOrd x) pos xs) [] dirs end

  fun stepping dirs pos = validate pos (map dirToOrd dirs)

  fun knight pos = validate pos (map Word8.fromInt [0x12, 0x21, 0x1F, 0x0E, ~0x12, ~0x21, ~0x1F,
  ~0x0E])

  fun pawnW pos true  = stepping [NW, N, NE] pos
    | pawnW pos false = stepping [N] pos

  fun pawnB pos true  = stepping [SW, S, SE] pos
    | pawnB pos false = stepping [S] pos

  fun moves (P.White P.Pawn) pos en = pawnW pos en
    | moves (P.Black P.Pawn) pos en = pawnB pos en
    | moves (P.Black rank) pos _ = doRank rank pos
    | moves (P.White rank) pos _ = doRank rank pos
    | moves P.Empty _ _ = []
  and doRank P.Knight pos = knight pos
    | doRank P.Bishop pos = sliding [NW,NE,SW,SE] pos
    | doRank P.Rook pos = sliding [N,W,S,E] pos
    | doRank P.Queen pos = sliding [N,S,W,E,NW,NE,SW,SE] pos
    | doRank P.King pos = stepping [N,S,W,E,NW,NE,SW,SE] pos
    | doRank P.Pawn _ = raise Fail "unreachable"
end

(*
functor MoveValidation(B: BOARD) = 
struct
  structure B = B;
  structure M = Moves;
  
  fun mkmvs piece pos enPas =
    let val moves = M.moves piece pos enPas
        val isWhite = case piece of
                           White => true
                         | Black => false
                         | Empty => raise Fail "cannot move Empty piece!"
    in
      

end*)
