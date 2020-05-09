module AsmParser exposing (parse, showDeadEnds, Instruction(..), AInstructionArg(..), Destinations, Jump)


import Parser.Advanced exposing (..)
import List.Extra


type Instruction
  = AInstruction AInstructionArg
  | CInstruction
    { destinations : Destinations
    , computation : String
    , jump : Jump
    }


type AInstructionArg
  = AInstructionNumber Int
  | AInstructionLabel String


type alias Destinations =
  { a : Bool
  , d : Bool
  , m : Bool
  }

type alias Jump =
  { lt : Bool
  , eq : Bool
  , gt : Bool
  }


type Context
  = AContext
  | CContext


type Problem
  = ExpectingAtSign
  | ExpectingEqualSign
  | ExpectingSemicolonSign
  | ExpectingStartOfLineComment
  | ExpectingEOF
  | ExpectingInstruction
  | ExpectingInt
  | InvalidNumber
  | NumberTooLarge
  | ExpectingDestinations String
  | ExpectingComputation
  | ExpectingJump String


type alias AsmParser a =
  Parser Context Problem a


parse : String -> Result (List (DeadEnd Context Problem)) (List Instruction)
parse src =
  run (succeed identity |= instructions |. end ExpectingEOF) src


instructions : AsmParser (List Instruction)
instructions =
  ( succeed (\start ins end -> (start, ins, end))
    |= getOffset
    |= ( loop [] <| \revDefs ->
      oneOf
      [ succeed (\d -> Loop (d :: revDefs))
        |. sps
        |= oneOf
          [ aInstruction
          , cInstruction      
          ]
        |. sps
      , succeed ()
        |> map (\_ -> Done (List.reverse revDefs))
      ]
    )
    |= getOffset
  )
  |> andThen
    (\(start, ins, end) ->
      if start == end then
        problem ExpectingInstruction
      else
        succeed ins
    )


aInstruction : AsmParser Instruction
aInstruction =
  ( succeed identity
    |. symbol (Token "@" ExpectingAtSign)
    |= inContext AContext
      (int ExpectingInt InvalidNumber)
  ) |> andThen
    (\number ->
      if number >= 2 ^ 31 then
        problem NumberTooLarge
      else
        succeed <| AInstruction <| AInstructionNumber number
    )


cInstruction : AsmParser Instruction
cInstruction =
  inContext CContext <|
  succeed (\dest comp jmp ->
    CInstruction
      { destinations =
        dest
      , computation =
        comp
      , jump =
        jmp
      }
    )
    |= ( optional { a = False, d = False, m = False } <|
      succeed identity
      |= destinations
      |. symbol (Token "=" ExpectingEqualSign)
    )
    |= computation
    |= ( optional { lt = False, eq = False, gt = False } <|
      succeed identity
      |. symbol (Token ";" ExpectingSemicolonSign)
      |= jump
    )


destinations : AsmParser Destinations
destinations =
  oneOf <|
    List.map
    (\str ->
      let
        dest =
          { a = String.contains "A" str
          , d = String.contains "D" str
          , m = String.contains "M" str
          }
      in
      map (\_ -> dest) <| keyword <| Token str (ExpectingDestinations str)
    )
    [ "AMD"
    , "AD"
    , "AM"
    , "MD"
    , "M"
    , "D"
    , "A"
    ]


computation : AsmParser String
computation =
  let
    comps =
      [ "0"
      , "1"
      , "-1"
      , "D"
      , "A"
      , "!D"
      , "!A"
      , "-D"
      , "-A"
      , "D+1"
      , "A+1"
      , "D-1"
      , "A-1"
      , "D+A"
      , "D-A"
      , "A-D"
      , "D&A"
      , "D|A"
      , "M"
      , "!M"
      , "-M"
      , "M+1"
      , "M-1"
      , "D+M"
      , "D-M"
      , "M-D"
      , "D&M"
      , "D|M"
      ]
  in
  ( getChompedString <|
    chompWhile (\c -> c /= '/' && c /= ';' && c /= ' ' && c /= '\n')
  ) |> andThen
    (\str ->
      if List.member str comps then
        succeed str
      else
        problem ExpectingComputation
    )



jump : AsmParser Jump
jump =
  oneOf <|
    List.map
    (\str ->
      let
        jmp =
          case str of
            "JGT" ->
              { lt = False, eq = False, gt = True }
            "JEQ" ->
              { lt = False, eq = True, gt = False }
            "JGE" ->
              { lt = False, eq = True, gt = True }
            "JLT" ->
              { lt = True, eq = False, gt = False }
            "JNE" ->
              { lt = True, eq = False, gt = True }
            "JLE" ->
              { lt = True, eq = True, gt = False }
            _ ->
              { lt = True, eq = True, gt = True }
      in
      map (\_ -> jmp) <| keyword <| Token str (ExpectingJump str)
    )
    [ "JGT"
    , "JEQ"
    , "JGE"
    , "JLT"
    , "JNE"
    , "JLE"
    , "JMP"
    ]


sps : AsmParser ()
sps =
  loop 0 <| ifProgress <|
    oneOf
      [ succeed () |. symbol (Token "//" ExpectingStartOfLineComment) |. chompWhile (\c -> c /= '\n')
      , spaces
      ]


ifProgress : AsmParser a -> Int -> AsmParser (Step Int ())
ifProgress parser offset =
  succeed identity
    |. parser
    |= getOffset
    |> map (\newOffset -> if offset == newOffset then Done () else Loop newOffset)


optional : a -> AsmParser a -> AsmParser a
optional default parser =
  oneOf
    [ parser
    , succeed default
    ]


showDeadEnds : String -> List (DeadEnd Context Problem) -> String
showDeadEnds src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper src) deadEndGroups


showDeadEndsHelper : String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper src (first, rests) =
  let
    location =
      showProblemLocation first.row first.col src
    problems =
      List.map (.problem >> showProblem) <| List.reverse <| first :: rests
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ "I'm expecting " ++ String.join " or " problems
  ++ (if String.isEmpty context then "" else " in the " ++ context)
  ++ "."


showProblem : Problem -> String
showProblem p =
  case p of
    ExpectingAtSign ->
      "a '@'"
    
    ExpectingEqualSign ->
      "a '='"
    
    ExpectingSemicolonSign ->
      "a ';'"
    
    ExpectingEOF ->
      "the end of program"
    
    ExpectingInstruction ->
      "an instruction"

    ExpectingInt ->
      "an integer"

    InvalidNumber ->
      "a decimal integer"
    
    NumberTooLarge ->
      "a number smaller than 2147483648 (2^31)"

    ExpectingDestinations str ->
      str

    ExpectingComputation ->
      "a computation like 'D+A' and '-1'"
    
    ExpectingJump str ->
      str

    ExpectingStartOfLineComment ->
      "the start of comment '//'"


showProblemContextStack : List { row : Int, col : Int, context : Context } -> String
showProblemContextStack contexts =
  String.join " of the " <| List.map (.context >> showProblemContext) contexts


showProblemContext : Context -> String
showProblemContext context =
  case context of
    AContext ->
      "A instruction"
    
    CContext ->
      "C instruction"


showProblemLocation : Int -> Int -> String -> String
showProblemLocation row col src =
  let
    _ = Debug.log "AL -> row" <| row
    _ = Debug.log "AL -> col" <| col
  in
  let
    rawLine =
      getLine row src
    line =
      String.fromInt row ++ "| " ++ (String.trimLeft <| rawLine)
    offset =
      String.length line - String.length rawLine - 1
    _ = Debug.log "AL -> offsettedCol" <| offsettedCol
    offsettedCol =
      offset + col
    underline =
      makeUnderline line offsettedCol (offsettedCol + 1)
  in
  line ++ "\n" ++ underline


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList row
    |> List.indexedMap (\i _ -> toUnderlineChar minCol maxCol i)
    |> String.fromList


toUnderlineChar : Int -> Int -> Int -> Char
toUnderlineChar minCol maxCol col =
  if minCol <= col && col < maxCol then
    '^'
  else
    ' '


getLine : Int -> String -> String
getLine row src =
  Maybe.withDefault ("CAN'T GET LINE AT ROW " ++ String.fromInt row) -- impossible
    <| List.Extra.getAt (row - 1) <| String.split "\n" src

