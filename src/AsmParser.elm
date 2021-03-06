module AsmParser exposing (parse, parseKeepLabels, showDeadEnds, ParserInstruction(..), ParserAInstructionArg(..), Instruction(..), Destinations, Jump)


import Parser.Advanced exposing (..)
import List.Extra
import Set exposing (Set)
import Dict exposing (Dict)
import EverySet


type Instruction
  = AInstruction Int
  | CInstruction
    { destinations : Destinations
    , computation : String
    , jump : Jump
    }


type ParserInstruction
  = ParserAInstruction ParserAInstructionArg
  | ParserCInstruction
    { destinations : Destinations
    , computation : String
    , jump : Jump
    }
  | ParserLabelInstruction (Located String) Int


type ParserAInstructionArg
  = ParserAInstructionNumber Int
  | ParserAInstructionLabel (Located String)


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
  | LabelContext


type Problem
  = ExpectingAtSign
  | ExpectingEqualSign
  | ExpectingSemicolonSign
  | ExpectingLeftParenSign
  | ExpectingRightParenSign
  | ExpectingStartOfLineComment
  | ExpectingStartOfMultiLineComment
  | ExpectingEndOfMultiLineComment
  | ExpectingEOF
  | ExpectingInstruction
  | ExpectingInt
  | ExpectingDestinations String
  | ExpectingComputation
  | ExpectingJump String
  | ExpectingLabel
  | ExpectingCustomLabel
  | ExpectingVariableName
  | InvalidNumber
  | NumberTooLarge Int
  | UndefinedSymbol String


type alias AsmParser a =
  Parser Context Problem a


type alias SymbolTable =
  Dict String Int


predefinedSymbolTable : SymbolTable
predefinedSymbolTable =
  Dict.fromList
    [ ("R0", 0)
    , ("R1", 1)
    , ("R2", 2)
    , ("R3", 3)
    , ("R4", 4)
    , ("R5", 5)
    , ("R6", 6)
    , ("R7", 7)
    , ("R8", 8)
    , ("R9", 9)
    , ("R10", 10)
    , ("R11", 11)
    , ("R12", 12)
    , ("R13", 13)
    , ("R14", 14)
    , ("R15", 15)
    , ("SCREEN", 2 ^ 16)
    , ("KBD", 2 ^ 16 + 19200) -- 320 * 240 * (32 / 8)
    , ("SP", 0)
    , ("LCL", 1)
    , ("ARG", 2)
    , ("THIS", 3)
    , ("THAT", 4)
    ]


predefinedLabels : Set String
predefinedLabels =
  Set.fromList <|
    Dict.keys predefinedSymbolTable


parse : String -> Result (List (DeadEnd Context Problem)) (List Instruction)
parse src =
  run
    ( succeed identity
      |= instructions
      |. end ExpectingEOF
    )
    src
  |> Result.mapError
  (\deadEnds ->
    List.reverse <| EverySet.toList <| EverySet.fromList <| deadEnds
  )
  |> Result.andThen
  (\(ast, declaredSymbolTable) ->
    let
      declaredSymbols : Set String
      declaredSymbols =
        Set.union predefinedLabels (Set.fromList <| Dict.keys declaredSymbolTable)
      
      undefinedSymbols : List (Located String)
      undefinedSymbols =
        List.foldl
          (\instruction names ->
            case instruction of
              ParserAInstruction arg ->
                case arg of
                  ParserAInstructionLabel name ->
                    if Set.member name.value declaredSymbols then
                      names
                    else
                      name :: names
                  
                  _ ->
                    names

              _ ->
                names
          )
          []
          ast

      symbolTable =
        Dict.union declaredSymbolTable predefinedSymbolTable
    in
    case undefinedSymbols of
      [] ->
        let
          substitutedAst =
            List.reverse <|
            List.foldl
              (\instruction nextAst ->
                case instruction of
                  ParserAInstruction arg ->
                    case arg of
                      ParserAInstructionLabel name ->
                        case Dict.get name.value symbolTable of
                          Nothing ->
                            nextAst
                          
                          Just number ->
                            AInstruction number :: nextAst
                      
                      ParserAInstructionNumber number ->
                        AInstruction number :: nextAst
                      
                  ParserCInstruction record ->
                    CInstruction record :: nextAst
                  
                  ParserLabelInstruction _ _ ->
                    nextAst
              )
              []
              ast
        in
        Ok substitutedAst

      _ ->
        Err <|
        List.map
          (\symbol ->
            { row = Tuple.first symbol.from
            , col = Tuple.second symbol.from
            , problem = UndefinedSymbol symbol.value
            , contextStack = []
            }
          )
          undefinedSymbols
  )


parseKeepLabels : String -> Result (List (DeadEnd Context Problem)) (List ParserInstruction)
parseKeepLabels src =
  run
    ( succeed
      Tuple.first
      |= instructions
      |. end ExpectingEOF
    )
    src


instructions : AsmParser (List ParserInstruction, SymbolTable)
instructions =
  let
    initialState =
      { instructionIndex =
        0
      , variableIndex =
        16
      , symbolTable =
        Dict.empty
      }
  in
  ( succeed (\start (ins, state) end -> (start, (ins, state.symbolTable), end))
    |= getOffset
    |= ( loop ([], initialState) <| \(revInstructions, {instructionIndex, variableIndex, symbolTable} as state) ->
      oneOf
      [ succeed
        (\instruction ->
          Loop
          ( instruction :: revInstructions
          , { instructionIndex =
              case instruction of
                ParserLabelInstruction _ _ ->
                  instructionIndex
                
                _ ->
                  instructionIndex + 1
            , variableIndex =
              case instruction of
                ParserAInstruction arg ->
                  case arg of
                    ParserAInstructionLabel name ->
                      if isVariableName name.value then
                        variableIndex + 1
                      else
                        variableIndex

                    ParserAInstructionNumber _ ->
                      variableIndex
                
                _ ->
                  variableIndex
            , symbolTable =
              case instruction of
                ParserLabelInstruction name number ->
                  Dict.insert name.value number symbolTable
                
                ParserAInstruction arg ->
                  case arg of
                    ParserAInstructionLabel name ->
                      if isVariableName name.value && (not <| Dict.member name.value symbolTable) then
                        Dict.insert name.value variableIndex symbolTable
                      else
                        symbolTable

                    ParserAInstructionNumber _ ->
                      symbolTable
                
                _ ->
                  symbolTable
            }
          )
        )
        |. sps
        |= oneOf
          [ aInstruction
          , labelInstruction instructionIndex
          , cInstruction
          ]
        |. sps
      , succeed ()
        |> map (\_ -> Done (List.reverse revInstructions, state))
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


aInstruction : AsmParser ParserInstruction
aInstruction =
  ( succeed identity
    |. symbol (Token "@" ExpectingAtSign)
    |= (inContext AContext <|
      oneOf
        [ map ParserAInstructionLabel <|
          oneOf
            [ usedLabel
            , variableName
            ]
        , map ParserAInstructionNumber <| int ExpectingInt InvalidNumber
        ]
    )
  ) |> andThen
    (\arg ->
      case arg of
        ParserAInstructionNumber number ->
          if number >= 2 ^ 31 then
            problem <| NumberTooLarge number
          else
            succeed <| ParserAInstruction <| ParserAInstructionNumber number
        
        ParserAInstructionLabel name ->
          succeed <| ParserAInstruction <| ParserAInstructionLabel name
    )


cInstruction : AsmParser ParserInstruction
cInstruction =
  inContext CContext <|
  succeed (\dest comp jmp ->
    ParserCInstruction
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


labelInstruction : Int -> AsmParser ParserInstruction
labelInstruction instructionIndex =
  succeed identity
  |. symbol (Token "(" ExpectingLeftParenSign)
  |= ( inContext LabelContext <|
    succeed (\name ->
      ParserLabelInstruction name instructionIndex
    )
      |= declaredLabel
      |. symbol (Token ")" ExpectingRightParenSign)
  )


variableName : AsmParser (Located String)
variableName =
  located <|
  variable
    { expecting =
      ExpectingVariableName
    , start =
      Char.isLower
    , inner =
      \c -> Char.isAlphaNum c || c == '_' || c == '.'
    , reserved =
      Set.empty
    }


declaredLabel : AsmParser (Located String)
declaredLabel =
  located <|
  variable
    { expecting =
      ExpectingCustomLabel
    , start =
      Char.isUpper
    , inner =
      \c -> Char.isAlphaNum c || c == '_' || c == '.'
    , reserved =
      predefinedLabels
    }


usedLabel : AsmParser (Located String)
usedLabel =
  located <|
  variable
    { expecting =
      ExpectingLabel
    , start =
      Char.isUpper
    , inner =
      \c -> Char.isAlphaNum c || c == '_' || c == '.'
    , reserved =
      Set.empty
    }


sps : AsmParser ()
sps =
  loop 0 <| ifProgress <|
    oneOf
      [ succeed () |. symbol (Token "--" ExpectingStartOfLineComment) |. chompWhile (\c -> c /= '\n')
      , multiComment (Token "{-" ExpectingStartOfMultiLineComment) (Token "-}" ExpectingEndOfMultiLineComment) Nestable
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
    [ backtrackable parser
    , succeed default
    ]


showDeadEnds : Maybe Int -> String -> List (DeadEnd Context Problem) -> String
showDeadEnds lineNumber src deadEnds =
  let
    deadEndGroups =
      List.Extra.groupWhile (\d1 d2 -> d1.row == d2.row && d1.col == d2.col) <| deadEnds
  in
  String.join "\n" <| List.map (showDeadEndsHelper lineNumber src) deadEndGroups


showDeadEndsHelper : Maybe Int -> String -> ((DeadEnd Context Problem), List (DeadEnd Context Problem)) -> String
showDeadEndsHelper lineNumber src (first, rests) =
  let
    location =
      showProblemLocation lineNumber first.row first.col src
    context =
      showProblemContextStack first.contextStack
  in
  location ++ "\n"
  ++ (case first.problem of
    InvalidNumber ->
      "I found an invalid number. I'm expecting a decimal integer"
    
    NumberTooLarge number ->
      "I found a number `" ++ String.fromInt number ++ "` that's too large. All numbers should be less than 2147483648 (2^31)"

    UndefinedSymbol symbol ->
      "I found an undefined symbol `" ++ symbol ++ "`. You should declare it as a label somewhere like so: `(" ++ symbol ++ ")`"
    
    _ ->
      let
        problemStrs =
          List.map (.problem >> showProblem) <| List.reverse <| first :: rests
      in
      "I'm expecting " ++ String.join " or " problemStrs
  )
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

    ExpectingLeftParenSign ->
      "a '('"
    
    ExpectingRightParenSign ->
      "a ')'"
    
    ExpectingEOF ->
      "the end of program"
    
    ExpectingInstruction ->
      "an instruction"

    ExpectingInt ->
      "an integer"

    ExpectingDestinations str ->
      str

    ExpectingComputation ->
      "a computation like 'D+A' and '-1'"
    
    ExpectingJump str ->
      str

    ExpectingStartOfLineComment ->
      "the start of a single-line comment '--'"
    
    ExpectingStartOfMultiLineComment ->
      "the start of a multi-line comment '{-'"
    
    ExpectingEndOfMultiLineComment ->
      "the end of a multi-line comment '-}'"

    ExpectingLabel ->
      "a label in all caps like `R0`"

    ExpectingCustomLabel ->
      "a custom label in all caps like `LOOP_0`"

    ExpectingVariableName ->
      "a variable name in all lowercase like `my_variable_0`"

    InvalidNumber ->
      "a valid decimal integer"
    
    NumberTooLarge _ ->
      "an integer less than 2147483648 (2^31)."

    UndefinedSymbol _ ->
      "a defined symbol"


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

    LabelContext ->
      "label declaration"


showProblemLocation : Maybe Int -> Int -> Int -> String -> String
showProblemLocation customLineNumber row col src =
  let
    rawLine =
      getLine row src
    lineNumber =
      case customLineNumber of
        Nothing ->
          row
        
        Just number ->
          number
    line =
      String.fromInt lineNumber ++ "| " ++ (String.trimLeft <| rawLine)
    offset =
      String.length line - String.length rawLine - 1
    offsettedCol =
      offset + col
    underline =
      makeUnderline line offsettedCol (offsettedCol + 1)
  in
  line ++ "\n" ++ underline


makeUnderline : String -> Int -> Int -> String
makeUnderline row minCol maxCol =
  String.toList (row ++ " ")
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


type alias Located a =
  { from : (Int, Int)
  , value : a
  , to : (Int, Int)
  }


located : AsmParser a -> AsmParser (Located a)
located parser =
  succeed Located
    |= getPosition
    |= parser
    |= getPosition


isVariableName : String -> Bool
isVariableName name =
  case String.uncons name of
    Nothing ->
      False
    
    Just (firstChar, _) ->
      Char.isLower firstChar

