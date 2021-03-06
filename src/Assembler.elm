module Assembler exposing (main, assembleInstruction, assembleProgram, parseProgram, parseProgramKeepLabels, emitProgram, instructionToString)

import Html exposing (div, p, pre, text)
import AsmParser exposing (parse, parseKeepLabels, showDeadEnds, ParserInstruction(..), ParserAInstructionArg(..), Instruction(..), Destinations, Jump)
import AsmEmitter exposing (emit)
import Binary


testAdd =
  """-- This file is part of www.nand2tetris.org
-- and the book "The Elements of Computing Systems"
-- by Nisan and Schocken, MIT Press.
-- File name: projects/06/add/Add.asm

-- Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2147483647  -- load 2 into A
D=A -- load 2 stored in A to D

@3  -- load 3 into A
D=D+A-- add 3 to 2

@0  -- load 0 into A
M=D -- set R0 to 2 + 3
  """


testJump =
  """-- This file is part of www.nand2tetris.org
-- and the book "The Elements of Computing Systems"
-- by Nisan and Schocken, MIT Press.
-- File name: projects/06/add/Add.asm

-- Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2  -- load 2 into A
D=A;JMP -- load 2 stored in A to D

@3  -- load 3 into A
D=D+A;JEQ-- add 3 to 2

@0  -- load 0 into A
M=D -- set R0 to 2 + 3
  """


testMult =
  """{- This file is part of www.nand2tetris.org
  and the book "The Elements of Computing Systems"
  by Nisan and Schocken, MIT Press.
  File name: projects/04/Mult.asm
-}

-- Multiplies R0 and R1 and stores the result in R2.
-- (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

-- Put your code here.
@R2 -- Multiplies R0 and R1 and stores the result in R2.
M=0

@i
M=1

(LOOP)
  @R1
  D=M
  @i
  D=M-D
  @END
  D;JGT
  @R0
  D=M
  @R2
  M=D+M
  @i
  M=M+1
  @LOOP
  0;JMP

(END)
  @END
  0;JMP
  """


testErrorSubtractBy2 =
  """-- This file is part of www.nand2tetris.org
-- and the book "The Elements of Computing Systems"
-- by Nisan and Schocken, MIT Press.
-- File name: projects/06/add/Add.asm

-- Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2
D=A-2
@3
D=D+A
@1
M=D
  """



testFillScreen =
  """@8192
D=A
@sizeE
M=D

(LOOP)
  @KBD
  A=M
  D=A
  @i
  M=0
  @FILL
  D;JNE
  (CLEAR)
    @i
    D=M
    @size
    D=D-M
    @LOOP
    D;JGE
    @i
    D=M
    @SCREEN
    A=D+A
    M=0
    @i
    M=M+1
    @CLEAR
    0;JMP
  (FILL)
    @i
    D=M
    @size
    D=D-M
    @LOOP
    D;JGE
    @i
    D=M
    @SCREEN
    A=D+A
    M=-1
    @i
    M=M+1
    @FILL
    0;JMP
  @LOOP
  0;JMP
  """


testVariableNameStartingWithDollarSign =
  """@111
D=A
@SP
AM=M+1
A=A-1
M=D
@333
D=A
@SP
AM=M+1
A=A-1
M=D
@888
D=A
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
@$basic_test_8
M=D
@SP
AM=M-1
D=M
@$basic_test_3
M=D
@SP
AM=M-1
D=M
@$basic_test_1
M=D
@$basic_test_3
D=M
@SP
AM=M+1
A=A-1
M=D
@$basic_test_1
D=M
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
M=M-D
@$basic_test_8
D=M
@SP
AM=M+1
A=A-1
M=D
@SP
AM=M-1
D=M
A=A-1
M=D+M
  """


source =
  testVariableNameStartingWithDollarSign


assembleInstruction : Int -> String -> Result String String
assembleInstruction lineNumber instruction =
  case parse instruction of
    Err err ->
      Err <| showDeadEnds (Just lineNumber) instruction err
    
    Ok ast ->
      Ok <| emit ast


parseProgram : String -> Result ((Int, Int), String) (List Instruction)
parseProgram program =
  case parse program of
    Err deadEnds ->
      case deadEnds of
        [] ->
          Err ((0, 0), "") -- impossible
        
        firstDeadEnd :: _ ->
          Err ((firstDeadEnd.row, firstDeadEnd.col), showDeadEnds Nothing program deadEnds)
    
    Ok ast ->
      Ok ast


parseProgramKeepLabels : String -> List String
parseProgramKeepLabels program =
  case parseKeepLabels program of
    Err _ ->
      []
    
    Ok ast ->
      List.map
        parserInstructionToString
        ast


emitProgram : List Instruction -> List Int
emitProgram instructions =
  let
    strToInt =
      Maybe.withDefault 0 << String.toInt
    convertTo2sComplement number =
      if number >= 2 ^ 31 then -- negative binary
        number - 2 ^ 32
      else -- positive or zero
        number
  in
  List.map ( convertTo2sComplement <<
    Binary.toDecimal << Binary.fromIntegers <<
    List.map strToInt << String.split "") <|
    String.split "\n" <| AsmEmitter.emit instructions


parserInstructionToString : ParserInstruction -> String
parserInstructionToString instruction =
  case instruction of
    ParserAInstruction arg ->
      case arg of
        ParserAInstructionLabel name ->
          "@" ++ name.value
        
        ParserAInstructionNumber number ->
          "@" ++ String.fromInt number
        
    ParserCInstruction record ->
      instructionToString <| CInstruction record

    ParserLabelInstruction label _ ->
      "(" ++ label.value ++ ")"


instructionToString : Instruction -> String
instructionToString instruction =
  case instruction of
    AInstruction number ->
      "@" ++ String.fromInt number
    
    CInstruction { destinations, computation, jump } ->
      ( case destinationsToString destinations of
        "" ->
          ""
        
        str ->
          str ++ "="
      )
      ++ computation
      ++ ( case jumpToString jump of
        "" ->
          ""
        
        str ->
          ";" ++ str
      )


destinationsToString : Destinations -> String
destinationsToString { a, m, d } =
  (if a then "A" else "")
  ++ (if m then "M" else "")
  ++ (if d then "D" else "")


jumpToString : Jump -> String
jumpToString { lt, eq, gt } =
  case ( lt, eq, gt ) of
    (False, False, False) ->
      ""
    
    (False, False, True) ->
      "JGT"
    
    (False, True, False) ->
      "JEQ"
    
    (False, True, True) ->
      "JGE"
    
    (True, False, False) ->
      "JLT"

    (True, False, True) ->
      "JNE"

    (True, True, False) ->
      "JLE"
    
    (True, True, True) ->
      "JMP"


assembleProgram : String -> Result String String
assembleProgram program =
  case parse program of
    Err err ->
      Err <| showDeadEnds Nothing program err
    
    Ok ast ->
      Ok <| emit ast


main =
  case parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , pre [] [ text <| 
          "❌ Parse error.\n\n"
          ++ showDeadEnds Nothing source err
        ]
        ]

    Ok program ->
      div []
        [ pre [] [ text source]
        , pre [] [ text "✔️ Passed parser." ]
        , pre [] [ text "🏭 Emitted machine code:" ]
        , pre [] [ text <| emit program ]
        ]