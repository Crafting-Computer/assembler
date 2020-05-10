module Assembler exposing (main, assembleInstruction, assembleProgram)

import Html exposing (div, p, pre, text)
import AsmParser exposing (parse, showDeadEnds)
import AsmEmitter exposing (emit)


add =
  """// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2147483647  // load 2 into A
D=A // load 2 stored in A to D

@3  // load 3 into A
D=D+A// add 3 to 2

@0  // load 0 into A
M=D // set R0 to 2 + 3
  """


jump =
  """// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2  // load 2 into A
D=A;JMP // load 2 stored in A to D

@3  // load 3 into A
D=D+A;JEQ// add 3 to 2

@0  // load 0 into A
M=D // set R0 to 2 + 3
  """


multiplication =
  """// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/04/Mult.asm

// Multiplies R0 and R1 and stores the result in R2.
// (R0, R1, R2 refer to RAM[0], RAM[1], and RAM[2], respectively.)

// Put your code here.
@R2
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


errorSubtractBy2 =
  """// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2
D=A-2
@3
D=D+A
@1
M=D
  """



source =
  multiplication


assembleInstruction : Int -> String -> Result String String
assembleInstruction lineNumber instruction =
  case parse instruction of
    Err err ->
      Err <| showDeadEnds (Just lineNumber) instruction err
    
    Ok ast ->
      Ok <| emit ast


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