module Main exposing (main)

import Html exposing (div, p, pre, text)
import AsmParser exposing (parse, showDeadEnds)


add =
  """// This file is part of www.nand2tetris.org
// and the book "The Elements of Computing Systems"
// by Nisan and Schocken, MIT Press.
// File name: projects/06/add/Add.asm

// Computes R0 = 2 + 3  (R0 refers to RAM[0])

@2  // load 2 into A
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
  jump


main =
  case parse source of
    Err err ->
      div []
        [ pre [] [ text source]
        , pre [] [ text <| 
          "❌ Parse error.\n\n"
          ++ showDeadEnds source err
        ]
        ]

    Ok program ->
      div []
        [ pre [] [ text source]
        , pre [] [ text "✔️ Passed parser." ]
        ]