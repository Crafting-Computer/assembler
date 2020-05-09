module AsmEmitter exposing (..)


import AsmParser exposing (Instruction(..), AInstructionArg(..),Destinations, Jump)
import Binary


emit : List Instruction -> String
emit instructions =
  String.join "\n" <|
  List.map
    emitInstruction
    instructions


emitInstruction : Instruction -> String
emitInstruction instruction =
  case instruction of
    AInstruction arg ->
      case arg of
        AInstructionNumber number ->
          String.join ""  <|
          List.map String.fromInt <|
          Binary.toIntegers <| Binary.ensureSize 32 <| Binary.fromDecimal number
        
        AInstructionLabel label ->
          label
        
    CInstruction { destinations, computation, jump } ->
      emitDestinations destinations
      ++ emitComputation computation
      ++ emitJump jump


emitDestinations : Destinations -> String
emitDestinations destinations =
  boolToString destinations.a
  ++ boolToString destinations.d
  ++ boolToString destinations.m


emitComputation : String -> String
emitComputation computation =
  case computation of
    "0" -> "0101010"
    "1" -> "0111111"
    "-1" -> "0111010"
    "D" -> "0001100"
    "A" -> "0110000"
    "!D" -> "0001101"
    "!A" -> "0110001"
    "-D" -> "0001111"
    "-A" -> "0110011"
    "D+1" -> "0011111"
    "A+1" -> "0110111"
    "D-1" -> "0001110"
    "A-1" -> "0110010"
    "D+A" -> "0000010"
    "D-A" -> "0010011"
    "A-D" -> "0000111"
    "D&A" -> "0000000"
    "D|A" -> "0010101"
    "M" -> "1110000"
    "!M" -> "1110001"
    "-M" -> "1110011"
    "M+1" -> "1110111"
    "M-1" -> "1110010"
    "D+M" -> "1000010"
    "D-M" -> "1010011"
    "M-D" -> "1000111"
    "D&M" -> "1000000"
    "D|M" -> "1010101"
    _ -> "INVALID COMPUTATION"


emitJump : Jump -> String
emitJump jump =
  boolToString jump.lt
  ++ boolToString jump.eq
  ++ boolToString jump.gt


boolToString : Bool -> String
boolToString b =
  if b then
    "1"
  else
    "0"

