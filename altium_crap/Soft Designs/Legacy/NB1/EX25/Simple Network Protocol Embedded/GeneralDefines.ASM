
; General Defines

;...............................................................................
TRUE                    .equ    0xFF
FALSE                   .equ    0x00
  
Bit0                    .equ    0
Bit1                    .equ    1
Bit2                    .equ    2
Bit3                    .equ    3
Bit4                    .equ    4
Bit5                    .equ    5
Bit6                    .equ    6
Bit7                    .equ    7

PortAsInput             .equ    0xFF
PortAsOutput            .equ    0x00
;...............................................................................

;...............................................................................
.define      Return     "RETLW 0"
;...............................................................................

;...............................................................................
; MACRO Definitions
;...............................................................................
SendMemoryToPort .MACRO PortAddress, MemoryToOutput
        MOVLW PortAsOutput
        TRIS PortAddress
        MOVF MemoryToOutput, W
        MOVWF PortAddress
    .ENDM

SendConstantToPort .MACRO PortAddress, Constant
        MOVLW PortAsOutput
        TRIS PortAddress
        MOVLW Constant
        MOVWF PortAddress
    .ENDM

GetWFromPort .MACRO PortAddress
        MOVLW PortAsInput
        TRIS PortAddress
        MOVF PortAddress, W
    .ENDM

GetMemoryFromPort .MACRO PortAddress, StorageLocation
        GetWFromPort PortAddress
        MOVWF StorageLocation ; store result
    .ENDM

; Jumps if MemoryToCheck < ComparisonValue
JumpIfBelow .MACRO MemoryToCheck, ComparisonValue, JumpLabel
        MOVLW ComparisonValue
        SUBWF MemoryToCheck, W
        BTFSS C ; borrow flag set means that MemoryToCheck is greater than ComparisonValue
        GOTO JumpLabel
    .ENDM

EqualityCore .MACRO MemoryToCheck, JumpLabel
        SUBWF MemoryToCheck, W
        BTFSC Z ; zero flag set means that MemoryToCheck equals value in W
        GOTO JumpLabel
    .ENDM

JumpIfEqual .MACRO MemoryToCheck, ComparisonValue, JumpLabel
        MOVLW ComparisonValue
        EqualityCore MemoryToCheck, JumpLabel
    .ENDM

JumpIfEqualMemoryLocation .MACRO MemoryA, MemoryB, JumpLabel
        MOVF MemoryB, W
        EqualityCore MemoryA, JumpLabel
    .ENDM

InequalityCore .MACRO MemoryToCheck, JumpLabel
        SUBWF MemoryToCheck, W
        BTFSS Z ; zero flag set means that MemoryToCheck equals value in W
        GOTO JumpLabel
    .ENDM

JumpIfNotEqual .MACRO MemoryToCheck, ComparisonValue, JumpLabel
        MOVLW ComparisonValue
        InequalityCore MemoryToCheck, JumpLabel
    .ENDM

JumpIfNotEqualMemoryLocation .MACRO MemoryA, MemoryB, JumpLabel
        MOVF MemoryB, W
        InequalityCore MemoryA, JumpLabel
    .ENDM

ClampMemoryToValue .MACRO MemoryToClamp, MaximumValue, EndLabel
        JumpIfBelow MemoryToClamp, MaximumValue, EndLabel
        BTFSC Z ; relies on implementation of JumpIfBelow to a degree - just an optimisation anyway
        GOTO EndLabel
        MOVLW MaximumValue
        MOVWF MemoryToClamp
    .ENDM
    

