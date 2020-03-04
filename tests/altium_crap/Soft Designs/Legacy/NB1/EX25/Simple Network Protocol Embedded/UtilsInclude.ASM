

; Keypad Codes
Key_1                   .equ    0x01
Key_2                   .equ    0x02
Key_3                   .equ    0x03
Key_C                   .equ    0x0C
Key_4                   .equ    0x04
Key_5                   .equ    0x05
Key_6                   .equ    0x06
Key_D                   .equ    0x0D
Key_7                   .equ    0x07
Key_8                   .equ    0x08
Key_9                   .equ    0x09
Key_E                   .equ    0x0E
Key_A                   .equ    0x0A
Key_0                   .equ    0x00
Key_B                   .equ    0x0B
Key_F                   .equ    0x0F
Key_Up                  .equ    Key_1
Key_Left                .equ    Key_4
Key_Right               .equ    Key_6
Key_Down                .equ    Key_9
Key_INVALID             .equ    0xFF


;...............................................................................
; MACRO Definitions
;...............................................................................
Delay1ms .MACRO
        MOVLW 10
        MOVWF CountOuter
        GCALL Delay100us
    .ENDM

Delay25ms .MACRO
        MOVLW 250
        MOVWF CountOuter
        GCALL Delay100us
    .ENDM

Delay100ms .MACRO
        .REPEAT 10
            MOVLW 100
            MOVWF CountOuter
            GCALL Delay100us
        .ENDREP
    .ENDM

DisplayLCDMessage .MACRO MessageString
        GCALL LCDClearLine
        .gen FOR LinePosition = #0 TO #16
            Delay1ms
            MOVF LinePosition,W
            GCALL MessageString ; get the byte at offset W
            .gen IF <NOT>W
                .gen BREAK
            .gen ENDIF
            MOVWF CharacterToDisplay
            GCALL LCDDisplayCharacter
        .gen ENDFOR
    .ENDM

DisplayLCDMessageWithDigit .MACRO MessageString, DigitToDisplay
        DisplayLCDMessage MessageString

        MOVF DigitToDisplay, W
        IORLW 0x30 ; turn the number into an ASCII digit
        MOVWF CharacterToDisplay
        GCALL LCDDisplayCharacter
    .ENDM

DisplayLCDMessageWithHexDigit .MACRO MessageString, DigitToDisplayAsHex
        DisplayLCDMessage MessageString
        MOVF DigitToDisplayAsHex, W
        MOVWF CharacterToDisplay
        GCALL LCDDisplayByteAsHex
    .ENDM

Is2Pwr .MACRO value ; Check if value is a power of 2 (if it is "Z" is set)
        DECF value,W ; if value is a power of 2, only one bit is set,
        ANDWF value,W ; so that value and (value - 1) have no common bits
    .ENDM
;...............................................................................

