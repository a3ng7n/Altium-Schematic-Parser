
.INCLUDE "GeneralDefines.asm"
.INCLUDE "UtilsInclude.asm"

;...............................................................................
LCD_DATA_PORT           .equ    PORTA       ; LCD data lines interface (bidirectional)
LCD_CTRL_PORT           .equ    PORTC       ; LCD control lines interface

; LCD_DATA_PORT feedback:
LCD_BUSY                .equ    LCD_CTRL_PORT.0  ; high if BUSY

LCD_LINE_ON             .equ    0b11101111
LCD_LINE_OFF            .equ    0b00010000
LCD_STROBE_ON           .equ    0b11011111
LCD_STROBE_OFF          .equ    0b00100000

KeypadDataPort         .equ PORTB
KeypadControlPort      .equ PORTF
RESET_BIT              .equ Bit0
.define RESET_KEYPAD "BCF KeypadControlPort, Bit0 \
                      BSF KeypadControlPort, Bit0 \
                      BCF KeypadControlPort, Bit0"
;...............................................................................

;...............................................................................
    .section data_Utils, data
; LCD variables
CharacterToDisplay .dsb 1
    .global CharacterToDisplay
LinePosition       .dsb 1
    .global LinePosition

; Used by the delay functions
CountInner         .dsb 1
CountOuter         .dsb 1
    .global CountOuter
LoopCounter        .dsb 1
LoopCounterInner   .dsb 1

; Keypad variables
LastKeyPress       .dsb 1
    .global LastKeyPress

Temp               .dsb 1
;...............................................................................

;...............................................................................
    .section bit_SNP, bit
FirstLine          .dsbit  1 ; 1 = display on first line, 0 = display on second line
    .global FirstLine
;...............................................................................

;...............................................................................
    .section Code_Utils, Code, at(0x600)

;...............................................................................
; Jump Table
; CALL instruction only allows 8 bit address so use a jump table
; CALL the Jump table which does a GOTO the address
;...............................................................................
LCDInit:                        GOTO    DoLCDInit
    .global LCDInit
LCDClearScreen:                 GOTO    DoLCDClearScreen
    .global LCDClearScreen
LCDClearLine:                   GOTO    DoLCDClearLine
    .global LCDClearLine
LCDDisplayCharacter:            GOTO    DoLCDDisplayCharacter
    .global LCDDisplayCharacter
LCDDisplayByteAsHex:            GOTO    DoLCDDisplayByteAsHex
    .global LCDDisplayByteAsHex
LCDDisplayNibbleAsHex:          GOTO    DoLCDDisplayNibbleAsHex
    .global LCDDisplayNibbleAsHex
LCDWaitTillReady:               GOTO    DoLCDWaitTillReady

Delay1s:                        GOTO    DoDelay1s
    .global Delay1s
Delay100us:                     GOTO    DoDelay100us
    .global Delay100us
Delay2us:                       GOTO    DoDelay2us
    .global Delay2us

KeypadInit:                     GOTO    DoKeypadInit
    .global KeypadInit
KeyScan:                        GOTO    DoKeyScan
    .global KeyScan
;...............................................................................

;...............................................................................
; String Lookup tables:
;...............................................................................
HexChars:
    ADDWF PCL,f
    RETLW '0123456789ABCDEF',0

KeypadMapping:
    ADDWF PCL, F
    RETLW 0x1, 0x2, 0x3, 0xC, \
          0x4, 0x5, 0x6, 0xD, \
          0x7, 0x8, 0x9, 0xE, \
          0xA, 0x0, 0xB, 0xF
;...............................................................................

;...............................................................................
; Initialises the LCD
;...............................................................................
DoLCDInit:
    SendConstantToPort LCD_CTRL_PORT, 0xFF

    CALL LCDClearScreen
    Return
;...............................................................................

;...............................................................................
; Clears LCD Screen
;...............................................................................
DoLCDClearScreen:

    BSF FirstLine
    CALL LCDClearLine ; clear first line

    BCF FirstLine
    CALL LCDClearLine ; clear second line

    Return
;...............................................................................

;...............................................................................
; Clears a line on the LCD. Control which line using the 'FirstLine' variable
;...............................................................................
DoLCDClearLine:
    MOVLW ' '
    MOVWF CharacterToDisplay
    .gen FOR LoopCounter = #0 TO #16
        MOVF LoopCounter, W
        MOVWF LinePosition
        CALL LCDDisplayCharacter
    .gen ENDFOR
    Return
;...............................................................................

;...............................................................................
; Sends the character in 'CharacterToDisplay' to the LCD
; at address 'LinePosition', on a line determined by the
; 'FirstLine' boolean.
;...............................................................................
DoLCDDisplayCharacter:
    CALL LCDWaitTillReady

    SendMemoryToPort LCD_DATA_PORT, CharacterToDisplay

    MOVLW PortAsOutput
    TRIS LCD_CTRL_PORT

    MOVF LinePosition, W
    IORLW 0xF0 ; set all the control lines to inactive
    ANDLW LCD_LINE_ON
    BTFSS FirstLine ; the next line turns on the 'line' bit if this isn't the first line
    IORLW LCD_LINE_OFF
    IORLW LCD_STROBE_OFF
    MOVWF LCD_CTRL_PORT

    ANDLW LCD_STROBE_ON
    MOVWF LCD_CTRL_PORT

    MOVLW 0xFF
    MOVWF LCD_CTRL_PORT

    Return
;...............................................................................

;...............................................................................
; LCDDisplayByteAsHex
; Given a value in CharacterToDisplay, the hex representation of that byte
; will be displayed on the LCD. For example, the value 0xF7 will display
; as "0xF7" on the LCD.
;...............................................................................
DoLCDDisplayByteAsHex:
    MOVF CharacterToDisplay, W
    MOVWF Temp ; save value

    ; display '0x' prefix
    MOVLW '0'
    MOVWF CharacterToDisplay
    GCALL LCDDisplayCharacter
    INCF LinePosition, F
    MOVLW 'x'
    MOVWF CharacterToDisplay
    GCALL LCDDisplayCharacter

    ; display most-significant hex digit
    INCF LinePosition, F
    MOVF Temp, W
    ANDLW 0xF0
    MOVWF CharacterToDisplay
    SWAPF CharacterToDisplay, F
    GCALL LCDDisplayNibbleAsHex

    ; followed by least-significant hex digit
    INCF LinePosition, F
    MOVF Temp, W
    ANDLW 0x0F
    MOVWF CharacterToDisplay
    GCALL LCDDisplayNibbleAsHex
    Return
;...............................................................................

;...............................................................................
; LCDDisplayNibbleAsHex
; Given a value in CharacterToDisplay, the least significant nibble will be
; displayed as a hex code. For example, the value 0x01 will appear as '1'
; and the value 0x0b will appear as 'b' on the LCD.
;...............................................................................
DoLCDDisplayNibbleAsHex:
    MOVLW 0x0F
    ANDWF CharacterToDisplay, W ; ensure we are only looking at a nibble
    CALL HexChars ; convert 'W' to ASCII code for hex character
    MOVWF CharacterToDisplay
    CALL LCDDisplayCharacter
    Return
;...............................................................................

;...............................................................................
; LCDWaitTillReady
; Won't return until the LCD busy signal indicates the LCD
; is ready for more data.
;...............................................................................
DoLCDWaitTillReady:
    MOVLW   PortAsInput
    TRIS    LCD_CTRL_PORT ; Set Port To Inputs

LWTR_CHECK_BUSY:
    BTFSS LCD_CTRL_PORT, Bit0 ; active low, so high = not busy
    GOTO LWTR_CHECK_BUSY
;    .gen REPEAT
;    .gen UNTIL <NOT> LCD_BUSY ; Check Busy flag, if High (Busy) then try again
    Return
;...............................................................................

;...............................................................................
DoDelay1s:
    .gen FOR LoopCounter = #0 TO #40
        MOVLW 250
        MOVWF CountOuter
        CALL Delay100us
    .gen ENDFOR

    Return
;...............................................................................

;...............................................................................
; Delay100us:
;   This function will consume (CountOuter * (99 * 4 + 4)) + 3 == (CountOuter * 400) + 3 cycles
;   At 40Mhz processor clock, this will generate a CountOuter * 100 uSecs software delay
;   Note: if CountOuter is zero, the function will consume (256 * 400) + 3 cycles
;...............................................................................
DoDelay100us:
    LoopOuter:
        MOVLW 99
        MOVWF CountInner
        LoopInner:
            NOP
            NOP
            DECFSZ CountInner,f
        GOTO LoopInner

        DECFSZ CountOuter,f
    GOTO LoopOuter

    Return
;...............................................................................

;...............................................................................
DoDelay2us:
    MOVLW 1
    MOVWF CountInner
    D2_LoopInner:
        NOP
        NOP
        DECFSZ CountInner,f
    GOTO D2_LoopInner

    Return
;...............................................................................

;...............................................................................
DoKeypadInit:
    RESET_KEYPAD
    Return
;...............................................................................

;...............................................................................
; Grabs the most recent value from the keypad controller.
; Note that the keypad controller already implements
; debouncing.
;...............................................................................
DoKeyScan:
    GetMemoryFromPort KeypadDataPort, Temp
    BTFSS Temp, Bit4 ; check the ValidKey level
    GOTO KeyScanError
    MOVF Temp, W
    ANDLW 0x0F ; only keep LS nibble
    CALL KeypadMapping ; turn keypad index into proper hex value
    GOTO KS_RETURN

KeyScanError:
    MOVLW Key_INVALID                     ; if an error occurred, return KeyInvalid

KS_RETURN:
    MOVWF LastKeyPress
    RESET_KEYPAD
    Return
;...............................................................................

END
