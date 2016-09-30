;...............................................................................
KeyPad                  .equ    PORTA       ; Buttons 1 to 4

; Keys ID:
Key_A                   .equ    0xC
Key_0                   .equ    0xD
Key_B                   .equ    0xE
Key_F                   .equ    0xF
Key_INVALID             .equ    0xFF

;...............................................................................
Is2Pwr  .MACRO  value                       ; Check if value is a power of 2 (if it is "Z" is set)
    DECF    value,W                         ; if value is a power of 2, only one bit is set,
    ANDWF   value,W                         ; so that value and (value - 1) have no common bits
    .ENDM

;...............................................................................
    .section data, data
; Data used:
StopWatch_Mode  .dsb    1                   ; 0=Stop 1=Count 2=reset
Minutes_X1      .dsb    1                   ; Stopwatch, minutes/ones digit
Minutes_X10     .dsb    1                   ; Stopwatch, minutes/tens digit
Minutes         .dsb    1                   ; Stopwatch, minutes      digit
Seconds_X1      .dsb    1                   ; Stopwatch, Seconds/ones digit
Seconds_X10     .dsb    1                   ; Stopwatch, Seconds/tens digit
Seconds         .dsb    1                   ; Stopwatch, Seconds      digit
TenthSeconds    .dsb    1                   ; Stopwatch, 1/10ths digit
Temp            .dsb    1                   ; Tempory which may be used for *any* leaf function
CountInner      .dsb    1                   ; Storage needed by Delay100us
CountOuter      .dsb    1                   ; Storage needed by Delay100us
CurrentKey      .dsb    1                   ; Latest key scanned by the KeyScan function

;...............................................................................
    .section bit, bit
LastTime        .dsbit  1

;...............................................................................
    .section Entry, Code, at(0)
__start:                                    ; Entrypoint of hardware
    GJMP    Main

;...............................................................................
    .section stopwatch, code, inpage       ; inpage allows for interesting optimizations for
                                            ; jumps and call within this section. However it is
                                            ; requiered NOT to change the code-pagebits directly.
                                            ; For calls and jumps out of the scope of the section,
                                            ; CALL/LCALL and GOTO/LJMP must be used.
                                            ; Also the section must fit in one page (512 instr.)

;...............................................................................
;Jump table for CALLs
; CALL instruction only allows 8 bit address so use a jump table
; CALL the Jump table which does a GOTO the address
;...............................................................................
KeyScan:                        GOTO    Do_KeyScan
DIG_Initialize                  GOTO    Do_DIG_Initialize

;...............................................................................
; HandleKeyPress:
;   selects the right action when a key-press is pressed
;...............................................................................
HandleKeyPress:
    MOVF    CurrentKey,W
    ANDLW   0xF
    ADDWF   PCL,f

    GOTO    HandleKey_Null                  ; 1
    GOTO    HandleKey_Null                  ; 2
    GOTO    HandleKey_Null                  ; 3
    GOTO    HandleKey_Null                  ; C
    
    GOTO    HandleKey_Null                  ; 4
    GOTO    HandleKey_Null                  ; 5
    GOTO    HandleKey_Null                  ; 6 
    GOTO    HandleKey_Null                  ; D
    
    GOTO    HandleKey_Null                  ; 7
    GOTO    HandleKey_Null                  ; 8
    GOTO    HandleKey_Null                  ; 9
    GOTO    HandleKey_Null                  ; E
    
    GOTO    Watch_Start                     ; A
    GOTO    Watch_Stop                      ; case 0
    GOTO    Watch_Reset                     ; B
    GOTO    HandleKey_Null                  ; F

Watch_Stop:
    CLRF    StopWatch_Mode                  ; Switch Stopwatch mode to "Stop" and fall through
                                            ;   to "RETLW" of HandleKey_Null
HandleKey_Null:
    RETLW   0

Watch_Start:
    MOVLW   1                               ; Switch Stopwatchmode to "Start"
    MOVWF   StopWatch_Mode
    GOTO     Watch_Display                  ; "Watch_Display" will return to caller

Watch_Reset:
    MOVLW   2                               ; Switch Stopwatchmode to "Reset"
    MOVWF   StopWatch_Mode
    CLRF    Seconds_X1
    CLRF    Seconds_X10
    CLRF    Minutes_X1
    CLRF    Minutes_X10
    CLRF    TenthSeconds
;   GOTO    Watch_Display                   ; Fall through to "Watch_Display"
                                            ; "Watch_Display" will return to caller

;...............................................................................
; Watch Display:
;   Updates display according to the values in the timer-bytes
;...............................................................................
Watch_Display:
    MOVF    TenthSeconds,W
    MOVWF   PORTC

    RLF     Seconds_X10,0
    MOVWF   Seconds
    RLF     Seconds,0
    MOVWF   Seconds
    RLF     Seconds,0
    MOVWF   Seconds
    RLF     Seconds,0
    IORWF   Seconds_X1,0
    MOVWF   PORTB

    RLF     Minutes_X10,0
    MOVWF   Minutes
    RLF     Minutes,0
    MOVWF   Minutes
    RLF     Minutes,0
    MOVWF   Minutes
    RLF     Minutes,0
    IORWF   Minutes_X1,0
    MOVWF   PORTA

    RETLW   0

;...............................................................................
; Delay100us:
;   This function will consume (W * (99 * 4 + 4)) + 3 == (W * 400) + 3 cycles
;   At 40Mhz processor clock, this will generate a W * 100 uSecs software delay
;   Note: if W is zero, the function will consume (256 * 400) + 3 cycles
;...............................................................................
Delay100us:
    MOVWF  CountOuter
    LoopOuter:
        MOVLW   99
        MOVWF   CountInner
        LoopInner:
            NOP
            NOP
            DECFSZ  CountInner,f
        GOTO    LoopInner

        DECFSZ  CountOuter,f
    GOTO    LoopOuter

    RETLW 0

;...............................................................................
IncrementTime:
    INCF    TenthSeconds,f                  ; Increment the TenthSeconds count
    MOVLW   10                              ; Do we need to roll over?
    SUBWF   TenthSeconds,W
    BTFSS   C
    RETLW   0                               ; If not then display the time

    CLRF    TenthSeconds
    INCF    Seconds_X1,f                    ; Increment the sec (ones) count
    MOVLW   10                              ; Do we need to roll over?
    SUBWF   Seconds_X1,W
    BTFSS   C
    RETLW   0                               ; If not then display the time

    CLRF    Seconds_X1                      ; Clear the sec (ones) count
    INCF    Seconds_X10,f                   ; Increment the sec (tens) count
    MOVLW   6                               ; Do we need to roll over?
    SUBWF   Seconds_X10,W
    BTFSS   C
    RETLW   0                               ; If not then display the time

    CLRF    Seconds_X10                     ; Clear the sec (tens) count
    INCF    Minutes_X1,f                    ; Increment the min (ones) count
    MOVLW   10                              ; Do we need to roll over?
    SUBWF   Minutes_X1,W
    BTFSS   C
    RETLW   0                               ; If not then display the time

    CLRF    Minutes_X1                      ; Clear the min (ones) count
    INCF    Minutes_X10,f                   ; Increment the sec (tens) count
    MOVLW   6                               ; Do we need to roll over?
    SUBWF   Minutes_X10,W
    BTFSC   C
    CLRF    Minutes_X10                     ; Clear the min (tens) count
    RETLW   0

;...............................................................................
; Look for two keypressed values, 25ms apart that are the same
;...............................................................................
Do_KeyScan:
    .gen REPEAT
        COMF    KeyPad,W                    ; Read Columns (complemented)
        ANDLW   0xF                         ; Only bottom 4-bits are connected to the keypad
        MOVWF   Temp                        ; Store First Read complemented in "Temp"

        MOVLW   50
        CALL   Delay100us                   ; Wait some time for key debounce

        COMF    KeyPad,W                    ; Read Columns Again
        ANDLW   0xF                         ; Only bottom 4-bits are valid
        XORWF   Temp,W                      ; W := Temp ^ W
    .gen UNTIL Z                            ; If the same as before then result will be zero

    CSNZ    Temp                            ; Read Temp - If any key pressed then lower 4 bits
    RETLW   Key_INVALID                     ; will be not zero, else return with error

    Is2Pwr  Temp                            ; Check if only ONE column is set,
    JNZ     KeyScanError                    ; else go to KeyScanError

    ;Test the Keys
    BTFSS   KeyPad.0                        ; Row3 + Col0 = KeyA
    RETLW   Key_A
    BTFSS   KeyPad.1                        ; Row3 + Col1 = Key0
    RETLW   Key_0
    BTFSS   KeyPad.2                        ; Row3 + Col2 = KeyB
    RETLW   Key_B
    BTFSS   KeyPad.3                        ; Row3 + Col3 = KeyF
    RETLW   Key_F

KeyScanError:
    RETLW   Key_INVALID                     ; if an error occurred, return KeyInvalid

;...............................................................................
;Initialisiation code to be executed after power-up (i.e.: before any other subroutines are used).
;Always CALL from top level only
;...............................................................................
Do_DIG_Initialize:
    CLRF    PORTA
    CLRF    PORTB
    CLRF    PORTC

    RETLW   0

;...............................................................................
Main:
    CLRF    StopWatch_Mode
    CLRF    Seconds
    CLRF    Seconds_X1
    CLRF    Seconds_X10
    CLRF    Minutes
    CLRF    Minutes_X1
    CLRF    Minutes_X10
    CLRF    TenthSeconds

    CALL  DIG_Initialize

    .gen REPEAT
        CALL   KeyScan                     ; return value 0xFF means no keypressed
        MOVWF   CurrentKey
    .gen UNTIL CurrentKey <NE> #0xFF        ; Wait until a key is pressed

;...............................................................................
MainLoop:
    CALL   KeyScan                         ; return value 0xFF means no keypressed
    MOVWF   CurrentKey                      ; save the currentkey for usage by HandleKeyPress

    .gen IF CurrentKey <NE> #0xFF
        CALL   HandleKeyPress
    .gen ENDIF

    .gen IF StopWatch_Mode <EQ> #1          ; Mode = 1 means "increment", check for clock signal

        .gen IF RA7
            .gen IF LastTime
                BCF     LastTime
                CALL   IncrementTime           ; increment the time
                CALL   Watch_Display           ; Display the updated time
            .gen ENDIF
        .gen ELSE
            .gen IF <NOT>LastTime
                BSF     LastTime
            .gen ENDIF
        .gen ENDIF
    .gen ENDIF

    GOTO     MainLoop
    
;...............................................................................

    .END
