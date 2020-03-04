;...............................................................................
PortAsInput             .equ    0xFF
PortAsOutput            .equ    0x0

KeyPad                  .equ    PORTA       ; Out on the TRIS Lines used as ouputs
LCD_CTRL                .equ    PORTB       ; LCD control lines interface
LCD_DATA                .equ    PORTC       ; LCD data lines interface (bidirectional)

; LCD_CTRL control bits
LCD_E                   .equ    LCD_CTRL.0  ; LCD Enable control line
LCD_RW                  .equ    LCD_CTRL.1  ; LCD Read/Write control line
LCD_RS                  .equ    LCD_CTRL.2  ; LCD Register-Select control line

;LCD_DATA feedback:
LCD_BUSY                .equ    LCD_DATA.7  ; high if BUSY

;LCD commands:
LCD_ClearScreen         .equ    0x01
LCD_ReturnHome          .equ    0x02
LCD_SetEntryMode        .equ    0x04
LCD_SetDisplayMode      .equ    0x08
LCD_SetCursorMode       .equ    0x10
LCD_SetFunction         .equ    0x20
LCD_SetCharMapAddress   .equ    0x40
LCD_SetDisplayAddress   .equ    0x80

;LCD EntryMode masks:
LCD_ShiftDisplay        .equ    0x01
LCD_ShiftIncrement      .equ    0x02

;LCD DisplayMode masks:
LCD_BlinkingOn          .equ    0x01
LCD_BlinkingOff         .equ    0x00
LCD_ShowCursor          .equ    0x02
LCD_HideCursor          .equ    0x00
LCD_DisplayOn           .equ    0x04
LCD_DisplayOff          .equ    0x00

; Keypad ID:
Key_1                   .equ    0x0
Key_2                   .equ    0x1
Key_3                   .equ    0x2
Key_C                   .equ    0x3
Key_4                   .equ    0x4
Key_5                   .equ    0x5
Key_6                   .equ    0x6
Key_D                   .equ    0x7
Key_7                   .equ    0x8
Key_8                   .equ    0x9
Key_9                   .equ    0xA
Key_E                   .equ    0xB
Key_A                   .equ    0xC
Key_0                   .equ    0xD
Key_B                   .equ    0xE
Key_F                   .equ    0xF
Key_Up                  .equ    Key_1
Key_Left                .equ    Key_4
Key_Right               .equ    Key_6
Key_Down                .equ    Key_9
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
Seconds_X1      .dsb    1                   ; Stopwatch, Seconds/ones digit
Seconds_X10     .dsb    1                   ; Stopwatch, Seconds/tens digit
TenthSeconds    .dsb    1                   ; Stopwatch, 1/10ths digit
Temp            .dsb    1                   ; Tempory which may be used for *any* leaf function
CountInner      .dsb    1                   ; Storage needed by Delay100us
CountOuter      .dsb    1                   ; Storage needed by Delay100us
CurrentKey      .dsb    1                   ; Latest key scanned by the KeyScan function
StringOffset    .dsb    1                   ; String lookup table offset

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
DisplayStartupMessage:          GOTO    Do_DisplayStartupMessage
LCD_Initialize:                 GOTO    Do_LCD_Initialize

;...............................................................................
; Lookup tables:
;...............................................................................
Lookup_StartupString1:
    ADDWF   PCL,f
    RETLW   'Stop Watch Demo',0

Lookup_StartupString2:
    ADDWF   PCL,f
    RETLW   'A:Go 0:Stp B:Rst',0

Do_LookupTable_Character:
    ADDWF   PCL,f
    ;RETLW   '0123456789ABCEF'
    RETLW   '123C456D789EA0BF'

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
;   Updates display text according to the values in the timer-bytes
;...............................................................................
Watch_Display:
    MOVLW   LCD_SetDisplayAddress | 0x49    ; Cursor to position 8 in second row
    CALL   LCD_SendCommand

    MOVLW   10
    CALL   Delay100us
    MOVLW   '0'
    ADDWF   Minutes_X10,W
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   '0'
    ADDWF   Minutes_X1,W
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   ':'
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   '0'
    ADDWF   Seconds_X10,W
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   '0'
    ADDWF   Seconds_X1,W
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   '.'
    CALL   LCD_SendCharacter

    MOVLW   10
    CALL   Delay100us
    MOVLW   '0'
    ADDWF   TenthSeconds,W
    CALL   LCD_SendCharacter

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
;Sends command to LCD
;Required command must be in W
;...............................................................................
LCD_SendCommand:
    MOVWF   Temp                            ; Store command in temp

    MOVLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs

    .gen REPEAT
        BCF     LCD_E                       ; Disable LCD - Set Enable Low
        BSF     LCD_RW                      ; Set RW High
        BCF     LCD_RS                      ; Set For Command RS Low

        BSF     LCD_E                       ; Enable LCD - Set Enable High
        BCF     LCD_E                       ; Disable LCD - Set Enable Low
    .gen UNTIL <NOT> LCD_BUSY               ; Check Busy flag, if High (Busy) then try again

    MOVLW   PortAsOutput
    TRIS    LCD_DATA                        ; Set Port To Outputs
    BCF     LCD_RW                          ; Set RW Low

    BSF     LCD_E                           ; Enable LCD 0 Set E High
    MOVF    Temp, W
    MOVWF   LCD_DATA                        ; Send data to LCD
    BCF     LCD_E                           ; Disable LCD - Set Enable Low
    RETLW   0

;...............................................................................
;Sends character to LCD
;Required character must be in W
;...............................................................................
LCD_SendCharacter:
    MOVWF   Temp                            ; Command to send is in W

    MOVLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs

    .gen REPEAT
        BCF     LCD_E                       ; Disable LCD - Set Enable Low
        BSF     LCD_RW                      ; Set RW High
        BCF     LCD_RS                      ; Set For Command RS Low

        BSF     LCD_E                       ; Enable LCD - Set Enable High
        BCF     LCD_E                       ; Disable LCD - Set Enable Low
    .gen UNTIL <NOT> LCD_BUSY               ; Check Busy flag, if High (Busy) then try again


    MOVLW PortAsOutput
    TRIS  LCD_DATA                          ; Set Port To Outputs
    BCF   LCD_RW                            ; Set RW Low

    BSF   LCD_RS                            ; Set For Data - RS High
    BSF   LCD_E                             ; Enable LCD 0 Set E High
    MOVF  Temp, W
    MOVWF LCD_DATA                          ; Send data to LCD
    BCF   LCD_E                             ; Disable LCD - Set Enable Low
    RETLW 0

;...............................................................................
; Look for two keypressed values, 25ms apart that are the same
;...............................................................................
Do_KeyScan:
    MOVLW   0b11110000                      ;Turn On All Rows
    MOVWF   KeyPad

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

    ;Test the Keys on row 0
    MOVLW   0b11111110                      ; Turn on only Row 0
    MOVWF   KeyPad
    BTFSS   KeyPad.0                        ; Row0 + Col0 = Key1
    RETLW   Key_1                           ; if key pressed, return with the key value set
    BTFSS   KeyPad.1                        ; Row0 + Col1 = Key2
    RETLW   Key_2
    BTFSS   KeyPad.2                        ; Row0 + Col2 = Key3
    RETLW   Key_3
    BTFSS   KeyPad.3                        ; Row0 + Col3 = KeyC
    RETLW   Key_C

    ;Test the Keys on row 1
    MOVLW   0b11111101                      ; Turn on only Row 1
    MOVWF   KeyPad
    BTFSS   KeyPad.0                        ; Row1 + Col0 = Key4
    RETLW   Key_4
    BTFSS   KeyPad.1                        ; Row1 + Col1 = Key5
    RETLW   Key_5
    BTFSS   KeyPad.2                        ; Row1 + Col2 = Key6
    RETLW   Key_6
    BTFSS   KeyPad.3                        ; Row1 + Col3 = KeyD
    RETLW   Key_D

    ;Test the Keys on row 2
    MOVLW   0b11111011                      ; Turn On Row 2
    MOVWF   KeyPad
    BTFSS   KeyPad.0                        ; Row2 + Col0 = Key7
    RETLW   Key_7
    BTFSS   KeyPad.1                        ; Row2 + Col1 = Key8
    RETLW   Key_8
    BTFSS   KeyPad.2                        ; Row2 + Col2 = Key9
    RETLW   Key_9
    BTFSS   KeyPad.3                        ; Row2 + Col3 = KeyE
    RETLW   Key_E

    ;Test the Keys on row 3
    MOVLW   0b11110111                      ; Turn On Row 3
    MOVWF   KeyPad
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
Do_DisplayStartupMessage:
    MOVLW   LCD_SetDisplayAddress | 0x00    ; Cursor to position 7 in first row
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW   10                         ; 10 x 100uS = 1.0ms delay
        CALL   Delay100us

        MOVF    StringOffset,W
        CALL   Lookup_StartupString1       ; get the byte at offset W

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_SetDisplayAddress | 0x40    ; Cursor to position 0 in Second row
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW   10
        CALL   Delay100us

        MOVF    StringOffset,W
        CALL   Lookup_StartupString2       ; get the byte at offset W

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    RETLW 0

;...............................................................................
;Initialisiation code to be executed after power-up (i.e.: before any other subroutines are used).
;Always CALL from top level only
;...............................................................................
Do_LCD_Initialize:
    BCF     LCD_E                           ; Disable LCD - Set Enable Low
    BCF     LCD_RS                          ; Set For Command RS Low

    MOVLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs
    BSF     LCD_RW                          ; Set RW High

    MOVLW   10
    CALL   Delay100us

    MOVLW   0x38
    CALL   LCD_SendCommand                 ; 8-bit-interface, 2-lines

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_SetDisplayMode | LCD_BlinkingOff | LCD_HideCursor | LCD_DisplayOff
    CALL   LCD_SendCommand

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_ClearScreen
    CALL   LCD_SendCommand

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_ReturnHome
    CALL   LCD_SendCommand

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_SetDisplayMode | LCD_DisplayOn
    CALL   LCD_SendCommand

    MOVLW   10
    CALL   Delay100us

    MOVLW   LCD_SetEntryMode | LCD_ShiftIncrement
    CALL   LCD_SendCommand

    RETLW   0

;...............................................................................
Main:
    CLRF    StopWatch_Mode
    CLRF    Seconds_X1
    CLRF    Seconds_X10
    CLRF    Minutes_X1
    CLRF    Minutes_X10
    CLRF    TenthSeconds

    CALL   LCD_Initialize
    CALL   DisplayStartupMessage

    .gen REPEAT
        CALL   KeyScan                     ; return value 0xFF means no keypressed
        MOVWF   CurrentKey
    .gen UNTIL CurrentKey <NE> #0xFF        ; Wait until a key is pressed

    MOVLW   LCD_SetDisplayAddress | 0x40    ; Cursor to position 0 in second row
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16       ; fill the second Row with whitespaces:
        MOVLW   10
        CALL   Delay100us
        MOVLW   ' '
        CALL   LCD_SendCharacter
    .gen ENDFOR

;...............................................................................
MainLoop:
    CALL   KeyScan                         ; return value 0xFF means no keypressed
    MOVWF   CurrentKey                      ; save the currentkey for usage by HandleKeyPress

    .gen IF CurrentKey <NE> #0xFF
        CALL   HandleKeyPress
    .gen ENDIF

    .gen IF StopWatch_Mode <EQ> #1          ; Mode = 1 means "increment", check for clock signal
        MOVLW   0x80
        TRIS    PORTA

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
