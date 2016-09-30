;...............................................................................
PortAsInput             .equ    0xFF
PortAsOutput            .equ    0x0

KeyPad                  .equ    PORTC       ; Out on the TRIS Lines used as ouputs
LCD_CTRL                .equ    PORTB       ; LCD control lines interface
LCD_DATA                .equ    PORTA       ; LCD data lines interface (bidirectional)

; LCD_CTRL control bits
LCD_E                   .equ    LCD_CTRL.0  ; LCD Enable control line
LCD_RS                  .equ    LCD_CTRL.1  ; LCD Register-Select control line

; LCD commands:
LCD_ClearScreen         .equ    0x01
LCD_ReturnHome          .equ    0x02
LCD_SetEntryMode        .equ    0x04
LCD_SetDisplayMode      .equ    0x08
LCD_SetCursorMode       .equ    0x10
LCD_SetFunction         .equ    0x20
LCD_SetCharMapAddress   .equ    0x40
LCD_SetDisplayAddress   .equ    0x80

; LCD EntryMode masks:
LCD_ShiftDisplay        .equ    0x01
LCD_ShiftIncrement      .equ    0x02

; LCD DisplayMode masks:
LCD_BlinkingOn          .equ    0x01
LCD_BlinkingOff         .equ    0x00
LCD_ShowCursor          .equ    0x02
LCD_HideCursor          .equ    0x00
LCD_DisplayOn           .equ    0x04
LCD_DisplayOff          .equ    0x00

; Pushbuttons
PUSH1                   .equ    KeyPad.1
PUSH2                   .equ    KeyPad.2
PUSH3                   .equ    KeyPad.3

; LEDS
LED1                    .equ    LCD_CTRL.4
LED2                    .equ    LCD_CTRL.5
LED3                    .equ    LCD_CTRL.6
LED4                    .equ    LCD_CTRL.7

; Flash Rate
Refresh                 .equ    0x0A

;...............................................................................
    .section data, data
; Data used:
Temp                    .dsb    1           ; Tempory which may be used for *any* leaf function
CountInner              .dsb    1           ; Storage needed by Delay100us
CountOuter              .dsb    1           ; Storage needed by Delay100us
CountInner_2            .dsb    1
CountOuter_2            .dsb    1
StringOffset            .dsb    1           ; String lookup table offset


;...............................................................................
    .section Entry, Code, at(0)
__start:                                    ; Entrypoint of hardware
    GJMP    Main

;...............................................................................
    .section displayer, code, inpage        ; inpage allows for interesting optimizations for
                                            ; jumps and call within this section. However it is
                                            ; required NOT to change the code-pagebits directly.
                                            ; For calls and jumps out of the scope of the section,
                                            ; CALL/LCALL and GOTO/LJMP must be used.
                                            ; Also the section must fit in one page (512 instr.)

;...............................................................................
; ump table for CALLs
;  CALL instruction only allows 8 bit address so use a jump table
;  CALL the Jump table which does a GOTO the address
;...............................................................................
DisplayStartupMessage:          GOTO    Do_DisplayStartupMessage
LCD_Initialize:                 GOTO    Do_LCD_Initialize
DisplayNext:                    GOTO    Do_DisplayNext
ClearDisplay:                   GOTO    Do_ClearDisplay

;...............................................................................
; Lookup tables:
;...............................................................................
Lookup_StartupString1:
    ADDWF   PCL,f
    RETLW   'Altium',0

Lookup_StartupString2:
    ADDWF   PCL,f
    RETLW   'LiveDesign!',0

; Not used in this program......
Lookup_StartupString3:         ;
    ADDWF   PCL,f              ;
    RETLW   'JTAG',0           ;
;...............................

Lookup_StartupString4:
    ADDWF   PCL,f
    RETLW   'Enabled',0

Lookup_StartupString5:
    ADDWF   PCL,f
    RETLW   'NEXAR',0

Lookup_LDLogoTop:
    ADDWF   PCL,f
    RETLW   0x00,0x01,0x02,0x03

Lookup_LDLogoBot:
    ADDWF   PCL,f
    RETLW   0x04,0x05,0x06,0x07

;...............................................................................
; Custom Character Elements for mini LD logo
;...............................................................................
Lookup_LDLogo_elements:
    ADDWF   PCL,f
    RETLW   0x00,0x01,0x03,0x06,0x06,0x0C,0x0C,0x18,\
            0x0F,0x10,0x00,0x08,0x08,0x08,0x10,0x10,\
            0x00,0x18,0x00,0x07,0x04,0x04,0x08,0x08,\
            0x10,0x0C,0x06,0x03,0x13,0x13,0x13,0x13,\
            0x19,0x19,0x1B,0x18,0x18,0x0C,0x06,0x01,\
            0x00,0x00,0x1E,0x00,0x00,0x01,0x00,0x10,\
            0x11,0x12,0x1E,0x00,0x00,0x00,0x18,0x07,\
            0x03,0x03,0x03,0x06,0x06,0x0C,0x18,0x00

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
; LongDelay:  Approximately W * 100 * 100 * 100us
;...............................................................................
LongDelay:
    MOVWF  CountOuter_2
    LoopOuter2:
        MOVLW   99
        MOVWF   CountInner_2
        LoopInner2:
                MOVLW   100
                CALL    Delay100us
                DECFSZ  CountInner_2,f
        GOTO    LoopInner2
        DECFSZ  CountOuter_2,f
    GOTO    LoopOuter2
    RETLW 0

;...............................................................................
; Sends command to LCD
; Required command must be in W
;...............................................................................
LCD_SendCommand:
    MOVWF   Temp                            ; Store command in temp
    BCF     LCD_RS                          ; Set for Command - RS Low
    BSF     LCD_E                           ; Enable LCD 0 Set E High
    NOP
    MOVF    Temp, W
    MOVWF   LCD_DATA                        ; Send data to LCD
    NOP
    BCF     LCD_E                           ; Disable LCD - Set Enable Low
    RETLW   0

;...............................................................................
; Sends character to LCD
; Required character must be in W
;...............................................................................
LCD_SendCharacter:
    MOVWF   Temp                            ; Command to send is in W
    BSF   LCD_RS                            ; Set For Data - RS High
    BSF   LCD_E                             ; Enable LCD 0 Set E High
    NOP
    MOVF  Temp, W
    MOVWF LCD_DATA                          ; Send data to LCD
    NOP
    BCF   LCD_E                             ; Disable LCD - Set Enable Low
    RETLW 0

;...............................................................................
; Clears the Display
;...............................................................................
Do_ClearDisplay:
    MOVLW   20
    CALL    Delay100us
    MOVLW   LCD_ClearScreen
    CALL    LCD_SendCommand
    BSF     LED4
    RETLW   0

;...............................................................................
; Displays the initial startup message to the LCD
;...............................................................................
Do_DisplayStartupMessage:

    MOVLW  LCD_SetDisplayAddress | 0x00    ; Cursor to position 7 in first row
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW   20                          ; 20 x 100uS = 2.0ms delay
        CALL   Delay100us

        MOVF    StringOffset,W
        CALL   Lookup_StartupString1        ; get the byte at offset W

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   20
    CALL   Delay100us

    MOVLW   LCD_SetDisplayAddress | 0x40    ; Cursor to position 0 in Second row
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW   20
        CALL   Delay100us

        MOVF    StringOffset,W
        CALL   Lookup_StartupString5        ; get the byte at offset W

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    BCF    LED2
    MOVLW  20
    CALL   Delay100us

    RETLW 0

;...............................................................................
; LCD Initialisiation code to be executed after power-up (i.e.: before any other
; subroutines are used). Always CALL from top level only
;...............................................................................
Do_LCD_Initialize:
    BCF     LCD_E                           ; Disable LCD - Set Enable Low
    BCF     LCD_RS                          ; Set For Command RS Low

    MOVLW   1
    CALL    LongDelay

    MOVLW   0x38
    CALL   LCD_SendCommand                  ; 8-bit-interface, 2-lines

    MOVLW   1                               ; Provide plenty of delay to support
    CALL    LongDelay                       ; write-only mode to slow LCDs

    MOVLW   LCD_SetDisplayMode | LCD_BlinkingOff | LCD_HideCursor | LCD_DisplayOff
    CALL   LCD_SendCommand

    MOVLW   200
    CALL   Delay100us

    MOVLW   LCD_ClearScreen
    CALL   LCD_SendCommand

    MOVLW   200
    CALL   Delay100us

    MOVLW   LCD_ReturnHome
    CALL   LCD_SendCommand

    MOVLW   200
    CALL   Delay100us

    MOVLW   LCD_SetDisplayMode | LCD_DisplayOn
    CALL   LCD_SendCommand

    MOVLW   200
    CALL   Delay100us

    MOVLW   LCD_SetEntryMode | LCD_ShiftIncrement
    CALL   LCD_SendCommand

    RETLW   0

;...............................................................................
; Load the custom characters into the CGRAM (character map)
;...............................................................................
LDLogo_setup:
    MOVLW   50
    CALL    Delay100us

    MOVLW  LCD_SetCharMapAddress
    CALL   LCD_SendCommand

    .gen FOR StringOffset = #0 TO #64
        MOVLW  50
        CALL   Delay100us

        MOVF   StringOffset,W
        CALL   Lookup_LDLogo_elements
        CALL   LCD_SendCharacter
    .gen ENDFOR
    RETLW   0

;...............................................................................
; DO_DisplayNext - updates the LCD with the next message
;...............................................................................
Do_DisplayNext:

    BSF     LED2
    MOVLW   50
    CALL    Delay100us

    MOVLW   LCD_SetDisplayAddress | 0x00    ; Cursor to position 0 in first row
    CALL    LCD_SendCommand

    .gen FOR StringOffset = #0 TO #4        ; Display upper four custom chars.
        MOVLW  50
        CALL   Delay100us

        MOVF   StringOffset,W
        CALL   Lookup_LDLogoTop
        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   50
    CALL    Delay100us

    MOVLW    LCD_SetDisplayAddress | 0x40
    CALL     LCD_SendCommand

    .gen FOR StringOffset = #0 TO #4        ; Display lower four custom chars.
        MOVLW  50
        CALL   Delay100us

        MOVF   StringOffset,W
        CALL   Lookup_LDLogoBot
        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   50
    CALL    Delay100us

    MOVLW   LCD_SetDisplayAddress | 0x06    ; Cursor to position 7 in first row
    CALL    LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW  50
        CALL   Delay100us

        MOVF   StringOffset,W
        CALL   Lookup_StartupString2

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   50
    CALL    Delay100us

    MOVLW   LCD_SetDisplayAddress | 0x47
    CALL    LCD_SendCommand

    .gen FOR StringOffset = #0 TO #16
        MOVLW  50
        CALL   Delay100us

        MOVF   StringOffset,W
        CALL   Lookup_StartupString4

        .gen IF <NOT>W
            .gen BREAK
        .gen ENDIF

        CALL   LCD_SendCharacter
    .gen ENDFOR

    MOVLW   50
    CALL    Delay100us

    RETLW 0

;...............................................................................
; Main initialisation
;...............................................................................
Main:
    MOVLW    PortAsOutput
    TRIS     LCD_DATA                       ; Set LCD Port To Outputs

    MOVLW    PortAsOutput
    TRIS     LCD_CTRL                       ; Set Port B IO for LCD control,
                                            ; and LEDs.

    CALL     LCD_Initialize
    CALL     DisplayStartupMessage
    BSF      LED1
    BSF      LED2

    MOVLW    5
    CALL     LongDelay
    CALL     LDLogo_setup
    CALL     ClearDisplay
    BCF      LED4
    BSF      LED3
    BCF      LED2
    BSF      LED1
    CALL     DisplayNext

;...............................................................................
; Main Loop scans (at approx. 2Hz) the puchbuttons and alternately flashes
; the LEDs.
;...............................................................................
MainLoop:
    BTFSS    PUSH3
    CALL     DisplayStartupMessage
    BTFSS    PUSH2
    CALL     DisplayNext
    BTFSS    PUSH1
    CALL     ClearDisplay
    MOVLW    1
    CALL     LongDelay
    BCF      LED4
    BSF      LED3
    MOVLW    1
    CALL     LongDelay
    BSF      LED4
    BCF      LED3
    GOTO     MainLoop
;...............................................................................


  .END
