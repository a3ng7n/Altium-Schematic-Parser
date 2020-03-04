.Include "DAC_Defines.Asm"
;...............................................................................
; Routines to driver a 2x16 Character LCD based on a KS00066 Controller
;...............................................................................

;...............................................................................
;LCD commands:
cLCD_ClearScreen         .equ    0x01
cLCD_ReturnHome          .equ    0x02
cLCD_SetEntryMode        .equ    0x04
cLCD_SetDisplayMode      .equ    0x08
cLCD_SetCursorMode       .equ    0x10
cLCD_SetFunction         .equ    0x20
cLCD_SetCharMapAddress   .equ    0x40
cLCD_SetDisplayAddress   .equ    0x80

;LCD EntryMode masks:
cLCD_ShiftDisplay        .equ    0x01
cLCD_ShiftIncrement      .equ    0x02

;LCD DisplayMode masks:
cLCD_BlinkingOn          .equ    0x01
cLCD_BlinkingOff         .equ    0x00
cLCD_ShowCursor          .equ    0x02
cLCD_HideCursor          .equ    0x00
cLCD_DisplayOn           .equ    0x04
cLCD_DisplayOff          .equ    0x00
;...............................................................................

;...............................................................................
.define      LCD_RW_0          "BCF  LCD_RW"
.define      LCD_RW_1          "BSF  LCD_RW"

.define      LCD_RS_0          "BCF  LCD_RS"
.define      LCD_RS_1          "BSF  LCD_RS"

.define      LCD_EN_0          "BCF  LCD_EN"
.define      LCD_EN_1          "BSF  LCD_EN"

.define      Wait_1us           "GCall Delay1us"
.define      Wait_10us          "GCall Delay10us"
;...............................................................................

;...............................................................................
    .Section Code_LCD, Code, InPage, at(0x600)
;...............................................................................

;...............................................................................
;Jump table for Calls
;GCall instruction only allows 8 bit address so use a jump table
;GCall the Jump table which does a Goto the address
;...............................................................................
Delay1us:                       Goto    Do_Delay1us
Delay10us:                      Goto    Do_Delay10us
Delay100us:                     Goto    Do_Delay100us
Delay1ms:                       Goto    Do_Delay1ms     
Delay2ms:                       Goto    Do_Delay2ms     
Delay10ms:                      Goto    Do_Delay10ms    
DisplayStartupMessage:          Goto    Do_DisplayStartupMessage
DisplayMessageString:           Goto    Do_DisplayMessageString  
DisplayWaveString:              Goto    Do_DisplayWaveString
DisplayBCD:                     Goto    Do_DisplayBCD 
BinaryToBCD:                    Goto    Do_BinaryToBCD
LCD_Initialize:                 Goto    Do_LCD_Initialize
LCD_DisplayValue:               Goto    Do_LCD_DisplayValue
LCD_ClearScreen:                Goto    Do_LCD_ClearScreen
LCD_TopLeft:                    Goto    Do_LCD_TopLeft    
LCD_TopRight:                   Goto    Do_LCD_TopRight   
LCD_BottomLeft:                 Goto    Do_LCD_BottomLeft 
LCD_BottomRight:                Goto    Do_LCD_BottomRight
;...............................................................................

;...............................................................................
    .global Delay100us
    .global DisplayStartupMessage   
    .global DisplayMessageString    
    .global LCD_Initialize       
    .global LCD_DisplayValue   
    .global LCD_ClearScreen
    .global LCD_TopLeft    
    .global LCD_TopRight   
    .global LCD_BottomLeft 
    .global LCD_BottomRight
    .global LCD_ShowCursor
    .global LCD_HideCursor
    .global LCD_SendCharacter
    .global DisplayWaveString
;...............................................................................

;...............................................................................
BackupValue:
    BCF     DA0
    MovF    ValueLo,W
    BSF     DA0
    MovWF   Scratch0
    
    BCF     DA0
    MovF    ValueHi,W
    BSF     DA0
    MovWF   Scratch1

    BCF     DA0
    Return
;...............................................................................

;...............................................................................
RestoreValue:
    BSF     DA0
    MovF    Scratch0,W
    BCF     DA0
    MovWF   ValueLo
    
    BSF     DA0
    MovF    Scratch1,W
    BCF     DA0
    MovWF   ValueHi

    Return
;...............................................................................

;...............................................................................
; At 50Mz each instruction takes 0.08uS so 12.5 instruction cyles required
; The call to get here is 2, the return is 1 so 9 Nops required
Do_Delay1us: 
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Nop
    Return
;...............................................................................

;...............................................................................
; The constants for this code were calculated using the spreadsheet located in
;   Altium\Help\TSK165 Software Delay Calculator.xls
;   A clock of 50MHz is assumed
;...............................................................................
Do_Delay10us:
    MovWF  CountOuter
    LoopOuter:
        MovLW   30
        MovWF   CountInner
        LoopInner:
            NOP
            DECFSZ  CountInner,f
        Goto    LoopInner

        DECFSZ  CountOuter,f
    Goto    LoopOuter

    Return
;...............................................................................

;...............................................................................
; The constants for this code were calculated using the spreadsheet located in
;   Altium\Help\TSK165 Software Delay Calculator.xls
;   A clock of 50MHz is assumed
;...............................................................................
Do_Delay100us:
    MovWF  CountOuter
    LoopOuterX:
        MovLW   243
        MovWF   CountInner
        LoopInnerX:
            NOP
            NOP
            DECFSZ  CountInner,f
        Goto    LoopInnerX

        DECFSZ  CountOuter,f
    Goto    LoopOuterX

    Return
;...............................................................................

;...............................................................................
Do_Delay1ms:
   MovLW 10
   Goto Do_Delay100us
;...............................................................................

;...............................................................................
Do_Delay2ms:
   MovLW 20
   Goto Do_Delay100us
;...............................................................................

;...............................................................................
Do_Delay10ms:
   MovLW 100
   Goto Do_Delay100us
;...............................................................................

;...............................................................................
LCD_ShowCursor:
    MovLW   cLCD_SetDisplayMode | cLCD_DisplayOn | cLCD_ShowCursor | cLCD_BlinkingOn
    GCall   LCD_SendCommand
    Return
;...............................................................................

;...............................................................................
LCD_HideCursor:
    MovLW   cLCD_SetDisplayMode | cLCD_DisplayOn | cLCD_ShowCursor | cLCD_BlinkingOff
    GCall   LCD_SendCommand
    Return
;...............................................................................

;...............................................................................
;Sends command to LCD
;Required command must be in W
;...............................................................................
LCD_SendCommand:
    MovWF   Temp                            ; Store command in Temp

    MovLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs

    Loop_WaitingToSendCommand:  
        LCD_EN_0                            ; Disable LCD - Set Enable Low
        LCD_RW_1                            ; Set RW High
        LCD_RS_0                            ; Set For Command RS Low
        LCD_EN_1                            ; Enable LCD - Set Enable High
        Wait_1us
        LCD_EN_0                            ; Disable LCD - Set Enable Low
        BTFSC LCD_BUSY                      ; Check Busy flag, if High (Busy) then try again
    Goto  Loop_WaitingToSendCommand
    Wait_10us
    
    MovLW   PortAsOutput
    TRIS    LCD_DATA                        ; Set Port To Outputs
    LCD_RW_0                                ; Set RW Low
    Wait_1us
    LCD_EN_1                                ; Enable LCD 0 Set E High
    Wait_1us
    MovF    Temp, W
    MovWF   LCD_DATA                        ; Send data to LCD
    Wait_1us
    LCD_EN_0                                ; Disable LCD - Set Enable Low
    
    MovLW   5                         
    GCall   Delay100us
    Return

;...............................................................................
;Sends character to LCD
;Required character must be in W
;...............................................................................
LCD_SendCharacter:
    MovWF   Temp                            ; Command to send is in W

    MovLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs

    Loop_WaitingToSendChar:  
        LCD_EN_0                            ; Disable LCD - Set Enable Low
        LCD_RW_1                            ; Set RW High
        LCD_RS_0                            ; Set For Command RS Low
        LCD_EN_1                            ; Enable LCD - Set Enable High
        Wait_1us
        LCD_EN_0                            ; Disable LCD - Set Enable Low
        BTFSC LCD_BUSY                      ; Check Busy flag, if High (Busy) then try again
    Goto  Loop_WaitingToSendCommand
    Wait_10us

    MovLW PortAsOutput
    TRIS  LCD_DATA                          ; Set Port To Outputs
    LCD_RW_0                                ; Set RW Low
    Wait_1us
    LCD_RS_1                                ; Set For Data - RS High
    Wait_1us
    LCD_EN_1                                ; Enable LCD 0 Set E High
    Wait_1us

    MovF  Temp, W
    MovWF LCD_DATA                          ; Send data to LCD
    Wait_1us
    LCD_EN_0                                ; Disable LCD - Set Enable Low
    
    MovLW   5                         
    GCall   Delay100us
    Return

;...............................................................................
;Initialisiation code to be executed after power-up (i.e.: before any other subroutines are used).
;Always GCall from top level only
;...............................................................................
Do_LCD_Initialize:
    LCD_EN_0                                ; Disable LCD - Set Enable Low
    LCD_RS_0                                ; Set For Command RS Low

    MovLW   PortAsInput
    TRIS    LCD_DATA                        ; Set Port To Inputs
    LCD_RW_1                                ; Set RW High

    MovLW   0x38
    GCall    LCD_SendCommand                 ; 8-bit-interface, 2-lines

    MovLW   cLCD_SetDisplayMode | cLCD_BlinkingOff | cLCD_HideCursor | cLCD_DisplayOff
    GCall   LCD_SendCommand

    MovLW   cLCD_ClearScreen
    GCall   LCD_SendCommand

    MovLW   cLCD_ReturnHome
    GCall   LCD_SendCommand

    MovLW   cLCD_SetDisplayMode | cLCD_DisplayOn
    GCall   LCD_SendCommand

    MovLW   cLCD_SetEntryMode | cLCD_ShiftIncrement
    GCall    LCD_SendCommand

    Return
;...............................................................................

;...............................................................................
;Initialisiation code to be executed after power-up (i.e.: before any other subroutines are used).
;Always GCall from top level only
;...............................................................................
Do_LCD_ClearScreen:
    MovLW   cLCD_ClearScreen
    GCall   LCD_SendCommand
    GCall   Delay2ms             ;2ms required to clear screen

    MovLW   cLCD_ReturnHome
    GCall   LCD_SendCommand      
    GCall   Delay2ms             ;2ms required to return home
    
    Return
;...............................................................................

;...............................................................................
;W Contains the message table index
;...............................................................................
Do_DisplayMessageString:
    MovWF   TableIndex
    MovLW   0
    MovWF   StringOffset
    
    StringDisplayLoop:
        MovF    TableIndex,W
        GCall   GetStringTableCharacter
        IORLW   0                           ; if (returned byte is zero)
        BTFSC   Z
        Goto    FinishedString
        
        GCall   LCD_SendCharacter
        IncF    StringOffset,F 
    Goto    StringDisplayLoop
    
    FinishedString:   
    Return
;...............................................................................

;...............................................................................
Do_DisplayWaveString:
    MovWF   TenK
    GCall   LCD_ClearScreen
    GCall   LCD_TopLeft
    MovLW   TableIndex_GeneratingWave
    GCall   DisplayMessageString
    
    GCall   LCD_BottomLeft
    MovF    TenK,W
    GCall   DisplayMessageString
    Return
;...............................................................................

;...............................................................................
Do_DisplayStartupMessage:
    GCall   LCD_ClearScreen
    
    GCall   LCD_TopLeft
    MovLW   TableIndex_Startup1
    GCall   DisplayMessageString
    
    GCall   LCD_BottomLeft
    MovLW   TableIndex_Startup2
    GCall   DisplayMessageString
    
    GCall   LCD_TopLeft
    Return
;...............................................................................

;...............................................................................
LookupTable_Character:
    ADDWF   PCL,f
    RETLW   '0123456789ABCEF'
;...............................................................................

;...............................................................................
Do_DisplayBCD:
    GCALL   LookupTable_Character
    GCall   LCD_SendCharacter
    Return
;...............................................................................

;...............................................................................
Do_LCD_TopLeft:
    MovLW   cLCD_SetDisplayAddress | 00H      
    GCall   LCD_SendCommand
    Return
;...............................................................................
    
;...............................................................................
Do_LCD_TopRight:
    MovLW   cLCD_SetDisplayAddress | 00H + 9 ; Cursor to position 0 in Second row
    GCall   LCD_SendCommand
    MovLW   0x7E
    GCall   LCD_SendCharacter
    Return
;...............................................................................

;...............................................................................
Do_LCD_BottomLeft:
    MovLW   cLCD_SetDisplayAddress | 40H 
    GCall   LCD_SendCommand
    Return
;...............................................................................
    
;...............................................................................
Do_LCD_BottomRight:
    MovLW   cLCD_SetDisplayAddress | 40H + 9 
    GCall   LCD_SendCommand
    MovLW   0x7E
    GCall   LCD_SendCharacter
    Return
;...............................................................................

;...............................................................................
; Since the reference voltage is 2.048 volts and the data in ValueHi,ValueLow is
; 10 bits, then 0000 => 0.0 Volts and 0x3FF (1023) => 2.048 Volts.
; Therefore 1000 (decimal) => 2.000 Volts
; So to display the mV values we need to double the number for correct display. 
Do_LCD_DisplayValue:
    Call    BackupValue
    BCF     C
    RLF     ValueLo,F
    RLF     ValueHi,F
    
    GCALL   BinaryToBCD
    
    MovF    Thou,W
    GCall   DisplayBCD
    
    MovLW   '.'
    GCall   LCD_SendCharacter
    
    MovF    Hund,W
    GCall   DisplayBCD
    MovF    Tens,W
    GCall   DisplayBCD
    MovF    Ones,W
    GCall   DisplayBCD
    
    MovLW   'V'
    GCall   LCD_SendCharacter
    
    MovLW   ' '
    GCall   LCD_SendCharacter

    Call    RestoreValue
    Return
;...............................................................................

;...............................................................................
.define  W_Save    "MovWF TempW"
.define  W_Restore "MovF  TempW,W"
;...............................................................................
    
;...............................................................................
; Enter with 16-bit binary number in ValueHi:ValueLo.
; Exits with BCD equivalent in TenK:Thou:Hund:Tens:Ones.
Do_BinaryToBCD:
        W_Save                                 
        MovLW   0xF0
        MovWF   Temp
        W_Restore
        
        SwapF   ValueHi,w
        AndLW   0x0F 
        
        ;.....
        W_Save                                 
        MovLW   0xF0
        MovWF   Temp
        W_Restore
        ;....
        AddWF   Temp,W
        
        MovWF   Thou
        AddWF   Thou,f
        ;.....
        W_Save                                 
        MovLW   0xE2
        MovWF   Temp
        W_Restore
        ;....
        AddWF   Temp,W
        ;addlw   0xE2
        
        MovWF   Hund
        
        ;.....
        W_Save                                 
        MovLW   0x32
        MovWF   Temp
        W_Restore
        ;....
        AddWF   Temp,W
        ;addlw   0x32

        MovWF   Ones

        MovF    ValueHi,w
        AndLW   0x0F
        AddWF   Hund,f
        AddWF   Hund,f
        AddWF   Ones,f
        ;.....
        W_Save                                 
        MovLW   0xE9
        MovWF   Temp
        W_Restore
        ;....
        AddWF   Temp,W
        ;addlw   0xE9

        MovWF   Tens
        AddWF   Tens,f
        AddWF   Tens,f

        SwapF   ValueLo,w
        AndLW   0x0F
        AddWF   Tens,f
        AddWF   Ones,f

        RLF     Tens,f 
        RLF     Ones,f 
        ComF    Ones,f 
        RLF     Ones,f 

        MovF    ValueLo,w 
        AndLW   0x0F 
        AddWF   Ones,f
        RLF     Thou,f 

        MovLW   7
        MovWF   TenK 

        MovLW   10           ; At this point, the original number is                                       
Lb1:                         ; equal to                                                    
        AddWF   Ones,f       ; TenK*10000+Thou*1000+Hund*100+Tens*10+Ones ;                
        DecF    Tens,f       ; If those entities are regarded as two's compliment binary.  
        BTFSS   C            ; To be precise, all of them are negative except TenK.        
        Goto    Lb1          ; Now the number needs to be normalized, but this             
Lb2:                         ; can all be done with simple byte arithmetic.                
        AddWF   Tens,f 
        DecF    Hund,f 
        BTFSS   C
        Goto    Lb2 
Lb3: 
        AddWF   Hund,f 
        DecF    Thou,f 
        BTFSS   C
        Goto    Lb3 
Lb4: 
        AddWF   Thou,f 
        DecF    TenK,f 
        BTFSS   C
        Goto    Lb4 

        Return
;...............................................................................

    

