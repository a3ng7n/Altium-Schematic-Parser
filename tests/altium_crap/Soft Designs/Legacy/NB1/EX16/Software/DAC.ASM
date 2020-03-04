.Include "DAC_Defines.Asm"

;...............................................................................
    .Section Code_AMain, Code, at(0)
__start:
    BCF     PA0  ;Set all pages to Zero         
    BCF     PA1
    BCF     DA0           
    BCF     DA1
    Goto MainProc
;...............................................................................
    
;...............................................................................
MainProc:
    MovLW   125
    TRIS    RatePort   ;Sets the divider to generate a rate of 6400Hz
    
    CLRF    TableIndex      
    GCall   DAC_Initialize
    GCall   LCD_Initialize
    GCall   DisplayStartupMessage
    
    MovLW   0
    GCall   ADC_WriteSetup
    
    MovLW   0
    GCall   ADC_WriteConfig
    
    ClearKey_0 
    ClearKey_1
    ClearKey_0 
    
WaitForInitialKeyPress:    
    btfss   ValidKey           
    Goto    WaitForInitialKeyPress   
    
    GCall   LCD_ClearScreen
    GOTO    MainLoop
;...............................................................................

;...............................................................................
MainLoop:
    ClearKey_0 
    btfsc   ValidKey           
    Goto    HandleKeyPress_TopLevel   
    GCall   ReadADCChannels  
    MovLW   100
    GCall   Delay100us
    Goto    MainLoop
;...............................................................................

;...............................................................................
WaitForAnyKeyPress:    
    Goto    WaitForAnyKeyPress
    Return
;...............................................................................

;...............................................................................
HandleKeyPress_TopLevel:
    MovF  KeyPadPort,W
    AndLW 0FH
    TRIS  KeyValueOnLEDs
    ClearKey_1 
    ClearKey_0 
    AddWF PCL,F
    
    Goto  HandleKey_1      ;1
    Goto  HandleKey_2      ;2
    Goto  HandleKey_3      ;3
    Goto  MakeRamp_1Kz     ;C
    
    Goto  HandleKey_4      ;4
    Goto  HandleKey_5      ;5
    Goto  HandleKey_6      ;6
    Goto  MakeSineWave     ;D
    
    Goto  HandleKey_7      ;7
    Goto  HandleKey_8      ;8
    Goto  HandleKey_9      ;9
    Goto  MakeRamp_50Kz    ;E
    
    Goto  HandleKey_A      ;A
    Goto  HandleKey_0      ;0
    Goto  HandleKey_B      ;B
    Goto  MakeTriangle_50Kz;F
;...............................................................................

;...............................................................................
ReadADCChannels:
    MovLW   255                   ;Read ADC Channel 0
    GCall   Delay100us
    
    MovLW   0                     ;Read ADC Channel 0
    MovWF   AdcChannel
    GCall   ADC_WriteConfig
    GCall   ADC_Read
    GCall   LCD_TopRight
    GCall   LCD_DisplayValue
    
    MovLW   255                   ;Read ADC Channel 0
    GCall   Delay100us
    MovLW   1                     ;Read ADC Channel 1
    MovWF   AdcChannel
    GCall   ADC_WriteConfig
    GCall   ADC_Read
    GCall   LCD_BottomRight
    GCall   LCD_DisplayValue
    
    Return
    ;Goto  MainLoop
;...............................................................................
    
;...............................................................................
HandleKey_Null:
    Goto  MainLoop
;...............................................................................
    
;...............................................................................
IncrementRampCounter:
;    MOVF    X_Hi,W
;    ADDWF   Y_Hi,F
;    MOVF    X_Lo,W
;    BTFSC   C
;    INCFSZ  X_Lo,W
;    ADDWF   Y_Lo,F

    AddWF   ValueLo,F
    BTFSS   C                         ; If carry set then work on the hi-byte
    Return                            
   
    IncF    ValueHi,F                 ; Max Value can be $3FF - 1023
    MovLW   04H                       ; If goes to $400 then wrap it
    SubWF   ValueHi,W
    BTFSS   Z
    Return
   
    CLRF    ValueLo
    CLRF    ValueHi
DoneIncrementRampCounter:
    Return
;...............................................................................

;...............................................................................
DecrementRampCounter:
    SubWF   ValueLo,F                 ; If wraps to $FF then dec high byte
    BTFSC   C                         ; If carry set then work on the hi-byte
    Return                            
    
    DecF    ValueHi,F                 ; Max Value can be $3FF - 1023
    MovLW   0xFF                      ; If wraps to $FF then back up to 04
    SubWF   ValueHi,W
    BTFSS   Z
    Return
    
    MovLW   0x04
    MovWF   ValueHi
    MovLW   0x00
    MovWF   ValueLo
DoneDecrementRampCounter:
    Return
;...............................................................................

;...............................................................................
DAC_PrepareChannel:
    MovLW   Max5841_Address_Write
    MovWF   I2C_Address
    Return
;...............................................................................

;...............................................................................
DAC_PrepareChannelDisplay:
    GCall   LCD_ShowCursor
    BTFSC   DacChannel,0
    Goto    DisplayChannel_1
DisplayChannel_0:    
    GCall   LCD_TopLeft
    MovLW   'A'
    GCall   LCD_SendCharacter
    MovLW   ':'
    GCall   LCD_SendCharacter
    Goto    DisplayChannel_Done
DisplayChannel_1:    
    GCall   LCD_BottomLeft
    MovLW   'B'
    GCall   LCD_SendCharacter
    MovLW   ':'
    GCall   LCD_SendCharacter
DisplayChannel_Done:
    GCall   LCD_DisplayValue
    GCall   DAC_PrepareChannel
    Return
;...............................................................................

;...............................................................................
HandleKey_A:
    MovLW   0
    MovWF   DacChannel
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_B:
    MovLW   1
    MovWF   DacChannel
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_0:
    MovLW   0
    MovWF   ValueHi
    MovLW   0
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    
    GCall   DAC_Write
    ;Goto    ReadADCChannels
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_1:
    MovLW   @MSB(100)
    MovWF   ValueHi
    MovLW   @LSB(100)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_2:
    MovLW   @MSB(200)
    MovWF   ValueHi
    MovLW   @LSB(200)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_3:
    MovLW   @MSB(300)
    MovWF   ValueHi
    MovLW   @LSB(300)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_4:
    MovLW   @MSB(400)
    MovWF   ValueHi
    MovLW   @LSB(400)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_5:
    MovLW   @MSB(500)
    MovWF   ValueHi
    MovLW   @LSB(500)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................
    
;...............................................................................
HandleKey_6:
    MovLW   @MSB(600)
    MovWF   ValueHi
    MovLW   @LSB(600)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_7:
    MovLW   @MSB(700)
    MovWF   ValueHi
    MovLW   @LSB(700)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_8:
    MovLW   @MSB(750)
    MovWF   ValueHi
    MovLW   @LSB(750)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_9:
    MovLW   @MSB(1000)
    MovWF   ValueHi
    MovLW   @LSB(1000)
    MovWF   ValueLo
    Call    DAC_PrepareChannelDisplay
    GCall   DAC_Write
    ;Goto  ReadADCChannels     ;F
    Goto    MainLoop
;...............................................................................

;...............................................................................
MakeRamp_1Kz:
    MovLW   TableIndex_Ramp1KHz
    GCall   DisplayWaveString
    
    MovLW   160
    TRIS    RatePort   ;Sets the divider to generate a rate of 5KHz
                       ;So 5 samples will generate 1KHz
    MovLW   000H
    MovWF   ValueHi
    MovLW   000H
    MovWF   ValueLo
    
    Call    DAC_PrepareChannel
RampLoop:
    BTFSS   RateFlag
    Goto    RampLoop
    RateFlag_1
    RateFlag_0

    GCall   DAC_Write
    MovLW   250                   ;Increment by 250mV
    GCall   IncrementRampCounter
    btfss   ValidKey              ;Another key pressed so exit
    Goto    RampLoop
    Goto    MainLoop
;...............................................................................

;...............................................................................
MakeRamp_50Kz:
    MovLW   TableIndex_Ramp50Hz
    GCall   DisplayWaveString

    MovLW   125
    TRIS    RatePort   ;Sets the divider to generate a rate of 6400Hz
                       ;So 5 samples will generate 1KHz
    CLRF    ValueHi
    CLRF    ValueLo
    
    Call    DAC_PrepareChannel
RampLoop2:
    BTFSS   RateFlag
    Goto    RampLoop2
    RateFlag_1
    RateFlag_0

    GCall   DAC_Write
    MovLW   8                     ;Increment by 16mV
    GCall   IncrementRampCounter
    btfss   ValidKey              ;Another key pressed so exit
    Goto    RampLoop2
    Goto    MainLoop
;...............................................................................

;...............................................................................
MakeTriangle_50Kz:
    MovLW   TableIndex_Triangle50Hz
    GCall   DisplayWaveString
    
    MovLW   125
    TRIS    RatePort   ;Sets the divider to generate a rate of 6400Hz
                       ;So 5 samples will generate 1KHz
    Call    DAC_PrepareChannel

TrinagleLoop:
    MovLW   0x04
    MovWF   ValueHi
    MovLW   0x00
    MovWF   ValueLo
    
RampDownLoop:
    BTFSS   RateFlag
    Goto    RampDownLoop
    RateFlag_1
    RateFlag_0

    GCall   DAC_Write
    MovLW   16                    ;Increment by 8mV
    GCall   DecrementRampCounter
    btfsc   ValidKey              ;Another key pressed so exit
    Goto    MainLoop
    
    MovF    ValueLo,W
    Btfss   Z
    Goto    RampDownLoop
    MovF    ValueHi,W
    Btfss   Z
    Goto    RampDownLoop
    
    CLRF    ValueHi
    CLRF    ValueLo

RampUpLoop:
    BTFSS   RateFlag
    Goto    RampUpLoop
    RateFlag_1
    RateFlag_0

    GCall   DAC_Write
    MovLW   16                    ;Increment by 8mV
    GCall   IncrementRampCounter
    btfsc   ValidKey              ;Another key pressed so exit
    Goto    MainLoop
    
    MovF    ValueLo,W
    Btfss   Z
    Goto    RampUpLoop
    MovF    ValueHi,W
    Btfss   Z
    Goto    RampUpLoop
    
    Goto    TrinagleLoop
;...............................................................................

;...............................................................................
MakeSineWave:
    MovLW   TableIndex_Sine50Hz
    GCall   DisplayWaveString

    MovLW   125
    TRIS    RatePort   ;Sets the divider to generate a rate of 6400Hz
                       ;This allows 128 samples to generate 50Hz
    MovLW   0
    MovWF   TableIndex
    
    Call    DAC_PrepareChannel
SineWaveLoop:
    BTFSS   RateFlag
    Goto    SineWaveLoop
    RateFlag_1
    RateFlag_0
    
    GCall   GetSine
    GCall   DAC_Write
    IncF    TableIndex,F
    IncF    TableIndex,F
    
    btfss   ValidKey              ;Another key pressed so exit
    Goto    SineWaveLoop
    Goto    MainLoop
;...............................................................................




;...............................................................................
    .Section LookupTables, Code, InPage, at(0x400)
;...............................................................................

;...............................................................................
;W sets the Table Number to use
;StringOffset sets the distance into the table
;...............................................................................
GetStringTableCharacter:
     MovWF   Temp
     
     MovLW   TableIndex_Startup1
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Startup1
     
     MovLW   TableIndex_Startup2
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Startup2
     
     MovLW   TableIndex_Sine50Hz             
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Sine50Hz

     MovLW   TableIndex_Ramp50Hz             
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Ramp50Hz

     MovLW   TableIndex_Ramp1KHz             
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Ramp1KHz

     MovLW   TableIndex_Triangle50Hz         
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_Triangle50Hz

     MovLW   TableIndex_GeneratingWave   
     SubWF   Temp,W 
     Btfsc   Z
     Goto    DisplayLookup_GeneratingWave
     
     RetLW   0
;...............................................................................

;...............................................................................
GetSine:    
; Requires the 8-Bit value to be in TableIndex
; Returns an 10-Bit unsigned value in ValueLo,ValueHi (ready for 10-Bit DAC
; the two least signifcant bits of ValueLo,ValueHi are set to zero.              
;...............................................................................
       MovLW 0FFH
       MovWF Temp
       
       MovF  TableIndex,W 
    
       Btfsc TableIndex,6   ; Is the counter in the 2nd or 4th quadrant?
       SubWF Temp,w         ; yes, complement it to normalize to 1st or 3rd
       AndLW 03FH           ; normalize to 1st quadrant for magnitude calc
       GCall SineTable      ; get magnitude
       
       Btfss TableIndex,7   ; Is the counter in 1st or 2nd quadrant?
       Goto  UpperHalf
LowerHalf:                  
       SubWF Temp,W         ; Counter is in 3rd or 4th quadrant so               
       MovWF ValueLo        ; invert the signal for the bottom half of sine wave 
       BCF   ValueLo,7
       Goto  ShiftValueUp
       
UpperHalf:
       MovWF ValueLo        ; Counter is in 1st or 2nd quadrant so               
       BSF   ValueLo,7      ; normal signal but double the amplidtude
       
ShiftValueUp:
       ;Sine value is in ValueLo as 8-Bit, shift it up to 10 bit for DAC
       CLRF  ValueHi
       
       RLF   ValueLo,F       ; Hi Bit into Carry
       RLF   ValueHi,F       ; Carry into low bit
       RLF   ValueLo,F       ; Hi Bit into Carry
       RLF   ValueHi,F       ; Carry into low bit
       
       BCF   ValueLo,0       ; Low 2 bits are undefined so clear them
       BCF   ValueLo,1
       
       Return
;...............................................................................

;...............................................................................
SineTable:
;...............................................................................
       AddWF PCL,F
       .db    0,  3,  6,  9, 13, 16, 19, 22, 25, 28, 31, 34, 37, 40, 43, 46
       .db   49, 52, 55, 58, 61, 63, 66, 69, 72, 74, 77, 79, 82, 84, 86, 89
       .db   91, 93, 95, 97, 99,101,103,105,107,108,110,112,113,114,116,117
       .db  118,119,120,121,122,123,124,124,125,126,126,126,127,127,127,127
;...............................................................................

;...............................................................................
DisplayLookup_Sine50Hz:
    MovF    StringOffset,W
    AddWF   PCL,f
    RETLW   '50Hz Sine Wave ',0
;...............................................................................

;...............................................................................
DisplayLookup_Ramp50Hz:
    MovF    StringOffset,W
    AddWF   PCL,f
    RETLW   '50 KHz Ramp     ',0
;...............................................................................

;...............................................................................
DisplayLookup_Ramp1KHz:
    MovF    StringOffset,W
    AddWF   PCL,f
    RETLW   '1.0 KHz Ramp   ',0
;...............................................................................

;...............................................................................
DisplayLookup_Triangle50Hz:
    MovF    StringOffset,W
    AddWF   PCL,f
    RETLW   '50 Hz Triangle ',0
;...............................................................................

;...............................................................................
DisplayLookup_GeneratingWave:
    MovF    StringOffset,W
    ADDWF   PCL,f
    RETLW   'Generating     ',0
;...............................................................................

;...............................................................................
DisplayLookup_Startup1:
    MovF    StringOffset,W
    ADDWF   PCL,f
    RETLW   'NanoBoard NB1  ',0
;...............................................................................

;...............................................................................
DisplayLookup_Startup2:
    MovF    StringOffset,W
    ADDWF   PCL,f
    RETLW   'DAC/ADC Demo   ',0
;...............................................................................

;...............................................................................
    .global GetStringTableCharacter
;...............................................................................
    
    .global __start
END        





