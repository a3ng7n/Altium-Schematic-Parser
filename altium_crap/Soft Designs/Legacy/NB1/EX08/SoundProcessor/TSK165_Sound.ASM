;...............................................................................
Flag_AckFailure       .equ   0
;...............................................................................

;...............................................................................
F_Z                   .equ   2               ; Workaround to Z not working
F_PA0                 .equ   5               ; Workaround to PA0 not working
F_PA1                 .equ   6               ; Workaround to PA1 not working
;...............................................................................

;...............................................................................
; SDA is on port C Bit 0 - Set PORTC bit 0 low to drive low, otherwise it will be pulled high by Pullup
; SCL is on port C Bit 1 - Set PORTC bit 1 low to drive low, otherwise it will be pulled high by Pullup
;...............................................................................

;...............................................................................
.define Return    "RetLW 0"
.define I2C_Delay "Call I2C_TimingDelay"
;...............................................................................

;...............................................................................
Max5841_Address_Write       .equ 0b01111010
Max5841_Address_Read        .equ 0b01111011

Max5841_LoadA_UpdateAll     .equ 000H
Max5841_LoadB_UpdateAll     .equ 010H
Max5841_LoadC_UpdateAll     .equ 020H
Max5841_LoadD_UpdateAll     .equ 030H

Max5841_LoadA               .equ 040H
Max5841_LoadB               .equ 050H
Max5841_LoadC               .equ 060H
Max5841_LoadD               .equ 070H

Max5841_UpdateAllAndLoadA   .equ 080H
Max5841_UpdateAllAndLoadB   .equ 090H
Max5841_UpdateAllAndLoadC   .equ 0A0H
Max5841_UpdateAllAndLoadD   .equ 0B0H

Max5841_LoadAndUpdateAll    .equ 0C0H
Max5841_LoadAll             .equ 0D0H
Max5841_UpdateAll           .equ 0E0H

Max5841_ExtendedCommandMode .equ 0F0H
Max5841_ReadA               .equ 0F1H
Max5841_ReadB               .equ 0F2H
Max5841_ReadC               .equ 0F4H
Max5841_ReadD               .equ 0F8H
;...............................................................................

;..............................................................................
cFlashCommand_DataWrite           .equ   002H    ;PP          
cFlashCommand_SlowRead            .equ   003H    ;READ        
cFlashCommand_FastRead            .equ   00BH    ;FAST_READ   
cFlashCommand_WriteEnable         .equ   006H    ;WREN        
cFlashCommand_WriteDisable        .equ   004H    ;WRDI        
cFlashCommand_ReadStatusRegister  .equ   005H    ;RDSR        
cFlashCommand_WriteStatusRegister .equ   001H    ;WRSR        
cFlashCommand_EnterDeepPowerDown  .equ   0B9H    ;DP          
cFlashCommand_ExitDeepPowerDown   .equ   0ABH    ;RES         
cFlashCommand_BulkErase           .equ   0C7H    ;BE          
cFlashCommand_EraseSector         .equ   0D8H    ;SE          
;..............................................................................

;..............................................................................
ControlBits                       .equ   PORTC
StatusBits                        .equ   PORTB
;..............................................................................

;..............................................................................
SDA                               .equ   ControlBits.0  
SCL                               .equ   ControlBits.1  
ClearSampleFlag                   .equ   ControlBits.2 
SPI_Enable                        .equ   ControlBits.3 
SPI_Clock                         .equ   ControlBits.4 
SPI_Mode                          .equ   ControlBits.5 
SPI_Select                        .equ   ControlBits.6 
SPI_DataOut                       .equ   ControlBits.7 

SPI_DataIn                        .equ   StatusBits.7  
SDA_OnB                           .equ   StatusBits.0 
SCL_OnB                           .equ   StatusBits.1 
SampleFlag                        .equ   StatusBits.2 ;This is the 22.046 HHz sample clock flag
OutputOn_DAC                      .equ   StatusBits.3 
OutputOn_Speaker                  .equ   StatusBits.4 
;..............................................................................

;...............................................................................
.define      SDA_0             "BCF  SDA"
.define      SCL_0             "BCF  SCL"
.define      SDA_1             "BSF  SDA"
.define      SCL_1             "BSF  SCL"
.define      SPI_Enable_High   "BSF  SPI_Enable"
.define      SPI_Enable_Low    "BCF  SPI_Enable"
.define      SPI_Clock_High    "BSF  SPI_Clock"
.define      SPI_Clock_Low     "BCF  SPI_Clock"
.define      SPI_Mode_High     "BSF  SPI_Mode"
.define      SPI_Mode_Low      "BCF  SPI_Mode"
.define      SPI_Select_High   "BSF  SPI_Select"
.define      SPI_Select_Low    "BCF  SPI_Select"
.define      SPI_DataOut_High  "BSF  SPI_DataOut"
.define      SPI_DataOut_Low   "BCF  SPI_DataOut"
.Define      SPI_Delay         "NOP"

;...............................................................................

;...............................................................................
; Command Sequence to write a command and 10 Bit data is:
;   1. Start Bit
;   2. First Byte  - A6 A5 A4 A3 A2 A1 A0 R/W      Write => 0111 1010     Read => 0111 1011
;   3. Check For ACK
;   4. Second Byte - C3 C2 C1 C0 D9 D8 D7 D6       See 5841 commands above
;   5. Check For ACK
;   6. Third Byte  - D5 D4 D3 D2 D1 D0 S1 S0       S1 & S0 are always zero
;   7. Check For ACK
;...............................................................................

;...............................................................................
    .section data, data
CountInner      .dsb    1
CountOuter      .dsb    1
CurrentKey      .dsb    1
Temp            .dsb    1
BitCounter      .dsb    1
DacData1        .dsb    1
DacData2        .dsb    1
DataByte        .dsb    1
I2C_Flags       .dsb    1
CommandByte1    .dsb    1
CommandByte2    .dsb    1
ValueLo         .dsb    1
ValueHi         .dsb    1
Instruction     .dsb    1
DacChannel      .dsb    1
I2C_Address     .dsb    1           

AddressByte2    .dsb    1     ; MSB Of 24 bit address
AddressByte1    .dsb    1     ;
AddressByte0    .dsb    1     ; LSB Of 24 bit address
SPI_ByteIn      .dsb    1
SPI_ByteOut     .dsb    1 
SampleCount     .dsb    1 
;...............................................................................

;...............................................................................
    .section bit, bit
LastTime        .dsbit  1
;...............................................................................

;...............................................................................
    .section dacmain, code, inpage       ; inpage allows for interesting optimizations for
__start:
    .global __start
    GJMP MainProc
;...............................................................................
                            
;...............................................................................
;Jump table for Calls
;Call instruction only allows 8 bit address so use a jump table
;Call the Jump table which does a goto the address
;...............................................................................
VariableDelay:            Goto Do_VariableDelay
WaitForStableKeys:        Goto Do_WaitForStableKeys
KeyScan:                  Goto Do_KeyScan
DisplayKey:               Goto Do_DisplayKey
I2C_TimingDelay:          Goto Do_I2C_TimingDelay
I2C_GetACK:               Goto Do_I2C_GetACK
I2C_SendACK:              Goto Do_I2C_SendACK
I2C_Initialize:           Goto Do_I2C_Initialize        

DAC_SendReadSequence:     Goto Do_DAC_SendReadSequence  
DAC_SendWriteSequence:    Goto Do_DAC_SendWriteSequence 
DAC_Write:                Goto Do_DAC_Write             
DAC_Read:                 Goto Do_DAC_Read              

SPI_Open:                 Goto DoSPI_Open
SPI_Close:                Goto DoSPI_Close
SPI_SendReceiveByte:      Goto DoSPI_SendReceiveByte
SPI_SendAddress24:        Goto DoSPI_SendAddress24
SPI_FlashMemoryRead:      Goto DoSPI_FlashMemoryRead
SPI_ReadFlashSignature:   Goto DoSPI_ReadFlashSignature
SPI_TestFlash:            Goto DoSPI_TestFlash

WaitForSampleClock:       Goto DoWaitForSampleClock        
;...............................................................................

;...............................................................................
HandleKeyPress_TopLevel:
    MovF  CurrentKey,W
    AndLW 0FH
    AddWF PCL,F
    
    Goto  HandleKey_Null   ;1
    Goto  HandleKey_Null   ;2
    Goto  HandleKey_Null   ;3
    Goto  HandleKey_Null   ;C
    
    Goto  HandleKey_Null   ;4
    Goto  HandleKey_Null   ;5
    Goto  HandleKey_6      ;6
    Goto  HandleKey_Null   ;D
    
    Goto  HandleKey_Null   ;7
    Goto  HandleKey_Null   ;8
    Goto  HandleKey_Null   ;9
    Goto  HandleKey_Null   ;E
    
    Goto  HandleKey_Null   ;A
    Goto  HandleKey_Null   ;0
    Goto  HandleKey_Null   ;B
    Goto  HandleKey_Null   ;F
;...............................................................................

;...............................................................................
DAC_GetWriteCommandForChannel:
    MovF    DacChannel,W
    AndLW   03H;
    AddWF   PCL,F
    .db Max5841_LoadA_UpdateAll  ;These are RetLW instructions - so will return with value in W
    .db Max5841_LoadB_UpdateAll
    .db Max5841_LoadC_UpdateAll
    .db Max5841_LoadD_UpdateAll
;...............................................................................

;...............................................................................
DAC_GetReadCommandForChannel:
    MovF    DacChannel,W
    AndLW   03H;
    AddWF   PCL,F
    .db Max5841_ReadA   ;These are RetLW instructions - so will return with value in W
    .db Max5841_ReadB
    .db Max5841_ReadC
    .db Max5841_ReadD
;...............................................................................

;...............................................................................
MainProc:
    BCF     STATUS, F_PA0           ; Select Register page 0
    BCF     STATUS, F_PA1
    
    MOVLW   0b00011000              ; Page 0
    MOVWF   STATUS                  ; Init STATUS
    
    Call    DAC_Initialize
    GOTO    MainLoop
;...............................................................................

;...............................................................................
WaitForAnyKeyPress:    
    Call    WaitForStableKeys       ; Stable No-Key will also Return
    Call    KeyScan                 ; Key value is CurrentKey - 0FFH means no keypressed
    ComF    CurrentKey,W            ; Zero in W if no key
    BTFSC   STATUS,F_Z              ; If Zero then no keys pressed so just increment and loop
    Goto    WaitForAnyKeyPress
    RetLW   0
;...............................................................................

;...............................................................................
MainLoop:
    Call    WaitForStableKeys       ; Stable No-Key will also Return
    Call    KeyScan                 ; Key value is CurrentKey - 0FFH means no keypressed
    ComF    CurrentKey,W            ; Zero in W if no key
    BTFSC   STATUS,F_Z              ; If Zero then no keys pressed so just increment and loop
    Goto    FinishedProcess
    Call    DisplayKey
    Goto    HandleKeyPress_TopLevel   

FinishedProcess:
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_Null:
    Goto  FinishedProcess
;...............................................................................
                        
;...............................................................................
IncrementRampCounter:
    IncF    ValueLo,F                 ; If doesn't wraps to zero then Z will be clear
    BTFSS   Z                         ; So skip if set else
    Return                            ; Exit
    
    IncF    ValueHi,F                 ; Max Value can be $3FF - 1023
    MovLW   04H                       ; If goes to $400 then wrap it
    SubWF   ValueHi,W
    BTFSS   Z
    Return
    
    MovLW   000H
    MovWF   ValueHi
DoneIncrementRampCounter:
    Return
;...............................................................................

;...............................................................................
DecrementRampCounter:
    DecF    ValueLo,F                 ; If wraps to $FF then dec high byte
    MovLW   0FFH                      
    SubWF   ValueLo,W
    BTFSS   Z                         ; So skip if set else
    Return                            ; Exit
    
    DecF    ValueHi,F                 ; Max Value can be $3FF - 1023
    MovLW   0FFH                      ; If wraps to $FF then back up to 04
    SubWF   ValueHi,W
    BTFSS   Z
    Return
    
    MovLW   03H
    MovWF   ValueHi
DoneDecrementRampCounter:
    Return
;...............................................................................

;...............................................................................
HandleKey_6:
    MovLW   000H
    MovWF   ValueHi
    MovLW   000H
    MovWF   ValueLo
    
    MovLW   0
    MovWF   DacChannel
    MovLW   Max5841_Address_Write
    MovWF   I2C_Address
RampLoop:
    Call    SPI_TestFlash
    ;Call    DAC_Write
    ;Call    IncrementRampCounter
    ;Goto    RampLoop
        
    Goto    FinishedProcess
;...............................................................................

;...............................................................................
HandleKey_7:
    Goto    FinishedProcess
;...............................................................................

;...............................................................................
HandleKey_8:
    Goto    FinishedProcess
;...............................................................................

;...............................................................................
DAC_Initialize:
    MovLW   Max5841_Address_Write
    MovWF   I2C_Address
    
    MovLW   Max5841_ExtendedCommandMode
    MovWF   CommandByte1
    
    MovLW   03CH
    MovWF   CommandByte2

    Call    DAC_SendWriteSequence
    Return
;...............................................................................

;...............................................................................
I2C_SendByte:
    MovWF    DataByte
    MovLW    8                     ; 8 bits to send
    MovWF    BitCounter

    Loop_SendByte:
        btfsc    DataByte,7        ; Check High Bit
        Goto     BitHigh
    BitLow:
        SDA_0
        Goto     SendBit
    BitHigh:
        SDA_1
        Goto     SendBit                             

    SendBit:
        I2C_Delay
        SCL_1                      ; pull clock line low
        I2C_Delay
        SCL_0                      ; pull clock line low
        I2C_Delay
        
        RLF      DataByte,F       ; Shift next bit to high position
        DecFSZ   BitCounter,F
    Goto Loop_SendByte

    Return
;...............................................................................

;...............................................................................
I2C_ReceiveByte:
    SDA_1                          ; {Release Data Line}
    I2C_Delay
    
    MovLW    0
    MovWF    DataByte
    MovLW    8                     ; 8 bits to read
    MovWF    BitCounter

    Loop_ReceiveByte:
        RLF      DataByte,F      ; Shift last bit read up towards MSB
        SCL_1                      ; High Before read
        I2C_Delay
        
        btfsc    SDA_OnB           ; Check SDA Bit
        Goto     ReadBitHigh
    ReadBitLow:
        BCF      DataByte,0
        Goto     ReceiveBit
    ReadBitHigh:
        BSF      DataByte,0
        Goto     ReceiveBit

    ReceiveBit:
        SCL_0                      ; pull clock line low
        I2C_Delay
        
        DecFSZ   BitCounter,F
    Goto Loop_ReceiveByte

    SDA_1
    I2C_Delay
    
    Return
;...............................................................................

;...............................................................................
I2C_SendStart:
    SCL_1
    I2C_Delay
    SDA_1
    I2C_Delay
    SDA_0
    I2C_Delay
    SCL_0
    I2C_Delay
    SDA_1
    I2C_Delay
    Return
;...............................................................................

;...............................................................................
I2C_SendStop:
    SDA_0
    ;I2C_Delay
    SCL_1
    I2C_Delay
    SDA_1
    I2C_Delay
    Return
;...............................................................................

;...............................................................................
Do_DAC_Write:
;...............................................................................
;          ValueHi                     ValueLo
;  -------------------------   -------------------------
;  |XX|XX|XX|XX|XX|XX|D9|D8|   |D7|D6|D5|D4|D3|D2|D1|D0|
;
;  Must be translated to =>
;
;     CommandByte1                 CommandByte2
;  -------------------------   -------------------------
;  |C3|C2|C1|C0|D9|D8|D7|D6|  |D5|D4|D3|D2|D1|D0|S1|S0|
;
;  S0 -> Zero
;  S1 -> Zero
;  C3..C0 = Command
;...............................................................................
    MovF  ValueHi,W
    MovWF CommandByte1

    MovF  ValueLo,W
    MovWF CommandByte2

    RLF   CommandByte2,F   ; Hi Bit into Carry
    RLF   CommandByte1,F   ; Carry into low bit
    RLF   CommandByte2,F   ; Hi Bit into Carry
    RLF   CommandByte1,F   ; Carry into low bit

    BCF   CommandByte2,0   ; Low 2 bits are undefined so clear them
    BCF   CommandByte2,1

    Call  DAC_GetWriteCommandForChannel

    IORWF CommandByte1,F     ;Add in the command bits

    Call DAC_SendWriteSequence
    Return
;...............................................................................

;...............................................................................
Do_DAC_Read:
    MovLW 0              ;Just so we can see if it changed
    MovWF ValueHi
    MovWF ValueLo
    
    Call  DAC_GetReadCommandForChannel
    MovWF CommandByte1
    
    Call  DAC_SendReadSequence
    
    MovF  CommandByte1,W
    MovWF ValueHi

    MovF  CommandByte2,W
    MovWF ValueLo

    RRF   ValueHi,F          ; Lo Bit into Carry  
    RRF   ValueLo,F          ; Carry into high bit 
    RRF   ValueHi,F          ; Lo Bit into Carry  
    RRF   ValueLo,F          ; Carry into high bit 
    
    MovLW 03H
    AndWF ValueHi,F
    
    Return
;...............................................................................

;...............................................................................
Do_DAC_SendWriteSequence:
;...............................................................................
; Prior to calling:
;    CommandByte1 = First Command Byte
;    CommandByte2 = Second Command Byte
;    I2C_Address  = Address of Target Device
;...............................................................................
    Call   I2C_Initialize
    Call   I2C_SendStart

    BCF    I2C_Address,0          ;Zero in lowest bit means write
    MovF   I2C_Address,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Write

    MovF   CommandByte1,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Write

    MovF   CommandByte2,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Write

DAC_Error_Write:
    Call I2C_SendStop;
    
    Return
;...............................................................................

;...............................................................................
Do_DAC_SendReadSequence:
;...............................................................................
; Prior to calling:
;    CommandByte1 = Command Byte
;    CommandByte2 = Not Used
;    I2C_Address  = Address of Target Device
; On Exit:
;    CommandByte1 = First Read Byte
;    CommandByte2 = Second Read Byte
;...............................................................................
    Call   I2C_Initialize
    Call   I2C_SendStart

                                  ;Send Address
    BCF    I2C_Address,0          ;Zero in lowest bit means write
    MovF   I2C_Address,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Read

                                  ;Send Command Byte
    MovF   CommandByte1,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Read

    Call   I2C_SendStart           ; Restart indicates a direction change
    
                                   ; Send Address Again - This time for read
    BSF    I2C_Address,0           ; One in lowest bit means read
    MovF   I2C_Address,W
    Call   I2C_SendByte
    Call   I2C_GetACK
    btfsc  I2C_Flags,Flag_AckFailure
    Goto   DAC_Error_Read
    
    Call   I2C_ReceiveByte
    MovF   DataByte,W  
    MovWF  CommandByte1
    Call   I2C_SendACK

    Call   I2C_ReceiveByte
    MovF   DataByte,W  
    MovWF  CommandByte2
    Call   I2C_SendACK
    
DAC_Error_Read:
    Call   I2C_SendStop;
    
    Return
;...............................................................................

;...............................................................................
Do_I2C_Initialize:
    SDA_1
    I2C_Delay
    SCL_0
    I2C_Delay
    
    Call I2C_SendStop
    Return
;...............................................................................

;...............................................................................
;At 40Mhz processor clock, this will generate a 2.0 uSecs software delay
;See the spreadsheet "TSK165 Software Delay Calculator.xls" for calculations
;This is used for the minimum pulse time for the Max5841 DAC which specs at min 1.3 uSecs
;...............................................................................
Do_I2C_TimingDelay:
    Return
    MovLW   2
    MovWF   CountInner
    I2C_DelayLoop:
        NOP
        NOP
        DECFSZ  CountInner,F
    GOTO    I2C_DelayLoop
    Return
;...............................................................................

;...............................................................................
Do_I2C_GetACK:
    SDA_1;
    SCL_1;

    BCF   I2C_Flags,Flag_AckFailure
    ;ClrF  BitCounter                 ;TimeOut counter - 255 checks
    MovLW 4
    MovWF BitCounter
WaitForACK:
    I2C_Delay
    IncF  BitCounter,F               ; Increase timeout counter each time ACK is not received
    BTFSC Z
    Goto  ACK_Received_No

    BTFSC SDA_OnB                    ; Test SDA pin. If low then Ack is being sent
    Goto  WaitForACK                 ; Otherwise, continue to wait

ACK_Received_Yes:
    BCF   I2C_Flags,Flag_AckFailure  ; clear flag bit (ACK received)
    Goto  ACK_Done

ACK_Received_No:
    BSF   I2C_Flags,Flag_AckFailure  ; Set flag bit (ACK NOT received)
    
ACK_Done:
    I2C_Delay
    SCL_0;
    I2C_Delay
    Return
;...............................................................................

;...............................................................................
Do_I2C_SendACK:
    SDA_0
    ;I2C_Delay
    SCL_1
    I2C_Delay
    SCL_0
    ;I2C_Delay
    SDA_1
    I2C_Delay
    Return
;...............................................................................

;...............................................................................
;At 40Mhz processor clock, this will generate a W * 100 uSecs software delay
;See the spreadsheet "TSK165 Software Delay Calculator.xls" for calculations
;W Holds iteration count
;...............................................................................
Do_VariableDelay:
    MovWF  CountOuter
    LoopOuter:
        MovLW   99
        MovWF   CountInner
        LoopInner:
            NOP
            NOP
            DECFSZ  CountInner,F
        GOTO    LoopInner

        DECFSZ  CountOuter,F
    GOTO    LoopOuter

    Return
;...............................................................................

;...............................................................................
; Look for two keypressed values, 25ms apart that are the same
;...............................................................................
Do_WaitForStableKeys:
    MovLW   0b11110000              ;Turn On All Rows
    MovWF   PORTA
    LoopForNoKeyPress:       
        MovF    PORTA,W             ; Read Columns
        AndLW   0FH                 ; Only bottom 4-bits are valid
        MovWF   Temp                ; Store First Read in Temp
       
        MovLW   250
        Call    VariableDelay       ; Wait 25ms for key debounce
       
        MovF    PORTA,W             ; Read Columns Again
        AndLW   0FH                 ; Only bottom 4-bits are valid
        SubWF   Temp,W              ; W := Temp - W  If the same as before then result will be zero

        BTFSS   STATUS,F_Z
        GOTO    Do_WaitForStableKeys
        
    MovLW   0b11111111              ;Turn Off All Rows
    MovWF   PORTA
    RETLW   0
;...............................................................................

;...............................................................................
Do_KeyScan:
;...............................................................................
       MovLW 0
       MovWF CurrentKey            ;Start off at CurrentKey = zero

       ;See if any key pressed
       MovLW   0b11110000  ;Turn On All Rows
       MovWF   PORTA
       ComF    PORTA,W     ; Read Not(A) - If any key pressed then lower 4 bits will be not zero 
       AndLW   0FH         ; Clear top bits to zero
       IORLW   0           ;
       ;JZ      EndKeyScan  ; If Zero then no keys pressed to loop
       BTFSC   Z
       GOTO    EndKeyScan
       
       MovLW  250
       Call   VariableDelay ;Wait  25ms to allow debounce
       
       ;Test the Keys on row 0
       MovLW 0b11111110  ;Turn On Row 0
       MovWF PORTA
       ;Test Col 0
       BTFSS PORTA,0      ;Row0 + Col0 = Key1    Skip Return if not 0
       RETLW 0            ;return with the key value set
       INCF  CurrentKey,F
       ;Test Col 1
       BTFSS PORTA,1      ;Row0 + Col1 = Key2 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 2
       BTFSS PORTA,2      ;Row0 + Col2 = Key3
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 3
       BTFSS PORTA,3      ;Row0 + Col3 = KeyC 
       RETLW 0            
       INCF  CurrentKey,F
       
       ;Test the Keys on row 1
       MovLW  0b11111101  ;Turn On Row 1
       MovWF PORTA
       ;Test Col 0
       BTFSS PORTA,0          ;Row1 + Col0 = Key4 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 1
       BTFSS PORTA,1          ;Row1 + Col1 = Key5 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 2
       BTFSS PORTA,2          ;Row1 + Col2 = Key6 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 3
       BTFSS PORTA,3          ;Row1 + Col3 = KeyD 
       RETLW 0            
       INCF  CurrentKey,F
       
       
       ;Test the Keys on row 2
       MovLW  0b11111011  ;Turn On Row 2
       MovWF PORTA
       MovF  PORTA,W      
       ;Test Col 0
       BTFSS PORTA,0          ;Row2 + Col0 = Key7 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 1
       BTFSS PORTA,1          ;Row2 + Col1 = Key8 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 2
       BTFSS PORTA,2          ;Row2 + Col2 = Key9 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 3
       BTFSS PORTA,3          ;Row2 + Col3 = KeyE 
       RETLW 0            
       INCF  CurrentKey,F

       ;Test the Keys on row 3
       MovLW  0b11110111  ;Turn On Row 2
       MovWF PORTA
       MovF  PORTA,W      
       ;Test Col 0
       BTFSS PORTA,0          ;Row3 + Col0 = KeyA 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 1
       BTFSS PORTA,1          ;Row3 + Col1 = Key0 
       RETLW 0
       INCF  CurrentKey,F
       ;Test Col 2
       BTFSS PORTA,2          ;Row3 + Col2 = KeyB 
       RETLW 0            
       INCF  CurrentKey,F
       ;Test Col 3
       BTFSS PORTA,3          ;Row3 + Col3 = KeyF 
       RETLW 0            
       
EndKeyScan:
       MovLW   0FFH
       MovWF   CurrentKey
       RETLW 0
;...............................................................................

;...............................................................................
Do_DisplayKey:
;...............................................................................
    ;COMF    CurrentKey,W
    ;TRIS    PORTA  ;Need to invert because Nanoboard LEDs are on for zero
    RetLW   0
;...............................................................................

;...............................................................................
;    .section FlashSPI, code, inpage       ; inpage allows for interesting optimizations for
;...............................................................................

;...............................................................................
DoSPI_SendAddress24:
    MovF  AddressByte2,W
    Call  SPI_SendReceiveByte   ;Address[23..16]

    MovF  AddressByte1,W
    Call  SPI_SendReceiveByte   ;Address[15..8]

    MovF  AddressByte0,W
    Call  SPI_SendReceiveByte   ;Address[7..0]
    Return
;...............................................................................
    
;................................................................................
DoSPI_SendReceiveByte:
    MovWF    SPI_ByteOut

    MovLW    8                     ; 8 bits to send
    MovWF    BitCounter
    
    MovLW    0
    MovWF    SPI_ByteIn
    
    Loop_ProcessByte:
        RLF      SPI_ByteIn,F         ; Shift last bit read up towards MSB

        ;Set the output bit
        btfsc    SPI_ByteOut,7        ; Check High Bit
        Goto     SPI_BitHigh
    SPI_BitLow:
        SPI_DataOut_Low
        Goto     DoneSendBit
    SPI_BitHigh:
        SPI_DataOut_High
        Goto     DoneSendBit
    DoneSendBit:
    
        ;Read the input bit
        btfsc    SPI_DataIn       ; Check SPI Data bit
        Goto     SPI_ReadBitHigh
    SPI_ReadBitLow:
        BCF      SPI_ByteIn,0
        Goto     SPI_DoneReceiveBit
    SPI_ReadBitHigh:
        BSF      SPI_ByteIn,0
        Goto     SPI_DoneReceiveBit
    SPI_DoneReceiveBit:
    
        SPI_Delay
        SPI_Clock_High            ; pull clock line low
        SPI_Delay
        SPI_Clock_Low             ; pull clock line low
        SPI_Delay

        RLF      SPI_ByteOut,F       ; Shift next bit to send into high position
        DecFSZ   BitCounter,F
    Goto Loop_ProcessByte

    Return
;................................................................................
    
;...............................................................................
; Read bytes Form flash memory until an FF Is found signifiying the end of the file.
; Assume that the address Is already setup In the address locations.
;...............................................................................
DoSPI_FlashMemoryRead:
    MovLW   000H
    MovWF   ValueHi
    
    Call SPI_Open
    
    MovLW 0xF2
    Call SPI_SendReceiveByte                                   ;Address of flash memory to SPI controller
    
    SPI_Mode_Low                                               ;Request SPI Bus from SPI controller

    MovLW cFlashCommand_FastRead
    Call SPI_SendReceiveByte

    Call SPI_SendAddress24

    MovLW 0
    Call SPI_SendReceiveByte
    
   .gen REPEAT
       IncF    SampleCount,F          ;Write this to a port so we can obsere the actual frequency achieved
       MovF    SampleCount,W
       TRIS    PortB
       
       MovLW 0
       Call    SPI_SendReceiveByte   ;Result In SPI_ByteIn
       MovF    SPI_ByteIn,W                        
       MovWF   ValueLo               
       
      ;btfss   OutputOn_Speaker     - Gated (U27) in the hardware side rather than software
       TRIS    PortA
       
       Call    WaitForSampleClock
       
       btfss   OutputOn_DAC
       Call    DAC_Write             
       
       MovF    SPI_ByteIn,W
   .gen UNTIL <NOT> W     
               
    Call SPI_Close;
    
    Return
;...............................................................................
    
;...............................................................................
DoSPI_Open:
    SPI_Mode_High      ;Don't want the SPI bus yet
    SPI_Select_Low     ;Enable Flash Device
    SPI_Clock_Low      ;Ready to clock on next rising edge
    SPI_DataOut_Low    ;Know state for data
    Return
;...............................................................................
    
;...............................................................................
DoSPI_Close:
    SPI_Select_High    ;Disable Flash Device
    SPI_Clock_High     ;Back to initial state
    SPI_DataOut_High   ;Back to initial state
    SPI_Mode_High      ;Disconnect from SPI BUS
    Return
;...............................................................................

;...............................................................................
DoSPI_ReadFlashSignature:
    Call SPI_Open
  
    MovLW 0xF2
    Call SPI_SendReceiveByte                  ;Address of flash memory to SPI controller

    SPI_Mode_Low                              ;Request SPI Bus from SPI controller
    MovLW cFlashCommand_ExitDeepPowerDown
    Call SPI_SendReceiveByte

    MovLW 0
    Call SPI_SendReceiveByte

    MovLW 0
    Call SPI_SendReceiveByte

    MovLW 0
    Call SPI_SendReceiveByte

    MovLW 0
    Call SPI_SendReceiveByte                  ;Reads signature into DataIn
    MovF    SPI_ByteIn,W
    MovWF   ValueLo
    TRIS    PortA

    Call SPI_Close
    Return
;...............................................................................

;...............................................................................
DoSPI_TestFlash:
    SPI_Enable_High
    Call     SPI_Open
    btfsc    SPI_DataIn                ;Check SPI Data bit
    Goto     SkipRead                   ;1 means that the SPI bus Is busy.
    Call     SPI_Close

    Call     SPI_ReadFlashSignature    ;Signature is now in DataIn - should be $12
    Call     SPI_FlashMemoryRead

SkipRead:
    SPI_Enable_Low
    Return
;...............................................................................

;...............................................................................
DoWaitForSampleClock:
    btfss SampleFlag              ; Wait until the flag goes high
    Goto  DoWaitForSampleClock
    BSF   ClearSampleFlag              ; A Pulse here will clear the DFF that  holds the flag
    BCF   ClearSampleFlag              ; The flag will go high again on the next rising edge of the 22.050 Khz clock
    Return
;...............................................................................

;...............................................................................
   .END
;...............................................................................







