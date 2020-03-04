.Include "DAC_Defines.Asm"

;...............................................................................
.define   SDA_0   "BCF  SDA_Write"
.define   SDA_1   "BSF  SDA_Write"
.define   SCL_0   "BCF  SCL_Write"
.define   SCL_1   "BSF  SCL_Write"
;...............................................................................

;...............................................................................
    .Section Code_I2C, Code, InPage, at(0x200)
;...............................................................................

;...............................................................................
;Jump table for Calls
;Call instruction only allows 8 bit address so use a jump table
;Call the Jump table which does a goto the address
;...............................................................................
I2C_TimingDelay:          Goto Do_I2C_TimingDelay
I2C_GetACK:               Goto Do_I2C_GetACK
I2C_SendACK:              Goto Do_I2C_SendACK
I2C_SendNotACK:           Goto Do_I2C_SendNotACK
I2C_Initialize:           Goto Do_I2C_Initialize        
I2C_SendByte:             Goto Do_I2C_SendByte
I2C_ReceiveByte:          Goto Do_I2C_ReceiveByte
I2C_SendStart:            Goto Do_I2C_SendStart
I2C_SendStop:             Goto Do_I2C_SendStop
DAC_Initialize:           Goto Do_DAC_Initialize
DAC_SendReadSequence:     Goto Do_DAC_SendReadSequence  
DAC_SendWriteSequence:    Goto Do_DAC_SendWriteSequence 
DAC_Write:                Goto Do_DAC_Write             
DAC_Read:                 Goto Do_DAC_Read     
ADC_WriteSetup:           Goto Do_ADC_WriteSetup            
ADC_WriteConfig:          Goto Do_ADC_WriteConfig             
ADC_Read:                 Goto Do_ADC_Read              
;...............................................................................

;...............................................................................
    .global I2C_Initialize   
    .global I2C_SendByte     
    .global I2C_ReceiveByte  
    .global I2C_SendStart    
    .global I2C_SendStop     
    .global DAC_Initialize
    .global DAC_SendReadSequence
    .global DAC_SendWriteSequence
    .global DAC_Write
    .global DAC_Read
    .global ADC_WriteSetup
    .global ADC_WriteConfig
    .global ADC_Read
;...............................................................................

;...............................................................................
DAC_GetWriteCommandForChannel:
    MovF    DacChannel,W
    AndLW   03H;
    AddWF   PCL,F
    .db Max5841_LoadA_UpdateAll  ;These are RetLW instructions - so will return will value in W
    .db Max5841_LoadB_UpdateAll
    .db Max5841_LoadC_UpdateAll
    .db Max5841_LoadD_UpdateAll
;...............................................................................

;...............................................................................
DAC_GetReadCommandForChannel:
    MovF    DacChannel,W
    AndLW   03H;
    AddWF   PCL,F
    .db Max5841_ReadA   ;These are RetLW instructions - so will return will value in W
    .db Max5841_ReadB
    .db Max5841_ReadC
    .db Max5841_ReadD
;...............................................................................

;...............................................................................
ADC_GetReadCommandForChannel:
    MovF    AdcChannel,W
    AndLW   03H;
    AddWF   PCL,F
    .db Max1037_Config_Channel0   ;These are RetLW instructions - so will return will value in W
    .db Max1037_Config_Channel1 
    .db Max1037_Config_Channel2 
    .db Max1037_Config_Channel3 
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
Do_I2C_SendByte:
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
Do_I2C_ReceiveByte:
    SDA_1                          ; {Release Data Line}
    I2C_Delay
    
    MovLW    0
    MovWF    DataByte
    MovLW    8                     ; 8 bits to read
    MovWF    BitCounter

    Loop_ReceiveByte:
        RLF      DataByte,F        ; Shift last bit read up towards MSB
        SCL_1                      ; High Before read
        I2C_Delay
        
        btfsc    SDA_Read          ; Check SDA Bit
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
Do_I2C_SendStart:
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
Do_I2C_SendStop:
    SDA_0
    ;I2C_Delay
    SCL_1
    I2C_Delay
    SDA_1
    I2C_Delay
    Return
;...............................................................................

;...............................................................................
Do_I2C_Initialize:
    SDA_1
    I2C_Delay
    SCL_0
    I2C_Delay
    
    GCall I2C_SendStop
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

    BTFSC SDA_Read                   ; Test SDA pin. If low then Ack is being sent
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
Do_I2C_SendNotACK:
    SDA_1
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
Do_DAC_Initialize:
    MovLW   Max5841_Address_Write
    MovWF   I2C_Address
    
    MovLW   Max5841_ExtendedCommandMode
    MovWF   CommandByte1
    
    MovLW   03CH
    MovWF   CommandByte2

    GCall   DAC_SendWriteSequence
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

    GCall  DAC_GetWriteCommandForChannel

    IORWF CommandByte1,F     ;Add in the command bits

    GCall DAC_SendWriteSequence
    Return
;...............................................................................
    
;...............................................................................
Do_DAC_Read:
    MovLW 0              ;Just so we can see if it changed
    MovWF ValueHi
    MovWF ValueLo
    
    GCall  DAC_GetReadCommandForChannel
    MovWF  CommandByte1
    
    GCall  DAC_SendReadSequence
    
    MovF   CommandByte1,W
    MovWF  ValueHi

    MovF   CommandByte2,W
    MovWF  ValueLo

    RRF    ValueHi,F          ; Lo Bit into Carry  
    RRF    ValueLo,F          ; Carry into high bit 
    RRF    ValueHi,F          ; Lo Bit into Carry  
    RRF    ValueLo,F          ; Carry into high bit 
    
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
    GCall   I2C_Initialize
    GCall   I2C_SendStart

    BCF     I2C_Address,0          ;Zero in lowest bit means write
    MovF    I2C_Address,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Write

    MovF    CommandByte1,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Write

    MovF    CommandByte2,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Write

DAC_Error_Write:
    GCall I2C_SendStop;
    
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
    GCall   I2C_Initialize
    GCall   I2C_SendStart
                                    ;Send Address
    BCF     I2C_Address,0           ;Zero in lowest bit means write
    MovF    I2C_Address,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Read

                                    ;Send Command Byte
    MovF    CommandByte1,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Read

    GCall   I2C_SendStart           ; Restart indicates a direction change
    
                                    ; Send Address Again - This time for read
    BSF     I2C_Address,0           ; One in lowest bit means read
    MovF    I2C_Address,W
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    DAC_Error_Read
    
    GCall   I2C_ReceiveByte
    MovF    DataByte,W  
    MovWF   CommandByte1
    GCall   I2C_SendACK

    GCall   I2C_ReceiveByte
    MovF    DataByte,W  
    MovWF   CommandByte2
    GCall   I2C_SendACK
    
DAC_Error_Read:
    GCall   I2C_SendStop;
    
    Return
;...............................................................................

;...............................................................................
Do_ADC_WriteSetup:
;...............................................................................
    GCall   I2C_Initialize
    GCall   I2C_SendStart
    
    MovLW   Max1037_Address_Write
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    ADC_SetupError

    MovLW   Max1037_Default_Setup
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    ADC_SetupError

ADC_SetupError:
    GCall   I2C_SendStop;
    Return
;...............................................................................

;...............................................................................
Do_ADC_WriteConfig:
;...............................................................................
    MovWF   AdcChannel                  ;Save channel number from W
    GCall   I2C_Initialize
    GCall   I2C_SendStart
    
    MovLW   Max1037_Address_Write
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    ADC_ConfigError

    GCall   ADC_GetReadCommandForChannel ;Set W with the config byte
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    ADC_ConfigError

ADC_ConfigError:
    GCall   I2C_SendStop;
    Return
;...............................................................................

;...............................................................................
Do_ADC_Read:
;...............................................................................
    GCall   I2C_Initialize
    GCall   I2C_SendStart
    
    MovLW   Max1037_Address_Read
    GCall   I2C_SendByte
    GCall   I2C_GetACK
    btfsc   I2C_Flags,Flag_AckFailure
    Goto    ADC_ReadError

    SCL_1                      ;release SCL
LoopWaitForSCLHigh:            ;The ADC will now hold SCL low until its converted, so watch that line
    btfss   SCL_Read
    goto    LoopWaitForSCLHigh
    
    GCall   I2C_ReceiveByte
    MovF    DataByte,W  
    MovWF   CommandByte1
    MovWF   ValueLo
    CLRF    ValueHi
    
    GCall   I2C_SendNotACK     ;Tell the ADC that no more data is required

ADC_ReadError:
    GCall   I2C_SendStop;
    
    BCF     C                  ;The ADC is 8 bits and the DAC is 10 bits so scale the ADC value up by 2 bits
    RLF     ValueLo,F
    RLF     ValueHi,F
    RLF     ValueLo,F
    RLF     ValueHi,F
    Return
;...............................................................................

    
END        
