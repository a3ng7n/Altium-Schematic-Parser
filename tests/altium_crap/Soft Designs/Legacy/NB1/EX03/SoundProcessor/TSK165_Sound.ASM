;...............................................................................
F_PA0                 .equ   5               ; Workaround to PA0 not working
F_PA1                 .equ   6               ; Workaround to PA1 not working
;...............................................................................

;...............................................................................
; SDA is on port C Bit 0 - Set PORTC bit 0 low to drive low, otherwise it will be pulled high by Pullup
; SCL is on port C Bit 1 - Set PORTC bit 1 low to drive low, otherwise it will be pulled high by Pullup
;...............................................................................

;...............................................................................
.define Return    "RetLW 0"
;...............................................................................

;..............................................................................
cFlashCommand_DataWrite           .equ   002H   ;PP
cFlashCommand_SlowRead            .equ   003H   ;READ
cFlashCommand_FastRead            .equ   00BH   ;FAST_READ
cFlashCommand_WriteEnable         .equ   006H   ;WREN
cFlashCommand_WriteDisable        .equ   004H   ;WRDI
cFlashCommand_ReadStatusRegister  .equ   005H   ;RDSR
cFlashCommand_WriteStatusRegister .equ   001H   ;WRSR
cFlashCommand_EnterDeepPowerDown  .equ   0B9H   ;DP
cFlashCommand_ExitDeepPowerDown   .equ   0ABH   ;RES
cFlashCommand_BulkErase           .equ   0C7H   ;BE
cFlashCommand_EraseSector         .equ   0D8H   ;SE
;..............................................................................

;..............................................................................
cAudio_START                   .equ   080H   ;Start of control byte
cAudio_DAC_Addressed           .equ   040H   ;A1  DAC_Addressed - The control byte configures the DAC
cAudio_ADC_Addressed           .equ   020H   ;A0  ADC_Addressed - The control byte configures the ADC
cAudio_ADC_InputC1             .equ   010H   ;C1  ADC_Input to VDD/2 to measure supply voltage (MAX1102-MAX1103 only)
cAudio_ContinuousConversion    .equ   008H   ;C0  Mode_ContinuousConversion
cAudio_EnableReferenceVoltage  .equ   004H   ;E2  EnableReferenceVoltage - don't care for MAX1104 so zero
cAudio_ADC_Enable              .equ   002H   ;E1
cAudio_DAC_Enable              .equ   001H   ;E0
;..............................................................................

;..............................................................................
cAudio_ContinuousDAC .equ   cAudio_START | cAudio_DAC_Addressed | cAudio_ContinuousConversion | cAudio_DAC_Enable
;..............................................................................

;..............................................................................
KeyPadPort                        .equ   PORTA
ToMultiplier                      .equ   PORTA
StatusBits                        .equ   PORTB
SoundDataOut                      .equ   PORTB
ControlBits                       .equ   PORTC
AudioControlBits                  .equ   PORTD
FromMultiplierLo                  .equ   PORTE
FromMultiplierHi                  .equ   PORTF
;..............................................................................

;..............................................................................
SPI_Clock                         .equ   ControlBits.0
SPI_DataOut                       .equ   ControlBits.1
SPI_Select                        .equ   ControlBits.2
SPI_Enable                        .equ   ControlBits.3
SPI_Mode                          .equ   ControlBits.4
UnusedControlBit1                 .equ   ControlBits.5
ClearKeyPadFlag                   .equ   ControlBits.6
ClearSampleFlag                   .equ   ControlBits.7
;..............................................................................

;..............................................................................
Audio_Clock                       .equ   AudioControlBits.0
Audio_DataOut                     .equ   AudioControlBits.1
Audio_CS                          .equ   AudioControlBits.2
Audio_UnusedControlBit1           .equ   AudioControlBits.3
Audio_UnusedControlBit2           .equ   AudioControlBits.4
Audio_UnusedControlBit3           .equ   AudioControlBits.5
Audio_UnusedControlBit4           .equ   AudioControlBits.6
Audio_UnusedControlBit5           .equ   AudioControlBits.7
;..............................................................................

;..............................................................................
SampleFlag                        .equ   StatusBits.0 ;This is the 11.023 KHz sample clock flag
KeyPadValid                       .equ   StatusBits.1
UnusedStatusBit2                  .equ   StatusBits.2
UnusedStatusBit3                  .equ   StatusBits.3
UnusedStatusBit4                  .equ   StatusBits.4
UnusedStatusBit5                  .equ   StatusBits.5
Audio_DataIn                      .equ   StatusBits.6
SPI_DataIn                        .equ   StatusBits.7
;..............................................................................

;...............................................................................
.define      SPI_Enable_High    "BSF  SPI_Enable"
.define      SPI_Enable_Low     "BCF  SPI_Enable"
.define      SPI_Clock_High     "BSF  SPI_Clock"
.define      SPI_Clock_Low      "BCF  SPI_Clock"
.define      SPI_Mode_High      "BSF  SPI_Mode"
.define      SPI_Mode_Low       "BCF  SPI_Mode"
.define      SPI_Select_High    "BSF  SPI_Select"
.define      SPI_Select_Low     "BCF  SPI_Select"
.define      SPI_DataOut_High   "BSF  SPI_DataOut"
.define      SPI_DataOut_Low    "BCF  SPI_DataOut"
.define      SPI_Delay          "NOP"
;...............................................................................

;...............................................................................
.define      Audio_Clock_High   "BSF  Audio_Clock"
.define      Audio_Clock_Low    "BCF  Audio_Clock"
.define      Audio_DataOut_High "BSF  Audio_DataOut"
.define      Audio_DataOut_Low  "BCF  Audio_DataOut"
.define      Audio_CS_High      "BSF  Audio_CS"
.define      Audio_CS_Low       "BCF  Audio_CS"
.define      Audio_Delay        "Call Delay_5Mhz"  ;Delays for 0.2uS
.define      KeyPadFlag_High    "BSF  ClearKeyPadFlag"
.define      KeyPadFlag_Low     "BCF  ClearKeyPadFlag"
.define      SampleFlag_High    "BSF  ClearSampleFlag"
.define      SampleFlag_Low     "BCF  ClearSampleFlag"
;...............................................................................

;...............................................................................
    .section data, data
CountInner      .dsb    1      ;0BH
CountOuter      .dsb    1      ;0CH
CurrentKey      .dsb    1      ;0DH
Temp            .dsb    1      ;0EH
BitCounter      .dsb    1      ;0FH
ValueLo         .dsb    1      ;10H
ValueHi         .dsb    1      ;11H
Instruction     .dsb    1      ;12H

AddressByte2    .dsb    1      ;13H     MSB Of 24 bit address
AddressByte1    .dsb    1      ;14H
AddressByte0    .dsb    1      ;15H     LSB Of 24 bit address

Flash_ByteIn    .dsb    1      ;16H
Flash_ByteOut   .dsb    1      ;17H

Audio_ByteIn    .dsb    1      ;18H
Audio_ByteOut   .dsb    1      ;1FH
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
;...............................................................................

;...............................................................................
Flash_Open:               Goto DoFlash_Open
Flash_Close:              Goto DoFlash_Close
Flash_SendReceiveByte:    Goto DoFlash_SendReceiveByte
Flash_SendAddress24:      Goto DoFlash_SendAddress24
Flash_MemoryRead:         Goto DoFlash_MemoryRead
Flash_ReadSignature:      Goto DoFlash_ReadSignature
Flash_RunTest:            Goto DoFlash_RunTest
WaitForSampleClock:       Goto DoWaitForSampleClock
;...............................................................................

;...............................................................................
Audio_Open:               Goto DoAudio_Open
Audio_Close:              Goto DoAudio_Close
Audio_SendReceiveByte:    Goto DoAudio_SendReceiveByte
Delay_5Mhz:               Goto DoDelay_5Mhz
;...............................................................................

;...............................................................................
HandleKeyPress_TopLevel:
    MovF  KeyPadPort,W
    AndLW 0FH
    KeyPadFlag_High
    KeyPadFlag_Low
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
MainProc:
    BCF     STATUS, F_PA0           ; Select Register page 0
    BCF     STATUS, F_PA1
    
    MOVLW   0b00011000              ; Page 0
    MOVWF   STATUS                  ; Init STATUS

    GOTO    MainLoop
;...............................................................................

;...............................................................................
WaitForAnyKeyPress:
    btfss   KeyPadValid
    Goto    WaitForAnyKeyPress
    Return
;...............................................................................

;...............................................................................
MainLoop:
    Call    WaitForAnyKeyPress       ; Stable No-Key will also Return
    Goto    HandleKeyPress_TopLevel
FinishedProcess:
    Goto    MainLoop
;...............................................................................

;...............................................................................
HandleKey_Null:
    Goto  FinishedProcess
;...............................................................................
                        
;...............................................................................
HandleKey_6:
    MovLW   000H
    MovWF   ValueHi
    MovLW   000H
    MovWF   ValueLo
    
    MovLW   0
    ;MovWF   DacChannel
    ;MovLW   Max5841_Address_Write
    ;MovWF   I2C_Address
RampLoop:
    Call    Flash_RunTest
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
;    .section FlashSPI, code, inpage       ; inpage allows for interesting optimizations for
;...............................................................................

;...............................................................................
DoFlash_SendAddress24:
    MovF  AddressByte2,W
    Call  Flash_SendReceiveByte   ;Address[23..16]

    MovF  AddressByte1,W
    Call  Flash_SendReceiveByte   ;Address[15..8]

    MovF  AddressByte0,W
    Call  Flash_SendReceiveByte   ;Address[7..0]
    Return
;...............................................................................

;................................................................................
DoFlash_SendReceiveByte:
    MovWF    Flash_ByteOut

    MovLW    8                     ; 8 bits to send
    MovWF    BitCounter

    MovLW    0
    MovWF    Flash_ByteIn

    Loop_ProcessByte:
        RLF      Flash_ByteIn,F         ; Shift last bit read up towards MSB

        ;Set the output bit
        btfsc    Flash_ByteOut,7        ; Check High Bit
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
        BCF      Flash_ByteIn,0
        Goto     SPI_DoneReceiveBit
    SPI_ReadBitHigh:
        BSF      Flash_ByteIn,0
        Goto     SPI_DoneReceiveBit
    SPI_DoneReceiveBit:

        SPI_Delay
        SPI_Clock_High            ; pull clock line low
        SPI_Delay
        SPI_Clock_Low             ; pull clock line low
        SPI_Delay

        RLF      Flash_ByteOut,F       ; Shift next bit to send into high position
        DecFSZ   BitCounter,F
    Goto Loop_ProcessByte

    Return
;................................................................................

;...............................................................................
; Read bytes Form flash memory until an FF Is found signifiying the end of the file.
; Assume that the address Is already setup In the address locations.
;...............................................................................
DoFlash_MemoryRead:
    MovLW   000H
    MovWF   ValueHi

    Call Flash_Open

    MovLW 0xF2
    Call Flash_SendReceiveByte                                 ;Address of flash memory to SPI controller

    SPI_Mode_Low                                               ;Request SPI Bus from SPI controller

    MovLW cFlashCommand_FastRead
    Call Flash_SendReceiveByte

    Call Flash_SendAddress24

    MovLW 0
    Call Flash_SendReceiveByte

   .gen REPEAT
       MovLW 0
       Call    Flash_SendReceiveByte   ;Result In Flash_ByteIn
       MovF    Flash_ByteIn,W
       MovWF   ValueLo
       MovWF   ToMultiplier            ; Write the Sound data to one of the inputs of the harware multiplier
                                       ; Multiplies by the value from DIP Switch
       NOP
       NOP
       NOP                             ; Give the multiplier time to settle
       NOP
       NOP

       MovF    FromMultiplierHi,W      ; Get the high byte from the multiplier output
       MovWF   SoundDataOut            ; Send it to the LAX and LEDs
       Call    Audio_SendReceiveByte   ; Send it to the CODEC DAC

       Call    WaitForSampleClock

       ;btfss   OutputOn_DAC
       ;Call    DAC_Write

       MovF    Flash_ByteIn,W
   .gen UNTIL <NOT> W

    Call Flash_Close;

    Return
;...............................................................................
    
;...............................................................................
DoFlash_Open:
    SPI_Mode_High      ;Don't want the SPI bus yet
    SPI_Select_Low     ;Enable Flash Device
    SPI_Clock_Low      ;Ready to clock on next rising edge
    SPI_DataOut_Low    ;Know state for data
    Return
;...............................................................................

;...............................................................................
DoFlash_Close:
    SPI_Select_High    ;Disable Flash Device
    SPI_Clock_High     ;Back to initial state
    SPI_DataOut_High   ;Back to initial state
    SPI_Mode_High      ;Disconnect from SPI BUS
    Return
;...............................................................................

;...............................................................................
DoFlash_ReadSignature:
    Call Flash_Open
  
    MovLW 0xF2
    Call Flash_SendReceiveByte                  ;Address of flash memory to SPI controller

    SPI_Mode_Low                              ;Request SPI Bus from SPI controller
    MovLW cFlashCommand_ExitDeepPowerDown
    Call Flash_SendReceiveByte

    MovLW 0
    Call Flash_SendReceiveByte

    MovLW 0
    Call Flash_SendReceiveByte

    MovLW 0
    Call Flash_SendReceiveByte

    MovLW 0
    Call Flash_SendReceiveByte                  ;Reads signature into DataIn
    MovF    Flash_ByteIn,W
    MovWF   ValueLo
    MovWF   SoundDataOut

    Call Flash_Close
    Return
;...............................................................................

;...............................................................................
DoFlash_RunTest:
    SPI_Enable_High
    Call     Flash_Open
    btfsc    SPI_DataIn                ;Check SPI Data bit
    Goto     SkipRead                  ;1 means that the SPI bus Is busy.
    Call     Flash_Close

    Call     Flash_ReadSignature    ;Signature is now in DataIn - should be $12
    Call     Audio_Open
    Call     Flash_MemoryRead
    Call     Audio_Close

SkipRead:
    SPI_Enable_Low
    Return
;...............................................................................

;...............................................................................
DoWaitForSampleClock:
    btfss SampleFlag              ; Wait until the flag goes high
    Goto  DoWaitForSampleClock
    SampleFlag_High               ; A Pulse here will clear the DFF that  holds the flag
    SampleFlag_Low                ; The flag will go high again on the next rising edge of the 22.050 Khz clock
    Return
;...............................................................................

;...............................................................................
DoAudio_Open:
    Audio_CS_High       ;Disable Codec
    Audio_Clock_Low     ;Don't want the SPI bus yet
    Audio_DataOut_Low   ;Don't want the SPI bus yet
    Audio_CS_Low        ;Enable Codec
    MovLW cAudio_ContinuousDAC
    Call  Audio_SendReceiveByte
    Return
;...............................................................................

;...............................................................................
DoAudio_Close:
    Audio_CS_High       ;Disable Codec
    Audio_Clock_Low     ;Don't want the SPI bus yet
    Audio_DataOut_Low   ;Don't want the SPI bus yet
    Return
;...............................................................................

;................................................................................
DoAudio_SendReceiveByte:
    MovWF    Audio_ByteOut

    MovLW    8                     ; 8 bits to send
    MovWF    BitCounter

    MovLW    0
    MovWF    Audio_ByteIn

    Loop_ProcessAudioByte:
        RLF      Audio_ByteIn,F         ; Shift last bit read up towards MSB

        ;Set the output bit
        btfsc    Audio_ByteOut,7        ; Check High Bit
        Goto     Audio_WriteBitHigh
    Audio_WriteBitLow:
        Audio_DataOut_Low
        Goto     DoneSendAudioBit
    Audio_WriteBitHigh:
        Audio_DataOut_High
        Goto     DoneSendAudioBit
    DoneSendAudioBit:

        ;Read the input bit
        btfsc    Audio_DataIn       ; Check SPI Data bit
        Goto     Audio_ReadBitHigh
    Audio_ReadBitLow:
        BCF      Audio_ByteIn,0
        Goto     Audio_DoneReceiveBit
    Audio_ReadBitHigh:
        BSF      Audio_ByteIn,0
        Goto     Audio_DoneReceiveBit
    Audio_DoneReceiveBit:

        Audio_Delay
        Audio_Clock_High
        Audio_Delay
        Audio_Clock_Low
        Audio_Delay

        RLF      Audio_ByteOut,F       ; Shift next bit to send into high position
        DecFSZ   BitCounter,F
    Goto Loop_ProcessAudioByte

    Return
;................................................................................

;................................................................................
DoDelay_5Mhz:
; Max clock for Max1104 is 6 MHz
; Assuming the processor clock is running at 100Mhz (best case) then
; Call + RETW + NOP + NOP  => 5 instruction cycles => 20 clock cyles => 0.2uS => 5MHz
    Nop
    Nop
    Return
;................................................................................

;...............................................................................
   .END
;...............................................................................







