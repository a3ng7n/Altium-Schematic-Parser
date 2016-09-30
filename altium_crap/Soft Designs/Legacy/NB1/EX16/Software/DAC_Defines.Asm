;...............................................................................
Flag_AckFailure       .equ   0
;...............................................................................

;...............................................................................
F_Z                   .equ   2               ; Workaround to Z not working
F_PA0                 .equ   5               ; Workaround to PA0 not working
F_PA1                 .equ   6               ; Workaround to PA1 not working
;...............................................................................

;...............................................................................
PortAsInput           .equ   0FFH
PortAsOutput          .equ   000H
;...............................................................................

;...............................................................................
Key_Left              .equ   04H
Key_Right             .equ   06H
Key_Up                .equ   01H
Key_Down              .equ   09H
Key_Enter             .equ   0CH
Key_Cancel            .equ   0FH
;...............................................................................

;...............................................................................
; SDA is on port C Bit 0 - Set PORTC bit 0 low to drive low, otherwise it will be pulled high by Pullup
; SCL is on port C Bit 1 - Set PORTC bit 1 low to drive low, otherwise it will be pulled high by Pullup
;...............................................................................

;...............................................................................
LCD_DATA              .equ   PORTA       ; LCD data lines interface (bidirectional)
LCD_BUSY              .equ   LCD_DATA.7  ; high if BUSY
;...............................................................................

;..............................................................................
KeyPadPort            .equ   PORTB
KeyValueOnLEDs        .equ   PORTB       ; Out on the TRIS Lines used as ouputs
;..............................................................................
 
;..............................................................................
LCD_RW                .equ   PORTC.0     ; LCD Read/Write control line
LCD_EN                .equ   PORTC.1     ; LCD Enable control line
LCD_RS                .equ   PORTC.2     ; LCD Register-Select control line

SDA_Write             .equ   PORTC.4  
SCL_Write             .equ   PORTC.5  
ClearKey              .equ   PORTC.6 
ClearRateFlag         .equ   PORTC.7

SDA_Read              .equ   PORTB.4
SCL_Read              .equ   PORTB.5
ValidKey              .equ   PORTB.6
RateFlag              .equ   PORTB.7

RatePort              .equ   PORTC       ;Rate is actually set on the TRIS output of PortC
;..............................................................................

;...............................................................................
.define      ClearKey_0        "BCF  ClearKey"
.define      ClearKey_1        "BSF  ClearKey"

.define      RateFlag_0        "BCF  ClearRateFlag"
.define      RateFlag_1        "BSF  ClearRateFlag"
;...............................................................................

;...............................................................................
;For the MAX5841L use these
;Max5841_Address_Write       .equ 0b 0111 101 0
;Max5841_Address_Read        .equ 0b 0111 101 1

;For the MAX5841M use these
 Max5841_Address_Write       .equ 0b10111010
 Max5841_Address_Read        .equ 0b10111011
;...............................................................................

;...............................................................................
Max1037_Address_Write       .equ 0b11001000
Max1037_Address_Read        .equ 0b11001001
;...............................................................................

;...............................................................................
;Max1037 Setup Register Format
;-----------------------------
;B7 = RegisterBit 1 => Setup Byte    0 => Configuration Byte
;B6 = Sel2        | 
;B5 = Sel1        | Ref voltage setup
;B4 = Sel0        |
;B3 = CLK Bit     1 => External Clock 0 => Internal Clock
;B2 = BIP/UNI     1 => Bipolar       0 => Unipolar
;B1 = Reset       1 => No Action     0 => Reset configuration register to default
;B0 = X           Don't Care

Max1037_Setup               .equ 0b10000000
Max1037_Clock_External      .equ 0b00001000
Max1037_Clock_Internal      .equ 0b00000000
Max1037_Mode_Bipolar        .equ 0b00000100
Max1037_Mode_UniPolar       .equ 0b00000000
Max1037_NoReset             .equ 0b00000010
Max1037_ForceResetConfig    .equ 0b00000000
                                              ; Ref Volts    AIN/REF              Internal Reference State 
                                              ; ----------------------------------------------------------
Max1037_VRef_VDD            .equ 0b00000000   ; VDD          Analog Input         Always Off
Max1037_VRef_External       .equ 0b00100000   ; External     Reference Input      Always Off 
Max1037_VRef_AutoShutdown   .equ 0b01000000   ; Internal     Analog Input         Auto Shutdown
Max1037_VRef_AlwaysOn       .equ 0b01010000   ; Internal     Analog Input         Always On 
Max1037_VRef_Generate       .equ 0b01110000   ; Internal     Reference Output     Always On

;Max1037 Config Register Format
;------------------------------
;B7 = RegisterBit 1 => Setup Byte    0 => Configuration Byte
;B6 = Scan1       | Scan Select Bits 
;B5 = Scan0       | 
;B4 = CS3         | Channel select bits - only CS0 and CS1 are used on the 4-Channel MAX1037
;B3 = CS2         | 
;B2 = CS1         |
;B1 = CS0         | 
;B0 = SGL/DIF     1 => Single-Ended  0 => Pseudo-Differential

Max1037_Config              .equ 0b01100000
Max1037_Channel0            .equ 0b00000000
Max1037_Channel1            .equ 0b00000010
Max1037_Channel2            .equ 0b00000100
Max1037_Channel3            .equ 0b00000110

Max1037_Mode_SingleEnded    .equ 0b00000001
Max1037_Mode_Differential   .equ 0b00000000
;...............................................................................

;...............................................................................
Max1037_Default_Setup       .equ Max1037_Setup  | Max1037_Clock_Internal | Max1037_Mode_UniPolar | Max1037_VRef_Generate | Max1037_NoReset
Max1037_Default_Config      .equ Max1037_Config | Max1037_Mode_SingleEnded
;...............................................................................

;...............................................................................
Max1037_Config_Channel0     .equ Max1037_Default_Config | Max1037_Channel0
Max1037_Config_Channel1     .equ Max1037_Default_Config | Max1037_Channel1
Max1037_Config_Channel2     .equ Max1037_Default_Config | Max1037_Channel2
Max1037_Config_Channel3     .equ Max1037_Default_Config | Max1037_Channel3
;...............................................................................

;...............................................................................
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
.define      Return            "RetLW 0"
.define      I2C_Delay         "GCall I2C_TimingDelay"
;...............................................................................

;...............................................................................
TableIndex_Startup1         .equ 1
TableIndex_Startup2         .equ 2
TableIndex_Sine50Hz         .equ 3
TableIndex_Ramp50Hz         .equ 4
TableIndex_Ramp1KHz         .equ 5
TableIndex_Triangle50Hz     .equ 6
TableIndex_GeneratingWave   .equ 7
;...............................................................................

