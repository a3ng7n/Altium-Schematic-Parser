;...............................................................................
PortAsInput             .equ    0xFF
PortAsOutput            .equ    0x0

LCD_DATA_PORT           .equ    PORTA       ; LCD data lines interface (bidirectional)
LCD_CTRL_PORT           .equ    PORTC       ; LCD control lines interface

; LCD_DATA_PORT feedback:
LCD_BUSY                .equ    LCD_CTRL_PORT.0  ; high if BUSY

LCD_LINE_ON             .equ    0xEF
LCD_LINE_OFF            .equ    0x10
LCD_STROBE_ON           .equ    0xDF
LCD_STROBE_OFF          .equ    0x20

CAN_CTRL_PORT           .equ    PORTB
CAN_ADDRESS_PORT        .equ    PORTA
CAN_COMMAND_PORT        .equ    PORTA
CAN_ADDRESS_LATCH_ENABLE_BIT .equ   4 ; bit 4 - driven low to latch the address being sent to the CAN
CAN_SELECT_BIT               .equ   5 ; bit 5 - driven high to select the CAN chip
CAN_WRITE_BIT                .equ   6 ; bit 6 - drive high to write to the CAN registers
CAN_READ_BIT                 .equ   7 ; bit 7 - drive high to read from the CAN registers
CAN_RESET               .equ    0X1F

MODE_REGISTER             .equ  0x00
COMMAND_REGISTER          .equ  0x01
STATUS_REGISTER           .equ  0x02
INTERRUPT_REGISTER        .equ  0x03
INTERRUPT_ENABLE_REGISTER .equ  0x04
BUS_TIMING_REGISTER_0     .equ  0x06
BUS_TIMING_REGISTER_1     .equ  0x07
OUTPUT_CONTROL_REGISTER   .equ  0x08
ACCEPTANCE_CODE1_REGISTER   .equ  0x11
ACCEPTANCE_MASK0_REGISTER   .equ  0x14
ACCEPTANCE_MASK1_REGISTER   .equ  0x15
ACCEPTANCE_MASK2_REGISTER   .equ  0x16
ACCEPTANCE_MASK3_REGISTER   .equ  0x17
CLOCK_DIVIDER_REGISTER    .equ  0x1F
TRANSMIT_BUFFER_REGISTER_ACR0 .equ 0x10
TRANSMIT_BUFFER_REGISTER_ACR1 .equ 0x11
TRANSMIT_BUFFER_REGISTER_ACR2 .equ 0x12
TRANSMIT_BUFFER_REGISTER_ACR3 .equ 0x13
TRANSMIT_BUFFER_REGISTER_AMR0 .equ 0x14
TRANSMIT_BUFFER_REGISTER_AMR1 .equ 0x15
TRANSMIT_BUFFER_REGISTER_AMR2 .equ 0x16
TRANSMIT_BUFFER_REGISTER_AMR3 .equ 0x17
TRANSMIT_BUFFER_REGISTER_0    .equ 0x18
TRANSMIT_BUFFER_REGISTER_1    .equ 0x19
TRANSMIT_BUFFER_REGISTER_2    .equ 0x1A

RECEIVE_BUFFER_REGISTER_AMR0  .equ 0x14

Bit0                    .equ    0
Bit2                    .equ    2

;...............................................................................
; MACRO Definitions
;...............................................................................
SetCANReg .MACRO RegisterAddress, RegisterValue
        MOVLW RegisterAddress
        MOVWF CANRegister
        MOVLW RegisterValue
        MOVWF CANCommand
        CALL CANSetRegister
    .ENDM
          
SetCANRegSameValue .MACRO RegisterAddress
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANSetRegister                       
    .ENDM
        
SetCANRegFromMemory .MACRO RegisterAddress, MemoryLocation
        MOVLW RegisterAddress
        MOVWF CANRegister
        MOVF MemoryLocation, W
        MOVWF CANCommand
        CALL CANSetRegister        
    .ENDM                

GetCANReg .MACRO RegisterAddress
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANGetRegister
    .ENDM
    
Delay1ms .MACRO
        MOVLW 10
        MOVWF CountOuter
        CALL Delay100us
    .ENDM

DisplayLCDMessage .MACRO MessageString, DigitToDisplay
        .gen FOR LinePosition = #0 TO #16
            Delay1ms
            MOVF LinePosition,W
            CALL MessageString ; get the byte at offset W
            .gen IF <NOT>W
                .gen BREAK
            .gen ENDIF            
            MOVWF CharacterToDisplay
            CALL LCDSendCharacter
        .gen ENDFOR
        
        MOVF DigitToDisplay, W
        IORLW 0x30 ; turn the number into an ASCII digit
        MOVWF CharacterToDisplay
        CALL LCDSendCharacter                
    .ENDM    
;...............................................................................
    
;...............................................................................
    .section data, data
Temp               .dsb 1 ; Temporary variable

MainCount          .dsb 1 ; Contains the ASCII code of the current character to output
LoopCounter        .dsb 1 ; for use anywhere a loop is needed
LoopCounterInner   .dsb 1 ; for use anywhere a loop is needed

; arguments for the character writing functions
CharacterToDisplay .dsb 1
LinePosition       .dsb 1
FirstLine          .dsb 1

; arguments for CAN functions
CANRegister        .dsb 1
CANCommand         .dsb 1
CANReceivedData    .dsb 1

; Storage needed by Delay function
CountInner         .dsb 1
CountOuter         .dsb 1


;...............................................................................
    .section Entry, Code, at(0)    
__start:                                    ; Entrypoint of hardware
    GJMP Main

;...............................................................................
;...............................................................................
; Jump Table
; CALL instruction only allows 8 bit address so use a jump table
; CALL the Jump table which does a GOTO the address
;...............................................................................
Delay1s:                        GOTO    DoDelay1s
Delay100us:                     GOTO    DoDelay100us
Delay2us:                       GOTO    DoDelay2us

CANInit:                        GOTO    DoCANInit
SetFrameToTransmit:             GOTO    DoSetFrameToTransmit
ReceiveFrame:                   GOTO    DoReceiveFrame
TransmitFrame:                  GOTO    DoTransmitFrame

CANGetRegister:                 GOTO    DoCANGetRegister
CANSetRegister:                 GOTO    DoCANSetRegister

LCDClearScreen:                 GOTO    DoLCDClearScreen
LCDSendCharacter:               GOTO    DoLCDSendCharacter
LCDWaitTillReady:               GOTO    DoLCDWaitTillReady
LCDClearScreenCore:             GOTO    DoLCDClearScreenCore
;...............................................................................

;...............................................................................
; String Lookup tables:
;...............................................................................
TransmitMessage:
    ADDWF   PCL,f
    RETLW   'Transmit: ',0

ReceiveMessage:
    ADDWF   PCL,f
    RETLW   'Receive:  ',0
;...............................................................................

;...............................................................................
Init:
    MOVLW 9
    MOVWF MainCount
    
    MOVLW 'X'
    MOVWF CANReceivedData
    
    MOVLW PortAsOutput
    TRIS LCD_CTRL_PORT
    MOVLW 0xFF
    MOVWF LCD_CTRL_PORT
    
    CALL LCDClearScreen
    CALL CANInit

    RETLW 0
;...............................................................................

;...............................................................................    
Main:
    CALL Init
MainLoop:
    CALL SetFrameToTransmit
    CALL TransmitFrame
    CALL ReceiveFrame
 
    ; display transmit message
    MOVLW 0x01
    MOVWF FirstLine
    DisplayLCDMessage TransmitMessage, MainCount
    
    ; display the last received character
    MOVLW 0x00
    MOVWF FirstLine
    DisplayLCDMessage ReceiveMessage, CANReceivedData
    
    DECFSZ MainCount, 1
    GOTO Delay
    MOVLW 9
    MOVWF MainCount  
              
Delay:
    CALL Delay1s
    
    GOTO MainLoop
;...............................................................................

;...............................................................................
; Initialise the CAN controller
;...............................................................................
DoCANInit:
    
    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    MOVLW CAN_RESET
    MOVWF CAN_CTRL_PORT ; reset all control lines

CI_RPT_Test_MODE0:
        SetCANReg MODE_REGISTER, 0x01
        GetCANReg MODE_REGISTER
        BTFSS CANCommand, Bit0
        GOTO CI_RPT_Test_MODE0

    SetCANReg ACCEPTANCE_MASK0_REGISTER, 0xFF
    SetCANRegSameValue ACCEPTANCE_MASK1_REGISTER
    SetCANRegSameValue ACCEPTANCE_MASK2_REGISTER
    SetCANRegSameValue ACCEPTANCE_MASK3_REGISTER

    SetCANReg CLOCK_DIVIDER_REGISTER, 0xC0
    SetCANReg BUS_TIMING_REGISTER_0, 0x09  ; 40 MHz 125kb/s
    SetCANReg BUS_TIMING_REGISTER_1, 0x1C  ; 40 MHz 125kb/s
    SetCANReg OUTPUT_CONTROL_REGISTER, 0x1A

    SetCANReg MODE_REGISTER, 0x00

    SetCANReg CLOCK_DIVIDER_REGISTER, 0x8F

    SetCANReg ACCEPTANCE_MASK0_REGISTER, 0xFF
    SetCANRegSameValue ACCEPTANCE_MASK1_REGISTER
    SetCANRegSameValue ACCEPTANCE_MASK2_REGISTER
    SetCANRegSameValue ACCEPTANCE_MASK3_REGISTER

    SetCANReg INTERRUPT_ENABLE_REGISTER, 0x01

CI_RPT_Test_Not_MODE0:

    SetCANReg MODE_REGISTER, 0x00
        
    GetCANReg MODE_REGISTER
    BTFSC CANCommand, Bit0
    GOTO CI_RPT_Test_Not_MODE0

    RETLW 0
;...............................................................................

;...............................................................................
; Set the CAN to transmit frame mode
;...............................................................................
DoSetFrameToTransmit:

SFTT_RPT_Test_Not_Can_Tx:
    GetCANReg STATUS_REGISTER
    BTFSS CANCommand, Bit2 ; check the Transmit Buffer status bit
    GOTO SFTT_RPT_Test_Not_Can_Tx

    SetCANReg TRANSMIT_BUFFER_REGISTER_ACR0, 0x03
    SetCANReg TRANSMIT_BUFFER_REGISTER_ACR1, 0x55
    SetCANReg TRANSMIT_BUFFER_REGISTER_ACR2, 0x00
    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_ACR3
    ; now set the transmit buffer to hold the value to output
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_AMR0, MainCount
    SetCANReg TRANSMIT_BUFFER_REGISTER_AMR1, 0x00
    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_AMR2
    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_AMR3

    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_0
    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_1
    SetCANRegSameValue TRANSMIT_BUFFER_REGISTER_2

    RETLW 0
;...............................................................................

;...............................................................................
DoTransmitFrame:
    ; Setting Bit 0 in the Command register tells the CAN to transmit the
    ; contents of its transmit buffer
    SetCANReg COMMAND_REGISTER, 0x01
    RETLW 0
;...............................................................................

;...............................................................................
DoReceiveFrame:

    GetCANReg INTERRUPT_REGISTER

    BTFSS CANCommand, Bit0 ; check the Receive Interrupt bit - if it's set, we need to read from the FIFO
    GOTO CheckBufferClear

ReadReceivedData:
    ; if receive interrupt active
    ; read data from receive buffer and put data1 into CANReceivedData register
    GetCANReg ACCEPTANCE_CODE1_REGISTER
    MOVF CANCommand, W
    XORLW 0x55
    .gen IF <NOT> W
        GetCANReg RECEIVE_BUFFER_REGISTER_AMR0
        MOVF CANCommand, W
        MOVWF CANReceivedData
    .gen ENDIF

    ; release receive buffer
    SetCANReg COMMAND_REGISTER, 0x04

CheckBufferClear:
    GetCANReg STATUS_REGISTER
    BTFSC CANCommand, Bit0 ; no more data to read
    GOTO ReadReceivedData

    RETLW 0
;...............................................................................

;...............................................................................
; Given a register address in 'CANRegister' and a command to be written to
; that address in 'CANCommand' this function will send the command to the CAN.
;...............................................................................
DoCANSetRegister:
    MOVLW PortAsOutput
    TRIS CAN_ADDRESS_PORT
    MOVF CANRegister, W
    MOVWF CAN_ADDRESS_PORT

    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    BCF CAN_CTRL_PORT, CAN_ADDRESS_LATCH_ENABLE_BIT ; latch the address
    BSF CAN_CTRL_PORT, CAN_SELECT_BIT

    MOVLW PortAsOutput
    TRIS CAN_COMMAND_PORT
    MOVF CANCommand, W
    MOVWF CAN_COMMAND_PORT

    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    BSF CAN_CTRL_PORT, CAN_WRITE_BIT

    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    MOVLW CAN_RESET
    MOVWF CAN_CTRL_PORT ; reset all control lines

    RETLW 0
;...............................................................................

;...............................................................................
; Given a register address in 'CANRegister' the value is read from that register
; into 'CANCommand'
;...............................................................................
DoCANGetRegister:
    MOVLW PortAsOutput
    TRIS CAN_ADDRESS_PORT
    MOVF CANRegister, W
    MOVWF CAN_ADDRESS_PORT

    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    BCF CAN_CTRL_PORT, CAN_ADDRESS_LATCH_ENABLE_BIT ; latch the address constant
    BSF CAN_CTRL_PORT, CAN_SELECT_BIT
    BSF CAN_CTRL_PORT, CAN_READ_BIT

    MOVLW PortAsInput
    TRIS CAN_COMMAND_PORT
    MOVF CAN_COMMAND_PORT, W
    MOVWF CANCommand ; store result

    MOVLW PortAsOutput
    TRIS CAN_CTRL_PORT
    MOVLW CAN_RESET
    MOVWF CAN_CTRL_PORT ; reset all control lines

    RETLW 0
;...............................................................................

;...............................................................................
; Clears LCD Screen
;...............................................................................
DoLCDClearScreen:

    MOVLW 0x01
    MOVWF FirstLine
    CALL LCDClearScreenCore ; clear first line

    MOVLW 0x00
    MOVWF FirstLine
    CALL LCDClearScreenCore ; clear second line
    
    RETLW 0
    
DoLCDClearScreenCore:
    .gen FOR LoopCounter = #0 TO #16
        MOVLW ' '
        MOVWF CharacterToDisplay
        MOVF LoopCounter, W
        MOVWF LinePosition
        CALL LCDSendCharacter
    .gen ENDFOR
    RETLW   0
;...............................................................................

;...............................................................................
; Sends the character in 'CharacterToDisplay' to the LCD 
; at address 'LinePosition', on a line determined by the
; 'FirstLine' boolean.
;...............................................................................
DoLCDSendCharacter:
    CALL LCDWaitTillReady
      
    MOVLW PortAsOutput
    TRIS LCD_DATA_PORT
    MOVF CharacterToDisplay, W
    MOVWF LCD_DATA_PORT ; output the character on the data lines
             
    MOVLW PortAsOutput
    TRIS LCD_CTRL_PORT

    MOVF LinePosition, W
    ANDLW LCD_LINE_ON
    BTFSS FirstLine, Bit0 ; the next line turns on the 'line' bit if this isn't the first line
    IORLW LCD_LINE_OFF
    IORLW LCD_STROBE_OFF
    MOVWF LCD_CTRL_PORT

    ANDLW LCD_STROBE_ON
    MOVWF LCD_CTRL_PORT
    
    MOVF LinePosition, W
    MOVLW 0xFF
    MOVWF LCD_CTRL_PORT

    RETLW 0
;...............................................................................

;...............................................................................
; Doesn't return until the LCD is ready
;...............................................................................
DoLCDWaitTillReady:
    MOVLW   PortAsInput
    TRIS    LCD_CTRL_PORT ; Set Port To Inputs

    .gen REPEAT
    .gen UNTIL LCD_BUSY ; Check Busy flag, if High (Busy) then try again
    RETLW 0
;...............................................................................

;...............................................................................
DoDelay1s:
    .gen FOR LoopCounter = #0 TO #40
        MOVLW 250
        MOVWF CountOuter
        CALL Delay100us
    .gen ENDFOR    
    
    RETLW 0 
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

    RETLW 0
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
    
    RETLW 0    
;...............................................................................    

END


