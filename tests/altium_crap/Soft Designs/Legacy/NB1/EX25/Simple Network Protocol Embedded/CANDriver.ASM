;...............................................................................
; CAN Driver code.
;
; To use this unit the user must implement the CAN Driver
; function 'ProcessReceivedMessage'.
;...............................................................................

.INCLUDE "GeneralDefines.asm"
.INCLUDE "NetworkInclude.asm"
.INCLUDE "PageSelectInclude.asm"

;...............................................................................
; CAN Defines
CAN_CTRL_PORT           .equ    PORTB
CAN_ADDRESS_PORT        .equ    PORTA
CAN_COMMAND_PORT        .equ    PORTA
CAN_RESET               .equ  0x00

CAN_ADDRESS_LATCH_ENABLE .equ   0x10     ; bit 4 - driven high to latch the address being sent to the CAN
CAN_SELECT               .equ   0x20     ; bit 5 - driven high to select the CAN chip
CAN_WRITE                .equ   0x40     ; bit 6 - drive high to write to the CAN registers
CAN_READ                 .equ   0x80     ; bit 7 - drive high to read from the CAN registers

MODE_REGISTER             .equ  0x00
COMMAND_REGISTER          .equ  0x01
STATUS_REGISTER           .equ  0x02
INTERRUPT_REGISTER        .equ  0x03
INTERRUPT_ENABLE_REGISTER .equ  0x04
BUS_TIMING_REGISTER_0     .equ  0x06
BUS_TIMING_REGISTER_1     .equ  0x07
OUTPUT_CONTROL_REGISTER   .equ  0x08

ACCEPTANCE_CODE0_REGISTER   .equ  0x10
ACCEPTANCE_CODE1_REGISTER   .equ  0x11
ACCEPTANCE_CODE2_REGISTER   .equ  0x12
ACCEPTANCE_CODE3_REGISTER   .equ  0x13

ACCEPTANCE_MASK0_REGISTER   .equ  0x14
ACCEPTANCE_MASK1_REGISTER   .equ  0x15
ACCEPTANCE_MASK2_REGISTER   .equ  0x16
ACCEPTANCE_MASK3_REGISTER   .equ  0x17
CLOCK_DIVIDER_REGISTER      .equ  0x1F
TRANSMIT_BUFFER_REGISTER_0  .equ 0x10
TRANSMIT_BUFFER_REGISTER_1  .equ 0x11
TRANSMIT_BUFFER_REGISTER_2  .equ 0x12
TRANSMIT_BUFFER_REGISTER_3  .equ 0x13
TRANSMIT_BUFFER_REGISTER_4  .equ 0x14
TRANSMIT_BUFFER_REGISTER_5  .equ 0x15
TRANSMIT_BUFFER_REGISTER_6  .equ 0x16
TRANSMIT_BUFFER_REGISTER_7  .equ 0x17
TRANSMIT_BUFFER_REGISTER_8  .equ 0x18
TRANSMIT_BUFFER_REGISTER_9  .equ 0x19
TRANSMIT_BUFFER_REGISTER_10 .equ 0x1A

RECEIVE_BUFFER_REGISTER_0   .equ 0x10
RECEIVE_BUFFER_REGISTER_1   .equ 0x11
RECEIVE_BUFFER_REGISTER_2   .equ 0x12
RECEIVE_BUFFER_REGISTER_3   .equ 0x13
RECEIVE_BUFFER_REGISTER_4   .equ 0x14
RECEIVE_BUFFER_REGISTER_5   .equ 0x15
RECEIVE_BUFFER_REGISTER_6   .equ 0x16
RECEIVE_BUFFER_REGISTER_7   .equ 0x17
RECEIVE_BUFFER_REGISTER_8   .equ 0x18
RECEIVE_BUFFER_REGISTER_9   .equ 0x19
RECEIVE_BUFFER_REGISTER_10  .equ 0x1A

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

SetCANRegFromCANCommand .MACRO RegisterAddress
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANSetRegister
    .ENDM

SetCANRegFromMemory .MACRO RegisterAddress, MemoryLocation
        MOVF MemoryLocation, W
        MOVWF CANCommand
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANSetRegister
    .ENDM

GetCANReg .MACRO RegisterAddress
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANGetRegister
    .ENDM

GetCANRegToMemory .MACRO RegisterAddress, MemoryLocation
        MOVLW RegisterAddress
        MOVWF CANRegister
        CALL CANGetRegister
        MOVF CANCommand, W
        MOVWF MemoryLocation
    .ENDM
;...............................................................................


;...............................................................................
    .section data_CANDriver, data
; arguments for CAN functions
CANRegister        .dsb 1
CANCommand         .dsb 1
;...............................................................................

;...............................................................................
    .section bit_SNP, bit
DataReceived       .dsbit  1 ; 1 = Data received, 0 = no new data
;...............................................................................

;...............................................................................
    .section Code_CANDriver, Code, at(0x400)

;...............................................................................
;...............................................................................
; Jump Table
; CALL instruction only allows 8 bit address so use a jump table
; CALL the Jump table which does a GOTO the address
;...............................................................................
TransmitData:                   GOTO    DoTransmitData
    .global TransmitData
ReceiveData:                    GOTO    DoReceiveData
    .global ReceiveData

CANInit:                        GOTO    DoCANInit
    .global CANInit
CANGetRegister:                 GOTO    DoCANGetRegister
CANSetRegister:                 GOTO    DoCANSetRegister
;...............................................................................

;...............................................................................
; TransmitData
; Transmits SNP format data packet via the CAN device.
; Params: SNPTxMsg - byte 0: MessageID = Fct Code + Destination NodeID byte
;                  - byte 1: Number of data bytes (max. value 8)
;                  - byte 2-9: Data bytes in little endian format
;...............................................................................
DoTransmitData:

    PAGE_2

TD_RPT_Test_Not_Can_Tx:
    GetCANReg STATUS_REGISTER
    BTFSS CANCommand, Bit2 ; check the Transmit Buffer status bit
    GOTO TD_RPT_Test_Not_Can_Tx

    ClampMemoryToValue SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET, SNP_MAX_DATA_BYTES, TD_SET_TX_BUF

TD_SET_TX_BUF:

    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_0, SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_1, SNPTxMsg + SNP_MESSAGEID_OFFSET
    SetCANReg TRANSMIT_BUFFER_REGISTER_2, 0x00

    ; the following calculates the offset into a jump table. Each label
    ; jumped to runs into the code after it so that all the required
    ; number of bytes get copied to the transmit buffers
    COMF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET, F
    INCF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET, F ; these 2 instructions turn the data length value into a negative number
    MOVLW SNP_MAX_DATA_BYTES
    ADDWF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET, W ; now W = 8 - DataLength

    ADDWF PCL, F
    GOTO TD_8_Tx_Bytes
    GOTO TD_7_Tx_Bytes
    GOTO TD_6_Tx_Bytes
    GOTO TD_5_Tx_Bytes
    GOTO TD_4_Tx_Bytes
    GOTO TD_3_Tx_Bytes
    GOTO TD_2_Tx_Bytes
    GOTO TD_1_Tx_Bytes
TD_8_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_10, SNPTxMsg + SNP_DATA_START_OFFSET + 7
TD_7_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_9, SNPTxMsg + SNP_DATA_START_OFFSET + 6
TD_6_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_8, SNPTxMsg + SNP_DATA_START_OFFSET + 5
TD_5_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_7, SNPTxMsg + SNP_DATA_START_OFFSET + 4
TD_4_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_6, SNPTxMsg + SNP_DATA_START_OFFSET + 3
TD_3_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_5, SNPTxMsg + SNP_DATA_START_OFFSET + 2
TD_2_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_4, SNPTxMsg + SNP_DATA_START_OFFSET + 1
TD_1_Tx_Bytes:
    SetCANRegFromMemory TRANSMIT_BUFFER_REGISTER_3, SNPTxMsg + SNP_DATA_START_OFFSET + 0

TD_START_XMIT:
    ; Setting Bit 0 in the Command register tells the CAN to transmit the
    ; contents of its transmit buffer
    SetCANReg COMMAND_REGISTER, 0x01

    GetCANReg STATUS_REGISTER
    BTFSC CANCommand, Bit3 ; check the Transmit Buffer status bit
    GOTO TD_START_XMIT

TD_RETURN:
    PAGE_0
    Return
;...............................................................................

;...............................................................................
; ReceiveData
; Receives MM2 format data packet via the CAN device.
; Params: DataReceived - Flag set to true if new data has been received
;         SNPRxMsg - byte 0: MessageID = Fct Code + Destination NodeID byte
;                  - byte 1: Number of data bytes (max. value 8)
;                  - byte 2-9: Data bytes in little endian format
;...............................................................................
DoReceiveData:

RD_CHECK_BUFFER_CLEAR:
    PAGE_2
    BCF DataReceived ; clear data received flag

    GetCANReg STATUS_REGISTER
    BTFSS CANCommand, Bit0 ; 1 = unread data in Rx FIFO
    GOTO RD_RETURN

ReadReceivedData:
    ; receive interrupt active so read data from
    ; receive buffer and put data1 into CANReceivedData register
    BSF DataReceived

    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_1, SNPRxMsg + SNP_MESSAGEID_OFFSET

    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_0, SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    MOVF SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET, W
    ANDLW 0x0F ; mask off non-Data Length Code bits
    MOVWF SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    ClampMemoryToValue SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET, SNP_MAX_DATA_BYTES, RD_GET_RX_BUF

RD_GET_RX_BUF:

    ; the following calculates the offset into a jump table. Each label
    ; jumped to runs into the code after it so that all the required
    ; number of bytes get copied from the receive buffers
    COMF SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET, F
    INCF SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET, F ; these 2 instructions turn the data length value into a negative number
    MOVLW SNP_MAX_DATA_BYTES
    ADDWF SNPRxMsg + SNP_DATA_LENGTH_INFO_OFFSET, W ; now W = 8 - DataLength

    ADDWF PCL, F
    GOTO RD_8_Rx_Bytes
    GOTO RD_7_Rx_Bytes
    GOTO RD_6_Rx_Bytes
    GOTO RD_5_Rx_Bytes
    GOTO RD_4_Rx_Bytes
    GOTO RD_3_Rx_Bytes
    GOTO RD_2_Rx_Bytes
    GOTO RD_1_Rx_Bytes
RD_8_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_10, SNPRxMsg + SNP_DATA_START_OFFSET + 7
RD_7_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_9, SNPRxMsg + SNP_DATA_START_OFFSET + 6
RD_6_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_8, SNPRxMsg + SNP_DATA_START_OFFSET + 5
RD_5_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_7, SNPRxMsg + SNP_DATA_START_OFFSET + 4
RD_4_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_6, SNPRxMsg + SNP_DATA_START_OFFSET + 3
RD_3_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_5, SNPRxMsg + SNP_DATA_START_OFFSET + 2
RD_2_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_4, SNPRxMsg + SNP_DATA_START_OFFSET + 1
RD_1_Rx_Bytes:
    GetCANRegToMemory RECEIVE_BUFFER_REGISTER_3, SNPRxMsg + SNP_DATA_START_OFFSET + 0

RD_RELEASE_BUF:
    ; release receive buffer
    SetCANReg COMMAND_REGISTER, 0x04

    BTFSS DataReceived
    GOTO RD_RETURN
    PAGE_0 ; switch back to default page for global call
    GCALL ProcessReceivedMessage
    GOTO RD_CHECK_BUFFER_CLEAR

RD_RETURN:
    PAGE_0 ; make sure we drop back to Page 0 (default page)
    Return
;...............................................................................

;...............................................................................
; Initialise the CAN controller
; Params: NodeID - Uses this value set up the message filters.
; Notes: The message format we receive is based on the MicroMessaging protocol.
;        The 8-bit MessageID = Fct Code (MS 3 bits) + NodeID (LS 5 Bits)
;        The message filters are set up in dual-filter mode.
;        One filter accepts any Fct Code + this node's NodeID.
;        The other filter accepts any Fct Code + NodeID=0 (Broadcast message)
;...............................................................................
DoCANInit:
    PAGE_2
    ; initialise flags
    BCF DataReceived

    SendConstantToPort CAN_CTRL_PORT, CAN_RESET  ; reset all control lines

CI_RPT_Test_MODE0:
    SetCANReg MODE_REGISTER, 0x01
    GetCANReg MODE_REGISTER
    BTFSS CANCommand, Bit0
    GOTO CI_RPT_Test_MODE0

; todo: calculate the following based on a given BPS
    SetCANReg CLOCK_DIVIDER_REGISTER, 0xC0
    SetCANReg BUS_TIMING_REGISTER_0, 0x09  ; 40 MHz 125kb/s
    SetCANReg BUS_TIMING_REGISTER_1, 0x1C  ; 40 MHz 125kb/s
    SetCANReg OUTPUT_CONTROL_REGISTER, 0x1A
    SetCANReg CLOCK_DIVIDER_REGISTER, 0x8F

    ; set first filter to accept messages for this NodeID only
    MOVLW 0x1F ; mask for turning off top 3 bits
    ANDWF NodeID, F
    SetCANRegFromMemory ACCEPTANCE_CODE0_REGISTER, NodeID
    SetCANReg ACCEPTANCE_MASK0_REGISTER, 0xE0
    SetCANReg ACCEPTANCE_MASK1_REGISTER, 0xFF

    ; Set second filter to accept Broadcast Messages
    SetCANReg ACCEPTANCE_CODE2_REGISTER, 0x00
    SetCANReg ACCEPTANCE_MASK2_REGISTER, 0xE0
    SetCANReg ACCEPTANCE_MASK3_REGISTER, 0xFF

    SetCANReg INTERRUPT_ENABLE_REGISTER, 0x01

CI_RPT_Test_Not_MODE0:
    SetCANReg MODE_REGISTER, 0x00 ; set value
    GetCANReg MODE_REGISTER ; then ensure value has changed
    BTFSC CANCommand, Bit0
    GOTO CI_RPT_Test_Not_MODE0

    PAGE_0

    Return
;...............................................................................

;...............................................................................
; Given a register address in 'CANRegister' and a command to be written to
; that address in 'CANCommand' this function will send the command to the CAN.
;...............................................................................
DoCANSetRegister:
    SendMemoryToPort CAN_ADDRESS_PORT, CANRegister

    MOVLW CAN_RESET
    XORLW CAN_ADDRESS_LATCH_ENABLE | CAN_SELECT
    MOVWF CAN_CTRL_PORT

    SendMemoryToPort CAN_COMMAND_PORT, CANCommand

    MOVLW CAN_RESET
    XORLW CAN_ADDRESS_LATCH_ENABLE | CAN_SELECT | CAN_WRITE
    MOVWF CAN_CTRL_PORT

    SendConstantToPort CAN_CTRL_PORT, CAN_RESET ; reset all control lines

    Return
;...............................................................................

;...............................................................................
; Given a register address in 'CANRegister' the value is read from that register
; into 'CANCommand'
;...............................................................................
DoCANGetRegister:
    SendMemoryToPort CAN_ADDRESS_PORT, CANRegister

    MOVLW CAN_RESET
    XORLW CAN_ADDRESS_LATCH_ENABLE | CAN_SELECT | CAN_READ
    MOVWF CAN_CTRL_PORT

    GetMemoryFromPort CAN_COMMAND_PORT, CANCommand
    SendConstantToPort CAN_CTRL_PORT, CAN_RESET ; reset all control lines
    Return
;...............................................................................

END
