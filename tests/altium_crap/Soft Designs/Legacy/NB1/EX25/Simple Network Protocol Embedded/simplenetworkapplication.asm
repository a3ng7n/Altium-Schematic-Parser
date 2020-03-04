
.INCLUDE "GeneralDefines.asm"
.INCLUDE "UtilsInclude.asm"
.INCLUDE "NetworkInclude.asm"
.INCLUDE "PageSelectInclude.asm"

;...............................................................................
NV01_LEDS_PORT         .equ PORTD ; output
NV02_SENSOR_PORT       .equ PORTD ; input
NODE_ID_PORT           .equ PORTE

LEDS_NVA          .equ 0x01
SENSOR_NVA        .equ 0x02
;...............................................................................

;...............................................................................
    .section data_SimpleNetworkApplication, data
Temp               .dsb 1 ; Temporary variable
TransmitTarget     .dsb 1
NetworkVariable1   .dsb 1 ; The Network Variable at address 0x01

.INCLUDE "NetworkBuffers.inc"

;...............................................................................
    .section bit_SimpleNetworkApplication, bit
GetSetMode         .dsbit 1 ; 0 = Get Mode, 1 = Set Mode

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
ProcessKeypress:            GOTO DoProcessKeypress
SetNetworkVariableCallback: GJMP DoSetNetworkVariableCallback
    .global SetNetworkVariableCallback
GetNetworkVariableCallback: GJMP DoGetNetworkVariableCallback
    .global GetNetworkVariableCallback
NetworkVariableValueCallback: GJMP DoNetworkVariableValueCallback
    .global NetworkVariableValueCallback
;...............................................................................

;...............................................................................
; String Lookup tables:
;...............................................................................
NodeIDMessage:
    ADDWF   PCL,f
    RETLW   'Node ID: ',0

SetTargetMessage:
    ADDWF   PCL,f
    RETLW   'SET target: ',0

GetTargetMessage:
    ADDWF   PCL,f
    RETLW   'GET target: ',0

GetMessage:
    ADDWF   PCL,f
    RETLW   'Received: ',0

GetAttemptMessage:
    ADDWF   PCL,f
    RETLW   'Getting...',0
;...............................................................................

;...............................................................................
Init:
    PAGE_0 ; default page for most variables
     
    BSF GetSetMode ; default to Set mode
    MOVLW 0x00
    MOVWF NetworkVariable1

    GCALL KeypadInit
    GCALL LCDInit ; Initialise LCD screen

    MOVLW 0x00
    MOVWF TransmitTarget

    ; read in NodeId from dipswitches and
    MOVLW 0x1F ; mask for turning off top 3 bits
    ANDWF NODE_ID_PORT, W
    MOVWF NodeID

    BSF FirstLine
    DisplayLCDMessageWithHexDigit NodeIDMessage, NodeID

    GCALL NetworkInit ; Initialise CAN device + network parameters

    Return
;...............................................................................

;...............................................................................
Main:
    CALL Init
MainLoop:

    GCALL Delay1s

    GCALL ReceiveData

    GCALL KeyScan
    JumpIfEqual LastKeyPress, Key_INVALID, MAIN_CONTINUE
    CALL ProcessKeypress

MAIN_CONTINUE:
    GOTO MainLoop
;...............................................................................

;...............................................................................
DoProcessKeypress:
    JumpIfEqual LastKeyPress, Key_INVALID, PK_RETURN ; no keypress = nothing to process

    JumpIfBelow LastKeyPress, Key_A, PROCESS_NUMBER

    ; the letter A is a toggle between Setting a value on the target and
    ; Getting a value from it
    JumpIfEqual LastKeyPress, Key_A, TOGGLE_SET_MODE

    ; a non-numeric key was pressed, so we send that value to the previously entered address
    ; if in 'Send' mode, or simply send a 'Get Value' message to the node.
    MOVF TransmitTarget, W
    MOVWF TargetNodeID

    BTFSS GetSetMode
    GOTO GET_NETWORK_VARIABLE

    ; SET_NETWORK_VARIABLE
    MOVF LastKeyPress, W
    MOVWF NetworkVariableValue

    MOVLW LEDS_NVA
    MOVWF NetworkVariableAddress

    GCALL SetNetworkRegister
    GOTO PK_RETURN

GET_NETWORK_VARIABLE:
    MOVLW SENSOR_NVA
    MOVWF NetworkVariableAddress
    BCF FirstLine
    DisplayLCDMessage GetAttemptMessage
    GCALL GetNetworkRegister
    GOTO PK_RETURN

TOGGLE_SET_MODE:
    BTFSS GetSetMode
    GOTO CLEAR_MODE
    BCF GetSetMode
    GOTO DISPLAY_TX_MODE_MESSAGE

CLEAR_MODE:
    BSF GetSetMode
    GOTO DISPLAY_TX_MODE_MESSAGE

PROCESS_NUMBER:
    ; a number was pressed - record the address as the target of the next keypress
    MOVF LastKeyPress, W
    ANDLW 0x1F ; make sure we are only keeping the bottom 5 bits for the address
    MOVWF TransmitTarget

DISPLAY_TX_MODE_MESSAGE:
    BSF FirstLine
    BTFSS GetSetMode
    GOTO SHOW_GET_MESSAGE
    DisplayLCDMessageWithHexDigit SetTargetMessage, TransmitTarget
    GOTO PK_RETURN

SHOW_GET_MESSAGE:
    DisplayLCDMessageWithHexDigit GetTargetMessage, TransmitTarget
    GOTO PK_RETURN

PK_RETURN:
    MOVLW Key_INVALID
    MOVWF LastKeyPress
    Return
;...............................................................................

;...............................................................................
; Since this module is larger than 1 page, we need to push some of the code to
; be after the end of "SimpleNetworkProtocol.asm" to avoid conflicts.
;...............................................................................
    .section Entry, Code, at(0x700)
;...............................................................................
; SetNetworkVariableCallback: Called whenever a 'Set Network Variable'
;                             message is received.
;...............................................................................
DoSetNetworkVariableCallback:
    JumpIfNotEqual NetworkVariableAddress, LEDS_NVA, SNVC_RETURN
        MOVF NetworkVariableValue, W
        MOVWF NV01_LEDS_PORT
        GOTO SNVC_RETURN

SNVC_RETURN:
    Return
;...............................................................................

;...............................................................................
; GetNetworkVariableCallback: Called whenever a 'Get Network Variable'
;                             message is received.
;...............................................................................
DoGetNetworkVariableCallback:
    JumpIfNotEqual NetworkVariableAddress, SENSOR_NVA, SNVC_RETURN
        MOVF NV02_SENSOR_PORT, W
        MOVWF NetworkVariableValue
        GOTO GNVC_RETURN

GNVC_RETURN:
    Return
;...............................................................................

;...............................................................................
; GetNetworkVariableCallback: Called whenever a 'Get Network Variable'
;                             message is received.
;...............................................................................
DoNetworkVariableValueCallback:
    JumpIfNotEqual NetworkVariableAddress, SENSOR_NVA, SNVC_RETURN
        ; display the next value for the given NetworkVariable
        BCF FirstLine
        DisplayLCDMessageWithHexDigit GetMessage, NetworkVariableValue
        GOTO NVVC_RETURN
NVVC_RETURN:
    Return
;...............................................................................

END


