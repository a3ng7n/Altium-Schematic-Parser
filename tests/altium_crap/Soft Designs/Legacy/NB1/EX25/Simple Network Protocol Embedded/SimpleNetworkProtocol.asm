;...............................................................................
; Simple Network Protocol code.
;
; This unit implements the CAN Driver function 'ProcessReceivedMessage'.
;
; To use this unit the application layer must allocate memory for
; SNPTxMsg and SNPRxMsg, giving them as much size as the application
; thinks they will need. If this node is going to be transmit or receive
; only, then they are not both needed. Also, if messages will never be
; more than 4 bytes (2 for the header, and 2 for the data - enough for
; the Get/SetNetworkVariable calls) then only this much data need be
; allocated. The maximum possible size of the message buffer is 10 bytes
; (2 for header, 8 for data).
;
; Callbacks: To include this unit in your own project, you also need
; to implement a number of 'callback' functions in your application layer.
; These are just methods that must be present for this unit to compile.
; They are called in various circumstances such as when a network variable
; has just been set, or when a request for a network variable value has been
; received since it is up to the application layer to interpret and supply
; these values.
; NOTE: These are not strictly 'callback' functions as in higher-level languages.
;       They are not variable pointers to functions, just descriptions of functions
;       that must have at least a skeletal implementation for compilation, and where
;       much of the application layer's linking to the network layer occurs.
;
; SetNetworkVariableCallback:
;     Called whenever a "Set Network Variable" message is received over the network.
;     The NetworkVariableAddress & NetworkVariableValue parameters will contain the
;     correct values before the call is made. The application uses these values in
;     its implementation of this callback.
;
; GetNetworkVariableCallback:
;     Called whenever a "Get Network Variable" message is received over the network.
;     The NetworkVariableAddress parameter will contain the address the sender is
;     requesting the value for. The application level should write the value for
;     the requested address into the NetworkVariableValue parameter. If this
;     parameter value is not supported, it would be best for the implementation
;     of this function to set the value to '0'.
;
; NetworkVariableValueCallback:
;     Called whenever a "Network Variable Value" message is received over the network.
;     The NetworkVariableAddress and NetworkVariableValue will contain the data just
;     received. These messages are sent in response to this node previously sending
;     a "Get Network Variable" message.
;...............................................................................


.INCLUDE "GeneralDefines.asm"
.INCLUDE "NetworkInclude.asm"
.INCLUDE "PageSelectInclude.asm"

;...............................................................................
    .section data_SimpleNetworkProtocol, data
; todo: Move to seperate "SimpleNetworkProtocol.asm
Temp               .dsb 1 ; for use by any function (only for short term use)

NodeID             .dsb 1 ; this node's own network ID
    .global NodeID

; The following variables are parameters for the various
; network communication functions
TargetNodeID       .dsb 1 ; the address of the Master Node.
    .global TargetNodeID
NetworkVariableAddress .dsb 1
    .global NetworkVariableAddress
NetworkVariableValue   .dsb 1
    .global NetworkVariableValue
;...............................................................................


;...............................................................................
    .section Code_SimpleNetworkProtocol, Code, at(0x200)

;...............................................................................
;...............................................................................
; Jump Table
; CALL instruction only allows 8 bit address so use a jump table
; CALL the Jump table which does a GOTO the address
;...............................................................................
NetworkInit:                    GOTO    DoNetworkInit
    .global NetworkInit

SetNetworkRegister:             GOTO    DoSetNetworkRegister
    .global SetNetworkRegister
GetNetworkRegister:             GOTO    DoGetNetworkRegister
    .global GetNetworkRegister
SendNetworkVariableValue:       GOTO    DoSendNetworkVariableValue
    .global SendNetworkVariableValue

ProcessReceivedMessage:         GOTO    DoProcessReceivedMessage
    .global ProcessReceivedMessage
;...............................................................................

;...............................................................................
; Initialises the network parameters and the CAN device. The Master node
; needs to perform certain initialisation tasks on the network. A Slave
; will start in Stop mode and will require a Start message from the Master
; node.
;...............................................................................
DoNetworkInit:
    MOVLW 0x1F
    ANDWF NodeID, F ; mask off 3 MS bits of NodeID

    GCALL CANInit

    ; initialise globals
    MOVLW 0x00
    MOVWF TargetNodeID
    MOVLW 0x00
    MOVWF NetworkVariableAddress
    MOVLW 0x00
    MOVWF NetworkVariableValue

NWInitEnd:
    Return
;...............................................................................

;...............................................................................
; SetNetworkRegister : Sets a network register on another machine.
; Params: TargetNodeID - The address for the node on which we wish
;                        to set the network variable
;         NetworkVariableAddress - A network variable's address
;                                  on the target node which is to be set.
;         NetworkVariableValue - The new value for the given network variable
;...............................................................................
DoSetNetworkRegister:
    MOVLW 0x02
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    PAGE_0
    MOVF NetworkVariableAddress, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET
    PAGE_0
    MOVF NetworkVariableValue, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET + 1
    PAGE_0
    SendMessageToNodePointer FCT_CODE_SET_VARIABLE, TargetNodeID
    Return
;...............................................................................

;...............................................................................
; GetNetworkRegister : Gets a network register on another machine (non-blocking).
;
; This method causes a 'Get Network Register' message to be sent to the
; appropriate node(s). As soon as this message is sent this function call
; returns. If successfully received, the target node will send the
; requested value later.
;
; Params: TargetNodeID - The address for the node on which we wish
;                        to set the network variable
;         NetworkVariableAddress - A network variable's address
;                                  on the target node which is to be set.
;...............................................................................
DoGetNetworkRegister:
    MOVLW 0x02
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    PAGE_0
    MOVF NetworkVariableAddress, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET
    PAGE_0
    MOVF NodeID, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET + 1
    PAGE_0
    SendMessageToNodePointer FCT_CODE_GET_VARIABLE, TargetNodeID

    Return
;...............................................................................

;...............................................................................
; SendNetworkVariableValue : Called automatically after receiving a 'GetNetworkRegister' message.
;
; This method allows
;
; Params: TargetNodeID - The address for the node on which we wish
;                        to set the network variable
;         NetworkVariableAddress - A network variable's address
;                                  on the target node which is to be set.
;         NetworkVariableValue - A network variable's address
;                                  on the target node which is to be set.
;...............................................................................
DoSendNetworkVariableValue:
    MOVLW 0x02
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_LENGTH_INFO_OFFSET
    PAGE_0
    MOVF NetworkVariableAddress, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET
    PAGE_0
    MOVF NetworkVariableValue, W
    PAGE_2
    MOVWF SNPTxMsg + SNP_DATA_START_OFFSET + 1
    PAGE_0
    SendMessageToNodePointer FCT_CODE_VARIABLE_VALUE, TargetNodeID
    Return
;...............................................................................

;...............................................................................
; ProcessReceivedMessage: Called whenever a message is received by the driver
;                         code. The data to be processed is in the 'SNPRxMsg'
;                         data structure.
;...............................................................................
DoProcessReceivedMessage:
    PAGE_2
    MOVF SNPRxMsg + SNP_MESSAGEID_OFFSET, W
    PAGE_0
    ANDLW 0xE0 ; Fct Code is in MS 3 bits
    MOVWF Temp
    BCF C
    .REPEAT 5
         RRF Temp, F
    .ENDREP

    ; Case FCT_CODE_SET_VARIABLE
    JumpIfNotEqual Temp, FCT_CODE_SET_VARIABLE, PRD_CASE_FCT_CODE_GET_VARIABLE
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET, W
        PAGE_0
        MOVWF NetworkVariableAddress
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET + 1, W
        PAGE_0
        MOVWF NetworkVariableValue
        GCALL SetNetworkVariableCallback
        GOTO PRD_RETURN

PRD_CASE_FCT_CODE_GET_VARIABLE:
    ; Case FCT_CODE_GET_VARIABLE
    JumpIfNotEqual Temp, FCT_CODE_GET_VARIABLE, PRD_CASE_FCT_CODE_VARIABLE_VALUE
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET, W
        PAGE_0
        MOVWF NetworkVariableAddress
        GCALL GetNetworkVariableCallback ; application should put value in NetworkVariableValue
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET + 1, W
        PAGE_0
        MOVWF TargetNodeID
        CALL SendNetworkVariableValue
        GOTO PRD_RETURN

PRD_CASE_FCT_CODE_VARIABLE_VALUE:
    ; Case FCT_CODE_VARIABLE_VALUE
    JumpIfNotEqual Temp, FCT_CODE_VARIABLE_VALUE, PRD_RETURN
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET, W
        PAGE_0
        MOVWF NetworkVariableAddress
        PAGE_2
        MOVF SNPRxMsg + SNP_DATA_START_OFFSET + 1, W
        PAGE_0
        MOVWF NetworkVariableValue
        GCALL NetworkVariableValueCallback
        GOTO PRD_RETURN

PRD_RETURN:
    Return
;...............................................................................

END
