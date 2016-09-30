
; Network property related defines

;...............................................................................
NETWORK_SETTINGS_PORT .equ PORTB

SNP_MAX_DATA_BYTES          .equ 0x08 ; the maximum amount of data that can be transmitted
SNP_MESSAGEID_OFFSET        .equ 0
SNP_DATA_LENGTH_INFO_OFFSET .equ 1
SNP_DATA_START_OFFSET       .equ 2

FCT_CODE_SET_VARIABLE       .equ 0 ; SNP code
FCT_CODE_GET_VARIABLE       .equ 1 ; SNP code
FCT_CODE_VARIABLE_VALUE     .equ 3 ; SNP code

MM2_OP_MODE_START           .equ 1
MM2_OP_MODE_STOP            .equ 2
MM2_OP_MODE_SERVICE         .equ 128
MM2_OP_MODE_RESET           .equ 129
;...............................................................................


;...............................................................................
; MACRO Definitions
;...............................................................................
SendMessageToNodePointer .MACRO MessageCode, TargetNodeID ; TargetNodeID is a memory location holding the target node ID value
        ; the data is assumed to already be in the appropriate part of MM2TxMsg
        ; along with the data length already having been set
        PAGE_2
        MOVLW MessageCode
        MOVWF SNPTxMsg + SNP_MESSAGEID_OFFSET
        BCF C
        .REPEAT 5
            RLF SNPTxMsg + SNP_MESSAGEID_OFFSET, F
        .ENDREP
        PAGE_0
        MOVF TargetNodeID, W
        ANDLW 0x1F ; mask off top 3 bits
        PAGE_2
        IORWF SNPTxMsg + SNP_MESSAGEID_OFFSET, F ; combine Fct + Node ID and write back into MessageID
        PAGE_0
        GCALL TransmitData
    .ENDM

SendMessageToNode .MACRO MessageCode, TargetNodeID ; pass in CONSTANTS only - NOT registers
        ; the data is assumed to already be in the appropriate part of MM2TxMsg
        ; along with the data length already having been set
        MOVLW MessageCode
        PAGE_2
        MOVWF SNPTxMsg + SNP_MESSAGEID_OFFSET
        BCF C
        .REPEAT 5
            RLF SNPTxMsg + SNP_MESSAGEID_OFFSET, F
        .ENDREP
        MOVLW TargetNodeID
        ANDLW 0x1F ; mask off top 3 bits
        IORWF SNPTxMsg + SNP_MESSAGEID_OFFSET, F ; combine Fct + Node ID and write back into MessageID
        PAGE_0
        GCALL TransmitData
    .ENDM
;...............................................................................
