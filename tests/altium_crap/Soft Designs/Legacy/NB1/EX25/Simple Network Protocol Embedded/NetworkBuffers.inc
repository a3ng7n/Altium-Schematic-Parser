
; Note: Choose the buffer size desired by uncommenting the appropriate
;       declarations.
;       They are also currently located in Data Bank 2.

;...............................................................................
;
; Tx/Rx buffer definitions for applications which only need to
; use the Get/SetNetworkVariable functions.
;
;...............................................................................

SNPTxMsg .dsb 4
    .global SNPTxMsg
SNPRxMsg .dsb 4
    .global SNPRxMsg

;...............................................................................
;
; Tx/Rx buffer definitions for applications which need the
; full buffers.
;
;...............................................................................

;SNPTxMsg .dsb 10
;    .global SNPTxMsg
;SNPRxMsg .dsb 10
;    .global SNPRxMsg




