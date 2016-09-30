        .extern main
        .section .text.cstart

        .global _exit
        .weak   _Exit
        .global _START
_START:

done:
        ;; call main with argv[0]==NULL & argc==0
        li      $4,0
        jal     main
        li      $5,0

_exit:
_Exit:
        j       _exit
        nop

        .endsec

        .end
