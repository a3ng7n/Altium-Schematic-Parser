#ifndef _INTERRUPT0_H

    #include "ntype.h"

    #define _INTERRUPT0_H

    #define MODE_2_TIMER(F) ((long)256-(long)CLOCK_FREQ/((long)F*12))

    void InitInterrupt0(void);
    void EnableInt0Interrupt( void );
    void DisableInt0Interrupt( void );
    
    extern void Interrupt0( void );  /* the function Interrupt0() will be called everytime int0 occured */


    void InitInterrupt1(void);
    void EnableInt1Interrupt( void );
    void DisableInt1Interrupt( void );
    
    extern void Interrupt1( void );  /* the function Interrupt0() will be called everytime int0 occured */


    void InitTimer0(WORD hzFreq );
    void EnableTimer0Interrupt( void );
    void DisableTimer0Interrupt( void );

    extern void Timer0Interrupt(void);

#endif /* _INTERRUPT0_H */
