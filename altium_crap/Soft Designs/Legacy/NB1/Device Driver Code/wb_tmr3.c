/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2004, Altium
|*
|*  DESCRIPTION:   TMR3 Dual Timer device driver
|*
\*****************************************************************************/

#include "wb_tmr3.h"

#define TMR_BASE(base)  ((volatile unsigned char *) base)

// redundant casts
#define TMR_TCON(base) TMR_BASE(base)[0]
#define TMR_TMOD(base) TMR_BASE(base)[1]
#define TMR_TLA(base)  TMR_BASE(base)[2]
#define TMR_TLB(base)  TMR_BASE(base)[3]
#define TMR_THA(base)  TMR_BASE(base)[4]
#define TMR_THB(base)  TMR_BASE(base)[5]

unsigned int tmr3_get_mode          ( unsigned int base, unsigned int timer )
{
    switch ( timer )
    {
        case 1 : return ( TMR_TMOD(base) & 0x03 );

        case 2 : return ( ( TMR_TMOD(base) & 0x30 ) >> 4 );
    }
    return ( 4 );
}


void          tmr3_init             ( unsigned int base )
{
    tmr3_t1_init ( base );
    tmr3_t2_init ( base );
}

void          tmr3_t1_init          ( unsigned int base )
{
    TMR_TCON(base) = TMR_TCON(base) & 0xF0;
    TMR_TMOD(base) = 0xF0 & TMR_TMOD(base);
    TMR_TLA(base) = 0x00;
    TMR_THA(base) = 0x00;
}

void          tmr3_t2_init          ( unsigned int base )
{
    TMR_TCON(base) = 0x0F & TMR_TCON(base);
    TMR_TMOD(base) = 0x0F & TMR_TMOD(base);
    TMR_TLB(base) = 0x00;
    TMR_THB(base) = 0x00;
}

void          tmr3_set_tmod         ( unsigned int base, unsigned char tmod )
{
    TMR_TMOD(base) = tmod;
}

unsigned char tmr3_get_tmod         ( unsigned int base )
{
    return(TMR_TMOD(base));
}

void          tmr3_set_tcon         ( unsigned int base, unsigned char tcon )
{
    TMR_TCON(base) = tcon;
}

unsigned char tmr3_get_tcon         ( unsigned int base )
{
    return(TMR_TCON(base));
}

unsigned char tmr3_get_th           ( unsigned int base, unsigned int timer )
{
    switch ( timer )
    {
        case 1 : return(TMR_THA(base));
                 break;
        case 2 : return(TMR_THB(base));
                 break;
    }
}

unsigned char tmr3_get_tl           ( unsigned int base, unsigned int timer )
{
    switch ( timer )
    {
        case 1 : return(TMR_TLA(base));
                 break;
        case 2 : return(TMR_TLB(base));
                 break;
    }
}

void          tmr3_set_th           ( unsigned int base, unsigned int timer, unsigned char th )
{
    switch ( timer )
    {
        case 1 : TMR_THA(base) = th;
                 break;
        case 2 : TMR_THB(base) = th;
                 break;
    }
}

void          tmr3_set_tl           ( unsigned int base, unsigned int timer, unsigned char tl )
{
    switch ( timer )
    {
        case 1 : TMR_TLA(base) = tl;
                 break;
        case 2 : TMR_TLB(base) = tl;
                 break;
    }
}

void          tmr3_t1_set_tf        ( unsigned int base, unsigned char tf)
{
    if (tf==0x01)
    {
        TMR_TCON(base) |= 0x20;
    }
    else
    {
        TMR_TCON(base) &= 0xDF;
    }
}

unsigned char tmr3_t1_get_tr        ( unsigned int base )
{
    unsigned char tr=0x00;

    tr = TMR_TCON(base) & 0x10;
    return(tr >> 4);
}

void          tmr3_t2_set_gate      ( unsigned int base, unsigned char gate )
{
    if (gate==0x01)
    {
        TMR_TMOD(base) = TMR_TMOD(base) | 0x80;
    }
    else
    {
        TMR_TMOD(base) = TMR_TMOD(base) & 0x7F;
    }
}

unsigned char tmr3_t2_get_gate      ( unsigned int base )
{
    unsigned char gate=0x00;

    gate = TMR_TMOD(base) & 0x80;
    return(gate >> 7);
}

void          tmr3_t2_set_ct        ( unsigned int base, unsigned char ct )
{
    if (ct==0x01)
    {
        TMR_TMOD(base) |= 0x40;
    }
    else
    {
        TMR_TMOD(base) &= 0xBF;
    }
}

unsigned char tmr3_t2_get_ct        ( unsigned int base )
{
    unsigned char ct=0x00;

    ct = TMR_TMOD(base) & 0x40;
    return(ct >> 6);
}

void          tmr3_t2_set_tf        ( unsigned int base, unsigned char tf)
{
    if (tf==0x01)
    {
        TMR_TCON(base) |= 0x80;
    }
    else
    {
        TMR_TCON(base) &= 0x7F;
    }
}

unsigned char tmr3_t2_get_tr        ( unsigned int base )
{
    unsigned char tr=0x00;

    tr = TMR_TCON(base) & 0x40;
    return(tr >> 6);
}

void          tmr3_set_mode         ( unsigned int base, unsigned int timer, tmr3_mode_t mode )
{
    if ( timer == 1 )
    {
        TMR_TMOD(base) |= 0x0F;

        switch ( mode )
        {
            case mode0_timer         : TMR_TMOD(base) &= 0xF0;
                                       break;

            case mode0_gated_timer   : TMR_TMOD(base) &= 0xF8;
                                       break;

            case mode0_counter       : TMR_TMOD(base) &= 0xF4;
                                       break;

            case mode0_gated_counter : TMR_TMOD(base) &= 0xFC;
                                       break;

            case mode1_timer         : TMR_TMOD(base) &= 0xF1;
                                       break;

            case mode1_gated_timer   : TMR_TMOD(base) &= 0xF9;
                                       break;

            case mode1_counter       : TMR_TMOD(base) &= 0xF5;
                                       break;

            case mode1_gated_counter : TMR_TMOD(base) &= 0xFD;
                                       break;

            case mode2_timer         : TMR_TMOD(base) &= 0xF2;
                                       break;

            case mode2_gated_timer   : TMR_TMOD(base) &= 0xFA;
                                       break;

            case mode2_counter       : TMR_TMOD(base) &= 0xF6;
                                       break;

            case mode2_gated_counter : TMR_TMOD(base) &= 0xFE;
                                       break;

            case mode3_timer         : TMR_TMOD(base) &= 0xF3;
                                       break;

            case mode3_gated_timer   : TMR_TMOD(base) &= 0xFB;
                                       break;

            case mode3_counter       : TMR_TMOD(base) &= 0xF7;
                                       break;

            case mode3_gated_counter : TMR_TMOD(base) &= 0xFF;
                                       break;
        }
    }
    else if ( timer == 2 )
    {
        TMR_TMOD(base) |= 0xF0;

        switch ( mode )
        {
            case mode0_timer         : TMR_TMOD(base) &= 0x0F;
                                       break;

            case mode0_gated_timer   : TMR_TMOD(base) &= 0x8F;
                                       break;

            case mode0_counter       : TMR_TMOD(base) &= 0x4F;
                                       break;

            case mode0_gated_counter : TMR_TMOD(base) &= 0xCF;
                                       break;

            case mode1_timer         : TMR_TMOD(base) &= 0x1F;
                                       break;

            case mode1_gated_timer   : TMR_TMOD(base) &= 0x9F;
                                       break;

            case mode1_counter       : TMR_TMOD(base) &= 0x5F;
                                       break;

            case mode1_gated_counter : TMR_TMOD(base) &= 0xDF;
                                       break;

            case mode2_timer         : TMR_TMOD(base) &= 0x2F;
                                       break;

            case mode2_gated_timer   : TMR_TMOD(base) &= 0xAF;
                                       break;

            case mode2_counter       : TMR_TMOD(base) &= 0x6F;
                                       break;

            case mode2_gated_counter : TMR_TMOD(base) &= 0xEF;
                                       break;

            case mode3_timer         : TMR_TMOD(base) &= 0x3F;
                                       break;

            case mode3_gated_timer   : TMR_TMOD(base) &= 0xBF;
                                       break;

            case mode3_counter       : TMR_TMOD(base) &= 0x7F;
                                       break;

            case mode3_gated_counter : TMR_TMOD(base) &= 0xFF;
                                       break;
        }
    }
}

unsigned int  tmr3_handle_interrupt (unsigned int base )
{
    unsigned int timer  = 0;
    unsigned int timer1 = 0;
    unsigned int timer2 = 0;

    timer1 = ( TMR_TCON(base) & 0x20 ) >> 5;  // get timer1 flag
    if ( timer1 == 0x01 ) tmr3_t1_set_tf ( base, 0x00 );

    timer2 = ( TMR_TCON(base) & 0x80 ) >> 7;  // get timer2 flag
    if ( timer2 == 0x02 ) tmr3_t2_set_tf ( base, 0x00 );

    timer = timer1 + timer2;

    return ( timer );
}

void          tmr3_set_delay_ms     ( unsigned int base, unsigned int timer, unsigned int clock_freq_hz, unsigned int delay_ms )
{
    unsigned int mode;
    unsigned int max_t;
    unsigned int t;

    mode = tmr3_get_mode            ( base, timer );

    switch ( mode )
    {
        case 0 : max_t = 0xFFFF;
                 break;

        case 1 : max_t = 0x1FFF;
                 break;

        case 2 : max_t = 0xFF;
                 break;
    }

    t = ( delay_ms * clock_freq_hz * 1000 ) - max_t;

    tmr3_set_tl ( base, timer, t );
    tmr3_set_th ( base, timer, t >> 8 );
}

void          tmr3_run              ( unsigned int base, unsigned int timer )
{
    switch ( timer )
    {
        case 1  : TMR_TCON(base) |= 0x10;
                  break;

        case 2  : TMR_TCON(base) |= 0x40;
                  break;

        default : ;
    }
}

void          tmr3_stop             ( unsigned int base, unsigned int timer )
{
    switch ( timer )
    {
        case 1  : TMR_TCON(base) &= 0xEF;
                  break;

        case 2  : TMR_TCON(base) &= 0xBF;
                  break;

        default : ;
    }
}



