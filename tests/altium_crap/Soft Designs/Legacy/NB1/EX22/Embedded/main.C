#include <stdlib.h>

// Wishbone Peripherals map to SFR space from 0x80, so Wishbone address 0x70
// is address 0xF0 in the MCU memory map.
#define PWMREG   (*(__sfr unsigned char *)0xfc)                 // Stores the PWM value.
#define PWMPLO   (*(__sfr unsigned char *)0xfd)                 // Low and high bytes of the
#define PWMPHI   (*(__sfr unsigned char *)0xfe)                 // 16-bit prescaler.
#define PWCON    (*(__sfr unsigned char *)0xff)                 // Control / Interrupt register.

// Control Register Bits:
#define PWEN 0x01                                               // PWM Enable
#define PIEN 0x02                                               // Interrupt Enable
#define PRIE 0x04                                               // Prescaler Interrupt Enable
#define PWIE 0x08                                               // PWM Counter Interrupt Enable
#define PRI  0x40                                               // Prescaler Interrupt Flag
#define PWI  0x80                                               // PWM Counter Interrupt Flag

unsigned char pulsevalue    = 0x00;
unsigned char pwm_ramp_rate = 0x08;
unsigned char pwm_int_count = 0x00;
__bit         reverse_flag  = 0;

void pwm_init( void )
{
    PWMREG = pulsevalue;
    PWMPLO = 0x80;
    PWMPHI = 0x00;
    PWCON  = PWEN | PIEN | PWIE | PRIE;                         // enable the PWM module and overflow interrupts
}

void pwm_reset ( void )
{
    PWCON  = 0x00;
    PWMREG = 0x00;
    PWMPLO = 0x00;
    PWMPHI = 0x00;
}


void tsk52_init( void )
{
    IEN0 = 0x81;
    IT0  = 1;
    EAL  = 1;
    P0   = 0;
}

void pwm_check_interrupts( void )
{
    if ( PWCON & PRI )
    {
       PWCON ^= PRI ;                                    // Clear PRI
       P0++ >= 0x0F ? PWCON ^= PRIE : NULL ;             // This line is to ensure PRIE and PRI are working correctly.
    }
    if ( PWCON & PWI )
    {
       PWCON ^= PWI ;                                    // Clear PWI
       if ( pwm_int_count++ == pwm_ramp_rate )
       {
          pwm_int_count = 0;
          if ( reverse_flag )
          {
             pulsevalue == 0x00 ? reverse_flag = 0 : pulsevalue-- ;
          }
          else
          {
             pulsevalue == 0xff ? reverse_flag = 1 : pulsevalue++ ;
          }
          PWMREG = pulsevalue;
       }
    }
}

void __interrupt(0x0003) pwm_int( void )
{
    EAL = 0;
    IE0 = 0;
    pwm_check_interrupts();
    EAL = 1;
}

void main ( void )
{
    pwm_reset();
    tsk52_init();
    pwm_init();
    while(1)
    {
       if ( !P1_7 )
       {
          IE0 = 0;                                              // Flick SW1 to disable interrupts and poll from software.
          EAL = 0;
          pwm_check_interrupts();
       }
       else
       {
          EAL = 1;
       }
    }
}
