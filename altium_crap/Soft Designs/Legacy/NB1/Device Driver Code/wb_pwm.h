/*****************************************************************************\
|*
|*  COPYRIGHT:     Copyright (c) 2005, Altium
|*
|*  DESCRIPTION:   WB_PWM Pulse-Width-Modulator device driver
|*
\*****************************************************************************/

#ifndef __wb_pwm_h__
#define __wb_pwm_h__

#define PWI   0x80
#define PRI   0x40
#define PFL1  0x20
#define PFL0  0x10
#define MOD1  0x20
#define MOD0  0x10
#define PWIE  0x08
#define PRIE  0x04
#define PIEN  0x02
#define PWEN  0x01

typedef enum
{ e_8bit  = 0,
  e_10bit = 1,
  e_12bit = 2,
  e_14bit = 3
} mode ;

/* Valid resolution settings are 0 1 2 and 3, others default to 0. */
char wb_pwm_set_res           ( unsigned int base,
                                mode res );

/* conbit is the bit-position of the bit you want to set/reset.
*/
void wb_pwm_set_control_bit   ( unsigned int base,
                                unsigned char conbit );

void wb_pwm_clear_control_bit ( unsigned int base,
                                unsigned char conbit );

/* enable != 0 to enable global interrupts
   enable == 0 to disable
*/
void wb_pwm_global            ( unsigned int base,
                                unsigned char enable );

/* pulsewidth is signed so you can have negative (reverse)
   drive with full-bridge connections to PWM+ and PWM- outputs.
*/
void wb_pwm_start             ( unsigned int base,
                                mode res,
                                unsigned int prescale,
                                int pulsewidth );

void wb_pwm_set_pre           ( unsigned int base,
                                unsigned int prescale );

char wb_pwm_set_pw            ( unsigned int base,
                                mode res,
                                int pulsewidth );

unsigned char wb_pwm_get_int  ( unsigned int base );

void wb_pwm_clear_int         ( unsigned int base,
                                char int_num );

void wb_pwm_stop              ( unsigned int base );

void wb_pwm_stop_reset        ( unsigned int base );

void wb_pwm_pw_byte           ( unsigned int base,
                                unsigned char lobyte,
                                unsigned char hibyte);

void wb_pwm_pre_byte          ( unsigned int base,
                                unsigned char lobyte,
                                unsigned char hibyte );

void wb_pwm_start_quick (unsigned int base, unsigned int config_word );
void wb_pwm_write_byte (unsigned int base_plus_offset, unsigned char byte );
unsigned char wb_pwm_get_byte (volatile unsigned char *base_plus_offset) ;

#endif
