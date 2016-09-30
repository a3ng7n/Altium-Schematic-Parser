/******************************************************************************\
|
|  COPYRIGHT:     Copyright (c) 2005, Altium
|
|  DESCRIPTION:   WB_PWM Pulse-Width-Modulator device driver
|
\******************************************************************************/

#include "wb_pwm.h"

/* PWM Speacial Function Registers */
#define PWM_BASE(BASE)   ((volatile unsigned char *)BASE)

/* PWMRG(BASE) is for WB_PWM8 and 8-bit mode of WB_PWMX */
#define PWMRG(BASE)      PWM_BASE(BASE)[0]

/* PWMRGLO(BASE) and PWMRGHI(BASE) are for WB_PWMX only */
#define PWMRGLO(BASE)    PWM_BASE(BASE)[0]
#define PWPLO(BASE)      PWM_BASE(BASE)[1]
#define PWPHI(BASE)      PWM_BASE(BASE)[2]
#define PWCON(BASE)      PWM_BASE(BASE)[3]
#define PWMRGHI(BASE)    PWM_BASE(BASE)[4]

/******************************************************************************\
|
| This function sets bit PWCON.conbit
|
\******************************************************************************/
void wb_pwm_set_control_bit ( unsigned int base, unsigned char conbit )
{
    PWCON(base) |= conbit ;
}

/******************************************************************************\
|
| This function clears bit PWCON.conbit
|
\******************************************************************************/
void wb_pwm_clear_control_bit ( unsigned int base, unsigned char conbit )
{
    PWCON(base) &= ~conbit ;
}

/******************************************************************************\
|
| This function sets the resolution mode of the device.
|
|  Arguments: base = base address of the PWM peripheral
|             res  = resolution mode which can be:
|                    {e_8bit,e_10bit,e_12bit,e_14bit}
|
|  Returns:   0 if bad mode is passed
|             1 if successful
|
\******************************************************************************/
char wb_pwm_set_res ( unsigned int base, mode res )
{
    switch ( res )
    {                                                     // PFL[1..0] =
        case e_8bit:
        {
            PWCON(base) &= 0xCF ;                         // B"00"
            break;
        }
        case e_10bit:
        {
            PWCON(base) = 0xDF & ( PWCON(base) | 0x10 ) ;     // B"01"
            break;
        }
        case e_12bit:
        {
            PWCON(base) = 0xEF & ( PWCON(base) | 0x20 ) ;     // B"10"
            break;
        }
        case e_14bit:
        {
            PWCON(base) = PWCON(base) | 0x30 ;            // B"11"
            break;
        }
        default :
        {
            return 0 ;
        }
    }
    return 1 ;
}

/******************************************************************************\
|
| This function sets bit PWCON.1 (PIEN)
|
| Arguments:  base   = base address of the PWM peripheral
|             enable = 0 or 1 (stopped or running, respectively)
|
\******************************************************************************/
void wb_pwm_global ( unsigned int base, unsigned char enable )
{
    ( enable ) ? wb_pwm_set_control_bit(base, PIEN) : wb_pwm_clear_control_bit (base, PIEN) ;
}

/******************************************************************************\
|
|  This function sets up PWMRG and PWPHI.PWPLO, and sets PWCON.0 (PWEN)
|
|   Arguments:  base     = base address of the PWM peripheral
|               res      = resolution mode which can be:
|                          {e_8bit,e_10bit,e_12bit,e_14bit}
|               prescale = integer number that will set the clock divide ratio
|               pulsewidth = signed integer representing pulsewidth. For full-
|                            brige drive purposes 0x80000000 is zero (50% P.W.)
|
\******************************************************************************/
void wb_pwm_start (unsigned int base, mode res, unsigned int prescale, int pulsewidth )
{
    wb_pwm_set_pw  (base, res, pulsewidth) ;
    wb_pwm_set_pre (base, prescale) ;
    wb_pwm_set_control_bit(base, PWEN) ;
}

/******************************************************************************\
|
| This function stores the prescaler value
|
|  Arguments:  base     = base address of the PWM peripheral
|              prescale = integer number that will set the clock divide ratio
|
\******************************************************************************/
void wb_pwm_set_pre ( unsigned int base, unsigned int prescale )
{
    prescale &= 0x0000FFFF ;
    wb_pwm_pre_byte ( base,
                      (unsigned char)prescale,
                      (unsigned char)prescale >> 8);
}


/******************************************************************************\
|
| This function stores pulsewidth value
|
|  Arguments:  base     = base address of the PWM peripheral
|              res      = resolution mode which can be:
|                         {e_8bit,e_10bit,e_12bit,e_14bit}
|              pulsewidth = signed integer representing pulsewidth. For full-
|                           brige drive purposes 0x80000000 is zero (50% P.W.)
|
|  Returns:    0 if bad mode is passed
|              1 if successful
|
\******************************************************************************/
char wb_pwm_set_pw (unsigned int base, mode res, int pulsewidth )
{
    /* Ensure that the desired resolution has been already defined
    The default resolution is zero (8-bit) for compatibility with
    the WB_PWM8 core
    */
    switch (res) // mode is either 0 1 2 or 3
    {
        case e_8bit:  // 8-bit mode. PWMRG(7) <= pulsewidth(31) ;
        {
            pulsewidth &= 0xFF000000 ;
            wb_pwm_pw_byte( base,
                            0x00,
                            (unsigned char) (pulsewidth >> 24) );
            break;
        }
        case e_10bit: // 10-bit mode. PWMRGHI(9) <= pulsewidth(31) ;
        {
            pulsewidth &= 0xFFC00000 ;
            wb_pwm_pw_byte( base,
                            (unsigned char) (pulsewidth >> 22),
                            (unsigned char) (pulsewidth >> 30));
            break;
        }
        case e_12bit: // 12-bit mode. PWMRGHI(11) <= pulsewidth(31) ;
        {
            pulsewidth &= 0xFFF00000 ;
            wb_pwm_pw_byte( base,
                            (unsigned char) (pulsewidth >> 20),
                            (unsigned char) (pulsewidth >> 28));
            break;
        }
        case e_14bit: // 14-bit mode. PWMRGHI(13) <= pulsewidth(31) ;
        {
            pulsewidth &= 0xFFFC0000 ;
            wb_pwm_pw_byte( base,
                            (unsigned char) (pulsewidth >> 18),
                            (unsigned char) (pulsewidth >> 26));
            break;
        }
        default:
        {
            return 0;
        }
    }
    return 1;
}

/******************************************************************************\
|
| This function gets the current PWI and PRI interrupt status
|
|  Arguments:  base     = base address of the PWM peripheral
|
|  Returns:    0 = No Interrupts at this time
|              1 = Prescaler Overflow has occurred
|              2 = PWM Counter overflow has occurred
|              3 = Prescaler AND PWM counter overflows have occurred
|
\******************************************************************************/
unsigned char wb_pwm_get_int ( unsigned int base )
{
    return PWCON(base) >> 6 ;
}

/******************************************************************************\
|
| This function clears the current PWI and/or PRI interrupt flags
|
|  Arguments:  base     = base address of the PWM peripheral
|              int_num  = number of interrupt to be cleared -
|
|  int_num :   0 = No Interrupts will be cleared
|  values      1 = Prescaler Overflow will be cleared
|              2 = PWM Counter overflow will be cleared
|              3 = Prescaler AND PWM overflow flags will be cleared
|
\******************************************************************************/
void wb_pwm_clear_int ( unsigned int base, char int_num )
{
    wb_pwm_clear_control_bit (base, int_num << 6);
}

/******************************************************************************\
|
| This function clears PWCON register, driving both outputs to zero.
|
|  Arguments:  base     = base address of the PWM peripheral
|
\******************************************************************************/
void wb_pwm_stop (unsigned int base)
{
    PWCON(base) = 0x00 ;
}

/******************************************************************************\
|
| This function clears all registers, driving both outputs to zero and forcing
| initial conditions.
|
|  Arguments:  base     = base address of the PWM peripheral
|
\******************************************************************************/
void wb_pwm_stop_reset (unsigned int base)
{
    PWCON(base)   = 0x00 ;
    PWMRGLO(base) = 0x00 ;
    PWMRGHI(base) = 0x00 ;
    PWPLO(base)   = 0x00 ;
    PWPHI(base)   = 0x00 ;
}

/******************************************************************************\
|
| This function performs byte-wise writes to the PWM register high and low bytes
|
|  Arguments:  base     = base address of the PWM peripheral
|              lobyte   = bottom 8-bits of PWM value
|              hibyte   = top 0 to 6 bits (depending on resolution mode)
|
\******************************************************************************/
void wb_pwm_pw_byte (unsigned int base, unsigned char lobyte, unsigned char hibyte)
{
    PWMRGLO(base) = lobyte;
    PWMRGHI(base) = hibyte;
}

/******************************************************************************\
|
| This function performs byte-wise writes to the prescaler register high and low bytes
|
|  Arguments:  base     = base address of the PWM peripheral
|              lobyte   = bottom 8-bits of Prescaler value
|              hibyte   = top 8 bits of prescaler value
|
\******************************************************************************/
void wb_pwm_pre_byte (unsigned int base, unsigned char lobyte, unsigned char hibyte )
{
    PWPLO(base) = lobyte ;
    PWPHI(base) = hibyte ;
}

/******************************************************************************\
|
| This function performs a 1-int to 4-byte rapid setup of the device.
| WARNING: does not configure low byte of prescaler.
|
|  Arguments:  base        = base address of the PWM peripheral
|              configword  = 32-bit integer.
|
|  configword split:  configword(31..24) => PWCON (Control Register)
|                     configword(23..16) => PWMRG(15..8)
|                     configword(15..8)  => PWMRG(7..0)
|                     configword(7..0)   => PWPHI (Prescaler High-Byte)
|
\******************************************************************************/
void wb_pwm_start_quick (unsigned int base, unsigned int config_word )
{
    PWPHI(base)   = (char) (0x00FF0000 & config_word) >> 16 ;
    PWMRGLO(base) = (char) (0x000000FF & config_word) ;
    PWMRGHI(base) = (char) (0x0000FF00 & config_word) >> 8 ;
    PWCON(base)   = (char) (0xFF000000 & config_word) >> 24 ;
}

/******************************************************************************\
|
| This function performs single byte write to any register in the peripheral
|
|  Arguments:  base     = base address of the PWM peripheral PLUS and offset
|                         for the individual register within the device
|              byte     = byte to be stored in the register
|
\******************************************************************************/
void wb_pwm_write_byte (unsigned int base_plus_offset, unsigned char byte )
{
    *PWM_BASE(base_plus_offset) = byte;
}

/******************************************************************************\
|
| This function performs single byte read from any register in the peripheral
|
|  Arguments:  base     = base address of the PWM peripheral PLUS and offset
|                         for the individual register within the device
|
|  Returns:    byte read from the register
|
\******************************************************************************/
unsigned char wb_pwm_get_byte (volatile unsigned char *base_plus_offset )
{
    return *base_plus_offset ;
}
