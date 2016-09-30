#include "video.h"

__input bool   SW1_START;       // Switch 1: Start video playback
__input bool   SW2_STOP;        // Switch 2: Stop video playback
__input bool   SW3_GRAYSCALE;   // Switch 3: Toggle grayscale mode
__input bool   SW4_HSI_MODE;    // Switch 4: Toggle HSI mode

__output bool  CTRL_INIT;       // Clear video memory
__output bool  CTRL_GRAYSCALE;  // Grayscale mode enable
__output bool  CTRL_HSI_MODE;   // HSI mode enable

/*******************************************************************************
 * I2C
 ******************************************************************************/

/*
 * I2C registers
 */
volatile __I2C uint8_t I2CM_CTRL  __at(0);  /* Control register         */
volatile __I2C uint8_t I2CM_STAT  __at(1);  /* Status register          */
volatile __I2C uint8_t I2CM_CLK0  __at(2);  /* Clock divisor, low byte  */
volatile __I2C uint8_t I2CM_CLK1  __at(3);  /* Clock divisor, high byte */
volatile __I2C uint8_t I2CM_WRDAT __at(4);  /* Write data buffer        */
volatile __I2C uint8_t I2CM_RDDAT __at(5);  /* Read data buffer         */

/*
 * I2C control bitmasks
 */
#define I2CM_CTRL_ENABLE 0x01  /* Enable (core powers down if not set)    */
#define I2CM_CTRL_IEN    0x02  /* Interrupt enable                        */
#define I2CM_CTRL_IACK   0x04  /* Interrupt acknowledge                   */
#define I2CM_CTRL_WR     0x08  /* Write data to I2C bus                   */
#define I2CM_CTRL_RD     0x10  /* Read data from I2C bus                  */
#define I2CM_CTRL_STO    0x20  /* Generate a stop condition               */
#define I2CM_CTRL_STA    0x40  /* Generate a start condition              */
#define I2CM_CTRL_NACK   0x80  /* Generate a negative acknowledge on read */

/*
 * I2C status bitmasks
 */
#define I2CM_STAT_INTREQ 0x01  /* Interrupt request                           */
#define I2CM_STAT_RXACK  0x02  /* ACK state from remote when transmitting     */
#define I2CM_STAT_BUSY   0x04  /* Core is busy, do not write control register */

#define I2C_WAIT_CYCLES  0     /* number of cycles to __wait() for interrupts */



/* Wait while core is busy */
static inline void      i2cm_busy_wait( void )
{
    while ( I2CM_STAT & I2CM_STAT_BUSY ) __wait( I2C_WAIT_CYCLES );
}

/* Wait for interrupt to appear */
static inline void      i2cm_intreq_wait( void )
{
    while ( !(I2CM_STAT & I2CM_STAT_INTREQ) ) __wait( I2C_WAIT_CYCLES );
}

/* Acknowledge the interrupt (wipe it) */
static inline void      i2cm_int_ack( void )
{
    I2CM_CTRL = I2CM_CTRL_ENABLE | I2CM_CTRL_IACK;
}

static void             i2cm_open( void )
{
    uint32_t sclk         = 100000;
    uint16_t wait_sleepms = 100;
    uint16_t clkdivisor   = FREQ_HZ / ( 5 * sclk ) - 1;
    int      i;

    // Initialize the I2C master device
    i2cm_busy_wait();
    I2CM_CTRL =  0;
    I2CM_CLK0 = clkdivisor & 0xFF;
    I2CM_CLK1 = ( clkdivisor >> 8 ) & 0xFF;

    // Reset slave statemachines
    for( i = 0; i < 9; i++ )
    {
        i2cm_busy_wait();
        I2CM_CTRL = I2CM_CTRL_ENABLE  | I2CM_CTRL_IEN | I2CM_CTRL_STO;
        i2cm_intreq_wait();
        i2cm_busy_wait();
        i2cm_int_ack();
    }
}

static void             i2cm_putchar( bool do_start, const uint8_t data )
{
    uint8_t ctrl = I2CM_CTRL_ENABLE | I2CM_CTRL_IEN | I2CM_CTRL_WR;

    if ( do_start ) ctrl |= I2CM_CTRL_STA;
    i2cm_busy_wait();
    I2CM_WRDAT = data;
    I2CM_CTRL = ctrl;
    i2cm_busy_wait();
    i2cm_intreq_wait();
    i2cm_busy_wait();
    i2cm_int_ack();
}

static void             i2cm_stop( void )
{
    i2cm_busy_wait();
    I2CM_CTRL = I2CM_CTRL_ENABLE | I2CM_CTRL_STO;
    i2cm_intreq_wait();
    i2cm_busy_wait();
    i2cm_int_ack();
}



/*******************************************************************************
 * TVP5150
 ******************************************************************************/

/*
 * TVP5150 registers
 */
#define TVP5150_VIDEO_INPUT             0x00
#define TVP5150_MISC                    0x03
#define TVP5150_SOFTWARE_RESET          0x05
#define TVP5150_FORMAT_AND_RATES        0x0D
#define TVP5150_SHARED_PINS             0x0F

/*
 * TVP5150 input sources
 */
#define TVP5150_COMP1                   0
#define TVP5150_S_VIDEO                 1
#define TVP5150_COMP2                   2

static void             tvp5150_setreg( uint8_t reg, uint8_t val )
{
    i2cm_putchar( true, 0xB8 );     /* Bus Address */
    i2cm_putchar( false, reg );
    i2cm_putchar( false, val );
    i2cm_stop();
}

static void             tvp5150_open( void )
{
    i2cm_open();

    /* Reset & set output data stream format */
    tvp5150_setreg( TVP5150_SOFTWARE_RESET, 0x27 );
    tvp5150_setreg( TVP5150_FORMAT_AND_RATES, 0x27 );

    /* Enable output VBLK */
    tvp5150_setreg( TVP5150_SHARED_PINS, 0x02 );

    /* Enable outputs */
    tvp5150_setreg( TVP5150_MISC, 0xAF );

    /* Select input source */
    tvp5150_setreg( TVP5150_VIDEO_INPUT, TVP5150_COMP1 );
}



/*******************************************************************************
 * BT656
 ******************************************************************************/

/*
 * BT656 run modes
 */
#define BT656_RM_DISABLE        0       /* Disable camera              */
#define BT656_RM_RUN            1       /* Let camera run continuously */
#define BT656_RM_SINGLE         2       /* Capture a single image      */

/*
 * BT656 registers
 */
volatile __VIDEO uint32_t BT656_MODE_REG   __at(0);   /* Mode register              */
volatile __VIDEO uint32_t BT656_STATUS_REG __at(4);   /* Status register            */
volatile __VIDEO uint32_t BT656_START_REG  __at(8);   /* Video buffer start address */
volatile __VIDEO uint32_t BT656_SIZE_REG   __at(12);  /* Video buffer size          */
volatile __VIDEO uint32_t BT656_BPL_REG    __at(16);  /* Bytes per line on output   */
volatile __VIDEO uint32_t BT656_VBPL_REG   __at(20);  /* Visible bytes per line     */
volatile __VIDEO uint32_t BT656_SCALE_REG  __at(24);  /* Scaling register           */

static void             bt656_open(void)
{
    BT656_MODE_REG  = BT656_COLOR_MODE << 2 | BT656_RM_DISABLE;
    BT656_START_REG = VID_BASE_0;
    BT656_SIZE_REG  = VID_BUFFER_SIZE;
    BT656_SCALE_REG = VID_FRAME_SKIP << 8 | VID_Y_ZOOM << 4 | VID_X_ZOOM;
    BT656_BPL_REG   = VGA_X_RES * BYTES_PER_PIXEL;
    BT656_VBPL_REG  = PAL_X_RES * BYTES_PER_PIXEL / ( VID_X_ZOOM + 1 );
}

static inline void     bt656_set_run_mode( uint8_t mode, bool use_grayscale )
{
    uint32_t cm = use_grayscale ? BT656_GRAYSCALE_MODE : BT656_COLOR_MODE;
    BT656_MODE_REG  = ( cm << 2 ) | mode;
}

static inline uint8_t  bt656_get_run_mode( void )
{
    return BT656_MODE_REG & 3;
}

/*******************************************************************************
 * VGA
 ******************************************************************************/

#define HSYNC           0x77000000      /* Synchronization pulse length */
#define HPORCH          0x00420000      /* Back porch length            */
#define HVISIBLE        0x00000257      /* Visible area                 */

#define VSYNC           0x05000000      /* Synchronization pulse length */
#define VPORCH          0x00180000      /* Back porch length            */
#define VVISIBLE        0x0000031F      /* Visible area                 */

#define HLENGTH         0x04100000      /* Number of pixels per line    */
#define VLENGTH         0x0000029A      /* Number of pixels per line    */

/*
 * VGA16 Registers
 */
volatile uint32_t __VGA CTRL    __at( 0);       /* Control              */
volatile uint32_t __VGA HTIM    __at( 8);       /* Horizontal timing    */
volatile uint32_t __VGA VTIM    __at(12);       /* Vertical timing      */
volatile uint32_t __VGA HVLEN   __at(16);       /* Hor/Ver lengths      */
volatile uint32_t __VGA VMBA    __at(20);       /* Video base address   */
volatile uint32_t __VGA DIV     __at(24);       /* System clock divider */


void vga_open( void )
{
    /* 800x600, 60Hz */
    CTRL  = 0x1300;
    VMBA  = VGA_BASE_0;
    DIV  = 0;
    HTIM  = HSYNC | HPORCH | (VGA_X_RES - 1);
    VTIM  = VSYNC | VPORCH | (VGA_Y_RES - 1);
    HVLEN = HLENGTH | VLENGTH;
    CTRL  = 0x1301;
}


/******************************************************************************/
// The entry point.
void init( void )
{
    bool current_grayscale;
    bool current_hsi_mode;
    bool grayscale = SW3_GRAYSCALE;
    bool hsi_mode = SW4_HSI_MODE;
    uint8_t mode = BT656_RM_RUN;

    // Initialize camera & VGA
    bt656_open();
    tvp5150_open();
    vga_open();

    // Send out an INIT pulse to initialize all components
    CTRL_INIT = 1; __wait(10); CTRL_INIT = 0;

    for (;;)
    {
        // Update current status & control register
        CTRL_GRAYSCALE = current_grayscale = grayscale;
        CTRL_HSI_MODE = current_hsi_mode = hsi_mode;

        // Set camera mode
        bt656_set_run_mode( mode, current_grayscale );

        do
        {
            if ( SW1_START )
            {
                mode = BT656_RM_RUN;
                bt656_set_run_mode( mode, current_grayscale );
            }
            else if ( SW2_STOP )
            {
                mode = BT656_RM_DISABLE;
                bt656_set_run_mode( mode, current_grayscale );
            }

            grayscale = SW3_GRAYSCALE;
            hsi_mode = SW4_HSI_MODE;

            // debounce
            __wait( FREQ_HZ / 10 );
        }
        while ( grayscale == current_grayscale && hsi_mode == current_hsi_mode );
    }
}




