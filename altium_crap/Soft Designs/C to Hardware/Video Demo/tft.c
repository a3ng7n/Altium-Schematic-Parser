#include "video.h"

#define HSYNC           0x04000000      /* Synchronization pulse length */
#define HPORCH          0x00140000      /* Back porch length            */
#define HVISIBLE        0x000000EF      /* Visible area                 */

#define VSYNC           0x01000000      /* Synchronization pulse length */
#define VPORCH          0x00010000      /* Back porch length            */
#define VVISIBLE        0x0000013F      /* Visible area                 */

#define HLENGTH         0x01100000      /* Number of pixels per line    */
#define VLENGTH         0x00000146      /* Number of pixels per line    */

__input bool            TFT_BUFFER;

/*
 * VGA32 TFT Registers
 */
volatile uint32_t __TFT CTRL    __at( 0);       /* Control              */
volatile uint32_t __TFT HTIM    __at( 8);       /* Horizontal timing    */
volatile uint32_t __TFT VTIM    __at(12);       /* Vertical timing      */
volatile uint32_t __TFT HVLEN   __at(16);       /* Hor/Ver lengths      */
volatile uint32_t __TFT VMBA    __at(20);       /* Video base address   */
volatile uint32_t __TFT DIV     __at(24);       /* System clock divider */


void tft_init( void )
{
        static bool current_buffer = true;

        VMBA  = TFT_BASE_0;
        CTRL  = 0x1300;  // disable
        DIV   = ( PAL_FREQ_HZ + PIXEL_FREQ_HZ / 2 ) / PIXEL_FREQ_HZ;
        HTIM  = HSYNC | HPORCH | HVISIBLE;
        VTIM  = VSYNC | VPORCH | VVISIBLE;
        HVLEN = HLENGTH | VLENGTH;
        CTRL  = 0x1301;  // enable

        for (;;)
        {
                if ( current_buffer != TFT_BUFFER )
                {
                        current_buffer = !current_buffer;
                        VMBA = current_buffer ? TFT_BASE_0 : TFT_BASE_1;
                }
        }
}

