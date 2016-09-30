#include "video.h"

/*
 * Ports
 */
__input  bool           VIDEO_BUFFER;   /* Buffer with last camera frame */
__output bool           TFT_BUFFER;     /* Non visible TFT buffer        */
__input  bool           R_DISABLE;      /* Disable red channel           */
__input  bool           G_DISABLE;      /* Disable green channel         */
__input  bool           B_DISABLE;      /* Disable blue channel          */

/*
 * Buffers
 */
__IMEM pixel_t          vid_buf0[VID_X_RES * VID_Y_RES] __at(VID_BASE_0);
__IMEM pixel_t          vid_buf1[VID_X_RES * VID_Y_RES] __at(VID_BASE_1);
__OMEM pixel_t          tft_buf0[TFT_X_RES * TFT_Y_RES] __at(TFT_BASE_0);
__OMEM pixel_t          tft_buf1[TFT_X_RES * TFT_Y_RES] __at(TFT_BASE_1);

/*
 * Histogram data
 */
static uint32_t         intensities[HIS_CACHE_SIZE][HIS_MAX_INTENSITY+1];
static uint32_t         averages[HIS_MAX_INTENSITY+1];

static int              max_intensity;
static int              current_frame;
static int              next_frame;



/*
 * Initialization (called only once after a reset)
 */
static void init( void )
{
        int i;
        int frame;

        /* Clear the TFT output buffers */
        for ( i = 0; i < (TFT_X_RES * TFT_Y_RES); i++ )
        {
                tft_buf0[i] = 0;
                tft_buf1[i] = 0;
        }

        /* Clear the intensity & average tables */
        for ( i = 0; i <= HIS_MAX_INTENSITY; i++ )
        {
                for ( frame = 0; frame < HIS_CACHE_SIZE; frame++ )
                {
                        intensities[frame][i] = 0;
                }
                averages[i] = 0;
        }

        current_frame = 0;
}


/*
 * Copies the camera image to the available video buffer
 * and collects intensity data for the histogram.
 */
static void copy_frame( __IMEM pixel_t* ibuf, __OMEM pixel_t* obuf )
{
        pixel_t mask = 0;
        int     i;

        if ( !R_DISABLE ) mask |= MASK_R;
        if ( !G_DISABLE ) mask |= MASK_G;
        if ( !B_DISABLE ) mask |= MASK_B;

        obuf += VID_Y_BORDER * TFT_X_RES;

        for ( i = 0; i < TFT_X_RES * VID_Y_RES; i++ )
        {
                pixel_t rgb = *(obuf++) = mask & *(ibuf++);

                rgb = EXTRACT_R(rgb) + EXTRACT_G(rgb) + EXTRACT_B( rgb );
                intensities[current_frame][rgb] += 1;
        }
}


/*
 * Calculate histogram data.
 */
static void calc_histogram( void )
{
        int i;

        for ( i = 0; i <= HIS_MAX_INTENSITY; i++ )
        {
                uint32_t average_intensity = 0;
                int      frame;

                /* Calculate the average intensities for all cached frames */
                for ( frame = 0; frame < HIS_CACHE_SIZE; frame++ )
                {
                        average_intensity += intensities[frame][i];
                }
                if ( average_intensity > max_intensity )
                {
                        max_intensity = average_intensity;
                }
                averages[i] = average_intensity;
        }

        for ( i = 0; i <= HIS_MAX_INTENSITY; i++ )
        {
                /* Normalize the averages to 1 */
                averages[i] = (averages[i] * HIS_Y_RES) / max_intensity;

                /* Clear the intensity table for the next frame */
                intensities[next_frame][i] = 0;
        }
}


/*
 * Draw the histogram on top of the camera image.
 */
static void draw_histogram( __OMEM pixel_t* obuf )
{
        int i, j, x, y;

        for ( x = 0; x < TFT_X_RES; x++ )
        {
                i = x * HIS_MAX_INTENSITY / TFT_X_RES;

                for ( j = 0; j < averages[i]; j++ )
                {
                        y = TFT_Y_RES - VID_Y_BORDER - j - 1;
                        obuf[ y * TFT_X_RES + x ] = HIS_COLOR;
                }
        }
}



void process( void )
{
        static volatile bool initialized = false;
        static bool     first = false;
        __OMEM pixel_t* obuf = first ? tft_buf1 : tft_buf0;
        __IMEM pixel_t* ibuf = VIDEO_BUFFER ? vid_buf1 : vid_buf0;

        next_frame = current_frame + 1;
        if ( next_frame == HIS_CACHE_SIZE )
        {
                next_frame = 0;
        }

        if ( !initialized )
        {
                init();
                initialized = true;
        }

        max_intensity = 0;
        copy_frame( ibuf, obuf );
        calc_histogram();
        draw_histogram( obuf );

        TFT_BUFFER = !first;
        first = !first;

        current_frame = next_frame;
}
