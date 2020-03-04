#include "video.h"

// Local memories instantiated by CHC
static uint19_t __imem0  hist_gen_cache[HIS_X_RES];                             // Contains moving average of the generated histograms

// External memories accessed over "HIS" Wishbone bus
__HIS uint10_t           hist_display[HIS_X_RES] __at(0);                       // The histogram that is displayed during the current frame-field.

// External memories accessed over "CONV" Wishbone bus
__CONV uint19_t          hist_generated[HIS_X_RES] __at(0);                     // The histogram that is generated during the current frame-field.
__CONV uint32_t          intensity_conv_tab[HIS_X_RES] __at(HIS_X_RES*4);       // Conversion table to convert old intensity (camera) to new intensity (display)

// Load histogram of current FRAME into 'hist_gen_cache' and return
// highest #occurence in cache entry.
static uint19_t load_histogram_to_cache( bool bw )                              // 1: black&withe video data => 8 bit intensity
{                                                                               // 0: color video data       => 6 bit intensity
    uint19_t max = 0;
    // The histogram contains empty slots due to the 565 coding.
    int      delta = 1;

    // Copy histogram of current frame to the cache
    for ( int i = 0; i < HIS_X_RES; i += delta )
    {
        uint32_t hist_g = hist_generated[i];

        // Merge new data in 1:8 ratio to stabilize histogram
        hist_g = ((hist_gen_cache[i] * (HIS_MOVING_AVERAGE - 1)) + hist_g) / HIS_MOVING_AVERAGE;

        max = ((uint19_t)hist_g > max) ? (uint19_t)hist_g : max;
        hist_gen_cache[i] = (int19_t)hist_g;

        // Fill unused cache entries
        hist_generated[i] = 0;
        for ( int j = 1; j < delta; j++ )
        {
            hist_gen_cache[i+j] = hist_gen_cache[i];
            hist_generated[i+j] = 0;
        }
    }
    return max;
 }

// Create contrast table based on histogram data
static void calculate_contrast_table( uint8_t threshold,                        // Treshold to select min an max intensities from histogram valid domain[0..255]
                                      uint8_t scale_factor,                     // Stretch histogram with this scale factor. NOTE: 9bits are required for scale factor 10.
                                      uint19_t max                              // Max value in hist_gen_cache[]
                                    )
{
    static uint8_t i_min = 0;                  // Cut-off intensity at left side of histogram
    static uint8_t i_max = HIS_X_RES - 1;      // Cut-off intensity at right side of histogram
    uint16_t       is_min, is_max;             // New intensity at i_min and i_max after scaling
    uint16_t       a;                          // New intensity distribution line 'y=ax+b'; fixed point math applied
    int24_t        b;
    int16_t        bc;
    short          i;                          // Loop counter
    short          pos;                        // Position

    uint19_t thresh = threshold * (max + 1) / HIS_Y_RES;

    // Determine min & max histogram positions for contrast
    for ( i = pos = 0; i < HIS_X_RES; i++ )
    {
        if ( thresh <= hist_gen_cache[i] )
        {
            pos = i;
            break;
        }
    }
    // Take weighted moving average to reduce noise effects, make i_min static!
    i_min = (((uint12_t)i_min * (HIS_MOVING_AVERAGE - 1)) + pos) / HIS_MOVING_AVERAGE;

    for ( i = pos = (HIS_X_RES - 1); i >=0 ; i-- )
    {
        if ( thresh <= hist_gen_cache[i] )
        {
            pos = i;
            break;
        }
    }
    // Take weighted moving average to reduce noise effects, make i_max static!
    i_max = (((uint12_t)i_max * (HIS_MOVING_AVERAGE - 1)) + pos) / HIS_MOVING_AVERAGE;

    is_min = (i_min << 8) - (scale_factor * i_min);
    is_max = (i_max << 8) + (scale_factor * (HIS_X_RES - i_max));

    a = ((is_max - is_min) / (i_max - i_min + 1));
    b = is_min - (a * i_min);

    for ( i = 0; i < HIS_X_RES; i++ )
    {
        bc = b >> 8;
        intensity_conv_tab[i] = (bc < 0) ? 0 : (bc > HIS_X_RES - 1) ? (HIS_X_RES - 1) : bc;
        b = b + a;
    }
}

// Create histogram to be displayed
static void create_histogram_to_display( uint19_t max )                         // Highest #of occurences in histogram
{
    for (int  i = 0; i < HIS_X_RES; i++ )
    {
        uint32_t hist;

        if ( max != 0 ) {
            hist = hist_gen_cache[i];
            hist = hist * HIS_Y_RES / max;
        } else {
            hist = 0;
        }

        // NOTE: for understanding ">>2" see sheet "pixel_pipelines.SchDoc", the values from each pipeline are added.
        // This "feature" exists because the histogram values (intensity occurence) of all pipelines have to be added.
        // Since the histogram and conversion tables use the same bus the behavior of both is the same.
        hist_display[ intensity_conv_tab[i] >> 2 ] = (uint10_t)hist;
    }
}

// Entry point.
// Note: this function is started by assetion of the frame interrupt FRAME_INTT
void histogram_generator(
                          uint8_t CONTRAST_THRESHOLD,                           // Contrast filter setting, provided via Virtual Instruument
                          uint8_t CONTRAST_STRETCH                              // Amount of contrast, provided via Virtual Instruument (0=Maximal, 255=No contrast filter)
                        )
{
    // Verify inputs
    __debug_printf( "histogram_generator(CONTRAST_THRESHOLD=%x, CONTRAST_STRETCH=%x)\n", CONTRAST_THRESHOLD, CONTRAST_STRETCH );

   // Calculate histogram and contrast conversion table that will be used
   // when processing the next field.
   uint19_t  max = load_histogram_to_cache( false );                            // max occurence/height in histogram

   calculate_contrast_table( CONTRAST_THRESHOLD, CONTRAST_STRETCH, max );       // calculate contrast conversion table

   create_histogram_to_display( max );
}

