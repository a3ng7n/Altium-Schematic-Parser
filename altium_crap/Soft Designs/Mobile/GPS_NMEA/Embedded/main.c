#include <stdio.h>
#include <timing.h>
#include <math.h>

#include "swplatform.h"

#define TEXT
#define pi 3.1415926535

char *mess;

posix_devctl_textdisplay_impl_t textio;     // Required for font switches (uses device control to install!)
graphics_t *graph;
canvas_t *canvas;
location_t *location;
gps_nmea_t *gps;

static void init( void )
{
    // Not necessary for functioning of the text interface, but we use this to show
    // you can still use graphics, even though there's a text interface on top of the stack.
    graph = graphics_open( GRAPHICS_1 );
    canvas = graphics_get_visible_canvas(graph);

    // Install device specific functions through POSIX device control
    posix_devctl( fileno(stdout), DEVCTL_TEXTDISPLAY_IMPL, (void*)&textio, sizeof( textio ), NULL );

    puts("Waiting for modem to start...");
    while (!modem_start(modem_open(MODEM_2G)));
    location = location_open(LOCATION_2G);
    location_start(location);

#if GPS_NMEA_EXTENDED == 1
    gps = gps_nmea_open(GPS_NMEA_2G);
#endif
}


#if GPS_NMEA_EXTENDED == 1
#ifndef TEXT
static void draw_sat(int elevation, int azimuth, int snr, bool used)
{
    int rx, ry;

    rx = (int)(120.0 + (float)(90 - elevation) * (float)sin((double)azimuth * pi / 180));
    ry = (int)(200.0 - (float)(90 - elevation) * (float)cos((double)azimuth * pi / 180));

    if (snr > 10)
    {
        graphics_fill_circle(canvas, rx, ry, snr/10, used ? GREENYELLOW : RED);
    }
    else
    {
        graphics_draw_pixel(canvas, rx, ry, WHITE);
    }
}

static void draw_sats(gps_info_t *gps)
{
    graphics_fill_circle(canvas, 120, 200, 110, GREY25);

    graphics_fill_triangle(canvas, 120,  90, 135, 120, 105, 120, SILVER);
    graphics_fill_triangle(canvas, 120, 310, 135, 280, 105, 280, SILVER);
    graphics_fill_triangle(canvas,  10, 200,  40, 215,  40, 185, SILVER);
    graphics_fill_triangle(canvas, 230, 200, 200, 215, 200, 185, SILVER);

    graphics_fill_circle(canvas, 120, 200, 90, NAVY);

    for (int i = 0; i < 12; i++)
    {
        draw_sat(gps->sats[i].elevation, gps->sats[i].azimuth, gps->sats[i].snr, gps->sats[i].used);
    }
}
#endif
#endif


int main(void)
{
    location_info_t location_info;
    gps_info_t * gps_info;
    char str[100];
    int sec = 0;

    puts("GPS application\n");

    init();

    puts("Waiting for GPS lock...");

    while (1)
    {
        if (!location_get_info(location,&location_info))
           continue;

        if (location_info.UTC.tm_sec != sec)
        {
            sec = location_info.UTC.tm_sec;
            fputs("\x1B[2J", stdout);   // puts() adds a newline at the end of the string, fputs() avoids this
            strftime(str, sizeof(str), "%a %d %b %y, %H:%M:%S", &location_info.UTC);
            puts(str);
            printf("lat:      %lf\nlong:     %lf\nalt:      %.1lf\n", location_info.latitude, location_info.longitude, location_info.altitude);
#if GPS_NMEA_EXTENDED == 1
            gps_info = gps_nmea_decode(gps,true);
            printf("Accuracy: %.1f\n", gps_info->PDOP);
#endif
            switch (gps_info->fix)
            {
            case 1:
                puts("fix:      2D\n");
                break;
            case 3:
                puts("fix:      3D\n");
                break;
            case 5:
                puts("fix:      2D, Diff\n");
                break;
            case 7:
                puts("fix:      3D, Diff\n");
                break;
            default:
                puts("fix:      Invalid\n");
                break;
            }
#if GPS_NMEA_EXTENDED == 1
#ifdef TEXT
            puts  ("       ID    E     A   SNR   Used");
            for (int i = 0; i < 12; i++)
            {
                printf("sat%-2d: %2d   %2d   %3d    %2d     %2d\n",
                        i,
                        (int)gps_info->sats[i].id,
                        (int)gps_info->sats[i].elevation,
                        (int)gps_info->sats[i].azimuth,
                        (int)gps_info->sats[i].snr,
                        (int)gps_info->sats[i].used);
            }
#else
            draw_sats(gps_info);
#endif
#endif
        }
    }
}


