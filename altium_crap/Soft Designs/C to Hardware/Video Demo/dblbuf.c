#include <stdint.h>
#include <stdbool.h>

volatile __output bool VIDEO_BUFFER;

void toggle_address_line(void)
{
    static bool video_buffer = 0;

    VIDEO_BUFFER = video_buffer = !video_buffer;
}

