#ifndef PROCESS_AUDIO_H
#define PROCESS_AUDIO_H

#include <stdint.h>
#include <stdbool.h>

typedef int16_t sample_device_t;  // type used by the CODEC to represent samples
typedef int32_t sample_long_t;    // type used internally, with headroom to allow for overflow

#define SAMPLE_DEVICE_MIN   (-SAMPLE_DEVICE_MAX-1)
#define SAMPLE_DEVICE_MAX   (0x7fff)
#define SAMPLE_LONG_MIN     (-SAMPLE_LONG_MAX-1)
#define SAMPLE_LONG_MAX     (0x7fffffff)

typedef uint16_t gain_t;
typedef int16_t  coefficient_t;
typedef uint5_t band_t;
typedef bool channel_t;

#define COEFFICIENT_SHIFT   12
#define GAIN_SHIFT          12
#define GAIN_UNITY          0x1000
#define LEVEL_SHIFT         9
#define LEVEL_200           0x200

#define BUFFER_SIZE 0x10000 // must be power of two
#define INDEX_SHIFT 8
// Get a old sample from a buffer.
// The buffers are cleared at startup, and 0-samples don't matter, so reads before writes are ok.
// If delay is too large for the buffer size, we fail: wrap around from overflow, thus a small delay.
#define OLD_INDEX(delay,index)  (((index) - ((delay) << INDEX_SHIFT)) & (BUFFER_SIZE - 1))

#endif
