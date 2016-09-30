#include "IIR.h"

int16_t do_iir ( int16_t input, int16_t alpha, int16_t beta,
                 int16_t gamma, int16_t * buff_x, int16_t * buff_y )
{
    int32_t y = 0;
    y -= beta * buff_y[1];
    y += alpha * input;
    y -= alpha * buff_x[1];
    y += gamma * buff_y[0];

    buff_x[1] = buff_x[0];
    buff_x[0] = input;
    buff_y[1] = buff_y[0];
    buff_y[0] = (int16_t)(y/16384);
    return buff_y[0];
}

int32_t scale (int16_t sample)
{
    return (int32_t)(sample<<16 | (sample & 1 ? 0x0000ffff : 0));
}

int16_t truncate (int32_t sample)
{
    return (int16_t)(sample>>16);
}
