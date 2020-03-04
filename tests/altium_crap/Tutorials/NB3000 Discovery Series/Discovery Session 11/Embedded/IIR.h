#include <stdint.h>
#ifndef  __IIR_H
#define  __IIR_H

typedef struct coe2pol
{
    int32_t  alpha;
    int32_t  beta;
    int32_t  gamma;
} coe2pol_t ;

int16_t do_iir ( int16_t input, int16_t alpha, int16_t beta,
                 int16_t gamma, int16_t * buff_x, int16_t * buff_y );

int32_t scale (int16_t sample);
int16_t truncate (int32_t sample);

#endif
