#include <stdio.h>
#include <timing.h>
#include <math.h>

#include "hardware.h"

#define DATA_SIZE 8*1024

typedef struct {
    float x, y, z;
} point_3d_t;

point_3d_t input_data[DATA_SIZE];
point_3d_t origin;

void generate_data(void)
{
    for (uint32_t i=0; i<DATA_SIZE; i++)
    {
        input_data[i].x = (float) rand();
        input_data[i].y = (float) rand();
        input_data[i].z = (float) rand();
    }
    origin.x = (float) rand();
    origin.y = (float) rand();
    origin.z = (float) rand();
}

uint32_t find_closest_point_sw(void)
{
    float shortest_distance_sqr = HUGE_VALF;
    float this_distance_sqr;
    uint32_t shortest_index=0;

    for (uint32_t i = 0; i<DATA_SIZE; i++)
    {
        float x_dist, y_dist, z_dist;
        x_dist = input_data[i].x - origin.x;
        y_dist = input_data[i].y - origin.y;
        z_dist = input_data[i].z - origin.z;
        this_distance_sqr = x_dist * x_dist + y_dist * y_dist + z_dist * z_dist;
        if (this_distance_sqr < shortest_distance_sqr)
        {
            shortest_distance_sqr = this_distance_sqr;
            shortest_index = i;
        }
    }
    return shortest_index;
}

#define FPU ((volatile float *) Base_FPU)
#define FPU_OP1  FPU[1]
#define FPU_OP2  FPU[2]
#define FPU_OP12 FPU[3]
#define FPU_ADD  FPU[2]
#define FPU_SUB  FPU[3]
#define FPU_MUL  FPU[4]

inline float fpu_add(float a, float b)
{
    FPU_OP1 = a;
    FPU_OP2 = b;
    return FPU_ADD;
}

inline float fpu_sub(float a, float b)
{
    FPU_OP1 = a;
    FPU_OP2 = b;
    return FPU_SUB;
}

inline float fpu_sqr(float a)
{
    FPU_OP12 = a;
    return FPU_MUL;
}

uint32_t find_closest_point_fpu(void)
{
    float shortest_distance_sqr = HUGE_VALF;
    float this_distance_sqr;
    uint32_t shortest_index=0;

    for (uint32_t i = 0; i<DATA_SIZE; i++)
    {
        float x_dist, y_dist, z_dist;
        x_dist = fpu_sub(input_data[i].x, origin.x);
        y_dist = fpu_sub(input_data[i].y, origin.y);
        z_dist = fpu_sub(input_data[i].z, origin.z);
        float x_dist_sqr, y_dist_sqr, z_dist_sqr;
        x_dist_sqr = fpu_sqr(x_dist);
        y_dist_sqr = fpu_sqr(y_dist);
        z_dist_sqr = fpu_sqr(z_dist);
        this_distance_sqr = fpu_add(x_dist_sqr, y_dist_sqr);
        this_distance_sqr = fpu_add(this_distance_sqr, z_dist_sqr);
        if (this_distance_sqr < shortest_distance_sqr)
        {
            shortest_distance_sqr = this_distance_sqr;
            shortest_index = i;
        }
    }
    return shortest_index;
}

point_3d_t *input_data_hw;
point_3d_t origin_hw;

void init_hw(point_3d_t *input)
{
    input_data_hw = input;
}

void set_origin_hw(float x, float y, float z)
{
    origin_hw.x = x;
    origin_hw.y = y;
    origin_hw.z = z;
}

uint32_t find_closest_point_hw(void)
{
    float shortest_distance_sqr = HUGE_VALF;
    float this_distance_sqr;
    uint32_t shortest_index=0;

    for (uint32_t i = 0; i<DATA_SIZE; i++)
    {
        float x_dist, y_dist, z_dist;
        x_dist = input_data_hw[i].x - origin_hw.x;
        y_dist = input_data_hw[i].y - origin_hw.y;
        z_dist = input_data_hw[i].z - origin_hw.z;
        this_distance_sqr = x_dist * x_dist + y_dist * y_dist + z_dist * z_dist;
        if (this_distance_sqr < shortest_distance_sqr)
        {
            shortest_distance_sqr = this_distance_sqr;
            shortest_index = i;
        }
    }
    return shortest_index;
}

void main( void )
{
    unsigned long long tick;
    uint32_t us_sw, us_hw, us_fpu;
    uint32_t index_sw, index_hw, index_fpu;
    float improvement;
    uint32_t iteration = 1;

    printf("\nFloating Point CHC example\n");
    printf("3d closest point finder, running on %dK points\n\n", DATA_SIZE/1024);

    while(1)
    {
        generate_data();
        printf("Iteration %d\n", iteration);
        printf("Closest point (software) = ");
        tick = clock_us();
        index_sw = find_closest_point_sw();
        us_sw = (uint32_t) elapsed_time_us(tick);
        printf("%4d: ", index_sw);
        printf("%7d us\n", us_sw);

        printf("Closest point (fpu)      = ");
        tick = clock_us();
        index_fpu = find_closest_point_fpu();
        us_fpu = (uint32_t) elapsed_time_us(tick);
        printf("%4d: ", index_fpu);
        printf("%7d us ", us_fpu);
        if (index_sw != index_fpu)
        {
            printf("ERROR!!!!!\n");
        }
        else
        {
            improvement = (float) us_sw;
            improvement = improvement / us_fpu;
            printf("%4.1fX improvement\n", improvement);
        }

        printf("Closest point (hardware) = ");
        tick = clock_us();
        init_hw(input_data);
        set_origin_hw(origin.x, origin.y, origin.z);
        index_hw = find_closest_point_hw();
        us_hw = (uint32_t) elapsed_time_us(tick);
        printf("%4d: ", index_hw);
        printf("%7d us ", us_hw);

        if (index_sw != index_hw)
        {
            printf("ERROR!!!!!\n");
        }
        else
        {
            improvement = (float) us_sw;
            improvement = improvement / us_hw;
            printf("%4.1fX improvement\n", improvement);
        }
        iteration++;
        delay_ms(1000);
    }
}   
