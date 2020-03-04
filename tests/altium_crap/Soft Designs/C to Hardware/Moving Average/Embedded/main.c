#include <stdio.h>
#include <timing.h>

#define DATA_SIZE 32*1024
#define TAPS 128
uint32_t input_data[DATA_SIZE];
uint32_t sw_output[DATA_SIZE];
uint32_t hw_output[DATA_SIZE];

void generate_data(void)
{
    for (uint32_t i=0; i<DATA_SIZE; i++)
    {
        input_data[i] = rand();
    }
}

void calculate_average_sw(void)
{
    for (uint32_t i = 0; i<DATA_SIZE; i++)
    {
        uint32_t accumulator = 0;
        for (uint32_t j = 0; j<TAPS; j++)
        {
            accumulator += input_data[(i-j) & (DATA_SIZE-1)];
        }
        sw_output[i] = accumulator / (TAPS);
    }
}

uint32_t *input_data_hw;
uint32_t *hw_output_hw;

void init_hw(uint32_t *input, uint32_t* output)
{
    input_data_hw = input;
    hw_output_hw  = output;
}

void calculate_average_hw(void)
{
    for (uint32_t i = 0; i<DATA_SIZE; i++)
    {
        uint32_t accumulator = 0;
        for (uint32_t j = 0; j<TAPS; j++)
        {
            accumulator += input_data_hw[(i-j) & (DATA_SIZE-1)];
        }
        hw_output_hw[i] = accumulator / (TAPS);
    }
}

uint32_t check_results(void)
{
    for (uint32_t i = 0; i< DATA_SIZE; i++)
    {
        if (hw_output[i] != sw_output[i])
            return 0;
    }
    return 1;
}

void main( void )
{
    unsigned long long tick;
    uint32_t ms_sw, ms_hw;
    uint32_t improvement;

    printf("\n\nNanoBoard CHC example\n");
    printf("%d tap moving average filter, running on %dKB of data\n\n", TAPS, DATA_SIZE*4/1024);

    generate_data();
    printf("Calculating average: software\n");
    tick = clock_ms();
    calculate_average_sw();
    ms_sw = (uint32_t) elapsed_time_ms(tick);
    printf("%d ms to complete\n\n", ms_sw);

    printf("Calculating average: hardware\n");
    tick = clock_ms();
    init_hw(input_data, hw_output);
    calculate_average_hw();
    ms_hw = (uint32_t) elapsed_time_ms(tick);
    printf("%d ms to complete\n\n", ms_hw);

    if (!check_results())
    {
        printf("HW Calculation error!\n");
    }
    else
    {
        improvement = ((ms_sw * 100) / ms_hw) - 100;
        printf("No errors found. %d%% improvement\n", improvement);
    }
}

