//------------------------------------------------------------------------------
// Generated: 3/06/2009 11:56:40 AM
//------------------------------------------------------------------------------

#include <LEDCTRL.h>

#define DATA(base) ((volatile uint32_t *) base)

#define RGB0_ADDR_OFFSET       0x0
#define RGB1_ADDR_OFFSET       0x1
#define RGB2_ADDR_OFFSET       0x2
#define RGB3_ADDR_OFFSET       0x3
#define RGB4_ADDR_OFFSET       0x4
#define RGB5_ADDR_OFFSET       0x5
#define RGB6_ADDR_OFFSET       0x6
#define RGB7_ADDR_OFFSET       0x7

//------------------------------------------------------------------------------
// Functions for the register: RGB0
//------------------------------------------------------------------------------

void wb_interface_set_rgb0(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB0_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB1
//------------------------------------------------------------------------------

void wb_interface_set_rgb1(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB1_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB2
//------------------------------------------------------------------------------

void wb_interface_set_rgb2(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB2_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB3
//------------------------------------------------------------------------------

void wb_interface_set_rgb3(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB3_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB4
//------------------------------------------------------------------------------

void wb_interface_set_rgb4(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB4_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB5
//------------------------------------------------------------------------------

void wb_interface_set_rgb5(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB5_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB6
//------------------------------------------------------------------------------

void wb_interface_set_rgb6(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB6_ADDR_OFFSET] = a_value;
}

//------------------------------------------------------------------------------
// Functions for the register: RGB7
//------------------------------------------------------------------------------

void wb_interface_set_rgb7(
    uint32_t a_addr_base,
    uint32_t a_value
)
{
    DATA(a_addr_base)[RGB7_ADDR_OFFSET] = a_value;
}

