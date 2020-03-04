//------------------------------------------------------------------------------
// Generated: 3/06/2009 11:56:40 AM
//------------------------------------------------------------------------------

#ifndef __LEDCTRL__WB_INTERFACE_H__
#define __LEDCTRL__WB_INTERFACE_H__

#include <stdint.h>

//------------------------------------------------------------------------------
// Note: a_addr_base is the base address of the custom wishbone interface,
// defined in hardware.h
//------------------------------------------------------------------------------

//------------------------------------------------------------------------------
// Functions for the register: RGB0
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb0(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB1
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb1(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB2
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb2(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB3
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb3(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB4
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb4(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB5
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb5(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB6
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb6(
    uint32_t a_addr_base,
    uint32_t a_value
);

//------------------------------------------------------------------------------
// Functions for the register: RGB7
//------------------------------------------------------------------------------

extern void wb_interface_set_rgb7(
    uint32_t a_addr_base,
    uint32_t a_value
);


#endif // __LEDCTRL__WB_INTERFACE_H__
