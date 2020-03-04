//
//	Software Platform Generated File
//	--------------------------------
//

#ifndef _SWPLATFORM_H
#define _SWPLATFORM_H

// Device ID's
#include "devices.h"

// Extra project headers
#include "generic_devices.h"

// Sofware Services
#include <interrupts.h>
#include <timers.h>
#include <timing.h>

// Top Level Stack Items
#include <per_isp1760.h>
#include <per_s29.h>
#include <serial.h>

// Lower Level Stack Items
#include <drv_terminal.h>
#include <per_terminal.h>

// POSIX Device I/O
#include <fcntl.h>

// Global variables to access Software Platform stacks
extern int serial_1;

 // Initialize all stacks in the Software Platform
extern void swplatform_init_stacks(void);

#endif
