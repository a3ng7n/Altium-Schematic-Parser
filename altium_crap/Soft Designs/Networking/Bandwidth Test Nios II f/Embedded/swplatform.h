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
#include <pthread.h>
#include <sched.h>
#include <semaphore.h>

// Top Level Stack Items
#include <lwip.h>
#include <lwipopts.h>
#include <serial.h>

// Lower Level Stack Items
#include <drv_emac32.h>
#include <drv_uart8.h>
#include <per_emac32.h>
#include <per_uart8.h>
#include <ethernet.h>

// POSIX Device I/O
#include <fcntl.h>

// Global variables to access Software Platform stacks
extern int      serial_1;
extern lwip_t * lwip_1;

 // Initialize all stacks in the Software Platform
extern void swplatform_init_stacks(void);

#endif
