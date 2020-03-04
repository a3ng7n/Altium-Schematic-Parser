//
//	Software Platform Generated File
//	--------------------------------
//


#include "swplatform.h"

// Global variables to access Software Platform stacks
int serial_1;

 // Initialize all stacks in the Software Platform
void swplatform_init_stacks(void)
{
    serial_1 = open("/dev/SERIAL_1", O_RDWR);
}
