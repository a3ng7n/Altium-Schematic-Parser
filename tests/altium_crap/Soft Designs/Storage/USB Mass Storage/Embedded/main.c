/*
 * USB Mass Storage Nanoboard SD card example
 *
 * This example allows the NB2 to be used as a mass storage device.
 */

#include <usb_msd.h>

#include "devices.h"

void main(void)
{
    usb_msd_t *msd;

    msd = msd_open(USB_MSD_1);

    while (true)
    {
        msd_handle_request(msd);
    }
}

