
#ifndef LEDS_H_
#define LEDS_H_

#include <sys/types.h>
#include "devices.h"
#include "ioport.h"
#include <drv_ioport.h>

#define IS_OUTPUT_TYPE(port)

#define IOPORT_GET_BIT(drv,port,bit)  					\
				ioport_get_value(drv,port) & (1 << bit)		\
//			#if IS_OUTPUT_TYPE(port)						\
				#error Cannot Get value of an OUTPUT port \
			#else											\
				ioport_get_value(drv,port) & (1 << bit)		\
			#endif											\

#define IOPORT_SET_BIT(drv,port,bit)  ioport_set_value(drv,port,ioport_get_value(drv,port) |  (1 << bit))
#define IOPORT_CLR_BIT(drv,port,bit)  ioport_set_value(drv,port,ioport_get_value(drv,port) & ~(1 << bit))
#define IOPORT_FLIP_BIT(drv,port,bit) ioport_set_value(drv,port,ioport_get_value(drv,port) ^  (1 << bit))

extern ioport_t * port;

#endif /* LEDS_H_ */
