#ifndef __WSWAP_H__
#define __WSWAP_H__

//-----------------------------------------------------------------------------
// swaps byte order for converting big-endian ints to little-endians and vice versa
// starting at p for numbytes bytes
//-----------------------------------------------------------------------------
void Wswap(void *p, unsigned char numbytes);



#endif  // __WSWAP_H__

