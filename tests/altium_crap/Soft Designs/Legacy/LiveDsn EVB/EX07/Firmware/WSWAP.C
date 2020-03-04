#include "wswap.h"

//-----------------------------------------------------------------------------
// swaps byte order for converting big-endian ints to little-endians and vice versa
// starting at p for numbytes bytes
//-----------------------------------------------------------------------------
void Wswap(void *p, unsigned char numbytes)
{
    unsigned char c;
    unsigned char *pc = (unsigned char *) p;
    unsigned char *pe = (unsigned char *) (((unsigned char *)p)+numbytes-1);
    numbytes /= 2;
    while (numbytes--)
    {
      c = *pc;
      *pc = *pe;
      *pe=c;
      pe--;
      pc++;
    }
}


