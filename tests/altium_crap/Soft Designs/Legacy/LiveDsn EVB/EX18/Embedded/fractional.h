// fractional.h

#ifndef fractional_h
#define fractional_h


#define _fract int
#define mpy(sint,frac) ( (_fract) (( (long)sint*frac)>>15) )
#define floatf(f) ( (_fract)(f*32768))
#define freqf(f,rate) ( floatf(((double)f*2)/rate) )
#define FRACT_MAX  0x7FFE
#define FRACT_MIN  0x8000

#define floatf2(f) ( (_fract)(f*16384))

#define PI ((double)3.1415927)



#define floatf_IIR(f) ( (_fract)(f*(16384)))
#define IIRscale 14
#endif // fractional_h
