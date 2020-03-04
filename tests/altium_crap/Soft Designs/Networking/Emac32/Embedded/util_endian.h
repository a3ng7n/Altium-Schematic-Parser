/*****************************************************************************\
|*
|*  VERSION CONTROL:    $Version$   $Date$
|*
|*  IN PACKAGE:         STDENDIAN
|*
|*  COPYRIGHT:          Copyright (c) 2007, Altium
|*
|*  DESCRIPTION:        Define standard endianess support functions
|*
 */

#ifndef _STDENDIAN_H
#define _STDENDIAN_H

#include <stdint.h>

/*
 * Macros to convert from a little endian entity to host format or vice versa
 * (usage of the inline functions below instead is promoted because of the inherent type checking)
 */

#ifdef __BIG_ENDIAN__

#define LITTLE8(val) ((uint8_t) (val))
#define LITTLE16(val) ((uint16_t)(((uint16_t)(val) << 8) | ((uint16_t)(val) >> 8)))
#define LITTLE32(val) ((uint32_t)(((uint32_t)(LITTLE16((uint16_t)(val)) << 16) | ((uint32_t)LITTLE16((uint16_t)((val) >> 16)) >> 16)))

#define BIG8(val) ((uint8_t) (val))
#define BIG16(val) ((uint16_t) (val))
#define BIG32(val) ((uint32_t) (val))

#else

#define BIG8(val) ((uint8_t) (val))
#define BIG16(val) ((uint16_t)(((uint16_t)(val) << 8) | ((uint16_t)(val) >> 8)))
#define BIG32(val) ((uint32_t)(((uint32_t)(BIG16((uint16_t)(val)) << 16) | ((uint32_t)BIG16((uint16_t)(((uint32_t)(val)) >> 16)) >> 16)))

#define LITTLE8(val) ((uint8_t) (val))
#define LITTLE16(val) ((uint16_t) (val))
#define LITTLE32(val) ((uint32_t) (val))

#endif


/*
 * Functions to convert from a little endian entity to host format or vice versa
 */

inline uint8_t little8( uint8_t val )
{
    return val;
}


inline uint16_t little16( uint16_t val )
{
#ifdef __BIG_ENDIAN__
    return (val << 8) | (val >> 8);
#else
    return val;
#endif
}


inline uint32_t little32( uint32_t val )
{
#ifdef __BIG_ENDIAN__
    return (( (uint32_t)little16( (uint16_t)val )) << 16 ) | ( (uint32_t)little16( val >> 16 ));
#else
    return val;
#endif
}


inline uint64_t little64( uint64_t val )
{
#ifdef __BIG_ENDIAN__
    return (( (uint64_t)little32( (uint32_t)val )) << 32 ) | ( (uint64_t)little32( val >> 32 ));
#else
    return val;
#endif
}


/*
 * Functions to convert from a big endian entity to host format or vice versa
 */

inline uint8_t big8( uint8_t val )
{
    return val;
}


inline uint16_t big16( uint16_t val )
{
#ifdef __BIG_ENDIAN__
    return val;
#else
    return (val << 8) | (val >> 8);
#endif
}


inline uint32_t big32( uint32_t val )
{
#ifdef __BIG_ENDIAN__
    return val;
#else
    return (( (uint32_t)big16( val )) << 16 ) | ( (uint32_t)big16( val >> 16 ));
#endif
}


inline uint64_t big64( uint64_t val )
{
#ifdef __BIG_ENDIAN__
    return val;
#else
    return (( (uint64_t)big32( val )) << 32 ) | ( (uint64_t)big32( val >> 32 ));
#endif
}


/*
 * Functions to read a little endian value from an 8-bit aligned address
 */

inline uint8_t read_little8from8( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_little16from8( void * ptr )
{
    return read_little8from8( ptr ) | ( (uint16_t)read_little8from8( (uint8_t *)ptr+1 ) << 8 );
}


inline uint32_t read_little32from8( void * ptr )
{
    return read_little16from8( ptr ) | ( (uint32_t)read_little16from8( (uint8_t *)ptr+2 ) << 16 );
}


inline uint64_t read_little64from8( void * ptr )
{
    return read_little32from8( ptr ) | ( (uint64_t)read_little32from8( (uint8_t *)ptr+4 ) << 32 );
}


/*
 * Functions to read a little endian value from a 16-bit aligned address
 */

inline uint8_t read_little8from16( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_little16from16( void * ptr )
{
    return little16( *(uint16_t *)ptr );
}


inline uint32_t read_little32from16( void * ptr )
{
    return read_little16from16( ptr ) | ( (uint32_t)read_little16from16( (uint16_t *)ptr+1 ) << 16 );
}


inline uint64_t read_little64from16( void * ptr )
{
    return read_little32from16( ptr ) | ( (uint64_t)read_little32from16( (uint16_t *)ptr+2 ) << 32 );
}


/*
 * Functions to read a little endian value from a 32-bit aligned address
 */

inline uint8_t read_little8from32( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_little16from32( void * ptr )
{
    return little16( *(uint16_t *)ptr );
}


inline uint32_t read_little32from32( void * ptr )
{
    return little32( *(uint32_t *)ptr );
}


inline uint64_t read_little64from32( void * ptr )
{
    return read_little32from32( ptr ) | ( (uint64_t)read_little32from32( (uint32_t *)ptr+1 ) << 32);
}


/*
 * Functions to read a big endian value from an 8-bit aligned address
 */

inline uint8_t read_big8from8( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_big16from8( void * ptr )
{
    return ( (uint16_t)read_big8from8( ptr ) << 8 ) | read_big8from8( (uint8_t *)ptr+1 );
}


inline uint32_t read_big32from8( void * ptr )
{
    return ( (uint32_t)read_big16from8( ptr ) << 16 ) | read_big16from8( (uint8_t *)ptr+2 );
}


inline uint64_t read_big64from8( void * ptr )
{
    return ( (uint64_t)read_big32from8( ptr ) << 32 ) | read_big32from8( (uint8_t *)ptr+4 );
}


/*
 * Functions to read a big endian value from a 16-bit aligned address
 */

inline uint8_t read_big8from16( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_big16from16( void * ptr )
{
    return big16( *(uint16_t *)ptr );
}


inline uint32_t read_big32from16( void * ptr )
{
    return ( (uint32_t)read_big16from16( ptr ) << 16 ) | read_big16from16( (uint16_t *)ptr+1 );
}


inline uint64_t read_big64from16( void * ptr )
{
    return ( (uint64_t)read_big32from16( ptr ) << 32 ) | read_big32from16( (uint16_t *)ptr+2 );
}


/*
 * Functions to read a big endian value from a 32-bit aligned address
 */

inline uint8_t read_big8from32( void * ptr )
{
    return *(uint8_t *)ptr;
}


inline uint16_t read_big16from32( void * ptr )
{
    return big16( *(uint16_t *)ptr );
}


inline uint32_t read_big32from32( void * ptr )
{
    return big32( *(uint32_t *)ptr );
}


inline uint64_t read_big64from32( void * ptr )
{
    return ( (uint64_t)read_big32from32( ptr ) << 32 ) | read_big32from32( (uint32_t *)ptr+1 );
}

/*
 * Functions to write a little endian value to an 8-bit aligned address
 */

inline void write_little8to8( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_little16to8( void * ptr, uint16_t val )
{
    write_little8to8( ptr, (uint8_t)val );
    write_little8to8( (uint8_t *)ptr + 1, val >> 8 );
}


inline void write_little32to8( void * ptr, uint32_t val )
{
    write_little16to8( ptr, (uint16_t)val );
    write_little16to8( (uint16_t *)ptr + 1, val >> 16 );
}


inline void write_little64to8( void * ptr, uint64_t val )
{
    write_little32to8( ptr, (uint32_t)val );
    write_little32to8( (uint32_t *)ptr + 1, val >> 32 );
}


/*
 * Functions to write a little endian value to a 16-bit aligned address
 */

inline void write_little8to16( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_little16to16( void * ptr, uint16_t val )
{
    *(uint16_t *)ptr = little16( val );
}


inline void write_little32to16( void * ptr, uint32_t val )
{
    write_little16to16( ptr, (uint16_t)val );
    write_little16to16( (uint16_t *)ptr + 1, val >> 16 );
}


inline void write_little64to16( void * ptr, uint64_t val )
{
    write_little32to16( ptr, (uint32_t)val );
    write_little32to16( (uint32_t *)ptr + 1, val >> 32 );
}


/*
 * Functions to write a little endian value to a 32-bit aligned address
 */

inline void write_little8to32( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_little16to32( void * ptr, uint16_t val )
{
    *(uint16_t *)ptr = little16( val );
}


inline void write_little32to32( void * ptr, uint32_t val )
{
    *(uint32_t *)ptr = little32( val );
}


inline void write_little64to32( void * ptr, uint64_t val )
{
    write_little32to32( ptr, (uint32_t)val );
    write_little32to32( (uint32_t *)ptr + 1, val >> 32 );
}


/*
 * Functions to write a big endian value to an 8-bit aligned address
 */

inline void write_big8to8( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_big16to8( void * ptr, uint16_t val )
{
    write_big8to8( ptr, val >> 8 );
    write_big8to8( (uint8_t *)ptr + 1, (uint8_t)val );
}


inline void write_big32to8( void * ptr, uint32_t val )
{
    write_big16to8( ptr, val >> 16 );
    write_big16to8( (uint16_t *)ptr + 1, (uint16_t)val );
}


inline void write_big64to8( void * ptr, uint64_t val )
{
    write_big32to8( ptr, val >> 32 );
    write_big32to8( (uint32_t *)ptr + 1, (uint32_t)val );
}


/*
 * Functions to write a big endian value to a 16-bit aligned address
 */

inline void write_big8to16( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_big16to16( void * ptr, uint16_t val )
{
    *(uint16_t *)ptr = big16( val );
}


inline void write_big32to16( void * ptr, uint32_t val )
{
    write_big16to16( ptr, val >> 16 );
    write_big16to16( (uint16_t *)ptr + 1, (uint16_t)val );
}


inline void write_big64to16( void * ptr, uint64_t val )
{
    write_big32to16( ptr, val >> 32 );
    write_big32to16( (uint32_t *)ptr + 1, (uint32_t)val );
}


/*
 * Functions to write a big endian value to a 32-bit aligned address
 */

inline void write_big8to32( void * ptr, uint8_t val )
{
    *(uint8_t *)ptr = val;
}


inline void write_big16to32( void * ptr, uint16_t val )
{
    *(uint16_t *)ptr = big16( val );
}


inline void write_big32to32( void * ptr, uint32_t val )
{
    *(uint32_t *)ptr = big32( val );
}


inline void write_big64to32( void * ptr, uint64_t val )
{
    write_big32to32( ptr, val >> 32 );
    write_big32to32( (uint32_t *)ptr + 1, (uint32_t)val );
}


#endif // _STDENDIAN_H
