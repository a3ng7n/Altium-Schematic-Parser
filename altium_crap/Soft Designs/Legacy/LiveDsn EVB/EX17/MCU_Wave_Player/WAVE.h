#ifndef __WAVE_H__
#define __WAVE_H__



typedef struct {
    unsigned char id[4];  // identifier string = "RIFF"
    unsigned long length; // file length after this header (file length - 8);
} RiffHeaderType;

// the wave file consists of the following wave header, followed by a wave format chunk, followed by a data chunk

typedef struct {
    RiffHeaderType RiffHeader;
    unsigned char WaveId[4];   // wave file identifier = "WAVE"
} WaveHeaderType;

typedef struct {
  unsigned char  chunkID[4];  // "fmt " for wave format
  unsigned long  chunkSize;   // size of rest of chunk (chunksize -8)

  unsigned int   wFormatTag;  // should be 1 if no compression is used
  unsigned int   wChannels;   // 1 = mono, 2 = stereo, 4 = quadro etc.
  unsigned long  dwSamplesPerSec;  //sample rate in samples/second
  unsigned long  dwAvgBytesPerSec; //  dwSamplesPerSec * wBlockAlign
  unsigned int   wBlockAlign;      // size of sample frame in bytes (eg 16 bit mono is 2 bytes)
  unsigned int   wBitsPerSample;   // nomen est omen
} FormatChunkType;

typedef struct {
  char           chunkID[4];   // "data" for waveform data
  long           chunkSize;    // number of data bytes to follow this chunk (not including the chunk)
} DataChunkType;


extern DataChunkType DC;                  // current data chunk
extern FormatChunkType WaveFormat;        // current wave format chunk


//-------------------------------
// reads wave file header
// returns 0    if no errors
//         1    if unsupported file type
//         0xFF if timeout
//-------------------------------
unsigned char WaveReadHeader(void);

//-------------------------------
// reads wave file format chunk
// returns 0    if no errors
//         1    if unsupported file type
//         0xFF if timeout
//-------------------------------
unsigned char WaveReadFormat(void);

//--------------------------------------
// reads the wave data chunk
// returns 0 : success
//         1 : memory limit exceeded
//--------------------------------------
unsigned char WaveReadData(void);




#endif
