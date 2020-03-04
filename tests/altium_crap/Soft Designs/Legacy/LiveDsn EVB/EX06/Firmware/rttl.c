//-------------------------------------------------
//  RTTTL (Ring Tone Text Transfer Language) player
//-------------------------------------------------

#include "rttl.h"

#define DATA_PORT P3
#define CONTROL_PORT P2

//--------------------------------------------

void sound(unsigned int divider)
{
     DATA_PORT = divider & 0xff;       //Latch low divisor value
     CONTROL_PORT &= 0xBF;
     CONTROL_PORT |= 0x40;

     DATA_PORT = (divider>>8) & 0xff;  //Latch high data and updat word.
     CONTROL_PORT &= 0x7F;
     CONTROL_PORT |= 0x80;
}

//--------------------------------------------

void Play(unsigned short note,unsigned int octave)
{
    unsigned int divider=note;

    switch(octave)
    {
    case 4:   divider = note;                break;
    case 5:   divider = note>>1;             break;
    case 6:   divider = note>>2;             break;
    case 7:   divider = note>>4;             break;
    }
    sound(divider);
}

//--------------------------------------------

static enum {HEADER,REPEAT,NOTE,WAIT,END} state=HEADER;
static const char __rom *p=0;
static const char __rom *tuneData=0;

unsigned char bps;
unsigned int defaultOctave = 0;
unsigned int defaultDuration = 0;

unsigned int i=0;
unsigned char loop = 0;
unsigned int octave = 0;
unsigned int duration = 0;
unsigned int note = 0;

//--------------------------------------------
// Cancel existing tune and reset to new tune
//--------------------------------------------

void resetTune(char __rom *tune)
{
     p=tune;
     sound(0);
     state = HEADER;
}

//--------------------------------------------
// Tune player
//--------------------------------------------
void PlayTune(void)
{
    static int period;

    static char lastTimer=0;
    static char timer=0;

    switch(state)
    {
    case HEADER:
     //Skip tune name
     while (*p != ':')
        p++;
     p++; //Skip ':'

     //Parse control block for default paramters
     defaultDuration = 4;
     defaultOctave = 6;
     bps = 63;
     loop=0;
     char par=0, value=0;
     while (*p != ':')
     {
        par=*(p++); //Get paramter name
        if (*p=='=')
        {
           p++; //Skip '='

           //Parse value
           value = 0;
           while (*p >='0' && *p<='9')
              value = (value*10)+*(p++)-'0';

           //Assign value to parameter
           switch (par)
           {
           case 'd': defaultDuration = value;   break;
           case 'o': defaultOctave = value;     break;
           case 'b': bps= value;                break;
           case 'l': loop = value;              break;
           }
        }
     }
     p++; //Skip ':'
     tuneData = p;
     state = NOTE;
     break;

     case REPEAT:
        if (loop==0)
           state = END;
        else
        {
            p=tuneData;
            if (loop!=15) //Endless repeat?
               loop--;
            state = NOTE;
        }
        break;

     case NOTE:
        // Parse duration
        duration = defaultDuration;;
        if (*p >='0' && *p <= '9')
        {
           duration = *(p++)-'0';
           if (*p >='0' && *p <= '9')
             duration = (duration*10)+(*(p++)-'0');
        }

        //Parse note               // A    B     C    D    E    F    G     A#     C#   D#     F#   G#
       const static int notes[] = {11364,10123,9555,8513,7584,7158,6378,10726,0,9019,8035,0,6757,6024};
       i=0;
       note = 0;
       if (*p>='a' && *p<='g')
       {
          i=*(p++)-'a';
          if (*p=='#')
          {
             p++;
             i+=7;
          }
          note = notes[i];
       }
       else if (*p == 'p')
          p++;

       //Parse octave
       octave = defaultOctave;
       if (*p >='4' && *p<='7')
          octave = *(p++)-'0';

        //Parse duration modifier ('.' means duration is 1.5 x longer)
       if (*p=='.')
       {
          duration = (3*duration)/4;
          p++;
       }
       Play(note, octave);
       period = 400/duration;
       state = WAIT;
     break;

     case WAIT:
      timer = P2 & 0x08;
      if (timer != lastTimer)
           period--;
      lastTimer = timer;
      if (period==1)
      {
         if (*(p++) == ',')
            state = NOTE;
         else
         {
            Play(0, 4);
            state = REPEAT;
         }
       }
       else

      break;

     case END:
       break;
     }
}
