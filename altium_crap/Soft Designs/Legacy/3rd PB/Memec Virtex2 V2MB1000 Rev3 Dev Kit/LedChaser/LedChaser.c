#define SPEED P2_0
#define DISPLAY_ON P2_1
#define DIRECTION P2_3

const short unsigned PMASK = 0xe0 ;

unsigned char

       __rom y[8][8] = {
       { 255, 127, 63, 31, 15, 4, 2, 0 },
       { 255, 0, 0, 0, 255, 0, 0, 0 },
       { 0, 1, 3, 15, 31, 15, 3, 1 },
       { 255, 63, 15, 0, 15, 63, 255, 0 },
       { 255, 127, 31, 2, 0, 0, 0, 0 },
       { 255, 0, 0, 0, 0, 0, 0, 0 },
       { 0, 1, 3, 15, 31, 63, 127, 255 },
       { 255, 127, 0, 127, 255, 255, 255, 255 }
       };

void main (void)
{
    unsigned char

    v = 1,
    x = 0,
    m = 0;

    short unsigned u = 0, i, j;

    for(;;)
    {
       if ( DISPLAY_ON )
       {
          if ( SPEED )
          {
             j = 0X01FF;
          }
          else
          {
             j = 0X07FF;
          }
          if ( u == (j) )
          {
             if ( DIRECTION )
             {
                v = __ror( v, 1 );
                u = 0;
             }
             else
             {
                 v = __rol( v, 1 );
                 u = 0;
             }
          }
          u++;
          for ( x = 8; x > 0; x-- )
          {
              m = (PMASK & P2);
              m = __ror(m,5);
              P0 = y[m][x-1];
              __asm("nop");
              __asm("nop");
              P1 = v;
              __asm("nop");
              P1 = 0;
              if ( DIRECTION )
              {
                 v = __ror( v, 1 );
              }
              else
              {
                 v = __rol( v, 1 );
              }
          }
          for ( i = 0; i == 0xFFFE; i++ )
          {
              __asm("nop");
          }
       }
    }
}
