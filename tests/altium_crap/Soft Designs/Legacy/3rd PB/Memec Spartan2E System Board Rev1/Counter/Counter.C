#define BYTE unsigned char

void LongDelay (void)
{
    unsigned int i=0;
    for(i=0;i<0xFFFF;i++)
    {
       __asm("nop");
    }
}

void VeryLongDelay (void)
{
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
    LongDelay();
}

void main (void)
{
    BYTE i=0x0;
    BYTE direction=0x0;

    while (1)
    {
       P0=i;

       if(P0==0)
       {
           direction++;
           direction=direction%2;
       }
       VeryLongDelay();
       if (direction==0)
       {
          i++;
          if (i>=0x0A)
          {
             i=0;
          }
       }
       else
       {
          i--;
          if (i<=0x00)
          {
             i=9;
          }
       }
    }
}

