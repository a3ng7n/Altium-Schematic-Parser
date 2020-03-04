#define BYTE unsigned char

void LongDelay (void)
{
    unsigned int i=0;
    unsigned int j=0;

    while(i<=0xFFF)
    {
       i++;
       while(j<=0xFFF)
       {
          j++;
       }
    }
}

void main (void)
{
    BYTE i=0x0;

    while (1)
    {
       P0=i;
       LongDelay();
       i++;
       if (i>=0xFF)
       {
          i=0;
       }
    }
}

