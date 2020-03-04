
#ifndef SEVEN_SEGMENT_H
#define SEVEN_SEGMENT_H

//------------------------------------
// Seven Segment write
//------------------------------------

void seven_setdigit(char position, char data);
void seven_clearall(void);
void seven_putch(char position, char symbol);
void seven_putlong(unsigned long value);
void seven_puts(char pos,  const char __rom *text);

#endif

