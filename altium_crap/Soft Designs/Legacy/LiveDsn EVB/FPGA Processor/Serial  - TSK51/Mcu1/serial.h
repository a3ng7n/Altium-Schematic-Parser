
#ifndef SERIAL_H
#define SERIAL_H

//------------------------------------
// Serial Port functions
//------------------------------------

void serial_init(char rate);
char serial_getch(void);
void serial_putch(char c);
void serial_puts(char __rom *p);

#endif
