#ifndef ntype_h
    #define ntype_h

    #define TRUE 1
    #define FALSE 0
        
    // Keyboard key definitions:
    #define SPCE0 0xE0
    #define SPACE 0x29
    #define ARROWUP 0x75
    #define ARROWDOWN 0x72
    #define ARROWLEFT 0x6B
    #define ARROWRIGHT 0x74

    #define ENABLE_INTERRUPTS() EA = TRUE
    #define DISABLE_INTERRUPTS() EA = FALSE

	#define P3_0  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b0)
	#define P3_1  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b1)
	#define P3_3  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b3)
	#define P3_5  ((*(__bsfr volatile __bitstruct_t *)0xB0).__b5)
	
    #define CLOCK_FREQ 40000000

    #define WORD unsigned short
    #define BYTE unsigned char
    #define word unsigned short
    #define byte unsigned char

    #define V_PHYSICAL_HEIGHT 600

    #define VSIZENC 60000
    

	void ClearDisplay(void);
	void InitC51Graphics(void);
	void InitVideo(void);
	void LineTo(short x2, short y2, byte color);
	void Rectangle(short x1, short y1, short x2, short y2, BYTE color);

#endif
