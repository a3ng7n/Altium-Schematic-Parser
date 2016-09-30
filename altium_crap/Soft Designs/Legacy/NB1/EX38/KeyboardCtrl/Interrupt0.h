#ifndef _INTERRUPT0_H
	#define _INTERRUPT0_H

	void InitInterrupt0(void);
	void EnableInt0Interrupt( void );
	void DisableInt0Interrupt( void );
	
	extern void Interrupt0( void );  /* the function Interrupt0() will be called everytime int0 occured */

#endif /* _INTERRUPT0_H */
