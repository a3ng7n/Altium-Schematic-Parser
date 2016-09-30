/*****************************************************************************\
|*
|*  IN PACKAGE:         C++ examples
|*
|*  COPYRIGHT:          Copyright (c) 2009, Altium
|*
|*  DESCRIPTION:        C++ example program with operator overloading
|*
|*                      Standard output (stdout) and standard error (stderr)
|*                      are connected to the graphics driver
 */

#include <stdio.h>

class   Complex
{
        int     re_part;
        int     im_part;

public:
        Complex(int re, int im) { re_part=re; im_part = im; }
        Complex(int re) { re_part = re; im_part = 0; }
        Complex() { re_part = 0; im_part = 0; }

        void    Print();

        friend  Complex operator+(Complex, Complex);
        friend  Complex operator-(Complex, Complex);
        friend  Complex operator-(Complex);
};

void    Complex::Print(void)
{
        printf("re = %2d, im = %2d\n", re_part, im_part);
}

Complex operator+(Complex c1, Complex c2)
{
        return Complex(c1.re_part + c2.re_part, c1.im_part + c2.im_part);
}

Complex operator-(Complex c1, Complex c2)
{
        return Complex(c1.re_part - c2.re_part, c1.im_part - c2.im_part);
}

Complex operator-(Complex c1)
{
        return Complex(-c1.re_part, -c1.im_part);
}


//
// Use complex class
//

int     main(void)
{
        Complex c1(1,2);
        Complex c2(2);
        Complex c3;

        printf( "C++ Operator overloading\n" );

        printf( "\nComplex number c1:\n" );
        c1.Print();
        printf( "\nComplex number c2:\n" );
        c2.Print();

        printf( "\n\nCalculate c1 + c2:\n" );
        c3 = c1 + c2;
        c3.Print();

        printf( "\nCalculate c2 - c1:\n" );
        c3 = c2 - c1;
        c3.Print();

        printf( "\nNegate previous:\n" );
        c3 = -c3;
        c3.Print();

        return 0;
}

