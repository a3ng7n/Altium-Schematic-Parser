unsigned int    __udiv32(unsigned int dividend, unsigned int divisor)
{
        unsigned int    result = dividend;
        unsigned int    remainder = 0;
        unsigned int    carry;
        unsigned int    new_carry;
        int             cnt;

        if (divisor == 1)
        {
                goto end;
        }

        if (dividend < divisor)
        {
                result = 0;
                remainder = dividend;
                goto end;
        }

        remainder = 0;
        carry = 0;

        for (cnt = 0; cnt < 32; cnt++)
        {
                /* highest bit of result will get shifted out and
                 * is the new value of carry
                 */
                new_carry = result >> 31;
                result = (result << 1) | carry;
                carry = new_carry;
                
                remainder = (remainder << 1) | carry;
                carry = 0;
                if (remainder >= divisor)
                {
                        remainder = remainder - divisor;
                        carry = 1;
                }
        }

        result = (result << 1) | carry;
end:
        return result;
}

int     __div32(int dividend, int divisor)
{
        int     result;
        int     sign = dividend ^ divisor;
        
        if (dividend < 0) dividend = -dividend;
        if (divisor < 0) divisor = -divisor;

        result = __udiv32(dividend, divisor);

        if (sign & 0x80000000) 
        {
                result = -result;
        }
        
        return result;
}
