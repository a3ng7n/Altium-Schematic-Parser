unsigned long long      __udiv64(unsigned long long dividend, unsigned long long divisor)
{
        unsigned long long      result = dividend;
        unsigned long long      remainder = 0;
        unsigned int            carry;
        unsigned int            new_carry;
        int                     cnt;

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

        for (cnt = 0; cnt < 64; cnt++)
        {
                /* highest bit of result will get shifted out and
                 * is the new value of carry
                 */
                new_carry = result >> 63;
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

long long       __div64(long long dividend, long long divisor)
{
        long long       result;
        long long       sign = dividend ^ divisor;
        
        if (dividend < 0) dividend = -dividend;
        if (divisor < 0) divisor = -divisor;

        result = __udiv64(dividend, divisor);

        if (sign & 0x8000000000000000LL) 
        {
                result = -result;
        }
        
        return result;
}


