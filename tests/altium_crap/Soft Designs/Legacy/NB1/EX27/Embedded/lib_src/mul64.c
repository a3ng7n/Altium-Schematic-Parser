long long       __mul64(long long arg1, long long arg2)
{
        long long       result = 0;
        unsigned long long      multiplier = (unsigned long long)arg2;

        while (multiplier)
        {
                if (multiplier & 1)
                {
                        result = result + arg1;
                }
                arg1 <<= 1;
                multiplier >>= 1;
        }

        return result;
}

