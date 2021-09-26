#if IS_16BIT
    typedef unsigned short value_type;
#else
    typedef unsigned char value_type;
#endif

extern unsigned char op;

extern value_type arg1;
extern value_type arg2;
extern value_type res;

void my_c_proc()
{
    switch (op)
    {
    case '+':
        res = arg1 + arg2;
        break;
    case '-':
        res = arg1 - arg2;
        break;
    case '*':
        res = arg1 * arg2;
        break;
    case '/':
        res = arg1 / arg2;
        break;
    case '%':
        res = arg1 % arg2;
        break;
    }
}
