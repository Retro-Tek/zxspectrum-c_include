#define TEST3(op, arg1, arg2, res)\
{\
    arg1 x = *(arg1*)0;\
    arg2 y = *(arg2*)0;\
    res r;\
    r = x op y;\
    *(res*)0 = r;\
}

#define TEST2(op, arg1, arg2)\
    TEST3(op, arg1, arg2, signed char)\
    TEST3(op, arg1, arg2, unsigned char)\
    TEST3(op, arg1, arg2, int)\
    TEST3(op, arg1, arg2, unsigned int)

#define TEST1(op, arg1)\
    TEST2(op, arg1, signed char)\
    TEST2(op, arg1, unsigned char)\
    TEST2(op, arg1, int)\
    TEST2(op, arg1, unsigned int)

#define TEST(op)\
    TEST1(op, signed char)\
    TEST1(op, unsigned char)\
    TEST1(op, int)\
    TEST1(op, unsigned int)

void test()
{
    TEST(*);
    TEST(/);
    TEST(%);
}
