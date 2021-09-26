; The C code performs arithmetic operation on variables '_arg1' and '_arg2' and stores result into '_res.'
; Result then checked for correctness. If everything is OK then GREEN border is shown, otherwise - the RED one.

                    DEVICE ZXSPECTRUM48

                    ORG #8000

                    INCLUDE "c_include.inc"

                    DEFINE IS_16BIT 1      ; BYTE/WORD switch
                    DEFINE OPERATION *     ; + - * / %
                    DEFINE ARGUMENT1_VALUE 5
                    DEFINE ARGUMENT2_VALUE 2
                    DEFINE EXPECTED_RESULT ARGUMENT1_VALUE OPERATION ARGUMENT2_VALUE

                    IF IS_16BIT
                        DEFINE VALUE_TYPE DW
                    ELSE
                        DEFINE VALUE_TYPE DB
                    ENDIF

                    ; This hack defines OPSYMBOL = 'OPERATION' (i.e. '+', '-', '*', '/' or '%' symbol).
                    DEFINE QUOTEMARK '
                    DEFINE OPSYMBOL (((QUOTEMARK OPERATION QUOTEMARK) >> 8) & #FF)

start               DI

                    CALL _my_c_proc

                    IF IS_16BIT
                        LD HL,(_res)
                        LD DE,EXPECTED_RESULT
                        OR A
                        SBC HL,DE
                    ELSE
                        LD A,(_res)
                        CP EXPECTED_RESULT
                    ENDIF

                    LD A,8+4 ; Green
                    JR Z,$+4 ; ZF=1 - ok / ZF=0 - error
                    LD A,8+2 ; Red

                    OUT (#FE),A
                    HALT

                    ; Labels that starts with underscore can be accessed from C code using extern declaration (with underscore omitted).
_op                 DB OPSYMBOL
_arg1               VALUE_TYPE ARGUMENT1_VALUE
_arg2               VALUE_TYPE ARGUMENT2_VALUE
_res                VALUE_TYPE 0

                    c_define "IS_16BIT" = IS_16BIT
                    c_include "example.c"

crt0                INCLUDE "crt0.asm"
                    DISPLAY "CRT0 size - ", /D, $-crt0, " bytes."

                    SAVESNA "example.sna", start
                    LABELSLIST "example.l"
