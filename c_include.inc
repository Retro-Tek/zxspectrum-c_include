; MIT License
; 
; Copyright (c) 2021 Dimitry Rubtsov (dimitry.rubtsov@gmail.com)
; 
; Permission is hereby granted, free of charge, to any person obtaining a copy
; of this software and associated documentation files (the "Software"), to deal
; in the Software without restriction, including without limitation the rights
; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
; copies of the Software, and to permit persons to whom the Software is
; furnished to do so, subject to the following conditions:
; 
; The above copyright notice and this permission notice shall be included in all
; copies or substantial portions of the Software.
; 
; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
; SOFTWARE.

;                    DEFINE C_USE_ZSDCC
                    DEFINE C_COMPILER_ARGS --fsigned-char

                    LUA PASS1
                        sj.shellexec('mkdir -p tmp')

                        _c_defines = {}

                        for i, reg in ipairs({'a','b','c','d','e','h','l','i','r','xl','ixl','xh','ixh','yl','iyl','yh','iyh','af','bc','de','hl','sp','ix','iy'}) do
                            _c_defines[reg] = reg..'_'
                            reg = string.upper(reg)
                            _c_defines[reg] = reg..'_'
                        end

                        function _c_define(name, value)
                            name = string.gsub(name, '^"', '')
                            name = string.gsub(name, '"$', '')

                            if string.find(name, '^[^%a_]') or string.find(name, '[^%a%d_]') then
                                sj.error('Invalid macro name: \"'..name..'\"')
                            end

                            _c_defines[name] = value
                        end
                    ENDLUA

                    ; Defines as macro for a C code. Has two forms:
                    ; 1) c_define "NAME"         ; #define NAME
                    ; 2) c_define "NAME" = VALUE ; #define NAME calc(VALUE)
                    ; Quotemarks around NAME are optional, but they prevents SjAsmPlus to do any substitutions.
                    ; VALUE is an arbitrary SjAsmPlus expression that will be calculated and the result (always INTEGER) will be used.
                    ; All labels used in VALUE expression MUST be defined BEFORE c_define is called. Otherwise they will silently became 0.
                    MACRO c_define NAMEVALUE
                        DEFINE _C_DEFINE_NAMEVALUE! NAMEVALUE

                        LUA ALLPASS
                            arg = sj.get_define('_C_DEFINE_NAMEVALUE!')
                            pos = string.find(arg, '=')

                            if pos then
                                sj.insert_define('_C_DEFINE_NAME!', string.sub(arg, 1, pos-1))
                                sj.insert_define('_C_DEFINE_VALUE!', string.sub(arg, pos+1))
                            else
                                sj.insert_define('_C_DEFINE_NAME!', arg)
                            end
                        ENDLUA

                        IFDEF _C_DEFINE_VALUE!
                            c_define1 _C_DEFINE_NAME!, _C_DEFINE_VALUE!
                            UNDEFINE _C_DEFINE_VALUE!
                        ELSE
                            c_define0 _C_DEFINE_NAME!
                        ENDIF
                        UNDEFINE _C_DEFINE_NAME!

                        UNDEFINE _C_DEFINE_NAMEVALUE!
                    ENDM

                    ; Same as c_define NAME
                    MACRO c_define0 NAME
                        DEFINE _C_DEFINE0_NAME! NAME
                        LUA PASS1
                            _c_define(sj.get_define('_C_DEFINE0_NAME!'), '')
                        ENDLUA
                        UNDEFINE _C_DEFINE0_NAME!
                    ENDM

                    ; Same as c_define "NAME" = VALUE
                    MACRO c_define1 NAME, VALUE
                        DEFINE _C_DEFINE1_NAME! NAME
.value                  EQU VALUE
                        LUA PASS1
                            _c_define(sj.get_define('_C_DEFINE1_NAME!'), sj.get_label('.value'))
                        ENDLUA
                        UNDEFINE _C_DEFINE1_NAME!
                    ENDM

                    ; Undefines a macro. Does nothing on absence.
                    MACRO c_undefine NAME
                        DEFINE _C_UNDEFINE_NAME! NAME
                        LUA PASS1
                            _c_define(sj.get_define('_C_UNDEFINE_NAME!'), nil)
                        ENDLUA
                        UNDEFINE _C_UNDEFINE_NAME!
                    ENDM

                    ; Undefines all macros.
                    MACRO c_undefine_all
                        LUA PASS1
                            _c_defines = {}
                        ENDLUA
                    ENDM

                    ; Translates FILENAME into asm and then INCLUDEs result.
                    MACRO c_include FILENAME
                        DEFINE _C_INCLUDE_FILENAME! FILENAME

                        LUA ALLPASS
                            file = sj.get_define('_C_INCLUDE_FILENAME!')
                            file = string.gsub(file, '^"', '')
                            file = string.gsub(file, '"$', '')

                            outfile = 'tmp/'..file..'.asm'

                            if tonumber(sj.get_define('__PASS__')) == 1 then
                                slash = string.find(file, '/[^/]*$')
                                if slash and slash>1 then
                                    sj.shellexec('mkdir -p "tmp/'..string.sub(file, 1, slash-1)..'"')
                                end

                                cmd = sj.get_define('C_USE_ZSDCC') and 'zsdcc' or 'sdcc'
                                cmd = cmd..' -S -mz80 --asm=z80asm'
                                cmd = cmd..' --oldralloc --reserve-regs-iy' -- Prevents SDCC from producing broken code (hopefully).

                                args = sj.get_define('C_COMPILER_ARGS')
                                if args then
                                    cmd = cmd..' '..args
                                end

                                -- There will be a problem on Windows due to command line length limitation (8191 chars max).
                                for name, value in pairs(_c_defines) do
                                    cmd = cmd..' -D'..name
                                    if value ~= '' then
                                        cmd = cmd..'='..value
                                    end
                                end

                                cmd = cmd..' -o tmp/out.asm '..file

                                print('Running: '..cmd)
                                sj.shellexec(cmd)
                                -- Stripping leading dot`s from labels
                                sj.shellexec([[sed -i 's/^\.//' tmp/out.asm]])
                                -- Fixing SDCC bugs
                                sj.shellexec([[sed -i 's/ ~ \$FF\>/ \& 255/' tmp/out.asm]])
                                sj.shellexec([[sed -i 's/\<DEFB \(\$[0-9A-Fa-f]\{4\}\)\>/DEFW \1/' tmp/out.asm]])
                                sj.shellexec([[sed -i 's/\<DEFB \([^\$].*\)\>/DEFW \1/' tmp/out.asm]]) -- assuming labels to be 16-bit
                                -- Fixing initialized data
                                sj.shellexec([[sed -i -z 's/[ \t]*;[ \t]*Area[ \t]\+INITIALIZED[ \t]*\n[^;]*/ WHILE 0\n& EDUP\n/' tmp/out.asm]]) -- IF/ENDIF will not compile (SjAsmPlus bug)
                                sj.shellexec([[sed -i 's/^__xinit_//' tmp/out.asm]])
                                -- Done
                                sj.shellexec('cp tmp/out.asm "'..outfile..'"')
                            else
                                sj.shellexec('cp "'..outfile..'" tmp/out.asm')
                            end
                        ENDLUA

                        DEFINE _ ; Somehow, this hack will strip a single leading underscore from every identifier.
                        DEFINE MODULE c_skip1
                        DEFINE XREF c_skip1
                        DEFINE XDEF c_skip1

                        OPT push --syntax=aF

.module                 INCLUDE "tmp/out.asm"
                        DISPLAY "Including \"", FILENAME, "\" - compiled size ", /D, $-.module, " bytes."

                        OPT pop

                        UNDEFINE _
                        UNDEFINE MODULE
                        UNDEFINE XREF
                        UNDEFINE XDEF

                        LUA ALLPASS
                            sj.shellexec('rm tmp/out.asm')
                        ENDLUA

                        UNDEFINE _C_INCLUDE_FILENAME!
                    ENDM

                    MACRO c_skip1 ARG
                    ENDM

                    MACRO .optsdcc ARG
                    ENDM
