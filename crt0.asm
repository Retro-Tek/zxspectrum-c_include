___sdcc_call_hl     IFUSED
                        JP (HL)
                    ENDIF

___sdcc_call_iy     IFUSED
                        JP (IY)
                    ENDIF


;  The code below was extracted from SDCC.
;  Copyright owners are mentioned before corresponding code parts.

;--------------------------------------------------------------------------
;  This library is free software; you can redistribute it and/or modify it
;  under the terms of the GNU General Public License as published by the
;  Free Software Foundation; either version 2, or (at your option) any
;  later version.
;
;  This library is distributed in the hope that it will be useful,
;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;  GNU General Public License for more details.
;
;  You should have received a copy of the GNU General Public License 
;  along with this library; see the file COPYING. If not, write to the
;  Free Software Foundation, 51 Franklin Street, Fifth Floor, Boston,
;   MA 02110-1301, USA.
;
;  As a special exception, if you link this library with other files,
;  some of which are compiled with SDCC, to produce an executable,
;  this library does not by itself cause the resulting executable to
;  be covered by the GNU General Public License. This exception does
;  not however invalidate any other reasons why the executable file
;   might be covered by the GNU General Public License.
;--------------------------------------------------------------------------


;--------------------------------------------------------------------------
;  mulchar.s
;
;  Copyright (c) 2017, Philipp Klaus Krause
;--------------------------------------------------------------------------

; unsigned char x unsigned char multiplication is done by code generation.

; operands have different sign

__mulsuchar:
        ld      hl,2+1
        ld      b, h
        add     hl,sp

        ld      e,(hl)
        dec     hl
        ld      c,(hl)
        jr      signexte

__muluschar:
        ld      hl,2
        ld      b, h
        add     hl,sp

        ld      e,(hl)
        inc     hl
        ld      c,(hl)
        jr      signexte

__mulschar:
        ld      hl,2+1
        add     hl,sp

        ld      e,(hl)
        dec     hl
        ld      c,(hl)

        ;; Need to sign extend before going in.
        ld      a,c
        rla
        sbc     a,a
        ld      b,a
signexte:
        ld      a,e
        rla
        sbc     a,a
        ld      d,a

        jp      __mul16


;--------------------------------------------------------------------------
;  mulchar.s
;
;  Copyright (C) 2000, Michael Hope
;--------------------------------------------------------------------------

__mulint:
        pop     af
        pop     bc
        pop     de
        push    de
        push    bc
        push    af

	;; 16-bit multiplication
	;;
	;; Entry conditions
	;; bc = multiplicand
	;; de = multiplier
	;;
	;; Exit conditions
	;; hl = less significant word of product
	;;
	;; Register used: AF,BC,DE,HL
__mul16::
	xor	a,a
	ld	l,a
	or	a,b
	ld	b,16

        ;; Optimise for the case when this side has 8 bits of data or
        ;; less.  This is often the case with support address calls.
        jr      NZ,2F
        ld      b,8
        ld      a,c
1:
        ;; Taken from z88dk, which originally borrowed from the
        ;; Spectrum rom.
        add     hl,hl
2:
        rl      c
        rla                     ;DLE 27/11/98
        jr      NC,3F
        add     hl,de
3:
        djnz    1B
        ret


;--------------------------------------------------------------------------
;  divunsigned.s
;
;  Copyright (C) 2000-2012, Michael Hope, Philipp Klaus Krause, Marco Bodrato
;--------------------------------------------------------------------------

        ;; Originally from GBDK by Pascal Felber.

__divuint:
        pop     af
        pop     hl
        pop     de
        push    de
        push    hl
        push    af

        jr      __divu16

__divuchar:
        ld      hl,2+1
        add     hl,sp

        ld      e,(hl)
        dec     hl
        ld      l,(hl)

        ;; Fall through
__divu8::
        ld      h,0x00
        ld      d,h
        ; Fall through to __divu16

        ;; unsigned 16-bit division
        ;;
        ;; Entry conditions
        ;;   HL = dividend
        ;;   DE = divisor
        ;;
        ;; Exit conditions
        ;;   HL = quotient
        ;;   DE = remainder
        ;;   carry = 0
        ;;   If divisor is 0, quotient is set to "infinity", i.e HL = 0xFFFF.
        ;;
        ;; Register used: AF,B,DE,HL
__divu16::
        ;; Two algorithms: one assumes divisor <2^7, the second
        ;; assumes divisor >=2^7; choose the applicable one.
        ld      a,e
        and     a,0x80
        or      a,d
        jr      NZ,.morethan7bits
        ;; Both algorithms "rotate" 24 bits (H,L,A) but roles change.

        ;; unsigned 16/7-bit division
.atmost7bits:
        ld      b,16           ; bits in dividend and possible quotient
        ;; Carry cleared by AND/OR, this "0" bit will pass trough HL.[*]
        adc     hl,hl
.dvloop7:
        ;; HL holds both dividend and quotient. While we shift a bit from
        ;;  MSB of dividend, we shift next bit of quotient in from carry.
        ;; A holds remainder.
        rla

        ;; If remainder is >= divisor, next bit of quotient is 1.  We try
        ;;  to compute the difference.
        sub     a,e
        jr      NC,.nodrop7     ; Jump if remainder is >= dividend
        add     a,e             ; Otherwise, restore remainder
        ;; The add above sets the carry, because sbc a,e did set it.
.nodrop7:
        ccf                     ; Complement borrow so 1 indicates a
                                ;  successful substraction (this is the
                                ;  next bit of quotient)
        adc     hl,hl
        djnz    .dvloop7
        ;; Carry now contains the same value it contained before
        ;; entering .dvloop7[*]: "0" = valid result.
        ld      e,a             ; DE = remainder, HL = quotient
        ret

.morethan7bits:
        ld      b,9            ; at most 9 bits in quotient.
        ld      a,l             ; precompute the first 7 shifts, by
        ld      l,h             ;  doing 8
        ld      h,0
        rr      l               ;  undoing 1
.dvloop:
        ;; Shift next bit of quotient into bit 0 of dividend
        ;; Shift next MSB of dividend into LSB of remainder
        ;; A holds both dividend and quotient. While we shift a bit from
        ;;  MSB of dividend, we shift next bit of quotient in from carry
        ;; HL holds remainder
        adc     hl,hl           ; HL < 2^(7+9), no carry, ever.

        ;; If remainder is >= divisor, next bit of quotient is 1. We try
        ;;  to compute the difference.
        sbc     hl,de
        jr      NC,.nodrop      ; Jump if remainder is >= dividend
        add     hl,de           ; Otherwise, restore remainder
	;; The add above sets the carry, because sbc hl,de did set it.
.nodrop:
        ccf                     ; Complement borrow so 1 indicates a
                                ;  successful substraction (this is the
                                ;  next bit of quotient)
        rla
        djnz    .dvloop
        ;; Take care of the ninth quotient bit! after the loop B=0.
        rl      b               ; BA = quotient
        ;; Carry now contains "0" = valid result.
        ld      d,b
        ld      e,a             ; DE = quotient, HL = remainder
        ex      de,hl           ; HL = quotient, DE = remainder
        ret


;--------------------------------------------------------------------------
;  divsigned.s
;
;  Copyright (C) 2000-2010, Michael Hope, Philipp Klaus Krause
;--------------------------------------------------------------------------

__divsint:
        pop     af
        pop     hl
        pop     de
        push    de
        push    hl
        push    af

        jp      __div16

__divschar:
        ld      hl, 2+1
        add     hl, sp

        ld      e, (hl)
        dec     hl
        ld      l, (hl)

__div8::
        ld      a, l            ; Sign extend
        rlca
        sbc     a,a
        ld      h, a
__div_signexte::
        ld      a, e            ; Sign extend
        rlca
        sbc     a,a
        ld      d, a
        ; Fall through to __div16

        ;; signed 16-bit division
        ;;
        ;; Entry conditions
        ;;   HL = dividend
        ;;   DE = divisor
        ;;
        ;; Exit conditions
        ;;   HL = quotient
        ;;   DE = remainder
        ;;
        ;; Register used: AF,B,DE,HL
__div16::
        ;; Determine sign of quotient by xor-ing high bytes of dividend
        ;;  and divisor. Quotient is positive if signs are the same, negative
        ;;  if signs are different
        ;; Remainder has same sign as dividend
        ld      a, h            ; Get high byte of dividend
        xor     a, d            ; Xor with high byte of divisor
        rla                     ; Sign of quotient goes into the carry
        ld      a, h            ; Get high byte of dividend
        push    af              ; Save sign of both quotient and reminder

        ; Take absolute value of dividend
        rla
        jr      NC, .chkde      ; Jump if dividend is positive
        sub     a, a            ; Substract dividend from 0
        sub     a, l
        ld      l, a
        sbc     a, a            ; Propagate borrow (A=0xFF if borrow)
        sub     a, h
        ld      h, a

        ; Take absolute value of divisor
.chkde:
        bit     7, d
        jr      Z, .dodiv       ; Jump if divisor is positive
        sub     a, a            ; Subtract divisor from 0
        sub     a, e
        ld      e, a
        sbc     a, a            ; Propagate borrow (A=0xFF if borrow)
        sub     a, d
        ld      d, a

        ; Divide absolute values
.dodiv:
        call    __divu16

.fix_quotient:
        ; Negate quotient if it is negative
        pop     af              ; recover sign of quotient
        ret	NC		; Jump if quotient is positive
        ld      b, a
        sub     a, a            ; Subtract quotient from 0
        sub     a, l
        ld      l, a
        sbc     a, a            ; Propagate borrow (A=0xFF if borrow)
        sub     a, h
        ld      h, a
        ld      a, b
	ret

__get_remainder::
        ; Negate remainder if it is negative and move it into hl
        rla
	ex	de, hl
        ret     NC              ; Return if remainder is positive
        sub     a, a            ; Subtract remainder from 0
        sub     a, l
        ld      l, a
        sbc     a, a             ; Propagate remainder (A=0xFF if borrow)
        sub     a, h
        ld      h, a
        ret


;--------------------------------------------------------------------------
;  divmixed.s
;
;  Copyright (C) 2010, Philipp Klaus Krause
;--------------------------------------------------------------------------

__divsuchar:
	ld	hl, 2+1
	add	hl, sp

	ld	e, (hl)
	dec	hl
	ld	l, (hl)
	ld	h, 0

	jp	__div_signexte

__divuschar:
	ld	hl, 2+1
	ld	d, h
	add	hl, sp

	ld	e, (hl)
	dec	hl
	ld	l, (hl)

	ld 	a, l	; Sign extend
	rlca
	sbc	a, a
	ld	h, a

	jp	__div16


;--------------------------------------------------------------------------
;  modunsigned.s
;
;  Copyright (C) 2009-2010, Philipp Klaus Krause
;--------------------------------------------------------------------------

__moduchar:
        ld      hl,2+1
        add     hl,sp

        ld      e,(hl)
        dec     hl
        ld      l,(hl)

        call    __divu8

	ex	de,hl

        ret

__moduint:
        pop     af
        pop     hl
        pop     de
        push    de
        push    hl
        push    af

        call    __divu16

        ex      de,hl

        ret


;--------------------------------------------------------------------------
;  modsigned.s
;
;  Copyright (C) 2009, Philipp Klaus Krause
;--------------------------------------------------------------------------

__modschar:
        ld      hl,2+1
        add     hl,sp

        ld      e,(hl)
        dec     hl
        ld      l,(hl)

        call    __div8

        jp	__get_remainder

__modsint:
        pop     af
        pop     hl
        pop     de
        push    de
        push    hl
        push    af

        call    __div16

        jp	__get_remainder


;--------------------------------------------------------------------------
;  modmixed.s
;
;  Copyright (C) 2010, Philipp Klaus Krause
;--------------------------------------------------------------------------

__modsuchar:
	ld      hl,2+1
	add     hl,sp

	ld      e,(hl)
	dec     hl
	ld      l,(hl)
	ld      h,0

	call    __div_signexte

	jp	__get_remainder

__moduschar:
	ld      hl,2+1
	ld      d, h
	add     hl,sp

	ld      e,(hl)
	dec     hl
	ld      l,(hl)

	ld      a,l	; Sign extend
	rlca
	sbc     a, a
	ld      h, a

	call	__div16

	jp	__get_remainder

