# Cmm: Implementing Exception Handling

The IEEE 754 specification for floating point numbers defines exceptions for certain floating point operations, including:

- range violation (overflow, underflow);
- rounding errors (inexact);
- invalid operation (invalid operand, such as comparison with a `NaN` value, the square root of a negative number or division of zero by zero); and,
- zero divide (a special case of an invalid operation).

Many architectures support floating point exceptions by including a special register as an addition to other exception handling registers. The IBM PPC includes the `FPSCR` ("Floating Point Status Control Register"); the Intel x86 processors use the `MXCSR` register. When the PPC performs a floating point operation it checks for possible errors and sets the `FPSCR`. Some processors allow a flag in the Floating-Point Unit (FPU) status and control register to be set that will disable some exceptions or the entire FPU exception handling facility. Some processors disable the FPU after an exception has occurred while others, notably Intel's x86 and x87 processors, continue to perform FPU operations. Depending on whether quiet NaNs (QNaNs) or signaling NaNs (SNaNs) are used by the software, an FPU exception may signal an interrupt for the software to pass to its own exception handler.

Some higher level languages provide facilities to handle these exceptions, including Ada, Fortran (F90 and later), C++ and C (C99, fenv.h, float.h on certain compilers); others may handle such exceptions without exposing a low-level interface. There are three reasons to handle FPU exceptions, and these reasons apply similarly to other exceptions:

- the facilities provide greater control;
- the facilities are efficient--more efficient than a higher-level software solution; and,
- FPU exceptions may be unavoidable, especially if several FPU operations are serially performed at the machine level so the higher level software has no opportunity to check the results in between operations.

## An Integral Exception Example

There has been at least one problem in GHC that would benefit from exception handling--in some cases, for `Integral`s. See bug ticket #1042. The bug occurs in `show`ing the number, in \[GhcFile(libraries/base/GHC/Show.lhs) GHC.Show\], `showSignedInt`, before conversion from base_2 to base_10, where a negative `Int` (always `Int32`) is negated in order to process it as a positive value when converting it to a string, base_10, causing an overflow error on some architectures. (Bear in mind that it would show up here in the example for #1042 because the function would be evaluated in GHCi here; the negation is the problem and the exception shows up in the _next_ instruction on that operand, here `DIV`.)

The exception example in #1042 does not occur on PowerPC machines, which dutifully print the two's complement of `(-2147483648::Int) `div` (-1::Int)`: `0`. (`-2147483648` is the minimum bound for signed Ints, so negating it should properly become, bitwise, a positive `2147483647` (all but bit 31 set); once negated again when divided by `-1` this would be `0`; `-0` is converted to `0`.) On some architectures such as Intel 64 and IA-32, negating the minimum bound does not wrap around to `0` but overflows, which is reported as a floating point "overflow" (`#O`) exception: the `NEG` instruction modifies the `OF` flag (bit 11) in the `EFLAGS` register--curiously enough, the `DIV` and `IDIV` instructions have _undefined_ effects on the `OF` flag.

The workaround was to avoid negating `minBound` `Int`s; note that no Intel instructions allow one to modify the `OF` flag directly. Alternative solutions might be to

1. mask the "exception" by clearing the interrupt flag, `IF`, using the `CLI` instruction; or,
2. conditionally unset the flag by using the `PUSHF` instruction on the `EFLAGS` register to push its lower word (bits 15-0, including the offending bit 11 (`OF`)) onto the stack, reset the `OF` bit, then push that back onto the stack and pop it into EFLAGS with `POPF`. Depending on variable register used, the assembler output would look similar to:

```plaintext
	; after NEG, MUL, other potential overflow operation ...
	jo	_reset_OF_flag	; jump near if overflow (OF=1)
	; continue rest of operation
	jmp	_continue_operation_on_int:
_reset_OF_flag:
	pushf	%eflags		; push low 16 bits of %eflags onto stack
	pop	%ax		; pop top of stack into low 16 bits of %eax
	and	$0xF7FF, %ax	; %ax = %ax & 0xF7FF
	push	%ax 		; push %ax value (with bit 11 set to 0) onto stack
	popf			; pop top of stack into lower 16 bits of %eflags
				; OF bit now reset

_continue_operation_on_int:	; this is a 32-bit address (also works in 64-bit mode)
	;...
```

## A Floating Point Exception Example

There was a long message thread on the Haskell-prime mailing list, "realToFrac Issues," beginning with [John Meacham's message](http://www.haskell.org/pipermail/haskell-prime/2006-February/000791.html) and ending with [Simon Marlow's message](http://www.haskell.org/pipermail/haskell-prime/2006-March/000840.html). The following code for converting a Float to a Double will _fail_ to produce a floating point exception or NaN on x86 machines (recall that 0.0/0.0 is NaN _and_ a definite FPU exception):

\[in GHCi-6.6 on PowerPC, OS X\]:

```plaintext
Prelude> 0.0/0.0
NaN

Prelude> realToFrac (0.0/0.0) :: Double
Infinity

Prelude> realToFrac (0.0/0.0 :: Float)
5.104235503814077e38

Prelude> realToFrac (0.0/0.0 :: Float) :: Double
5.104235503814077e38

Prelude> realToFrac (1.0/0.0)
Infinity
Prelude> realToFrac (1.0/0.0 :: Float)
3.402823669209385e38
```

This bug is not due to the lack of FPU exceptions in Cmm but bears mention as the internal conversion performed in 'realToFrac' on 'Float's would benefit from FPU exceptions: with Haskell-support for FPU exceptions this realToFrac would be able to issue an exception for NaN, Infinity or rounding errors when converting a Float to a Double and vice versa. There is a related problem with rounding errors in the functions 'encodeFloat', 'decodeFloat', 'encodeDouble' and 'decodeDouble', see [ReplacingGMPNotes/TheCurrentGMPImplementation](replacing-gmp-notes/the-current-gmp-implementation).