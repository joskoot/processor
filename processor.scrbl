#lang scribble/manual

@;════════════════════════════════════════════════════════════════════════════════════════════════════

@(require
   "scribble-utensils.rkt"
   racket
   "processor.rkt"
   (for-syntax)
   (for-label racket "processor.rkt")
   (for-template "processor.rkt"))

@(define period (roman "."))

@title{A simulator of a computer processor}
@author{Jacob J. A. Koot}

@;@defmodule["processor.rkt" #:packages ()]
@(defmodule processor/processor #:packages ())

@section{Introduction}

The simulator described in the present document executes a binary coded program
that can be made with its @nbsl["sec-assembler"]{assembler}@period
Every instruction consists of one word.
An instruction that does not access memory takes one clock cycle,
the next instruction being read within the same cycle.
When the instruction accesses memory,
the next instruction is read during an additional cycle.
Input and output from or to a port takes as many cycles as words read or written
plus an additional cycle to read the next instruction.
In fact a program takes exactly as many cycles as
the number of words transferred from or to memory.
Memory caches, memory banking and virtual memory are not simulated.

@Tabular[
 (("Notation" " ")
  ("n..m" ": from n up to but not including m.")
  ("n to m" ": from n up to and including m."))]

@section{Definitions}

@Tabular[
 (("Word"
   @roman{Consists of 64 bits.})
  ("Address"
   @roman{Consists of 24 bits.})
  ("Datum"
   @roman{Consists of 40 bits.})
  (@nbsl["sec-registers"]{Register}
    @roman{Contains a word or an address and is clocked when selected.@(lb)
  At clock-up time it accepts its input without altering its output.@(lb)
  At clock-drop it transfers its input to its output.})
  (@nbsl["sec-circuits"]{Circuit}
    @roman{Has one or more inputs and no or one output. Not clocked.@(lb)
  The output is a function of the inputs.@(lb)
  When the circuit is selected the output is available within clock-up period.})
  (@nbsl["sec-memory"]{Memory}
    @roman{Contains 2@↑{24} words.})
  (@nbsl["sec-alu"]{@tt{@bold{ALU}}} @roman{Arithmetic unit.})
  (@nbsl["sec-cmp"]{@tt{@bold{CMP}}} @roman{Arithmetic comparison unit, used for conditional jumps.})
  (@tt{@bold{IR}} @roman{Instruction register.}))
 #:sep (hspace 2)
 #:row-properties '((top top-border bottom-border) (top bottom-border))]

It would be nice to have a memory of more than 2@↑{24} words.
In principle 24 can be increased to 40
resulting in an address space of a little bit more than one Tera word or 8TB,
but as the simulator keeps memory in a vector,
this makes the @nbrl[assemble]{assembler} and procedure @nbr[execute] use more words
than acceptable for DrRacket or Racket
or available in your computer.
Memory caches and banking would slow down the simulation.
Virtual memory in a file with a limited number of pages in RAM
would slow down the simulation too,
even without page faults,
but would allow a larger address space.

@section[#:tag "sec-instruction-register"]{The instruction register}

Instruction register @tt{@bold{IR}} contains a word in which the components
listed below are discerned.
Bits are counted from low to high significance starting from 0.
The length of every component is a multiple of 4, even when less bits are required.
This makes reading hexadecimally printed instructions easier. 

@Tabular[
 (("name" "bits" "length" "description")
  ("opcode" "56..64" "8 bits" "instruction code")
  ("cc"     "52..56" "4 bits" "circuit control")
  ("Ra"     "48..52" "4 bits" "register designator")
  ("Rb"     "44..48" "4 bits" "register designator")
  ("Rc"     "40..44" "4 bits" "register designator")
  ("datum"  "00..40" "40 bits" "datum"))
 #:row-properties '((top-border bottom-border) ()()()()() bottom-border)
 #:column-properties '(left left right left)
 #:sep @(hspace 2)]

The opcode determines which ones of the other components are relevant.
cc is an extension of the opcode used by the arithmetic circuit @nbsl["sec-alu"]{@tt{@bold{ALU}}}
and the comparing circuit @nbsl["sec-cmp"]{@tt{@bold{CMP}}}@period

@section[#:tag "sec-registers"]{Registers}

@Tabular[
 (("name" "size" "usage" "register designator?" "initial value")
  (@roman{@tt{@bold{R0}} to @tt{@bold{R7}}} "word" "general purpose" "yes" "0")
  (@tt{@bold{SP}} "address" "stack pointer"        "no" @roman{2@↑{24}−1})
  (@tt{@bold{PC}} "address" "program counter"      "no" "0")
  (@tt{@bold{IR}} "word"    "instruction register" "no" "na"))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()()() bottom-border)]

Register designators can appear explicitly in assembler code.
@tt{@bold{SP}}, @tt{@bold{PC}} and @tt{@bold{IR}} are used implicitly
depending on the operation code in the @tt{@bold{IR}}@period

@section{Clock}
The simulator has a virtual clock.
Virtually registers and circuits run simultaneously.
Registers are clocked.
At clock raise they accept their inputs without altering their outputs.
At clock drop they transfer their inputs to their outputs.
An instruction is executed by first triggering the involved registers for clock raise
and subsequently triggering them for clock drop.
Memory, when activated, starts reading or writing at clock-raise
and finishes reading before clock-drop and writing within the clock-cycle.
Other elements are not clocked.
When their inputs change the possible mutation of their outputs is effectuated within clock-up time.

@section[#:tag "sec-memory"]{Memory}

@Tabular[
 (("name" "size" "in/out" "description")
  (@tt{@bold{MA}} "address" "input"
    @roman{memory address})
  (@tt{@bold{MW}} "word" "input"
    @roman{word to be written in memory at address @tt{@bold{MA}}})
  (@tt{@bold{MR}} "word" "output"
    @roman{word read from memory at address @tt{@bold{MA}}}))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()() bottom-border)]

When @tt{@bold{MW}} is selected,
memory takes the word to be written and address @tt{@bold{MA}} at clock raise time and
starts writing immediately.
When @tt{@bold{MR}} is selected, memory takes address @tt{@bold{MA}} at clock-raise time,
starts reading immediately and delivers the read word before clock-drop.
Reading from and writing to memory cannot be done within the same clock cycle.

@section[#:tag "sec-alu"]{Arithmetic unit}

The arithmetic unit @tt{@bold{ALU}} has two word inputs: @tt{@bold{A1}} and @tt{@bold{A2}}@period
It has one word output @tt{@bold{AO}}@period
The @tt{cc} field of the @tt{@bold{IR}} tells the @tt{@bold{ALU}}
which arithmetic operation to perform. Arithmetic is done in two's complement.
Overflow is ignored. Arithmetic instructions take one clock-cycle.

@section[#:tag "sec-cmp"]{Compare unit}
The arithmetic compare unit @tt{@bold{CMP}} wants one or two word inputs:
@tt{@bold{C1}} and @tt{@bold{C2}}@period
When @tt{@bold{C2}} is ommitted @tt{@bold{C1}} is compared to zero.
It has no output.
The @tt{cc} field of the @tt{@bold{IR}} tells the @tt{@bold{CMP}}
which arithmetic comparison to perform.
It is used in conditional jump operations.
If the comparator yields false, it inhibits the jump.
The comparison is made for two's complement words and is not faulted by overflow.
Jumps, both unconditional ones and both accepted or inhibited conditional ones take one clock-cycle.

@section[#:tag "sec-circuits"]{Circuits}

Circuits are not clocked. They provide their outputs without waiting for clock-drop.

@Tabular[
 (("name" "size" "in/out" "description")
  (@tt{@bold{P+}} "address" "output"
    @roman{@tt{@bold{PC}}+1 modulo 2@↑{24}})
  (@tt{@bold{S+}} "address" "output"
    @roman{@tt{@bold{SP}}+1 modulo 2@↑{24}})
  (@tt{@bold{S-}} "address" "output"
    @roman{@tt{@bold{SP}}−1 modulo 2@↑{24}})
  (@tt{@bold{DA}} "word" "output"
    @roman{the sign extended datum part of the @tt{@bold{IR}}})
  (@tt{@bold{A1}} "word" "input"
    @roman{@tt{@bold{ALU}} input})
  (@tt{@bold{A2}} "word" "input"
    @roman{@tt{@bold{ALU}} input})
  (@tt{@bold{AO}} "word" "output"
    @roman{@tt{@bold{ALU}} output})
  (@tt{@bold{C1}} "word" "input"
    @roman{@tt{@bold{CMP}} input})
  (@tt{@bold{C2}} "word" "input"
    @roman{@tt{@bold{CMP}} input (zero if omitted)}))
 #:sep (hspace 2)
 #:row-properties '((top-border bottom-border) ()()()()()()()() bottom-border)]

@section[#:tag "sec-busses"]{Busses}

Virtually, the simulated processor does nothing else than clocking busses between
registers, circuits, memory and the input and output controller.
There are fifteen busses, three switchable ones and twelve fixed ones.
They can transfer their signals simultaneously during the same clock cycle.
The input of a bus is called its `entrance´ and its output its `exit´.
The opcode in the @tt{@bold{IR}} controls which register or circuit output is connected
to the entrance of a switchable bus and to which register or circuit input its exit is connected.
These switches are marked as ‘↑’.
A fixed bus always has the same register or circuit output to its entrance and the same
register or circuit input from its exit.
Four of the fixed busses, marked by ‘f’, always are open.
The other eight, marked by ‘s’, are open during clock up period when selected by the opcode.
When a datum is transferred to a word register it is sign extended.
When a datum is used as an address it is truncated to its 24 lower significant bits.
When a word is transferred to an address register it is truncated to its 24 lower significant bits.
When an address is transferred to a word-register it is extended with zero bits
at the high significant end.

@Tabular[
 (("from↓ to→" @tt{@bold{Rn}} @tt{@bold{SP}} @tt{@bold{S+}} @tt{@bold{S-}} @tt{@bold{PC}}
               @tt{@bold{P+}} @tt{@bold{A1}} @tt{@bold{A2}} @tt{@bold{C1}} @tt{@bold{C2}}
               @tt{@bold{MW}} @tt{@bold{MA}} @tt{@bold{IR}} @tt{@bold{DA}})
  @;              Rn  SP  S+  S-  PC  P+  A1  A2  C1  C2  MW  MA  IR  DA  
  (@tt{@bold{Rn}} "↑" " " " " " " " " "↑" "↑" "↑" "↑" "↑" "↑" "↑" " " " ")
  (@tt{@bold{SP}} " " " " "f" "f" " " " " " " " " " " " " " " "s" " " " ")
  (@tt{@bold{S+}} " " "s" " " " " " " " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{S-}} " " "s" " " " " " " " " " " " " " " " " " " "s" " " " ")
  (@tt{@bold{PC}} " " " " " " " " " " "s" " " " " " " " " " " "s" " " " ")
  (@tt{@bold{P+}} " " " " " " " " "f" " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{AO}} "↑" " " " " " " " " " " " " " " " " " " " " " " " " " ")
  (@tt{@bold{MR}} "↑" " " " " " " " " " " " " " " " " " " " " " " "s" " ")
  (@tt{@bold{IR}} " " " " " " " " " " " " " " " " " " " " " " " " " " "f")
  (@tt{@bold{DA}} "↑" " " " " " " " " " " " " " " " " " " " " "s" " " " "))
 #:sep "│"
 #:column-properties '(center)
 #:row-properties '((top-border bottom-border) bottom-border)]

A bus exit can be connected to more than one input at the same time,
but a bus entrance can be connected to one output only at the same time.
This means that in every column only one ‘↑’ or ‘s’ can be open at the same time.
Because there are three switchable busses only, not more than three connections ‘↑’
of the whole table can be selected at the same time.
The ‘s’ busses to @tt{@bold{P+}} → @tt{@bold{MA}} and @tt{@bold{MR}} → @tt{@bold{IR}}
make it possible to read the next instruction during the same clock cycle as the operation proper,
provided the input of @tt{@bold{MA}} is not connected to the exit of another bus
and the output of @tt{@bold{MR}} is not connected to the entrance of another bus,
otherwise an additional cycle is needed to read the next instruction.
@nb{A jump} is made by selecting the busses @tt{@bold{Rn}/@bold{DA}} → @tt{@bold{MA}}
and @tt{@bold{MR}} → @tt{@bold{IR}}
and adjusting the @tt{@bold{PC}} with bus @tt{@bold{Rn}/@bold{DA}} → @tt{@bold{P+}}.
The f bus @tt{@bold{P+}} → @tt{@bold{PC}} assures that the @tt{@bold{PC}}
always to points to the memory word following the word from which the last instruction was read.

@section[#:tag "sec-assembler"]{Assembler}

‘@tt{Ra}’, ‘@tt{Rb}’ and ‘@tt{Rb}’ are register designators: @tt{R0} to @tt{R7}@period @(lb)
An instruction has the form
@inset{@tt{(opcode-mnemonic etc ...)}}
It may be given a name for its address by writing
@inset{@nb{@tt{(addres : opcode-mnemonic etc ...)}}}
where @tt{addres} is a symbol other than a register designator
and all @tt{addres}ses must be distinct.
Comments can be inserted as:
@inset{@tt{(: @roman{text of the comment})}}
This implies that a colon cannot be used as address.
@nb{All elements} of an instruction must be separated by blank space.
‘@tt{datum}’ denotes a datum in the form of an exact integer or an @tt{address}@period
If the datum is an exact integer it is truncated to its 40 lower significant bits.
When occurring in a @tt{DATUM} declaration it is truncated to the 64 lower significant bits.
Arithmetical operations are in two's complement. Overflow is ignored.

@Tabular[
 ((@nb{@tt{(STP)}} @roman{Halts the processor.})
  (@nb{@tt{(NOP)}} @roman{No operation.})
  (@nb{@tt{(SET Ra Rb)}} @roman{@tt{Ra} ← @tt{Rb}@period})
  (@nb{@tt{(SET Ra datum)}} @roman{@tt{Ra} ← @tt{datum} (sign extended).})
  (@nb{@tt{(ADD Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb+Rc}@period})
  (@nb{@tt{(ADD Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb+datum}@period})
  (@nb{@tt{(SUB Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb}−@tt{Rc}@period})
  (@nb{@tt{(SUB Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb}−@tt{datum}@period})
  (@nb{@tt{(MUL Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb}×@tt{Rc}@period})
  (@nb{@tt{(MUL Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb}×@tt{datum}@period})
  (@nb{@tt{(DIV Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb/Rc}, integer division@period})
  (@nb{@tt{(DIV Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb/Rdatum},
  integer division@period})
  (@nb{@tt{(SHL Ra Rb Rc)}} @roman{Put @tt{Rb} into @tt{Ra},
  left shifted by @tt{Rc} bits.})
  (@nb{@tt{(SHL Ra Rb datum)}} @roman{Put @tt{Rb} into @tt{Ra},
  left shifted by @tt{datum} bits.})
  (@nb{@tt{(SHR Ra Rb Rc)}} @roman{Put @tt{Rb} into @tt{Ra},
  right shifted by @tt{Rc} bits without sign extension.})
  (@nb{@tt{(SHR Ra Rb datum)}}
    @roman{Put @tt{Rb} into @tt{Ra} right shifted by @tt{datum} bits, no sign extension.})
  (@nb{@tt{(SHE Ra Rb Rc)}} @roman{Put @tt{Rb} into @tt{Ra},
  right shifted by @tt{Rc} bits and sign extended.})
  (@nb{@tt{(SHE Ra Rb datum)}} @roman{Put @tt{Rb} into @tt{Ra},
  right shifted by @tt{datum} bits and sign extended.})
  (@nb{@tt{(NEG Ra Rb)}} @roman{@tt{Ra} ← −@tt{Rb}@period})
  (@nb{@tt{(NEG Ra datum)}} @roman{@tt{Ra} ← −@tt{datum}@period})
  (@nb{@tt{(AND Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb˄Rc}, bitwise and.})
  (@nb{@tt{(AND Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb˄datum}, bitwise and.})
  (@nb{@tt{(IOR Ra Rb Rc)}} @roman{@tt{Ra} ← @tt{Rb˅Rc}, bitwise inclusive or.})
  (@nb{@tt{(IOR Ra Rb datum)}} @roman{@tt{Ra} ← @tt{Rb˅datum}, bitwise inclusive or.})
  (@nb{@tt{(NOT Ra Rb)}} @roman{@tt{Ra} ← @tt{@roman{¬}Rb}, bitwise not.})
  (@nb{@tt{(NOT Ra datum)}} @roman{@tt{Ra} ← @tt{@roman{¬}datum}, bitwise not.})
  (@nb{@tt{(JMP Ra)}} @roman{Jump to @tt{Ra}@period})
  (@nb{@tt{(JMP datum)}} @roman{Jump to @tt{datum}@period})
  (@nb{@tt{(=0? Ra Rb)}} @roman{If @tt{Ra}=0 jump to @tt{Rb}
  else go to next instruction.})
  (@nb{@tt{(=0? Ra datum)}} @roman{If @tt{Ra}=0 jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(<0? Ra Rb)}} @roman{If @tt{Ra}<0 jump to @tt{Rb}
  else go to next instruction.})
  (@nb{@tt{(<0? Ra datum)}} @roman{If @tt{Ra}<0 jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(>0? Ra Rb)}} @roman{If @tt{Ra}>0 jump to @tt{Rb}
  else go to next instruction.})
  (@nb{@tt{(>0? Ra datum)}} @roman{If @tt{Ra}>0 jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(≤0? Ra Rb)}} @roman{If @tt{Ra}≤0 jump to @tt{Rb}
  else go to next instruction.})
  (@nb{@tt{(≤0? Ra datum)}} @roman{If @tt{Ra}≤0 jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(≥0? Ra Rb)}} @roman{If @tt{Ra}≥0 jump to @tt{Rb}
  else go to next instruction.})
  (@nb{@tt{(≥0? Ra datum)}} @roman{If @tt{Ra}≥0 jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(EQ? Ra Rb Rc)}} @roman{If @tt{Ra}=@tt{Rb} jump to @tt{Rc}
  else go to next instruction.})
  (@nb{@tt{(EQ? Ra Rb datum)}} @roman{If @tt{Ra}=@tt{Rb} jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(LT? Ra Rb Rc)}} @roman{If @tt{Ra}<@tt{Rb} jump to @tt{Rc}
  else go to next instruction.})
  (@nb{@tt{(LT? Ra Rb datum)}} @roman{If @tt{Ra}<@tt{Rb} jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(GT? Ra Rb Rc)}} @roman{If @tt{Ra}>@tt{Rb} jump to @tt{Rc}
  else go to next instruction.})
  (@nb{@tt{(GT? Ra Rb datum)}} @roman{If @tt{Ra}>@tt{Rb} jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(LE? Ra Rb Rc)}} @roman{If @tt{Ra}≤@tt{Rb} jump to @tt{Rc}
  else go to next instruction.})
  (@nb{@tt{(LE? Ra Rb datum)}} @roman{If @tt{Ra}≤@tt{Rb} jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(GE? Ra Rb Rc)}} @roman{If @tt{Ra}≥@tt{Rb} jump to @tt{Rc}
  else go to next instruction.})
  (@nb{@tt{(GE? Ra Rb datum)}} @roman{If @tt{Ra}≥@tt{Rb} jump to @tt{datum}
  else go to next instruction.})
  (@nb{@tt{(MRD Ra Rb)}}
    @roman{Read memory at addres @tt{Rb} and put the word in @tt{Ra}@period})
  (@nb{@tt{(MRD Ra datum)}}
    @roman{Read memory at addres @tt{datum} and put the word in @tt{Ra}@period})
  (@nb{@tt{(MWR Ra Rb)}} @roman{Write @tt{Ra} in memory at address @tt{Rb}@period})
  (@nb{@tt{(MWR Ra datum)}} @roman{Write @tt{Ra} in memory at address @tt{datum}@period})
  (@nb{@tt{(PSH Ra)}} @roman{Push @tt{Ra} onto the stack.})
  (@nb{@tt{(PSH datum)}} @roman{Push @tt{datum} onto the stack.})
  (@nb{@tt{(POP Ra)}} @roman{Pop a word from stack into @tt{Ra}@period})
  (@nb{@tt{(OUT Ra)}} @roman{Print @tt{Ra} on @nbr[OUT-port].})
  (@nb{@tt{(OUT datum)}} @roman{Print @tt{datum} on @nbr[OUT-port].})
  (@nb{@tt{(INP Ra)}} @roman{Reads a datum from @nbr[INP-port] and puts it in Ra.})
  (@nb{@tt{(WRT Ra Rb)}}
    @roman{Write words from memory at addresses from Ra .. Ra+Rb})
  (@nb{@tt{(WRT Ra datum)}}
    @roman{Write words from memory at addresses from Ra .. Ra+datum})
  (@nb{@tt{(RÆD Ra Rb)}}
    @roman{Read words into memory at addresses from Ra .. Ra+Rb})
  (@nb{@tt{(RÆD Ra datum)}}
    @roman{Read words into memory at addresses from Ra .. Ra+datum})
  (@nb{@tt{(DATUM datum)}} @roman{Not ment to be executed as instruction.})
  (@nb{@tt{(DATA datum ...)}} @roman{Expanded to repeated @tt{DATUM}@period})
  (@nb{@tt{(: @roman{comment} ...)}} @roman{Ignored.}))
 #:sep (hspace 2)
 #:row-properties '(top top)]

The @tt{PSH} and @tt{POP} instructions use the @tt{@bold{SP}} for addressing.
When pushing, address @tt{@bold{SP}} is used and @tt{@bold{SP}} is decreased by one.
When popping @tt{@bold{S+}} is used as address and @tt{@bold{SP}} is increased by one.
At the start of execution @tt{@bold{SP}} is 2@↑{24}−1.
@tt{PSH} and @tt{POP} instructions should be balanced like parentheses.
Instructions @tt{WRT} and @tt{RÆD} use direct access to memory.
They take as many cycles as words read or written
plus an additional cycle to read the next instruction.
@tt{SHL}, @tt{SHR} and @tt{SHE} take the shift counts as signed.
A negative shift-count for @tt{SHL} effectively does @tt{SHR} and reversely.
A negative shift count for @tt{SHE} effectively does @tt{SHL} without sign extension and reversely.
                                                                 
@section{Provided}

@defproc[(assemble (‹instrs› (listof #, @nbsl["sec-assembler"]{instructions}))) void?]{
 @nbsl["sec-assembler"]{Assembles}
 the instructions and puts them in memory starting from address 0.@(lb)
 Before assembling the memory is cleared.}

@defproc[(execute (‹instrs› (or/c #f #, @nbsl["sec-assembler"]{instructions}) #f)) void?]{ 
 Resets all registers and executes the program currently in memory starting at address 0.
 @nb{If @nbr[‹instrs›]} is a list of instructions, the @nbrl[assemble]{assembler} is called first.}

@defparam*[show ‹options›
 (or/c #f 'all
   (listof
     'source-code
     'binary-code
     'instructions
     'registers))
 (listof
   'source-code
   'binary-code
   'instructions
   'registers)
 #:value 'all]{
 @nbr[(show 'all)] activates all options.@(lb)
 @nbr[(show #f)] and @nbr[(show '())] disable all options.

 When this parameter contains @nbr['source-code],@(lb)
 the @nbrl[assemble]{assembler} shows the program to be assembled.
 
 When this parameter contains @nbr['binary-code],@(lb)
 procedure @nbr[assemble] shows the assembled binary code.

 When this parameter contains @nbr['instructions],@(lb)
 procedure @nbr[execute] prints all executed instructions:
 @inset{@tt{line-nr address mnemonic opcode cc ra rb rc datum cycle-count}}
 The elements are printed in hexadecimal form,@(lb)
 the line-nr, mnemonic and cycle-count excepted.

 When this parameter contains @nbr['registers],@(lb)
 procedure @nbr[execute] shows the contents
 of registers after completion of the program.}

@defparam[align ‹n› exact-nonnegative-integer? #:value 3]{
 In a print of the executed instructions as indicated by parameter @nbr[show],
 each line begins with a line number.
 This number is right aligned in a field of @tt{(@nbr[align])} digits.
 Line numbers requiring more digits are not truncated.}

@defparam[OUT-port ‹port› output-port? #:value (current-output-port)]{
 Parameter specifying on which port instructions @tt{WRT} and @tt{OUT} prints output.}

@defparam[INP-port ‹port› output-port? #:value (current-input-port)]{
 Parameter specifying from which port instructions @tt{INP} and @tt{RÆD} read input.
 @Interaction[
 (parameterize
   ((INP-port (open-input-string "#x0123456789abcdef")))
   (execute '((INP R0) (NOT R1 R0))))]
 @Interaction[
 (show '(instructions))
 (INP-port (open-input-string "1 2 3 4 5"))
 (assemble
   '((SET R0 data)
     (RÆD R0 5)
     (STP)
     (data : DATA)))
 (print-memory 3 5)
 (execute)
 (print-memory 3 5)]}

@defparam*[catch-crash ‹yes/no› any/c boolean? #:value #f]{
 In case of a crash procedure @nbr[execute]
 prints a message on the @nbr[current-error-port] and halts by raising an exception.
 If this parameter is set to a true value,
 this exception is catched and procedure @nbr[execute] returns normally.}

@defproc[(print-memory (‹n› exact-nonnegative-integer?) (‹m› exact-nonnegative-integer?)) void?]{
 Prints words @nbr[‹n›]..@nbr[‹n›]+@nbr[‹m›] of memory.
 Each word is preceded by its hexadecimal address.}

@defproc[(print-stack (‹n› exact-nonnegative-integer?)) void?]{
 Shows the last @nbr[‹n›] words of memory, where the stack is located.
 Each word is preceded by its hexadecimal address.

 @Interaction[
 (parameterize ((show #f))
   (execute
     '((PSH 1)
       (PSH 2)
       (PSH 3)
       (POP R1)
       (POP R2)
       (POP R3)
       (OUT R1)
       (OUT R2)
       (OUT R3))))
 (print-stack 5)]}

@defproc[(print-registers) void?]{
 Prints registers @nbr[R0] .. @nbr[R7],
 @nbr[SP] and @nbr[PC].}

@defparam[max-nr-of-instrs ‹n› exact-nonnegative-integer? #:value 1000]{
 Puts a limit on the number of instructions to be executed by procedure @nbr[execute].@(lb)
 The procedure halts when the limit is exceeded.

 @Interaction[
 (parameterize ((max-nr-of-instrs 5) (show '(instructions)))
   (execute '((loop : JMP loop))))]}

@defproc[(reset) void?]{
 Resets memory and registers.}

@Elemtag{Rx}
@defproc[#:link-target? #f (Rx (‹arg› (or/c #f exact-integer? 'clock) #f))
         exact-nonnegative-integer?]{
 @nbpr{Rx} is one of the following:}
@inset{@deftogether[
 ((defidform #:kind "general purpose word register" R0)
  (defidform #:kind "general purpose word register" R1)
  (defidform #:kind "general purpose word register" R2)
  (defidform #:kind "general purpose word register" R3)
  (defidform #:kind "general purpose word register" R4)
  (defidform #:kind "general purpose word register" R5)
  (defidform #:kind "general purpose word register" R6)
  (defidform #:kind "general purpose word register" R7)
  (defidform #:kind "address register, stack pointer" SP)
  (defidform #:kind "address register, program counter" PC)
  (defidform #:kind "instruction register" IR))]}

When called with an integer, it stores the integer in its input without changing its output.
The integer is truncated to its 64/24 lower significant bits in case of a word/address.
When called with @nbr['clock], it copies its input to its output.
In all cases the output is returned.
Register @tt{@bold{Rx}} is printed as @tt{#<Rx:h...>} where each @tt{h} is an hexadecimal digit,
16 digits for a word and 6 digits for an address.
@Interaction[
 (list (R0 #xA ) (R0) (R0 'clock) R0)
 (list (R1 (R0)) (R1) (R1 'clock) R1)]

@section{Examples}

@subsection{Factorial}

@Interaction[
 (assemble
   '((: the R0-th factorial will be computed)
     (INP R0)
     (: the factorial is computed in R1)
     (SET R1 1)
     (: R2 is constant 1)
     (SET R2 1)
     (: do the computation)
     (LE? R0 R2 end)
     (loop : MUL R1 R0 R1)
     (SUB R0 R0 R2)
     (GT? R0 R2 loop)
     (end : OUT R1)))
 (parameterize ((INP-port (open-input-string "5")))
   (execute))]

@subsection{Fibonacci}

R0 : The number of elements to be computed and printed, possibly a few more.@(lb)
R1 : Fibonacci number, initially 0@(lb)
R2 : Next fibonacci number, initially 1@(lb)
R4 : 1 for decrementing R0

@Interaction[
 (assemble
   '((SET R0 10)
     (SET R1 0)
     (SET R2 1)
     (SET R4 1)
     (=0? R0 end)
     (loop : OUT R1)
     (ADD R3 R1 R2)
     (SET R1 R2)
     (SET R2 R3)
     (SUB R0 R0 R4)
     (>0? R0 loop)
     (end : OUT R1)
     (OUT R2)
     (STP)))
 (let ((op (open-output-string)))
   (parameterize ((OUT-port op))
     (execute)
     (display (get-output-string op))))]

@subsection{Subroutine call}

Computes 2j+1 reading j from the @nbr[INP-port].

@Interaction[
 (parameterize ((INP-port (open-input-string "3")))
   (execute
     '((INP R0)
       (PSH R0)
       (PSH return)
       (JMP subroutine)
       (return : POP R0)
       (OUT R0)
       (STP)
       (subroutine : POP R5) (: return address)
       (POP R6) (: j)
       (SET R1 1)
       (SET R2 2)
       (MUL R6 R6 R2)
       (ADD R6 R6 R1)
       (PSH R6) (JMP R5) (: return 2j+1))))
 (print-stack 5)]

@subsection{Self-modification}

By means of instruction @tt{MWR} a program can modify itself.
In the following example instruction @green{@tt{NOP}} at address @green{@tt{noot}}
is replaced by instruction @green{@tt{OUT}} found at address @green{@tt{aap}}.

@Interaction[
 (execute
   '((MRD R1 aap)
     (MWR R1 noot)
     (NOP)
     (noot : NOP)
     (STP)
     (aap : OUT R1)))]

@larger{@bold{The end}}
