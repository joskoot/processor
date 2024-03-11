#lang racket/base

(require "processor.rkt")

(define (printl . args)
  (display "\n════════════════════════════════════════════════════════════════════")
  (apply printf args))

(printl "~n")
(execute '((SET R1 10) (SET R2 20) (EQ? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "~n")
(execute '((SET R1 aap) (SET R2 20) (LT? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "~n")
(execute '((SET R1 aap) (SET R2 20) (JMP aap) (NOP) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "~nFactorial~n")

(execute
  '((SET R0 5)
    (SET R1 1)
    (SET R2 1)
    (loop : =0? R0 end)
    (MUL R1 R0 R1)
    (SUB R0 R0 R2)
    (JMP loop)
    (end : OUT R1)))

(printl "~nFibonacci~n")

(let ((op (open-output-string)))
  (parameterize ((OUT-port op))
    (execute
      '((SET R0 10)
        (SET R1 0)
        (SET R2 1)
        (SET R6 1)
        (loop : =0? R0 end)
        (OUT R1)
        (ADD R3 R1 R2)
        (SET R1 R2)
        (SET R2 R3)
        (SUB R0 R0 R6)
        (JMP loop)
        (end : OUT R1)
        (OUT R2))))
  (newline)
  (display (get-output-string op)))

(printl "~n")
(execute '((=0? R0 aap) (aap : STP)))

(printl "~n")
(execute '((OUT R0)))

(printl "~n")
(parameterize ((INP-port (open-input-string "123"))) (execute '((INP R0) (OUT R0))))

(printl "~n")
(execute '((SET R0 111) (PSH R0) (POP R1)))
(print-stack 5)
(newline)

(printl "~n")
(execute
  '((SET R1 1)
    (SET R2 aap)
    (MRD R3 aap)
    (ADD R4 R2 R1)
    (ADD R3 R3 R3)
    (MWR R3 R4)
    (STP)
    (aap : DATUM #x111)))

(print-memory 10)
(newline)

(printl "~n")
(parameterize ((show-instructions #f) (show-registers #f))
  (execute
    '((SET R1 1)
      (SET R2 aap)
      (MRD R3 aap)
      (ADD R4 R2 R1)
      (ADD R3 R3 R3)
      (MWR R3 R4)
      (STP)
      (aap : DATUM #x111))))
(newline)
(print-memory 10)

(printl "~n")
(parameterize ((INP-port (open-input-string "1 2 3 4 5")) (show-instructions #f))
  (execute
    '((SET R1 10) (SET R2 5) (RÆD R1 R2) (NOP)))
  (newline)
  (print-memory 20))

(printl "~nSubroutine~n")
(execute
  '((MRD R1 aap)
    (MWR R1 noot)
    (NOP)
    (noot : NOP)
    (STP)
    (aap : OUT R1)))
(printl "~n")
(parameterize ((INP-port (open-input-string "3")))
  (execute
    '((INP R0)
      (PSH R0)
      (PSH return)
      (JMP subroutine)
      (return : POP R0)
      (OUT R0)
      (STP)
      (: pop return address) (subroutine : POP R5)
      (: pop j) (POP R6)
      (SET R1 1)
      (SET R2 2)
      (MUL R6 R6 R2)
      (ADD R6 R6 R1)
      (: return 2j+1) (PSH R6)
      (: return) (JMP R5) )))

(printl "~n")
(execute
  '((SET R1 start)
    (SET R2 end)
    (SUB R3 R2 R1)
    (WRT R1 R3)
    (STP)
    (start : DATA 1 2 3 4 5)
    (end : DATA 0)))

(printl "~n")
(assemble '((DATA -1 -2 -3)))

(printl "~n")
(execute '((NOP) (aap : DATA #x01F888FFFFFFFFFF aap)))

(printl "~n")
(let ((op (open-output-string)))
(parameterize ((max-nr-of-instrs 21) (OUT-port op))
  (execute '((SET R1 -1) (SET R2 -1) (aap : OUT R2) (ADD R2 R2 R1) (JMP aap)))
  (newline)
  (display (get-output-string op))))

(printl "~n")
(execute '((SET R1 aap) (aap : DATA #x02021FFFFFFFFFFF aap)))

(printl "~n")
(execute '((SET R1 aap) (aap : DATA #x03FFFFFFFFFFFFFF aap)))
(print-registers)

(printl "~n")
(execute '((SET R1 aap) (aap : DATA #x030FFFFFFFFFFFFF aap)))
(print-registers)

;═════════════════════════════════════════════════════════════════════════════════════════════════════
