#lang racket/base

(require "processor.rkt")

(execute '((SET R1 10) (SET R2 20) (EQ? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(execute '((SET R1 aap) (SET R2 20) (LT? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(execute '((SET R1 aap) (SET R2 20) (JMP aap) (LT? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printf "Factorial~n~n")

(execute
  '((SET R0 5)
    (SET R1 1)
    (SET R2 1)
    (loop : =0? R0 end)
    (MUL R1 R0 R1)
    (SUB R0 R0 R2)
    (JMP loop)
    (end : OUT R1)))

(printf "Fibonacci~n~n")

(let ((op (open-output-string)))
  (parameterize ((OUTPUT-port op))
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
  (display (get-output-string op))
  (newline))

(execute '((=0? R0 aap) (aap : STP)))

(execute '((OUT R0)))

(parameterize ((INPUT-port (open-input-string "123"))) (execute '((INP R0) (OUT R0))))

(execute '((SET R0 111) (PSH R0) (POP R1)))
(print-stack 5)
(newline)

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

(parameterize ((print-instrs? #f))
  (execute
    '((SET R1 1)
      (SET R2 aap)
      (MRD R3 aap)
      (ADD R4 R2 R1)
      (ADD R3 R3 R3)
      (MWR R3 R4)
      (STP)
      (aap : DATUM #x111))))

(print-memory 10)

;═════════════════════════════════════════════════════════════════════════════════════════════════════
