#lang racket/base

;(require "simulator.rkt")
(require "simulator.rkt")
;(require processor/simulator)

(define (printl . args)
  (display "\n════════════════════════════════════════════════════════════════════\n")
  (apply printf args))

(printl "Check EQ?, R5 must be set to 12345~n")
(execute '((SET R1 10) (SET R2 20) (EQ? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "Check EQ?, R5 must not be set to 12345~n")
(execute '((SET R1 aap) (SET R2 20) (LT? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "Check JMP, R5 must not be set to 12345~n")
(execute '((SET R1 aap) (SET R2 20) (JMP aap) (NOP) (SET R5 12345) (aap : ADD R3 R1 R2)))

(printl "Factorial~n")
(execute
  '((SET R0 5)
    (SET R1 1)
    (SET R2 1)
    (loop : =0? R0 end)
    (MUL R1 R0 R1)
    (SUB R0 R0 R2)
    (JMP loop)
    (end : OUT R1)))

(printl "Fibonacci~n")
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

(printl "No OUT expected~n")
(execute '((=0? R0 aap) (OUT 12345) (aap : STP)))

(printl "")
(execute '((OUT R0) (aap : OUT aap)))

(printl "")
(parameterize ((INP-port (open-input-string "123"))) (execute '((INP R0) (OUT R0))))

(printl "stack")
(execute '((SET R0 111) (PSH R0) (POP R1)))
(print-stack 5)
(newline)

(printl "Check MRD and MWR~n")
(execute
  '((SET R1 1)
    (SET R2 aap)
    (MRD R3 aap)
    (ADD R4 R2 R1)
    (ADD R3 R3 R3)
    (MWR R3 R4)
    (STP)
    (aap : DATUM #x111)))
(newline)
(print-memory 0 10)

(printl "Check MRD and MWR~n")
(parameterize ((show #f))
  (execute
    '((SET R1 1)
      (SET R2 aap)
      (MRD R3 aap)
      (ADD R4 R2 R1)
      (ADD R4 R3 R3)
      (MWR R3 R4)
      (STP)
      (aap : DATUM #x111))))
(newline)
(print-memory 0 15)

(printl "Check RÆD~n")
(parameterize ((INP-port (open-input-string "1 2 3 4 5")))
  (execute
    '((SET R1 10) (SET R2 5) (RÆD R1 R2) (NOP)))
  (newline)
  (print-memory 10 6))

(printl "Replace NOP by OUT at addres noot~n")
(execute
  '((MRD R1 aap)
    (MWR R1 noot)
    (NOP)
    (noot : NOP)
    (STP)
    (aap : OUT R1)))

(printl "Subroutine~n")
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

(printl "Check WRT~n")
(execute
  '((SET R1 start)
    (SET R2 end)
    (SUB R3 R2 R1)
    (WRT R1 R3)
    (STP)
    (start : DATA 1 2 3 4 5)
    (end : DATA 0)))

(printl "Data only~n")
(assemble '((DATA -1 -2 -3)))

(printl "DATA producing a NOP instruction~n")
(execute '((NOP) (aap : DATA #x01FFFFFFFFFFFFFF aap)))

(printl "DATA producing a SET instruction~n")
(execute '((SET R1 aap) (aap : DATA #x02021FFFFFFFFFFF aap)))

(printl "Infinite loop~n")
(let ((op (open-output-string)))
  (parameterize ((max-nr-of-instrs 10) (catch-crash #t))
    (execute '((JMP 0)))))

(printl "Infinite loop~n")
(let ((op (open-output-string)))
  (parameterize ((max-nr-of-instrs 21) (OUT-port op) (catch-crash #t))
    (execute '((SET R1 -1) (SET R2 -1) (aap : OUT R2) (ADD R2 R2 R1) (JMP aap)))
    (newline)
    (display (get-output-string op))))

(printl "Unknown cc~n")
(parameterize ((catch-crash #t))
  (execute '((SET R1 aap) (aap : DATA #x03FFFFFFFFFFFFFF aap))))

(printl "Unknown opcode~n")
(parameterize ((catch-crash #t))
  (execute '((SET R1 aap) (aap : DATA #xFF00000000000000 aap))))

(printl "Unknown register~n")
(parameterize ((catch-crash #t))
  (execute '((SET R1 aap) (aap : DATA #x030FFFFFFFFFFFFF aap))))
(newline)
(print-registers)
(newline)

(printl "Program reading from the string-port~n")
(require (only-in racket ~r))
(displayln "\nAssemble factorial code")
(define factorial-code
  (assemble
    '((SET R0 5)
      (SET R1 1)
      (SET R2 1)
      (loop : =0? R0 end)
      (MUL R1 R0 R1)
      (SUB R0 R0 R2)
      (JMP loop)
      (end : OUT R1))
    #:offset 5 #:target 'list))
(displayln "\nPut the code in a string-port")
(define input
  (let ((op (open-output-string)))
    (for ((instr (in-list factorial-code))) (writeln (cdr instr) op))
    (open-input-string (get-output-string op))))

(displayln "\nMake program reading the factorial code from IN-port")
(assemble
  `((SET R0 5)
    (RÆD R0 ,(length factorial-code))
    (WRT R0 ,(length factorial-code))
    (JMP R0)))
(displayln "\nNotice that the code for the factorial is not yet in memory\n")
(print-memory 0 10)
(parameterize ((show '(instructions)) (INP-port input)) (execute))

(printl "Integer square root\n")

(require (only-in racket open-output-nowhere))

(assemble
  '((INP R0)
    (SET R1 R0)
    (loop : MUL R2 R1 R1)
    (LE? R2 R0 end)
    (ADD R4 R2 R0)
    (MUL R5 R1 2)
    (DIV R1 R4 R5)
    (JMP loop)
    (end : OUT R0) (OUT R1)))
(parameterize ((current-output-port (open-output-nowhere)))
  (let ((p (open-output-string)))
    (parameterize ((OUT-port p) (show #f))
      (for ((k (in-range 101)))
        (parameterize ((INP-port (open-input-string (format "~s" k))))
          (execute))))
    (parameterize ((current-input-port (open-input-string (get-output-string p))))
      (let loop ((k 0))
        (cond
          ((equal? (read) eof) (format "all ~s roots correct" k))
          (else
            (let ((sqr (begin (read) (read) (read) (read)))
                  (root (begin (read) (read) (read) (read) (read))))
              (unless (= root (integer-sqrt sqr)) (error "root" sqr root)))
            (loop (add1 k))))))))

(printl "offset and start\n")

(execute #:offset #xFFF000FF #:start #xFFF000FF
  '((a : SET R1 b)
    (b : SET R2 c)
    (c : SET R3 d)
    (d : SET R4 e)
    (e : SET R5 f)
    (f : SET R6 g)
    (g : SET R7 h)
    (h : SET R0 a)))
   
(printl "End: all is well.~n")

;═════════════════════════════════════════════════════════════════════════════════════════════════════
