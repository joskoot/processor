#lang racket/base

(require "processor.rkt" test/test (only-in racket open-output-nowhere))
;(require processor/processor)

(define-syntax-rule (T name program ((R v) ...) input output)
  (test name
    ((let ((op (open-output-string)))
       (parameterize
         ((OUT-port op)
          (INP-port (open-input-string input))
          (current-output-port (open-output-nowhere)))
         (execute program))
       (display (get-output-string op))
       (and (= (R) v) ...)))
    '(#t)
    #:output output))

(define-syntax-rule (TM name program ((R v) ...) input output n m)
  (test name
    ((let ((op (open-output-string)))
       (parameterize
         ((OUT-port op)
          (INP-port (open-input-string input))
          (current-output-port (open-output-nowhere)))
         (execute program))
       (print-memory n m)
       (display (get-output-string op))
       (and (= (R) v) ...)))
    '(#t)
    #:output output))

(define-syntax-rule (TS name program ((R v) ...) input output n)
  (test name
    ((let ((op (open-output-string)))
       (parameterize
         ((OUT-port op)
          (INP-port (open-input-string input))
          (current-output-port (open-output-nowhere)))
         (execute program))
       (print-stack n)
       (display (get-output-string op))
       (and (= (R) v) ...)))
    '(#t)
    #:output output))

(T "Check EQ?, R5 must be set to 12345"
  '((SET R1 10) (SET R2 20) (EQ? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2))
  ((R1 10) (R2 20) (R3 30) (R5 12345))
  "" "")

(T "Check EQ?, R5 must not be set to 12345"
  '((SET R1 aap) (SET R2 20) (LT? R1 R2 aap) (SET R5 12345) (aap : ADD R3 R1 R2))
  ((R1 4) (R2 20) (R3 24) (R5 0))
  "" "")
  

(T "Check JMP, R5 must not be set to 12345"
  '((SET R1 aap) (SET R2 20) (JMP aap) (NOP) (SET R5 12345) (aap : ADD R3 R1 R2))
  ((R1 5) (R2 20) (R3 25) (R5 0))
  "" "")

(T "Factorial"
  '((SET R0 5)
    (SET R1 1)
    (SET R2 1)
    (loop : =0? R0 end)
    (MUL R1 R0 R1)
    (SUB R0 R0 R2)
    (JMP loop)
    (end : OUT R1))
  ((R1 120))
  "" "output : 0000000000000078 : 120")

(T "Fibonacci"
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
    (OUT R2))
  ()
  "" "
output : 0000000000000000 : 0
output : 0000000000000001 : 1
output : 0000000000000001 : 1
output : 0000000000000002 : 2
output : 0000000000000003 : 3
output : 0000000000000005 : 5
output : 0000000000000008 : 8
output : 000000000000000D : 13
output : 0000000000000015 : 21
output : 0000000000000022 : 34
output : 0000000000000037 : 55
output : 0000000000000059 : 89")

(T "No OUT expected"
  '((=0? R0 aap) (OUT 12345) (aap : STP))
  ()
  "" "")

(T "output"
  '((OUT R0) (aap : OUT aap))
  ()
  "" "
output : 0000000000000000 : 0
output : 0000000000000001 : 1")

(T "INP"
  '((INP R0) (OUT R0))
  ((R0 123))
  "123" "output : 000000000000007B : 123")
    

(TS "PSH POP"
  '((SET R0 111) (PSH R0) (POP R1))
  ((R1 111) (R0 111))
  "" "
FFFFFB : 0000000000000000 : 0
FFFFFC : 0000000000000000 : 0
FFFFFD : 0000000000000000 : 0
FFFFFE : 0000000000000000 : 0
FFFFFF : 000000000000006F : 111" 5)

(TM "Check MRD and MWR~n"
  '((SET R1 1)
    (SET R2 aap)
    (MRD R3 aap)
    (ADD R4 R2 R1)
    (ADD R3 R3 R3)
    (MWR R3 R4)
    (STP)
    (aap : DATUM #x111))
  ()
  "" "
000000 : 0201800000000001 : 144537400540921857
000001 : 0202800000000007 : 144818875517632519
000002 : 0F03800000000007 : 1081849072987406343
000003 : 0304210000000000 : 217334965904343040
000004 : 0303330000000000 : 217073282136932352
000005 : 0E03400000000000 : 1009721110205300736
000006 : 0000000000000000 : 0
000007 : 0000000000000111 : 273
000008 : 0000000000000222 : 546
000009 : 0000000000000000 : 0" 0 10)

(TM "Check RÆD"
  '((SET R1 10) (SET R2 5) (RÆD R1 R2) (NOP))
  ()
  "1 2 3 4 5"
  "
000000 : 020180000000000A : 144537400540921866
000001 : 0202800000000005 : 144818875517632517
000002 : 0A01200000000000 : 720892599728078848
000003 : 0100000000000000 : 72057594037927936
000004 : 0000000000000000 : 0
000005 : 0000000000000000 : 0
000006 : 0000000000000000 : 0
000007 : 0000000000000000 : 0
000008 : 0000000000000000 : 0
000009 : 0000000000000000 : 0
00000A : 0000000000000001 : 1
00000B : 0000000000000002 : 2
00000C : 0000000000000003 : 3
00000D : 0000000000000004 : 4
00000E : 0000000000000005 : 5
00000F : 0000000000000000 : 0
000010 : 0000000000000000 : 0
000011 : 0000000000000000 : 0
000012 : 0000000000000000 : 0
000013 : 0000000000000000 : 0" 0 20)
  
(T "Replace NOP by OUT at addres noot"
  '((MRD R1 aap)
    (MWR R1 noot)
    (NOP)
    (noot : NOP)
    (STP)
    (aap : OUT R1))
  ()
  ""
  "output : 0901000000000000 : 648799821318062080")

(T "Subroutine~n"
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
    (: return) (JMP R5))
  ((R0 7) (R1 1) (R2 2))
  "3"
  "output : 0000000000000007 : 7")

(T "Check WRT"
  '((SET R1 start)
    (SET R2 end)
    (SUB R3 R2 R1)
    (WRT R1 R3)
    (STP)
    (start : DATA 1 2 3 4 5)
    (end : DATA 0))
  ()
  ""
  "
output : 0000000000000001 : 1
output : 0000000000000002 : 2
output : 0000000000000003 : 3
output : 0000000000000004 : 4
output : 0000000000000005 : 5")

(test "Data only"
  ((assemble '((DATA -1 -2 -3)))) #f
  #:output "Source-code
000000 : (DATA -1 -2 -3)
Assembled code
000000 : FF F F F F FFFFFFFFFF : -1
000001 : FF F F F F FFFFFFFFFE : -2
000002 : FF F F F F FFFFFFFFFD : -3")


(test "DATA producing a NOP instruction"
  ((parameterize ((show '(binary-code instructions)))
     (execute '((NOP) (aap : DATA #x01F888FFFFFFFFFF aap))))) #f
  #:output "Assembled code
000000 : 01 0 0 0 0 0000000000 : 72057594037927936
000001 : 01 F 8 8 8 FFFFFFFFFF : 142014021355175935
000002 : 00 0 0 0 0 0000000001 : 1
Executing
  0 : NOP : 01 0 0 0 0 0000000000 : 000000 : 1
  1 : NOP : 01 F 8 8 8 FFFFFFFFFF : 000001 : 2
  2 : STP : 00 0 0 0 0 0000000001 : 000002 : 3")
  

(T "DATA producing a SET instruction"
  '((SET R1 aap) (aap : DATA #x02021FFFFFFFFFFF aap))
  ((R1 1) (R2 1)) "" "")

(test "Infinite loop"
  ((parameterize ((max-nr-of-instrs 10) (catch-crash #f) (show #f))
     (execute '((JMP 0)))))
  '()
  #:output "Executing"
  #:error "execute: max nr of instrs exceeded: 10")

(test "Infinite loop"
  ((let ((op (open-output-string)))
     (parameterize ((max-nr-of-instrs 21) (OUT-port op) (catch-crash #t) (show #f))
       (execute '((SET R1 -1) (SET R2 -1) (aap : OUT R2) (ADD R2 R2 R1) (JMP aap)))
       (newline)
       (display (get-output-string op)))))
  #f
  #:output "Executing
output : FFFFFFFFFFFFFFFF : -1
output : FFFFFFFFFFFFFFFE : -2
output : FFFFFFFFFFFFFFFD : -3
output : FFFFFFFFFFFFFFFC : -4
output : FFFFFFFFFFFFFFFB : -5
output : FFFFFFFFFFFFFFFA : -6
output : FFFFFFFFFFFFFFF9 : -7"
  #:error "execute: max nr of instrs exceeded: 21"
  #:exn #f)

(test "Unknown cc"
  ((parameterize ((catch-crash #t) (show #f))
     (execute '((SET R1 aap) (aap : DATA #x03FFFFFFFFFFFFFF aap)))))
  #f
  #:output "Executing"
  #:error "unknown opcode/cc 3 15"
  #:exn #f)

(test "Unknown opcode"
  ((parameterize ((catch-crash #t) (show #f))
     (execute '((SET R1 aap) (aap : DATA #xFF00000000000000 aap)))))
  #f
  #:output "Executing"
  #:error "unknown opcode/cc 255 0"
  #:exn #f)

(test "Unknown register"
  ((parameterize ((catch-crash #f) (show #f))
     (execute '((SET R1 aap) (aap : DATA #x030FFFFFFFFFFFFF aap)))))
  '()
  #:output "Executing"
  #:error "execute: unknow register nr: 15"
  #:exn #t)

(test-report)