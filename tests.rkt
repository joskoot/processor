#lang racket/base

(require "simulator.rkt" test/test (only-in racket open-output-nowhere))
;(require processor/simulator test/test (only-in racket open-output-nowhere))

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
FFFFB : 0000000000000000 : 0
FFFFC : 0000000000000000 : 0
FFFFD : 0000000000000000 : 0
FFFFE : 0000000000000000 : 0
FFFFF : 000000000000006F : 111" 5)

(TM "Check MRD and MWR"
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
00000 : 0218000000000001 : 150870587516911617
00001 : 0228000000000007 : 155374187144282119
00002 : 2738000000000007 : 2826008766174986247
00003 : 0342100000000000 : 234767722762731520
00004 : 0333300000000000 : 230580782484160512
00005 : 2634000000000000 : 2752825272230215680
00006 : 0000000000000000 : 0
00007 : 0000000000000111 : 273
00008 : 0000000000000222 : 546
00009 : 0000000000000000 : 0"
  0 10)

(TM "Check RÆD"
  '((SET R1 10) (SET R2 5) (RÆD R1 R2) (NOP))
  ()
  "1 2 3 4 5"
  "
00000 : 021800000000000A : 150870587516911626
00001 : 0228000000000005 : 155374187144282117
00002 : 2412000000000000 : 2599139934946197504
00003 : 0100000000000000 : 72057594037927936
00004 : 0000000000000000 : 0
00005 : 0000000000000000 : 0
00006 : 0000000000000000 : 0
00007 : 0000000000000000 : 0
00008 : 0000000000000000 : 0
00009 : 0000000000000000 : 0
0000A : 0000000000000001 : 1
0000B : 0000000000000002 : 2
0000C : 0000000000000003 : 3
0000D : 0000000000000004 : 4
0000E : 0000000000000005 : 5
0000F : 0000000000000000 : 0
00010 : 0000000000000000 : 0
00011 : 0000000000000000 : 0
00012 : 0000000000000000 : 0
00013 : 0000000000000000 : 0" 0 20)
  
(T "Replace NOP by OUT at addres noot"
  '((MRD R1 aap)
    (MWR R1 noot)
    (NOP)
    (noot : NOP)
    (STP)
    (aap : OUT R1))
  ()
  ""
  "output : 2310000000000000 : 2526519390954848256")

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
00000 : (DATA -1 -2 -3)
Assembled code
00000 : FF F F F FFFFFFFFFFF : -1
00001 : FF F F F FFFFFFFFFFE : -2
00002 : FF F F F FFFFFFFFFFD : -3")

(test "DATA producing a NOP instruction"
  ((parameterize ((show '(binary-code instructions)))
     (execute '((NOP) (aap : DATA #x01F888FFFFFFFFFF aap))))) #f
  #:output "
 Assembled code
00000 : 01 0 0 0 00000000000 : 72057594037927936
00001 : 01 F 8 8 8FFFFFFFFFF : 142014021355175935
00002 : 00 0 0 0 00000000001 : 1
Executing
0 : NOP : 01 0 0 0 00000000000 : 00000 : 1
1 : NOP : 01 F 8 8 8FFFFFFFFFF : 00001 : 2
2 : STP : 00 0 0 0 00000000001 : 00002 : 3")
  
(T "DATA producing a SET instruction"
  '((SET R1 aap) (aap : DATA #x0221FFFFFFFFFFFF aap))
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

(test "Unknown opcode"
  ((parameterize ((catch-crash #t) (show #f))
     (execute '((SET R1 aap) (aap : DATA #xFF00000000000000 aap)))))
  #f
  #:output "Executing"
  #:error "execute: unknown opcode: #xff"
  #:exn #f)

(test "Unknown register"
  ((parameterize ((catch-crash #f) (show #f))
     (execute '((SET R1 aap) (aap : DATA #x030FFFFFFFFFFFFF aap)))))
  '()
  #:output "Executing"
  #:error "execute: unknow register nr: #xf"
  #:exn #t)

(test "reading a program and running it"
  ((parameterize ((show #f))
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
     (define input
       (let ((op (open-output-string)))
         (for ((instr (in-list factorial-code))) (writeln (cdr instr) op))
         (open-input-string (get-output-string op))))
     (assemble
       `((SET R0 5)
         (RÆD R0 ,(length factorial-code))
         (JMP R0)))
     (parameterize ((INP-port input) (OUT-port (current-output-port))) (execute))))
  #F
  #:output "Executing
output : 0000000000000078 : 120
Execution halted after 37 cycles")

(test "AND and IOR"
  ((parameterize ((show #f) (OUT-port (current-output-port)))
     (execute
       '((SET R1 #xFF0000FFFF)
         (SET R2 #x00FF00FF00)
         (AND R3 R1 R2)
         (IOR R4 R1 R2)
         (OUT R3) (OUT R4)))))
  #f
  #:output
  "
Executing
output : 000000000000FF00 : 65280
output : 000000FFFF00FFFF : 1099494916095
Execution halted after 7 cycles")

(test "ROT"
  ((parameterize ((show #f) (current-output-port (open-output-nowhere)))
     (execute
       '((MRD R0 aap)
         (ROT R1 R0 -8)
         (ROT R2 R0 8)
         (STP)
         (aap : DATUM #xFFF000FFFFFFFFF0))))
   (displayln (~wx (R1)))
   (displayln (~wx (R2))))
  #f
  #:output "F0FFF000FFFFFFFF F000FFFFFFFFF0FF")

(test "integer square root"
  ((parameterize ((current-output-port (open-output-nowhere)))
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
     (define p (open-output-string))
     (parameterize ((OUT-port p) (show #f))
       (for ((k (in-range 101)))
         (parameterize ((INP-port (open-input-string (format "~s" k))))
           (execute))))
     (parameterize ((current-input-port (open-input-string (get-output-string p))))
       (let loop ((k 0))
         (cond
           ((equal? (read) eof) k)
           (else
             (let ((sqr (begin (read) (read) (read) (read)))
                   (root (begin (read) (read) (read) (read) (read))))
               (unless (= root (integer-sqrt sqr)) (error "root" sqr root)))
             (loop (add1 k))))))))
  '(101))



(test-report)