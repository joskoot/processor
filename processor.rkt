#lang racket/base

(provide
  assembler
  execute
  INPUT-port
  OUTPUT-port
  max-nr-of-instrs
  print-registers?
  print-program?
  print-instrs?
  print-memory
  print-stack
  align)

(require
  (only-in racket ~r natural? match)
  (for-syntax racket/base))

(define SHIFT arithmetic-shift)
(define AND bitwise-and)
(define IOR bitwise-ior)
(define NOT bitwise-not)
(define BIT-SET? bitwise-bit-set?)
(define INTEGER? exact-integer?)
(define force-bool (λ (x) (and x #t)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define W-size 64)
(define W-digits (quotient W-size 4))
(define W-mask (sub1 (SHIFT 1 W-size)))
(define W-sign-bit (sub1 W-size))
(define W-sign-extension (SHIFT -1 W-size))

(define (W-negative? w)
  (BIT-SET? w W-sign-bit))

(define (W-sign-extend w)
  (if (W-negative? w)
    (IOR W-sign-extension w)
    w))

(define (W-fmt-hex w)
  (string-upcase
    (~r w #:base 16 #:min-width W-digits #:pad-string "0")))

(define (W-fmt-dec w) (W-sign-extend w))
(define A-size 24)
(define A-digits (quotient A-size 4))
(define A-mask (sub1 (SHIFT 1 A-size)))

(define (A-fmt-hex a)
  (string-upcase
    (~r a #:base 16 #:min-width A-digits #:pad-string "0")))

(define D-size 24)
(define D-mask (sub1 (SHIFT 1 D-size)))
(define D-sign-bit (sub1 D-size))
(define D-sign-extension (AND W-mask (SHIFT -1 D-size)))
(define (D-negative? d) (BIT-SET? d D-sign-bit))
(define (D-sign-extend d) (if (D-negative? d) (IOR D-sign-extension d) d))
(define (D->W d) (AND W-mask (D-sign-extend d)))

(struct R (name proc)
  #:property prop:object-name 0
  #:property prop:procedure 1)

(struct W R ()
  #:property prop:custom-write (λ (w p m) (fprintf p "#<~s:~a>" (R-name w) (W-fmt-hex (w)))))

(struct A R ()
  #:property prop:custom-write (λ (a p m) (fprintf p "#<~s:~a>" (R-name a) (A-fmt-hex (a)))))

(define (make-W-proc)
  (let ((in 0) (out 0))
    (λ ((event #f))
      (cond
        ((not event) out)
        ((INTEGER? event) (set! in (AND W-mask event)) out)
        (else (set! out in) in)))))

(define (make-A-proc)
  (let ((in 0) (out 0))
    (λ ((event #f))
      (cond
        ((not event) out)
        ((INTEGER? event) (set! in (AND A-mask event)) out)
        (else (set! out in) in)))))

(define (make-W name) (W name (make-W-proc)))
(define (make-A name) (A name (make-A-proc)))
(define opcode-size 8)
(define r-size 4)
(define cc-size 4)
(define opcode-pos (- W-size opcode-size))
(define cc-pos (- opcode-pos r-size))
(define ra-pos (- cc-pos r-size))

(define (compose-instr opcode cc ra rb rc d)
  (IOR
    (SHIFT opcode 56)
    (SHIFT cc 52)
    (SHIFT ra 48)
    (SHIFT rb 44)
    (SHIFT rc 40)
    (AND D-mask d)))

(define (decompose-instr instr)
  (values
    (AND #xFF (SHIFT instr -56))
    (AND #xF (SHIFT instr -52))
    (AND #xF (SHIFT instr -48))
    (AND #xF (SHIFT instr -44))
    (AND #xF (SHIFT instr -40))
    (D->W (AND instr D-mask))))

(define IR (make-W 'IR))
(define R0 (make-W 'R0))
(define R1 (make-W 'R1))
(define R2 (make-W 'R2))
(define R3 (make-W 'R3))
(define R4 (make-W 'R4))
(define R5 (make-W 'R5))
(define R6 (make-W 'R6))
(define SP (make-A 'SP))
(define PC (make-A 'PC))
(define (DA) (D->W (AND D-mask (IR))))
(define R-vector (vector R0 R1 R2 R3 R4 R5 R6 SP DA))
(define registers (list R0 R1 R2 R3 R4 R5 R6 SP))
(define R-hash (hasheq 0 R0 1 R1 2 R2 3 R3 4 R4 5 R5 6 R6 7 SP 8 DA))
(define (r->R r) (vector-ref R-vector r))

(define (reset-registers)
  (clock
    (R0 0)
    (R1 0)
    (R2 0)
    (R3 0)
    (R4 0)
    (R5 0)
    (R6 0)
    (SP A-mask)
    (PC 0)))

(define last-addr 'yet-to-be-assigned)

(define-syntax (clock stx)
  (syntax-case stx ()
    ((_ (R w) ...)
     #'(begin
         (R w) ... (R 'clock) ...))))

(define-syntax (clock! stx)
  (syntax-case stx ()
    ((_ (R w) ...)
     #'(begin
         (clock (R w) ...)
         (next-instr)))))

(define-syntax (clock+ stx)
  (syntax-case stx ()
    ((_ (R w) ...)
     #'(begin
         (set! last-addr (PC))
         (clock (R w) ... (IR (memory-ref (PC))) (PC (add1 (PC))))))))

(define (next-instr)
  (set! last-addr (PC))
  (clock (IR (memory-ref(PC))) (PC (add1 (PC)))))

(define memory 'yet-to-be-assigned)
(define (reset-memory) (set! memory (make-vector (add1 A-mask))))
(define cycle-count 0)

(define (memory-set! a w)
  (set! cycle-count (add1 cycle-count))
  (vector-set! memory (AND A-mask a) w))

(define (memory-ref a)
  (set! cycle-count (add1 cycle-count))
  (vector-ref memory (AND A-mask a)))

(define (print-memory (n (add1 A-mask)))
  (for ((k (in-range (min (min n (add1 (count-instrs)))))))
    (printf "~a: ~a ~s~n"
      (string-upcase (~r #:base 16 #:min-width (align) k))
      (W-fmt-hex (vector-ref memory k))
      (W-fmt-dec (vector-ref memory k)))))

(define (print-stack n)
  (for ((k (in-range (- A-mask n -1) (add1 A-mask))))
    (printf "~a: ~a ~s~n"
      (string-upcase (~r #:base 16 #:min-width (align) k))
      (W-fmt-hex (vector-ref memory k))
      (W-fmt-dec (vector-ref memory k)))))

(define (count-instrs)
  (let loop ((k A-mask))
    (cond
      ((zero? k) (if (zero? (vector-ref memory 0)) 0 1))
      ((zero? (vector-ref memory k)) (loop (sub1 k)))
      (else (add1 k)))))

(define (MRD ra rb)
  (clock! ((r->R ra) (memory-ref ((r->R rb))))))

(define (MWR ra (rb #f))
  (define Ra (r->R ra))
  (memory-set! ((r->R rb)) (Ra))
  (next-instr))

(define (SET ra rb)
  (define Ra (r->R ra))
  (define Rb (r->R rb))
  (clock+ (Ra (Rb))))

(define (ALU cc ra rb rc)
  (clock+
    ((r->R ra)
     ((vector-ref ALU-vector cc)
      (W-sign-extend ((r->R rb)))
      (W-sign-extend ((r->R rc)))))))

(define (SHIFTR w k) (SHIFT w (- k)))
(define (SHIFTE w k) (SHIFT (W-sign-extend w) (- k)))

(define ALU-vector
  (vector
    +
    -
    *
    quotient
    SHIFT
    SHIFTR
    SHIFTE
    AND
    IOR
    (λ (w1 w2) (- w1))
    (λ (w1 w2) (NOT w1))))

(define (JMP ra)
  (define a ((r->R ra)))
  (set! last-addr a)
  (clock (IR (memory-ref a)) (PC (add1 a))))

(define (CMP cc ra rb rc)
  (define Ra (r->R ra))
  (define Rb (r->R rb))
  (define a ((r->R rc)))
  (cond
    (((vector-ref CMP-vector cc)
      (W-sign-extend ((r->R ra)))
      (W-sign-extend ((r->R rb))))
     (set! last-addr a)
     (clock (IR (memory-ref a)) (PC (add1 a))))
    (else (next-instr))))

(define CMP-vector (vector = < > <= >=))

(define (IF? cc ra rb)
  (define Ra (r->R ra))
  (define a ((r->R rb)))
  (cond
    (((vector-ref IF-vector cc) (Ra))
     (set! last-addr a)
     (clock (IR (memory-ref a)) (PC (add1 a))))
    (else (next-instr))))

(define (=0? w) (zero? (W-sign-extend w)))
(define (<0? w) (W-negative? w))
(define (>0? w) (not (W-negative? w)))
(define (≤0? w) (<= (W-sign-extend w) 0))
(define (≥0? w) (>= (W-sign-extend w) 0))
(define IF-vector (vector =0? <0? >0? ≤0? ≥0? even? odd?))

(define (PSH ra)
  (define w ((r->R ra)))
  (define sp (SP))
  (memory-set! sp w)
  (clock! (SP (sub1 sp))))

(define (POP ra)
  (define sp (add1 (SP)))
  (clock! ((r->R ra) (memory-ref sp)) (SP sp)))

(define (OUT (ra #f))
  (define Ra (r->R ra))
  (define w (Ra))
  (fprintf (OUTPUT-port) "output : ~s : ~a : ~s~n"
    (if (R? Ra) (R-name Ra) 'datum)
    ((if (A? Ra) A-fmt-hex W-fmt-hex) w)
    (W-fmt-dec w))
  (next-instr))

(define (OUT-guard p)
  (unless (output-port? p)
    (raise-argument-error 'OUT-port "output-port?" p))
  p)

(define OUTPUT-port (make-parameter (current-output-port) OUT-guard 'OUT-port))

(define (INP-guard p)
  (unless (input-port? p)
    (raise-argument-error 'INP-port "input-port?" p))
  p)

(define INPUT-port (make-parameter (current-input-port) INP-guard 'INP-port))

(define (INP ra)
  (define input (read (INPUT-port)))
  (unless (INTEGER? input)
    (raise-argument-error "exact integer?" input))
  (clock+ ((r->R ra) input)))

(define (max-nr-of-instrs-guard n)
  (unless (natural? n) (raise-argument-error 'max-nr-of-instrs "natural?" n))
  n)

(define print-registers? (make-parameter #t force-bool 'print-parameters?))
(define print-program? (make-parameter #t force-bool 'print-program?))
(define print-instrs? (make-parameter #t force-bool 'print-instrs?))
(define max-nr-of-instrs (make-parameter 1000 max-nr-of-instrs-guard 'max-nr-of-instrs))

(define (execute (program #f))
  (when program (assembler program))
  (printf "Executing~n")
  (reset-registers)
  (set! cycle-count 0)
  (next-instr)
  (executor 0)
  (newline))

(define (align-guard n) (unless (natural? n) (raise-argument-error 'align "narural?" n)) n)

(define align (make-parameter 3 align-guard 'align))

(define (reset) (reset-memory) (reset-registers))

(define (executor k)
  (define-values (opcode cc ra rb rc dd) (decompose-instr (IR)))
  (when (print-instrs?)
    (printf "~a : ~a : ~s : ~a ~a ~a ~a ~a ~a : ~s~n"
      (string-upcase (~r #:base 10 #:min-width (align) k))
      (A-fmt-hex last-addr)
      (mnemonic opcode cc)
      (string-upcase (~r #:base 16 #:min-width 2 #:pad-string "0" opcode))
      (string-upcase (~r #:base 16 #:min-width 1 #:pad-string "0" cc))
      (string-upcase (~r #:base 16 #:min-width 1 #:pad-string "0" ra))
      (string-upcase (~r #:base 16 #:min-width 1 #:pad-string "0" rb))
      (string-upcase (~r #:base 16 #:min-width 1 #:pad-string "0" rc))
      (string-upcase (~r #:base 16 #:min-width 10 #:pad-string "0" dd))
      cycle-count))
  (cond
    ((zero? opcode)
     (if (print-instrs?)
       (printf "Program halted~n~n")
       (printf "Program halted (~s cycles)~n~n" cycle-count))
     (when (print-registers?)
       (for ((R (in-list registers))) (printf "~s~n" R))
       (printf "~s~n" PC)))
    (else
      (case opcode
        ((#x01) (next-instr))
        ((#x02) (SET ra rb))
        ((#x03) (ALU cc ra rb rc))
        ((#x04) (CMP cc ra rb rc))
        ((#x05) (IF? cc ra rb))
        ((#x06) (JMP ra))
        ((#x07) (OUT ra))
        ((#x08) (INP ra))
        ((#x09) (PSH ra))
        ((#x0A) (POP ra))
        ((#x0B) (MRD ra rb))
        ((#x0C) (MWR ra rb)))
      (cond
        ((< k (max-nr-of-instrs)) (executor (add1 k)))
        (else (printf "Max nr of instrs exceeded~n"))))))

(define (mnemonic opcode cc)
  (case opcode
    ((#x00) 'STP)
    ((#x01) 'NOP)
    ((#x02) 'SET)
    ((#x03) (case cc
              ((#x0) 'ADD)
              ((#x1) 'SUB)
              ((#x2) 'MUL)
              ((#x3) 'DIV)
              ((#x4) 'SHL)
              ((#x5) 'SHR)
              ((#x6) 'SHE)
              ((#x7) 'AND)
              ((#x8) 'IOR)
              ((#x9) 'MIN)
              ((#xA) 'NOT)
              (else #f)))
    ((#x04) (case cc
              ((#x0) 'EQ?)
              ((#x1) 'LT?)
              ((#x2) 'GT?)
              ((#x3) 'LE?)
              ((#x4) 'GE?)
              (else #f)))
    ((#x05) (case cc
              ((#x0) '=0?)
              ((#x1) '<0?)
              ((#x2) '>0?)
              ((#x3) '≤0?)
              ((#x4) '≥0?)
              (else #f)))
    ((#x06) 'JMP)
    ((#x07) 'OUT)
    ((#x08) 'INP)
    ((#x09) 'PSH)
    ((#x0A) 'POP)
    ((#x0B) 'MRD)
    ((#x0C) 'MWR)
    (else #f)))

(define (assembler instrs)
  (reset-memory)
  (define addr-hash (make-hasheq))
  (define register-hash (hasheq 'R0 0 'R1 1 'R2 2 'R3 3 'R4 4 'R5 5 'R6 6 'SP 7))
  
  (define (register-nr r)
    (hash-ref register-hash r (λ () (error 'assemble "unknown register: ~s" r))))
  
  (define (register? r) (hash-ref register-hash r #f))
  (define (addr? a) (and (symbol? a) (not (register? a))))
  (define (make-DATUM-instr d) (list 'DATUM d))
  
  (define alu-hash
    (hasheq 'ADD 0 'SUB 1 'MUL 2 'DIV 3 'SHL 4 'SHR 5 'SHE 6 'AND 7 'IOR 8 'MIN 9 'NOT 10))

  (define cmp-hash (hasheq 'EQ? 0 'LT? 1 'GT? 2 'LE? 3 'GE? 4))
  (define if-hash (hasheq '=0? 0 '<0? 1 '>0? 2 '≤0? 3 '≥0? 4))
  
  (define (check-datum d)
    (or (addr? d)
      (INTEGER? d)
      (error 'assemble "illegal address: ~s" d)))

  (define (addr-set! a k)
    (when (hash-ref addr-hash a #f)
      (error 'assemble "duplicate address: ~s" a))
    (hash-set! addr-hash a k))

  (define (addr-ref datum)
    (hash-ref addr-hash datum
      (λ ()
        (error 'assemble "unknown datum: ~s" datum))))

  (define (instr-error instr) (error 'assemble "incorrect instruction; ~s" instr))
  
  (define (assemble-phase1 k instrs)
    (cond
      ((null? instrs) '())
      (else
        (match (car instrs)
          ((list ': rest ...) (assemble-phase1 k (cdr instrs)))
          ((list addr ': 'DATA data ...)
           #:when (addr? addr)
           #:when (andmap check-datum data)
           #:do ((addr-set! addr k))
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) (cdr instrs))))
          ((list 'DATA data ...)
           #:when (andmap check-datum data)
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) (cdr instrs))))
          ((list addr ': rest ...)
           #:when (addr? addr)
           #:do ((addr-set! addr k))
           (cons rest (assemble-phase1 (add1 k) (cdr instrs))))
          ((list rest ...)
           (cons rest (assemble-phase1 (add1 k) (cdr instrs))))))))

  (define (assemble-phase2 k instrs)
    (unless (null? instrs)
      (memory-set! k
        (match (car instrs)
          ((list 'DATUM datum)
           (if (symbol? datum) (addr-ref datum) (AND W-mask datum)))
          ((list 'STP) 0)
          ((list 'NOP) (compose-instr 1 0 0 0 0 0))
          ((list 'SET ra rb)
           (cond
             ((register? rb)
              (compose-instr 2 0 (register-nr ra) (register-nr rb) 0 0))
             ((INTEGER? rb)
              (compose-instr 2 0 (register-nr ra) 8 0 (AND D-mask rb)))
             (else (compose-instr 2 0 (register-nr ra) 8 0 (addr-ref rb)))))
          ((list alu ra rb rc)
           #:do ((define cc (hash-ref alu-hash alu #f)))
           #:when cc
           (compose-instr 3 cc (register-nr ra) (register-nr rb) (register-nr rc) 0))
          ((list cmp ra rb rc)
           #:do ((define cc (hash-ref cmp-hash cmp #f)))
           #:when cc
           (cond
             ((register? rc)
              (compose-instr 4 cc (register-nr ra) (register-nr rb) (register-nr rc) 0))
             ((INTEGER? rc)
              (compose-instr 4 cc (register-nr ra) (register-nr rb) 8 (AND D-mask rc)))
             (else (compose-instr 4 cc (register-nr ra) (register-nr rb) 8 (addr-ref rc)))))
          ((list if? ra rb)
           #:do ((define cc (hash-ref if-hash if? #f)))
           #:when cc
           (cond
             ((register? rb) (compose-instr 5 cc (register-nr ra) (register-nr rb) 0 0))
             ((INTEGER? rb) (compose-instr 5 cc (register-nr ra) 8 0 (AND D-mask rb)))
             (else (compose-instr 5 cc (register-nr ra) 8 0 (addr-ref rb)))))
          ((list 'JMP ra)
           (cond
             ((register? ra) (compose-instr 6 0 (register-nr ra) 0 0 0))
             ((INTEGER? ra) (compose-instr 6 0 8 0 0 (AND D-mask ra)))
             (else (compose-instr 6 0 8 0 0 (addr-ref ra)))))
          ((list 'OUT ra)
           (cond
             ((register? ra) (compose-instr 7 0 (register-nr ra) 0 0 0))
             ((INTEGER? ra) (compose-instr 7 0 8 0 0 (AND D-mask ra)))
             (else (compose-instr 7 0 8 0 0 (addr-ref ra)))))
          ((list 'INP ra) (compose-instr 8 0 (register-nr ra) 0 0 0))
          ((list 'PSH ra)
           (cond
             ((register? ra) (compose-instr 9 0 (register-nr ra) 0 0 0))
             ((INTEGER? ra) (compose-instr 9 0 8 0 0 (AND D-mask ra)))
             (else (compose-instr 9 0 8 0 0 (addr-ref ra)))))         
          ((list 'POP ra)
           (cond
             ((register? ra) (compose-instr 10 0 (register-nr ra) 0 0 0))
             ((INTEGER? ra) (compose-instr 10 0 8 0 0 (AND D-mask ra)))
             (else (compose-instr 10 0 (register-nr ra) 0 0 0))))
          ((list 'MRD ra rb)
           (cond
             ((register? rb) (compose-instr 11 0 (register-nr ra) (register-nr rb) 0 0))
             ((INTEGER? rb) (compose-instr 11 0 (register-nr ra) 8 0 (AND D-mask rb)))
             (else (compose-instr 11 0 (register-nr ra) 8 0 (addr-ref rb)))))
          ((list 'MWR ra rb)
           (cond
             ((register? rb) (compose-instr 12 0 (register-nr ra) (register-nr rb) 0 0))
             ((INTEGER? rb) (compose-instr 12 0 (register-nr ra) 8 0 (AND D-mask rb)))
             (else (compose-instr 12 0 (register-nr ra) 8 0 (addr-ref rb)))))
           
          (else (instr-error else))))
      (assemble-phase2 (add1 k) (cdr instrs))))
  
  (assemble-phase2 0 (assemble-phase1 0 instrs))
  (when (print-program?) (printf "Program~n") (print-memory) (newline)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════
