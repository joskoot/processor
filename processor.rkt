#lang racket/base

;═════════════════════════════════════════════════════════════════════════════════════════════════════
; Documentation can be made from file "processor.scrbl"
;═════════════════════════════════════════════════════════════════════════════════════════════════════

(provide
  assemble execute
  show-source-code show-registers show-assembled-program show-instructions
  max-nr-of-instrs align catch-exn reset
  INP-port OUT-port
  print-memory print-stack print-registers
  R0 R1 R2 R3 R4 R5 R6 SP PC IR)

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(require (only-in racket ~r match natural?))
(define INTEGER? exact-integer?)
(define AND bitwise-and)
(define IOR bitwise-ior)
(define NOT bitwise-not)
(define SHIFT arithmetic-shift)
(define-syntax-rule (~R x ...) (string-upcase (~r x ...)))
(define (~h n width) (~R n #:base 16 #:min-width width #:pad-string "0"))
(define (force-bool x) (and x #t))
(define W-size 64) ; words
(define A-size 24) ; addresses (have no sign, never negative)
(define D-size 40) ; datum-part of an instruction
(define W-digits (quotient W-size 4))
(define A-digits (quotient A-size 4))
(define W-mask (sub1 (SHIFT 1 W-size)))
(define A-mask (sub1 (SHIFT 1 A-size)))
(define D-mask (sub1 (SHIFT 1 D-size)))
(define W-sign-extension (SHIFT -1 W-size))
(define D-sign-extension (SHIFT -1 D-size))
(define W-sign-bit (SHIFT 1 (sub1 W-size)))
(define D-sign-bit (SHIFT 1 (sub1 D-size)))
(define (W-negative? w) (not (zero? (AND W-sign-bit w))))
(define (D-negative? w) (not (zero? (AND D-sign-bit w))))
(define (D-sign-extend d) (if (D-negative? d) (IOR d D-sign-extension) d))
(define (mask-W w) (AND w W-mask))
(define (mask-A a) (AND a A-mask))
(define (mask-D d) (AND d D-mask))
(define (D->W d) (if (D-negative? d) (mask-W (D-sign-extend d)) d))
(define (W-fmt-hex w) (~h w W-digits))
(define (A-fmt-hex a) (~h a A-digits))

(define (W-sign-extend w)
  (let ((w (if (E? w) (w) w)))
    (if (W-negative? w)
      (IOR w W-sign-extension)
      w)))
  
(struct E (name proc) #:property prop:object-name 0 #:property prop:procedure 1)
(define (W-printer o p m) (fprintf p "#<~s:~a>" (E-name o) (W-fmt-hex ((E-proc o)))))
(define (A-printer o p m) (fprintf p "#<~s:~a>" (E-name o) (A-fmt-hex ((E-proc o)))))
(struct W E () #:property prop:custom-write W-printer)
(struct A E () #:property prop:custom-write A-printer)

(define (make-W/A-proc mask)
  (let ((in 0) (out 0))
    (λ ((event #f))
      (cond
        ((not event) out)
        ((eq? event 'clock) (set! out in) in)
        (else (set! in (AND mask event)) out)))))

(define (make-W-proc) (make-W/A-proc W-mask))
(define (make-A-proc) (make-W/A-proc A-mask))
(define (make-W name) (W name (make-W-proc)))
(define (make-A name) (A name (make-A-proc)))
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
(define register-vector (vector R0 R1 R2 R3 R4 R5 R6 SP PC)) 
(define (reset-registers) (for ((R (in-vector register-vector))) (clock (R 0))) (clock (SP A-mask)))
(define (P+ . ignore) (mask-A (add1 (PC))))
(define (S+ . ignore) (mask-A (add1 (SP))))
(define (S- . ignore) (mask-A (sub1 (SP))))
(define (DA . ignore) (mask-W (D-sign-extend (mask-D (IR)))))
(define (ALU op Ra Rb) (mask-W (op (W-sign-extend (Ra)) (W-sign-extend (Rb)))))
(define (SHL w k) (SHIFT w (max 0 k)))
(define (SHR w k) (SHIFT (mask-W w) (- (max 0 k))))
(define (SHE w k) (SHIFT w (- (max 0 k))))
(define (CMP op Ra (Rb #f)) (op (W-sign-extend (Ra)) (if Rb (W-sign-extend (Rb)) 0)))
(define (make-memory) (make-vector (SHIFT 1 A-size) 0))
(define memory (make-memory))
(define (reset-memory) (set! memory (make-memory)))
(define cycle-count 0) ; A program takes as many clock cycles as references to memory.
(define (reset) (reset-memory) (reset-registers) (set! cycle-count 0))

(define (MEM addr (w #f))
  (set! cycle-count (add1 cycle-count))
  (if w
    (vector-set! memory (mask-A addr) w)
    (vector-ref memory (mask-A addr))))
 
(define (print-registers)
  (for ((R (in-vector register-vector)))
    (printf "~s : ~s~n" R (W-sign-extend (R)))))

(define (print-memory n)
  (unless (natural? n)
    (raise-argument-error 'print-memory "natural?" n))
  (for ((k (in-range n)))
    (define w (vector-ref memory k))
    (printf "~a : ~a : ~s~n" (A-fmt-hex k) (W-fmt-hex w) w)))

(define (print-stack n)
  (unless (natural? n)
    (raise-argument-error 'print-stack "natural?" n))
  (define end (add1 A-mask))
  (for ((k (in-range (- end n) end)))
    (define w (vector-ref memory k))
    (printf "~a : ~a : ~s~n" (A-fmt-hex k) (W-fmt-hex w) w)))

(define (OUT-port-guard p)
  (unless (output-port? p) (raise-argument-error 'OUT-port "output-port?" p))
  p)

(define (INP-port-guard p)
  (unless (input-port? p) (raise-argument-error 'INP-port "input-port?" p))
  p)

(define (align-guard n)
  (unless (natural? n)
    (raise-argument-error 'align "natural?" n))
  n)

(define (max-nr-of-instrs-guard n)
  (unless (natural? n)
    (raise-argument-error 'max-nr-of-instrs "natural?" n))
  n)

(define show-registers (make-parameter #t force-bool 'show-registers))
(define show-instructions (make-parameter #t force-bool 'show-instructions))
(define show-assembled-program (make-parameter #t force-bool 'show-assembled-program))
(define show-source-code (make-parameter #t force-bool 'show-source-code))
(define align (make-parameter 3 align-guard 'align))
(define OUT-port (make-parameter (current-output-port) OUT-port-guard 'OUT-port))
(define INP-port (make-parameter (current-input-port) INP-port-guard 'INP-port))
(define max-nr-of-instrs (make-parameter 1000 max-nr-of-instrs-guard 'max-nr-of-instrs))
(define catch-exn (make-parameter #t force-bool 'catch-exn?))

(define (ALIGN k)
  (let* ((strng (format "~s" k)) (n (string-length strng)) (m (align)))
    (string-append (make-string (max 0 (- m n)) #\space) strng)))

(define-values (opcode-pos cc-pos ra-pos rb-pos rc-pos) (values 56 52 48 44 40))
(define-values (opcode-mask cc-mask r-mask) (values #xFF #xF #xF))

(define (compose-instr op cc ra rb rc d)
  (IOR
    (SHIFT op opcode-pos)
    (SHIFT cc cc-pos)
    (SHIFT ra ra-pos)
    (SHIFT rb rb-pos)
    (SHIFT rc rc-pos)
    (mask-D d)))

(define (decompose-instr instr)
  (values
    (AND opcode-mask (SHIFT instr (- opcode-pos)))
    (AND cc-mask     (SHIFT instr (- cc-pos)))
    (AND r-mask      (SHIFT instr (- ra-pos)))
    (AND r-mask      (SHIFT instr (- rb-pos)))
    (AND r-mask      (SHIFT instr (- rc-pos)))
    (mask-D instr)))

(define mnemonics
  `((#x0 #f  STP #f)
    (#x1 #f  NOP #f)
    (#x2 #f  SET #f)
    (#x3 #x0 ADD ,+)
    (#x3 #x1 SUB ,-)
    (#x3 #x2 MUL ,*)
    (#x3 #x3 DIV ,quotient)
    (#x3 #x4 AND ,AND)
    (#x3 #x5 IOR ,IOR)
    (#x3 #x6 SHL ,SHL)
    (#x3 #x7 SHR ,SHR)
    (#x3 #x8 SHE ,SHE)
    (#x3 #x9 NEG ,(λ (x (y 0)) (- (W-sign-extend x))))
    (#x3 #xA NOT ,(λ (x (y 0)) (NOT x)))
    (#x4 #x0 EQ? ,(λ (x (y 0)) (= (x) (y))))
    (#x4 #x1 LT? ,(λ (x (y 0)) (< (W-sign-extend x) (W-sign-extend y))))
    (#x4 #x2 GT? ,(λ (x (y 0)) (> (W-sign-extend x) (W-sign-extend y))))
    (#x4 #x3 LE? ,(λ (x (y 0)) (<= (W-sign-extend x) (W-sign-extend y))))
    (#x4 #x4 GE? ,(λ (x (y 0)) (>= (W-sign-extend x) (W-sign-extend y))))
    (#x5 #x0 =0? ,zero?)
    (#x5 #x1 <0? ,(λ (x (y 0)) (< (W-sign-extend x) 0)))
    (#x5 #x2 >0? ,(λ (x (y 0)) (> (W-sign-extend x) 0)))
    (#x5 #x3 ≤0? ,(λ (x (y 0)) (<= (W-sign-extend x) 0)))
    (#x5 #x4 ≥0? ,(λ (x (y 0)) (>= (W-sign-extend x) 0)))             
    (#x6 #f  JMP #f)
    (#x7 #f  PSH #f)
    (#x8 #f  POP #f)
    (#x9 #f  MRD #f)
    (#xA #f  MWR #f)
    (#xB #f  OUT #f)
    (#xC #f  INP #f)
    (#xD #f  WRT #f)
    (#xE #f  RÆD #f)))

(define mnemonic->opcode/cc
  (let
    ((mnemonics (map caddr mnemonics))
     (opcodes (map car mnemonics))
     (ccs (map cadr mnemonics)))
    (let*
      ((mnemonic->opcode/cc (λ (mnemonic opcode cc) (cons mnemonic (list opcode (or cc 0)))))
       (mnemonic-hash (make-immutable-hasheq (map mnemonic->opcode/cc mnemonics opcodes ccs))))
      (λ (mnemonic) (apply values (hash-ref mnemonic-hash mnemonic (λ () (list 'DATUM #f))))))))

(define (opcode/cc->mnemonic/CC opc cc)
  (apply values
    (or
      (for/first
        ((descr (in-list mnemonics))
         #:when (and (= opc (car descr)) (or (not (cadr descr)) (= cc (cadr descr)))))
        (cddr descr))
      (error 'execute "unknown opcode/cc: #x~a #x~a" (~h opc 2) (~h cc 1)))))

(define-syntax-rule
  (clock (R v) ...)
  (begin (R v) ... (R 'clock) ...))

(define-syntax-rule ; Read next instr in same cycle
  (clock+ (R v) ...)
  (clock (R v) ... (IR (MEM (PC))) (PC (P+))))

(define-syntax-rule ; Read next instr in next cycle
  (clock! (R v) ...)
  (begin (clock (R v) ...) (clock+)))

(define (next-instr) (clock+))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (execute (program #f))
  (define-syntax-rule (catch-exn:fail expr ...)
    (with-handlers ((exn:fail? (λ (exn) (displayln (exn-message exn) (current-error-port)))))
      (begin expr ...)))

  (define (executor k)
    (define r->R
      (let ((registers (vector R0 R1 R2 R3 R4 R5 R6 SP DA)))
        (λ (r)
          (cond
            ((<= r 8) (vector-ref registers r))
            (else (error 'execute "unknown register designator: #x~a" (~h r 1)))))))
    
    (define-syntax-rule (rs->Rs (r R) ...) (begin (define R (r->R r)) ...))
    (define-values (opcode cc ra rb rc da) (decompose-instr (IR)))
    (define-values (mnemonic CC) (opcode/cc->mnemonic/CC opcode cc))
    
    (when (show-instructions)
      (printf "~a : ~a : ~a : ~a ~a ~a ~a ~a ~a : ~s~n"
        (ALIGN k)
        (~h (mask-A (sub1 (PC))) 6)
        mnemonic
        (~h opcode 2)
        (~h cc 1)
        (~h ra 1)
        (~h rb 1)
        (~h rc 1)
        (~h da 10)
        cycle-count))
    
    (cond
      ((zero? opcode)
       (cond
         ((show-registers) (printf " ~nRegisters after program termination~n") (print-registers))
         (else (printf " ~nProgram terminated after ~s cycles~n" cycle-count))))
      (else
        (case opcode
          ((#x01) ; STP
           (next-instr))
          ((#x02) ; NOP
           (rs->Rs (ra Ra) (rb Rb))
           (clock+ (Ra (Rb))))
          ((#x03) ; ADD, SUV, MUL, DIV, etc. 
           (rs->Rs (ra Ra) (rb Rb) (rc Rc))
           (clock+ (Ra (ALU CC Rb Rc))))
          ((#x04) ; Conditional jump: EQ?, LR?, GT?, etc.
           (rs->Rs (ra Ra) (rb Rb) (rc Rc))
           (cond
             ((CC Ra Rb)
              (define addr (Rc))
              (clock (IR (MEM addr)) (PC (add1 addr))))
             (else (next-instr))))
          ((#x05) ; Conditional jump: =0?, <0?, >0?, etc.
           (rs->Rs (ra Ra) (rb Rb))
           (cond
             ((CC (Ra))
              (define addr (Rb))
              (clock (IR (MEM addr)) (PC (add1 addr))))
             (else (next-instr))))
          ((#x06) ; JMP
           (rs->Rs (ra Ra))
           (define addr (Ra))
           (clock (IR (MEM addr)) (PC (add1 addr))))
          ((#x07) ; PSH
           (rs->Rs (ra Ra))
           (MEM (SP) (Ra)) (clock+ (SP (S-))))
          ((#x08) ; POP
           (rs->Rs (ra Ra))
           (define addr (S+)) (clock! (SP addr) (Ra (MEM addr))))
          ((#x09) ; MRD
           (rs->Rs (ra Ra) (rb Rb))
           (define addr (Rb)) (clock! (Ra (MEM addr))))
          ((#x0A) ; MWR
           (rs->Rs (ra Ra) (rb Rb))
           (MEM (Rb) (Ra)) (next-instr))
          ((#x0B) ; OUT
           (rs->Rs (ra Ra))
           (fprintf (OUT-port) "output: ~a : ~s~n" (W-fmt-hex (Ra)) (W-sign-extend (Ra)))
           (next-instr))
          ((#x0C) ; INP
           (rs->Rs (ra Ra))
           (clock! (Ra (read (INP-port)))))
          ((#x0D) ; WRT
           (rs->Rs (ra Ra) (rb Rb))
           (define start (Ra)) (define length (Rb))
           (for ((addr (in-range start (+ start length) +1)))
             (define w (MEM addr))
             (fprintf (OUT-port) "output : ~a ~s~n" (W-fmt-hex w) (W-sign-extend w)))
           (next-instr))
          ((#x0E) ; RÆD
           (rs->Rs (ra Ra) (rb Rb))
           (define start (Ra)) (define length (W-sign-extend (Rb)))
           (for ((addr (in-range start (+ start length) +1))) (MEM addr (read (INP-port))))
           (next-instr))
          (else (error 'execute "unrecognized opcode/cc: #x~a #x~a" (~h opcode 2) (~h cc 1))))
        (cond
          ((>= k (max-nr-of-instrs)) (printf "Max nr of instructions exceeded : max=~s~n" k))
          (else (executor (add1 k)))))))
  
  (when program (assemble program))
  (set! cycle-count 0)
  (reset-registers)
  (next-instr)
  (when (show-instructions) (printf " ~nExecuting~n"))
  (if (catch-exn) (catch-exn:fail (executor 0)) (executor 0)))

; - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -

(define (assemble program)
  
  (unless (and (list? program) (andmap list? program))
    (raise-argument-error 'assemble "listof instr" program))
  
  (when (show-source-code)
    (printf " ~nSource code~n")
    (for ((instr (in-list program))) (printf "~s~n" instr)))
  
  (define registers '(R0 R1 R2 R3 R4 R5 R6 SP))
  (define R-hash (for/hasheq ((R (in-list registers)) (k (in-naturals))) (values R k)))
  (define (R->r R) (hash-ref R-hash R))
  (define addr-hash (make-hasheq))
  (define (make-DATUM-instr datum) (list 'DATUM (and W-mask datum)))
  
  (define (set-addr! addr k)
    (unless (and (symbol? addr) (not (memq addr registers)))
      (error 'assemble "incorrect address: ~s" addr))
    (when (hash-ref addr-hash addr #f)
      (error 'assemble "duplicate address: ~s" addr))
    (hash-set! addr-hash addr k))
  
  (define (check-datum datum)
    (unless
      (or
        (INTEGER? datum)
        (and (symbol? datum) (not (memq datum registers))))
      (error 'assemble "incorrect datum: ~s" datum)))
  
  (define (get-datum datum)
    (cond
      ((symbol? datum) (hash-ref addr-hash datum))
      (else (D->W datum))))
  
  (define (assemble-phase1 k program)
    (define (instr-error instr) (error 'assembler "incorrect instruction: ~s" instr)) 
    (cond
      ((null? program) '())
      (else
        (define instr (car program))
        (match instr
          ((list ': rest ...) (assemble-phase1 k (cdr program)))
          ((list addr ': 'DATA data ...)
           (set-addr! addr k)
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) (cdr program))))
          ((list 'DATA data ...)
           (unless (for-each check-datum data)
             (error 'assemble "incorrect DATA: ~s" data))
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) (cdr program))))
          ((list addr ': rest ...)
           (set-addr! addr k)
           (cons rest (assemble-phase1 (add1 k) (cdr program))))
          ((list rest ...)
           (cons rest (assemble-phase1 (add1 k) (cdr program))))))))
  
  (define (assemble-phase2 k program)
    (when (show-assembled-program) (printf " ~nAssembled program~n"))
    (for ((instr (in-list program)) (k (in-naturals)))
      (define i (assemble-instr instr))
      (vector-set! memory k i)
      (when (show-assembled-program)
        (define-values (opc cc ra rb rc da) (decompose-instr i))
        (printf "~a : ~a ~a ~a ~a ~a ~a : ~s~n"
          (~h k 6)
          (~h opc 2)
          (~h cc 1)
          (~h ra 1)
          (~h rb 1)
          (~h rc 1)
          (~h da 10)
          (W-sign-extend i)))))

  (define (assemble-instr instr)
    (define (R? R) (memq R registers))
    (define (check-R R) (unless (R? R) (instr-error)))
    (define (instr-error) (error 'assembler "incorrect instruction: ~s" instr))
    (define-values (opcode cc) (mnemonic->opcode/cc (car instr)))
    (define (compose-ALU-instr Ra Rb (Rc #f))
      (cond
        (Rc
          (check-R Ra) (check-R Rb) (check-R Rc)
          (compose-instr opcode cc (R->r Ra) (R->r Rb) (R->r Rc) 0))
        (else
          (check-R Ra) (check-R Rb)
          (compose-instr opcode cc (R->r Ra) 0 (R->r Rb) 0))))
    (define (compose-CMP-instr Ra Rb Rc)
      (check-R Ra) (check-R Rb)
      (if (R? Rc)
        (compose-instr opcode cc (R->r Ra) (R->r Rb) (R->r Rc) 0)
        (compose-instr opcode cc (R->r Ra) (R->r Rb) 8 (get-datum Rc))))
    (define (compose-<>?-instr Ra Rb)
      (check-R Ra)
      (if (R? Rb)
        (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
        (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
    
    (match instr
      ((list 'DATUM datum) (if (symbol? datum) (hash-ref addr-hash datum) (mask-W datum)))
      ((list 'STP) (compose-instr opcode cc 0 0 0 0))
      ((list 'NOP) (compose-instr opcode cc 0 0 0 0))
      ((list 'SET Ra Rb)
       (check-R Ra)
       (if (R? Rb)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
      ((list 'ADD Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'SUB Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'MUL Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'DIV Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'AND Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'IOR Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'SHL Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'SHR Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'SHE Ra Rb Rc) (compose-ALU-instr Ra Rb Rc))
      ((list 'NEG Ra Rb)    (compose-ALU-instr Ra Rb))
      ((list 'NOT Ra Rb)    (compose-ALU-instr Ra Rb))
      ((list 'EQ? Ra Rb Rc) (compose-CMP-instr Ra Rb Rc))
      ((list 'LT? Ra Rb Rc) (compose-CMP-instr Ra Rb Rc))
      ((list 'GT? Ra Rb Rc) (compose-CMP-instr Ra Rb Rc))
      ((list 'LE? Ra Rb Rc) (compose-CMP-instr Ra Rb Rc))
      ((list 'GE? Ra Rb Rc) (compose-CMP-instr Ra Rb Rc))
      ((list '=0? Ra Rb)    (compose-<>?-instr Ra Rb))
      ((list '<0? Ra Rb)    (compose-<>?-instr Ra Rb))
      ((list '>0? Ra Rb)    (compose-<>?-instr Ra Rb))
      ((list '≤0? Ra Rb)    (compose-<>?-instr Ra Rb))
      ((list '≥0? Ra Rb)    (compose-<>?-instr Ra Rb))
      ((list 'JMP Ra)
       (if (R? Ra)
         (compose-instr opcode cc (R->r Ra) 0 0 0)
         (compose-instr opcode cc 8 0 0 (get-datum Ra))))
      ((list 'PSH Ra)
       (if (R? Ra)
         (compose-instr opcode cc (R->r Ra) 0 0 0)
         (compose-instr opcode cc 8 0 0 (get-datum Ra))))
      ((list 'POP Ra)
       (check-R Ra)
       (compose-instr opcode cc (R->r Ra) 0 0 0))
      ((list 'MRD Ra Rb)
       (check-R Ra)
       (if (R? Rb)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
      ((list 'MWR Ra Rb)
       (check-R Ra)
       (if (R? Rb)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
      ((list 'OUT Ra)
       (if (R? Ra)
         (compose-instr opcode cc (R->r Ra) 0 0 0)
         (compose-instr opcode cc 8 0 0 (get-datum Ra))))
      ((list 'INP Ra)
       (check-R Ra)
       (compose-instr #x0C #x0 (R->r Ra) 0 0 0))
      ((list 'WRT Ra Rb)
       (check-R Ra)
       (if (R? Rb)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
      ((list 'RÆD Ra Rb)
       (check-R Ra)
       (if (R? Rb)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8 0 (get-datum Rb))))
      (else (instr-error))))
  
  (reset-memory)
  (assemble-phase2 0 (assemble-phase1 0 program)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════
#|
TODO
Allow strings of up to 8 ASCII characters (32 up to and including 126 as data)
Convert them to words
Add STR instruction to print a word as a string.
|#

