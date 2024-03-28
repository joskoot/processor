#lang racket/base

(provide
  execute
  assemble
  R0 R1 R2 R3 R4 R5 R6 R7 SP PC IR
  register?
  clock
  INP-port
  OUT-port
  max-nr-of-instrs
  show
  print-registers
  print-memory
  print-stack
  align
  reset
  catch-crash)

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(require (only-in racket ~r natural? match flatten remove-duplicates))
(define INTEGER? exact-integer?)
(define AND bitwise-and)
(define IOR bitwise-ior)
(define NOT bitwise-not)
(define SHIFTL arithmetic-shift)
(define (SHIFTR e k) (arithmetic-shift e (- k)))
(define (~h e width) (string-upcase (~r e #:min-width width #:base 16 #:pad-string "0")))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (force-bool b) (and b #t))

(define (OUT-port-guard p)
  (unless (output-port? p) (raise-argument-error 'OUT-port "output-port?" p))
  p)

(define (INP-port-guard p)
  (unless (input-port? p) (raise-argument-error 'IN-port "input-port?" p))
  p)

(define (align-guard n)
  (unless (natural? n) (raise-argument-error 'align "natural?" n))
  n)

(define (max-nr-of-instrs-guard n)
  (unless (natural? n) (raise-argument-error 'max-nr-of-instrs "natural?" n))
  n)

(define (show-guard x)
  (cond
    ((not x) '())
    ((eq? x 'all) (all-show-options))
    ((and (list? x) (null? (remove* all-show-options x))) (remove-duplicates x))
    (else (raise-argument-error 'show "list of show details" x))))

(define OUT-port (make-parameter (current-output-port) OUT-port-guard 'OUT-port))
(define INP-port (make-parameter (current-input-port) INP-port-guard 'INP-port))
(define align (make-parameter 3 align-guard 'alugn))
(define catch-crash (make-parameter #f force-bool 'catch-crash))
(define max-nr-of-instrs (make-parameter 1000 max-nr-of-instrs-guard 'max-nr-of-instrs))
(define all-show-options '(source-code binary-code instructions registers))
(define show (make-parameter all-show-options show-guard 'show))
(define (show-binary-code) (memq 'binary-code (show)))
(define (show-source-code) (memq 'source-code (show)))
(define (show-instructions) (memq 'instructions (show)))
(define (show-registers) (memq 'registers (show)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (print-registers)
  (for ((R (in-list registers))) (printf "~s : ~s~n" R (W-sign-extend (R))))
  (printf "~s~n" PC)
  (printf "~s~n" SP))

(define (ALIGN k)
  (let* ((str (format "~s" k)) (n (string-length str)) (m (align)))
    (if (< n m)
      (string-append (make-string (- m n) #\space) str)
      str)))

(define (reset-registers)
  (for ((R (in-list registers))) (clock (R 0)))
  (clock (PC 0) (SP A-mask)))

(define (reset) (reset-registers) (MEM-reset))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(struct R (name proc) #:property prop:object-name 0 #:property prop:procedure 1)
(define register? R?)
(define (W-custom-write W port mode) (fprintf port "#<~s:~a>" (R-name W) (W-fmt-hex (W))))
(define (A-custom-write A port mode) (fprintf port "#<~s:~a>" (R-name A) (A-fmt-hex (A))))
(struct W R () #:property prop:custom-write W-custom-write)
(struct A R () #:property prop:custom-write A-custom-write)
(define (W-fmt-hex w) (~h w W-digits))
(define (A-fmt-hex a) (~h a A-digits))

(define W-digits 16)
(define W-bits (* 4 W-digits))
(define W-mask (sub1 (SHIFTL 1 W-bits)))
(define W-sign-bit (SHIFTL 1 (sub1 W-bits)))
(define W-sign-extension (SHIFTL -1 W-bits))
(define (W-positive? w) (zero? (AND W-sign-bit w)))
(define (W-sign-extend w) (if (W-positive? w) w (IOR W-sign-extension w)))

(define A-digits 6)
(define A-bits (* 4 A-digits))
(define MEM-size (SHIFTL 1 A-bits))
(define A-mask (sub1 MEM-size))
(define A-sign-bit (SHIFTL 1 (sub1 A-bits)))

(define D-digits 10)
(define D-bits (* 4 D-digits))
(define D-mask (sub1 (SHIFTL 1 D-bits)))
(define D-sign-bit (SHIFTL 1 (sub1 D-bits)))
(define D-sign-extension (AND W-mask (SHIFTL -1 D-bits)))
(define (D-positive? d) (zero? (AND D-sign-bit d)))
(define (D-sign-extend d) (if (D-positive? d) d (IOR D-sign-extension d)))

(define (make-W name)
  (W name
    (let ((in 0) (out 0))
      (λ ((event #f))
        (cond
          ((not event) out)
          ((INTEGER? event) (set! in (AND W-mask event)) out)
          (else (set! out in) in))))))

(define (make-A name)
  (A name
    (let ((in 0) (out 0))
      (λ ((event #f))
        (cond
          ((not event) out)
          ((eq? event 'clock) (set! out in) in)
          (else (set! in (AND A-mask event)) out))))))

(define register-names '(R0 R1 R2 R3 R4 R5 R6 R7))
(define registers (map make-W register-names))
(define-values (R0 R1 R2 R3 R4 R5 R6 R7) (apply values registers))
(define R->r-hash (for/hasheq ((R (in-list register-names)) (r (in-range 8))) (values R r)))
(define r->R-hash (for/hasheq ((R (in-list registers)) (r (in-range 8))) (values r R)))
(define SP (make-A 'SP))
(define PC (make-A 'PC))
(define IR (make-W 'IR))

(define (DA) (D-sign-extend (AND D-mask (IR))))
(define (PSH Ra) (MEM-set! (SP) (Ra)) (clock+ (SP (sub1 (SP)))))
(define (POP Ra) (define sp (add1 (SP))) (clock! (Ra (MEM-ref sp)) (SP sp)))
(define (NOP) (clock+))
(define (SET Ra Rb) (clock+ (Ra (Rb))))
(define (ALU cmp Ra Rb Rc) (clock+ (Ra (cmp (W-sign-extend (Rb)) (W-sign-extend (Rc))))))
(define (ALU-unary cmp Ra Rb) (clock+ (Ra (cmp (W-sign-extend (Rb))))))
(define (JMP Ra) (clock (IR (MEM-ref (Ra))) (PC (add1 (Ra)))))
(define (SHL Ra Rb Rc) (clock+ (Ra (SHIFTL (Rb) (W-sign-extend (Rc))))))
(define (SHR Ra Rb Rc) (clock+ (Ra (SHIFTR (Rb) (W-sign-extend (Rc))))))
(define (SHE Ra Rb Rc) (clock+ (Ra (SHIFTR (W-sign-extend (Rb)) (W-sign-extend (Rc))))))
(define (CMP cmp Ra Rb Rc) (if (cmp (W-sign-extend (Ra)) (W-sign-extend (Rb))) (JMP Rc) (clock+)))
(define (CMP-unary cmp Ra Rb) (if (cmp (W-sign-extend (Ra)) 0) (JMP Rb) (clock+)))
(define (MRD Ra Rb) (clock! (Ra (MEM-ref (AND A-mask (Rb)))))) ; Read one word from memory
(define (MWR Ra Rb) (MEM-set! (AND A-mask (Rb)) (Ra)) (clock+)) ; Write one word to memory
(define (INP Ra) (clock! (Ra (inp 'INP)))) ; Read one word from INP-port
(define (OUT R) (out (R)) (clock+)) ; Write one word to OUT-port

(define (RÆD Ra Rb) ; Read Rb words into memory starting at address Ra
  (define from (Ra))
  (define n (Rb))
  (for ((addr (in-range from (+ from n) 1))) (MEM-set! addr (AND W-mask (inp 'RÆD))))
  (clock+))

(define (WRT Ra Rb) ; Write Rb words from memory starting at address Ra
  (define from (Ra))
  (define n (Rb))
  (for ((addr (in-range from (+ from n) 1))) (out (MEM-ref addr)))
  (clock+))

(define (inp who) ; Read one word
  (define w (read (INP-port)))
  (unless (INTEGER? w) (raise-argument-error who "exact-integer?" w))
  w)

(define (out w) ; Write one word
  (fprintf (OUT-port) "output : ~a : ~s~n"
    (W-fmt-hex w)
    (W-sign-extend w)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define MEM (make-vector MEM-size 0))
(define (MEM-reset) (set! MEM (make-vector MEM-size 0)))
(define cycle-count 0)

(define (MEM-ref a)
  (define addr (AND A-mask a))
  (set! cycle-count (add1 cycle-count))
  (vector-ref MEM addr))

(define (MEM-set! a w)
  (define addr (AND A-mask a))
  (set! cycle-count (add1 cycle-count))
  (vector-set! MEM addr w))

(define (print-memory n m)
  (unless (natural? n) (raise-argument-error 'print-memory "natural?" n))
  (unless (natural? m) (raise-argument-error 'print-memory "natural?" n))
  (for ((k (in-range (min MEM-size n) (min MEM-size (+ n m)))))
    (define w (MEM-ref k))
    (printf "~a : ~a : ~s~n" (A-fmt-hex k) (W-fmt-hex w) (W-sign-extend w))))

(define (print-stack n)
  (unless (natural? n) (raise-argument-error 'print-memory "natural?" n))
  (for ((k (in-range (- MEM-size n) MEM-size)))
    (define w (MEM-ref k))
    (printf "~a : ~a : ~s~n" (A-fmt-hex k) (W-fmt-hex w) (W-sign-extend w))))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define M-table
  `((#x00 (#f  STP ,(λ () (void))))
    (#x01 (#f  NOP ,NOP))
    (#x02 (#f  SET ,SET))
    (#x03 (#x0 ADD ,(λ (Ra Rb Rc) (ALU +         Ra Rb Rc)))
          (#x1 SUB ,(λ (Ra Rb Rc) (ALU -         Ra Rb Rc)))
          (#x2 MUL ,(λ (Ra Rb Rc) (ALU *         Ra Rb Rc)))
          (#x3 DIV ,(λ (Ra Rb Rc) (ALU quotient  Ra Rb Rc)))
          (#x4 REM ,(λ (Ra Rb Rc) (ALU remainder Ra Rb Rc)))
          (#x5 MOD ,(λ (Ra Rb Rc) (ALU modulo    Ra Rb Rc)))
          (#x6 SHL ,(λ (Ra Rb Rc) (SHL           Ra Rb Rc)))
          (#x7 SHR ,(λ (Ra Rb Rc) (SHR           Ra Rb Rc)))
          (#x8 SHE ,(λ (Ra Rb Rc) (SHE           Ra Rb Rc)))
          (#x9 AND ,(λ (Ra Rb Rc) (AND           Ra Rb Rc)))
          (#xA IOR ,(λ (Ra Rb Rc) (IOR           Ra Rb Rc))))
    (#x04 (#x0 NEG ,(λ (Ra Rb)    (ALU-unary -   Ra Rb   )))
          (#x1 NOT ,(λ (Ra Rb)    (ALU-unary NOT Ra Rb   ))))
    (#x05 (#f  JMP ,JMP))
    (#x06 (#x0 EQ? ,(λ (Ra Rb Rc) (CMP =         Ra Rb Rc)))
          (#x1 LT? ,(λ (Ra Rb Rc) (CMP <         Ra Rb Rc)))
          (#x2 GT? ,(λ (Ra Rb Rc) (CMP >         Ra Rb Rc)))
          (#x3 LE? ,(λ (Ra Rb Rc) (CMP <=        Ra Rb Rc)))
          (#x4 GE? ,(λ (Ra Rb Rc) (CMP >=        Ra Rb Rc))))
    (#x07 (#x0 =0? ,(λ (Ra Rb)    (CMP-unary =   Ra Rb   )))
          (#x1 <0? ,(λ (Ra Rb)    (CMP-unary <   Ra Rb   )))
          (#x2 >0? ,(λ (Ra Rb)    (CMP-unary >   Ra Rb   )))
          (#x3 ≤0? ,(λ (Ra Rb)    (CMP-unary <=  Ra Rb   )))
          (#x4 ≥0? ,(λ (Ra Rb)    (CMP-unary >=  Ra Rb   ))))
    (#x08 (#f  INP ,INP))
    (#x09 (#f  OUT ,OUT))
    (#x0A (#f  RÆD ,RÆD))
    (#x0B (#f  WRT ,WRT))
    (#x0C (#f  PSH ,PSH))
    (#x0D (#f  POP ,POP))
    (#x0E (#f  MWR ,MWR))
    (#x0F (#f  MRD ,MRD))))

(define M-table-opcode car)
(define M-table-entry-cc car)
(define M-table-entry-mnemonic cadr)
(define M-table-entry-proc caddr)

(define opcode/cc->M/proc-hash
  (for/hasheq ((entry (in-list M-table)))
    (define opcode (M-table-opcode entry))
    (define subtable (cdr entry))
    (values opcode
      (cond
        ((null? (cdr subtable))
         (define descr (car subtable))
         (list (M-table-entry-mnemonic descr) (M-table-entry-proc descr)))
        (else
          (for/hasheq ((descr (in-list subtable)))
            (values (M-table-entry-cc descr)
              (list (M-table-entry-mnemonic descr) (M-table-entry-proc descr)))))))))

(define (opcode/cc->M/proc opcode cc)
  (define (opcode/cc-error) (error "unknown opcode/cc" opcode cc))
  (define entry (hash-ref opcode/cc->M/proc-hash opcode opcode/cc-error))
  (cond
    ((hash? entry)
     (define entri (hash-ref entry cc opcode/cc-error))
     (values (car entri) (cadr entri)))
    (else (values (car entry) (cadr entry)))))

(define M->opcode/cc-hash
  (make-immutable-hasheq
    (let loop ((table M-table))
      (cond
        ((null? table) '())
        (else
          (define entry (car table))
          (define opcode (car entry))
          (cond
            ((= (length entry) 2)
             (define descr (cadr entry))
             (cons
               (cons (M-table-entry-mnemonic descr) (list opcode 0))
               (loop (cdr table))))
            (else
              (append
                (let loop ((subtable (cdr entry)))
                  (cond
                    ((null? subtable) '())
                    (else
                      (define descr (car subtable))
                      (cons
                        (cons
                          (M-table-entry-mnemonic descr)
                          (list opcode (M-table-entry-cc descr)))
                        (loop (cdr subtable))))))
                (loop (cdr table))))))))))

(define (M->opcode/cc M)
  (define (M-error) (error "unknown mnemonic" M))
  (apply values (hash-ref M->opcode/cc-hash M M-error)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define opcode-pos 56)
(define cc-pos 52)
(define ra-pos 48)
(define rb-pos 44)
(define rc-pos 40)

(define (compose-instr opcode cc ra rb rc da)
  (IOR
    (SHIFTL opcode opcode-pos)
    (SHIFTL cc cc-pos)
    (SHIFTL ra ra-pos)
    (SHIFTL rb rb-pos)
    (SHIFTL rc rc-pos)
    (AND D-mask da)))

(define (decompose-instr instr)
  (values
    (AND #xFF (SHIFTR instr opcode-pos))
    (AND #xF (SHIFTR instr cc-pos))
    (AND #xF (SHIFTR instr ra-pos))
    (AND #xF (SHIFTR instr rb-pos))
    (AND #xF (SHIFTR instr rc-pos))
    (AND D-mask instr)))    

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define-syntax-rule (clock (R v) ...) (begin (R v) ... (R 'clock) ...))
(define-syntax-rule (clock+ (R v) ...) (clock (R v) ... (IR (MEM-ref (PC))) (PC (add1 (PC)))))
(define-syntax-rule (clock! (R v) ...) (begin (clock (R v) ...) (clock+)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (r->R r)
  (cond
    ((< r 8) (hash-ref r->R-hash r))
    ((= r 8) DA)
    (else (error 'execute "unknow register nr: ~s" r))))

(define (R->r R)
  (if (eq? R 'DA) 8
    (hash-ref R->r-hash R (λ () (error 'assemble "unknown register: ~s" R)))))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (execute (source-code #f))
  (define (handler exn) (displayln (exn-message exn) (current-error-port)))
  (when source-code (assemble source-code))
  (reset-registers)
  (set! cycle-count 0)
  (printf "~nExecuting~n")
  (cond
    ((catch-crash)
     (with-handlers ((exn:fail? handler))
       (clock+) (executor 0 0)))
    (else (clock+) (executor 0 0))))

(define (executor k pc)
  (when (> k (max-nr-of-instrs)) (error 'execute "max nr of instrs exceeded: ~s" (max-nr-of-instrs)))
  (define-values (opcode cc ra rb rc da) (decompose-instr (IR)))
  (define-values (M proc) (opcode/cc->M/proc opcode cc))
  (when (show-instructions)
    (printf "~a : ~s : ~a ~a ~a ~a ~a ~a : ~a : ~s~n"
      (ALIGN k)
      M
      (~h opcode 2)
      (~h cc 1)
      (~h ra 1)
      (~h rb 1)
      (~h rc 1)
      (~h da 10)
      (~h pc 6)
      cycle-count))
  (cond
    ((zero? opcode)
     (unless (show-instructions) (printf "Execution halted after ~s cycles~n" cycle-count))
     (when (show-registers)
       (printf "~nRegisters~n")
       (print-registers)))
    (else
      (case (procedure-arity proc)
        ((0) (proc))
        ((1) (proc (r->R ra)))
        ((2) (proc (r->R ra) (r->R rb)))
        ((3) (proc (r->R ra) (r->R rb) (r->R rc))))
      (executor (add1 k) (AND A-mask (sub1 (PC)))))))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (assemble source-code)
  (unless (and (list? source-code) (andmap list? source-code))
    (raise-argument-error 'assemble "(listof list?)" source-code))
  (define (addr? a) (and (symbol? a) (not (memq a register-names))))
  (define addr-hash (make-hasheq))
  (define (datum? d) (or (addr? d) (INTEGER? d)))
  (define (set-addr! addr k)
    (when (hash-ref addr-hash addr #f) (error 'assemble "duplicate address: ~s" addr))
    (hash-set! addr-hash addr k))
  (define (get-datum a)
    (if (INTEGER? a)
      (AND D-mask a)
      (hash-ref addr-hash a (λ () (error 'assemble "unknown datum: ~s" a)))))
  (define (instr-error instr) (error 'assemble "incorrect instuction: ~s" instr))
  
  (define (assemble-phase1 k source-code)
    (cond
      ((null? source-code) '())
      (else
        (define instr (car source-code))
        (define remaining-source-code (cdr source-code))
        (match instr
          ((list ': comment ...)
           (assemble-phase1 k remaining-source-code))
          ((list addr ': 'DATA data ...)
           (unless (addr? addr) (instr-error instr))
           (set-addr! addr k)
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) remaining-source-code)))
          ((list 'DATA data ...)
           (append (map make-DATUM-instr data)
             (assemble-phase1 (+ k (length data)) remaining-source-code)))
          ((list addr ': rest ...)
           (unless (addr? addr) (instr-error instr))
           (set-addr! addr k)
           (cons rest (assemble-phase1 (add1 k) remaining-source-code)))
          ((list rest ...)
           (cons rest (assemble-phase1 (add1 k) remaining-source-code)))))))
  
  (define (make-DATUM-instr datum) (list 'DATUM datum))
  
  (define (assemble-phase2 k source-code)
    (when (show-binary-code) (printf "~nAssembled code~n"))
    (for ((instr (in-list source-code)) (k (in-naturals)))
      (define assembled-instr (assemble-instr instr))
      (when (show-binary-code)
        (define-values (opcode cc ra rb rc da) (decompose-instr assembled-instr))
        (printf "~a : ~a ~a ~a ~a ~a ~a : ~s~n"
          (A-fmt-hex k)
          (~h opcode 2)
          (~h cc 1)
          (~h ra 1)
          (~h rb 1)
          (~h rc 1)
          (~h da 10)
          (W-sign-extend assembled-instr)))
      (MEM-set! k assembled-instr)))
  
  (define (assemble-instr instr)
    (match instr
      ((list 'DATUM datum)
       (unless (datum? datum) (instr-error instr))
       (if (symbol? datum)
         (hash-ref addr-hash datum (λ () (error 'DATUM "unknown address: ~s" datum)))
         (AND W-mask datum)))
      ((list M)
       (define-values (opcode cc) (M->opcode/cc M))
       (compose-instr opcode cc 0 0 0 0))
      ((list M R)
       (define-values (opcode cc) (M->opcode/cc M))
       (if (memq R register-names)
         (compose-instr opcode cc (R->r R) 0 0 0)
         (compose-instr opcode cc 8        0 0 (get-datum R))))
      ((list M Ra Rb)
       (define-values (opcode cc) (M->opcode/cc M))
       (if (memq Rb register-names)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode cc (R->r Ra) 8         0 (get-datum Rb))))
      ((list M Ra Rb Rc)
       (define-values (opcode cc) (M->opcode/cc M))
       (if (memq Rc register-names)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) (R->r Rc) 0)
         (compose-instr opcode cc (R->r Ra) (R->r Rb) 8 (get-datum Rc))))))
  
  (MEM-reset)
  
  (when (show-source-code)
    (printf "~nSource-code~n")
    (for ((instr (in-list source-code)) (k (in-naturals)))
      (printf "~a : ~a~n" (A-fmt-hex k) instr)))
  
  (assemble-phase2 0 (assemble-phase1 0 source-code)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(reset)

;═════════════════════════════════════════════════════════════════════════════════════════════════════
