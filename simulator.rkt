#lang racket/base

#|════════════════════════════════════════════════════════════════════════════════════════════════════
Simulation of a CPU

R = register 64 bits
W = word 64 bits
A = address 20 bits
D = datum 44 bits
M = memory (expt 2 20) = 1048576 words

════════════════════════════════════════════════════════════════════════════════════════════════════|#

(provide
  execute
  assemble
  R0 R1 R2 R3 R4 R5 R6 R7 SP PC IR
  R? W? A?
  clock
  INP-port
  OUT-port
  max-nr-of-instrs
  show
  align
  catch-crash
  print-registers
  print-memory
  print-stack
  reset
  memory-ref memory-set!
  ~wx ~ax
  mnemonic->opcode)

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(require (only-in racket ~r natural? match remove-duplicates))
(define INTEGER? exact-integer?)
(define AND bitwise-and)
(define IOR bitwise-ior)
(define NOT bitwise-not)
(define SHIFTL arithmetic-shift)
(define (SHIFTR e k) (arithmetic-shift e (- k)))
(define (force-bool b) (and b #t))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

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

(define (reset) (reset-registers) (M-reset))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(struct R (name proc) #:property prop:object-name 0 #:property prop:procedure 1)
(define (W-custom-write W port mode) (fprintf port "#<~s:~a>" (R-name W) (~wx (W))))
(define (A-custom-write A port mode) (fprintf port "#<~s:~a>" (R-name A) (~ax (A))))
(struct W R () #:property prop:custom-write W-custom-write)
(struct A R () #:property prop:custom-write A-custom-write)

(define W-digits 16)
(define W-bits (* 4 W-digits))
(define W-mask (sub1 (SHIFTL 1 W-bits)))
(define W-sign-bit (SHIFTL 1 (sub1 W-bits)))
(define W-sign-extension (SHIFTL -1 W-bits))
(define (W-positive? w) (zero? (AND W-sign-bit w)))
(define (W-sign-extend w) (if (W-positive? w) w (IOR W-sign-extension w)))

(define A-digits 5)
(define A-bits (* 4 A-digits))
(define M-size (SHIFTL 1 A-bits))
(define A-mask (sub1 M-size))
(define A-sign-bit (SHIFTL 1 (sub1 A-bits)))

(define D-digits 11)
(define D-bits (* 4 D-digits))
(define D-mask (sub1 (SHIFTL 1 D-bits)))
(define D-sign-bit (SHIFTL 1 (sub1 D-bits)))
(define D-sign-extension (AND W-mask (SHIFTL -1 D-bits)))
(define (D-positive? d) (zero? (AND D-sign-bit d)))
(define (D-sign-extend d) (if (D-positive? d) d (IOR D-sign-extension d)))

(define (~hx e width) (string-upcase (~r e #:min-width width #:base 16 #:pad-string "0")))
(define (~wx e) (~hx e W-digits))
(define (~ax e) (~hx e A-digits))

(define (make-R name A/W mask)
  (A/W name
    (let ((in 0) (out 0))
      (λ ((event #f))
        (cond
          ((not event) out)
          ((INTEGER? event) (set! in (AND mask event)) out)
          (else (set! out in) in))))))

(define-syntax-rule (clock (R v) ...) (begin (R v) ... (R 'clock) ...))
(define-syntax-rule (clock+ (R v) ...) (clock (R v) ... (IR (M-ref (PC))) (PC (add1 (PC)))))
(define-syntax-rule (clock! (R v) ...) (begin (clock (R v) ...) (clock+)))

(define (make-W name) (make-R name W W-mask))
(define (make-A name) (make-R name A A-mask))
(define register-names '(R0 R1 R2 R3 R4 R5 R6 R7))
(define registers (map make-W register-names))
(define da (length registers))
(define-values (R0 R1 R2 R3 R4 R5 R6 R7) (apply values registers))
(define R->r-hash (for/hasheq ((R (in-list register-names)) (r (in-range da))) (values R r)))
(define r->R-hash (for/hasheq ((R (in-list registers)) (r (in-range da))) (values r R)))
(define SP (make-A 'SP))
(define PC (make-A 'PC))
(define IR (make-W 'IR))

(define (DA) (D-sign-extend (AND D-mask (IR))))
(define (PSH Ra) (M-set! (SP) (Ra)) (clock+ (SP (sub1 (SP)))))
(define (POP Ra) (define sp (add1 (SP))) (clock! (Ra (M-ref sp)) (SP sp)))
(define (NOP) (clock+))
(define (SET Ra Rb) (clock+ (Ra (Rb))))
(define (ALU cmp Ra Rb Rc) (clock+ (Ra (cmp (W-sign-extend (Rb)) (W-sign-extend (Rc))))))
(define (ALU-unary cmp Ra Rb) (clock+ (Ra (cmp (W-sign-extend (Rb))))))
(define (JMP Ra) (clock (IR (M-ref (Ra))) (PC (add1 (Ra)))))
(define (SHL Ra Rb Rc) (clock+ (Ra (SHIFTL (Rb) (W-sign-extend (Rc))))))
(define (SHR Ra Rb Rc) (clock+ (Ra (SHIFTR (Rb) (W-sign-extend (Rc))))))
(define (SHE Ra Rb Rc) (clock+ (Ra (SHIFTR (W-sign-extend (Rb)) (W-sign-extend (Rc))))))
(define (ROT Ra Rb Rc) (clock+ (Ra (rotate (Rb) (W-sign-extend (Rc))))))
(define (CMP cmp Ra Rb Rc) (if (cmp (W-sign-extend (Ra)) (W-sign-extend (Rb))) (JMP Rc) (clock+)))
(define (CMP-unary cmp Ra Rb) (if (cmp (W-sign-extend (Ra)) 0) (JMP Rb) (clock+)))
(define (MRD Ra Rb) (clock! (Ra (M-ref (AND A-mask (Rb)))))) ; Read one word from memory
(define (MWR Ra Rb) (M-set! (AND A-mask (Rb)) (Ra)) (clock+)) ; Write one word to memory
(define (INP Ra) (clock! (Ra (inp 'INP)))) ; Read one word from INP-port
(define (OUT R) (out (R)) (clock+)) ; Write one word to OUT-port

(define (rotate w n)
  (define m (modulo (- W-bits n) W-bits))
  (define mask (sub1 (SHIFTL 1 m)))
  (IOR (SHIFTR (AND (NOT mask) w) m) (SHIFTL (AND mask w) (- W-bits m))))

(define (RÆD Ra Rb) ; Read Rb words into memory starting at address Ra
  (define from (Ra))
  (define n (Rb))
  (for ((addr (in-range from (+ from n) 1))) (M-set! addr (AND W-mask (inp 'RÆD))))
  (clock+))

(define (WRT Ra Rb) ; Write Rb words from memory starting at address Ra
  (define from (Ra))
  (define n (Rb))
  (for ((addr (in-range from (+ from n) 1))) (out (M-ref addr)))
  (clock+))

(define (inp who) ; Read one word into register
  (define w (read (INP-port)))
  (unless (INTEGER? w) (raise-argument-error who "exact-integer?" w))
  w)

(define (out w) ; Write one word from register
  (fprintf (OUT-port) "output : ~a : ~s~n"
    (~wx w)
    (W-sign-extend w)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define M (make-vector M-size 0))
(define (M-reset) (for ((k (in-range M-size))) (vector-set! M k 0)))
(define cycle-count 0) ; Nr of cycles is nr of references to M, counting direct access cycles equally.

(define (M-ref a)
  (define addr (AND A-mask a))
  (set! cycle-count (add1 cycle-count))
  (vector-ref M addr))

(define (M-set! a w)
  (define addr (AND A-mask a))
  (set! cycle-count (add1 cycle-count))
  (vector-set! M addr w))

(define (print-memory n m)
  (unless (natural? n) (raise-argument-error 'print-memory "natural?" n))
  (unless (natural? m) (raise-argument-error 'print-memory "natural?" n))
  (for ((k (in-range (min M-size n) (min M-size (+ n m)) +1))) ; keep k in range of addresses
    (define w (M-ref k))
    (printf "~a : ~a : ~s~n" (~ax k) (~wx w) (W-sign-extend w))))

(define (print-stack n)
  (unless (natural? n) (raise-argument-error 'print-memory "natural?" n))
  (for ((k (in-range (- M-size n) M-size +1))) ; keep k in range of addresses
    (define w (M-ref k))
    (printf "~a : ~a : ~s~n" (~ax k) (~wx w) (W-sign-extend w))))

(define memory-ref M-ref)
(define (memory-set! a w) (vector-set! M (AND A-mask a) (AND W-mask w)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define mnemonics ; M = mnemonic
  `((STP #f) ; never called
    (NOP ,NOP)
    (SET ,SET)
    (ADD ,(λ (Ra Rb Rc) (ALU + Ra Rb Rc)))
    (SUB ,(λ (Ra Rb Rc) (ALU - Ra Rb Rc)))
    (MUL ,(λ (Ra Rb Rc) (ALU * Ra Rb Rc)))
    (DIV ,(λ (Ra Rb Rc) (ALU quotient Ra Rb Rc)))
    (REM ,(λ (Ra Rb Rc) (ALU remainder Ra Rb Rc)))
    (MOD ,(λ (Ra Rb Rc) (ALU modulo Ra Rb Rc)))
    (SHL ,SHL)
    (SHR ,SHR)
    (SHE ,SHE)
    (ROT ,ROT)
    (AND ,(λ (Ra Rb Rc) (ALU AND Ra Rb Rc)))
    (IOR ,(λ (Ra Rb Rc) (ALU IOR Ra Rb Rc)))
    (NEG ,(λ (Ra Rb) (ALU-unary - Ra Rb)))
    (NOT ,(λ (Ra Rb) (ALU-unary NOT Ra Rb)))
    (JMP ,JMP)
    (EQ? ,(λ (Ra Rb Rc) (CMP = Ra Rb Rc)))
    (LT? ,(λ (Ra Rb Rc) (CMP < Ra Rb Rc)))
    (GT? ,(λ (Ra Rb Rc) (CMP > Ra Rb Rc)))
    (LE? ,(λ (Ra Rb Rc) (CMP <= Ra Rb Rc)))
    (GE? ,(λ (Ra Rb Rc) (CMP >= Ra Rb Rc)))
    (=0? ,(λ (Ra Rb) (CMP-unary = Ra Rb)))
    (<0? ,(λ (Ra Rb) (CMP-unary < Ra Rb)))
    (>0? ,(λ (Ra Rb) (CMP-unary > Ra Rb)))
    (≤0? ,(λ (Ra Rb) (CMP-unary <= Ra Rb)))
    (≥0? ,(λ (Ra Rb) (CMP-unary >= Ra Rb)))
    (MWR ,MWR)
    (MRD ,MRD)
    (INP ,INP)
    (OUT ,OUT)
    (RÆD ,RÆD)
    (WRT ,WRT)
    (PSH ,PSH)
    (POP ,POP)
    (INP ,INP)
    (OUT ,OUT)
    (RÆD ,RÆD)
    (WRT ,WRT)))

(define opcode->M/proc-hash
  (for/hasheq ((entry (in-list mnemonics)) (opcode (in-naturals)))
    (values opcode entry)))

(define (opcode->M/proc opcode)
  (define (opcode-error) (error 'execute "unknown opcode: #x~x" opcode))
  (apply values (hash-ref opcode->M/proc-hash opcode opcode-error)))

(define M->opcode-hash
  (for/hasheq ((entry (in-list mnemonics)) (opcode (in-naturals)))
    (values (car entry) opcode)))

(define (mnemonic->opcode M)
  (define (M-error) (error 'assemble "unknown mnemonic: ~s" M))
  (hash-ref M->opcode-hash M M-error))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define opcode-pos 56)
(define ra-pos 52)
(define rb-pos 48)
(define rc-pos 44)

(define (compose-instr opcode ra rb rc da)
  (IOR
    (SHIFTL opcode opcode-pos)
    (SHIFTL ra ra-pos)
    (SHIFTL rb rb-pos)
    (SHIFTL rc rc-pos)
    (AND D-mask da)))

(define (decompose-instr instr)
  (values
    (AND #xFF (SHIFTR instr opcode-pos))
    (AND #xF (SHIFTR instr ra-pos))
    (AND #xF (SHIFTR instr rb-pos))
    (AND #xF (SHIFTR instr rc-pos))
    (AND D-mask instr)))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (r->R r)
  (cond
    ((< r da) (hash-ref r->R-hash r))
    ((= r da) DA)
    (else (error 'execute "unknow register nr: #x~x" r))))

(define (R->r R)
  (if (eq? R 'DA) da
    (hash-ref R->r-hash R (λ () (error 'assemble "unknown register: ~s" R)))))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(define (execute (source-code #f) #:start (start 0) #:offset (offset 0))
  (unless (natural? start) (raise-argument-error 'execute "natural?" start))
  (unless (natural? offset) (raise-argument-error 'execute "natural?" offset))
  (set! start (AND A-mask start))
  (set! offset (AND A-mask offset))
  (define (handler exn) (displayln (exn-message exn) (current-error-port)))
  (when source-code (assemble source-code #:offset offset))
  (reset-registers)
  (clock (PC start))
  (set! cycle-count 0)
  (printf "~nExecuting~n")
  (cond
    ((catch-crash)
     (with-handlers ((exn:fail? handler))
       (clock+) (executor 0 start)))
    (else (clock+) (executor 0 start))))

(define (executor k pc)
  (when (> k (max-nr-of-instrs)) (error 'execute "max nr of instrs exceeded: ~s" (max-nr-of-instrs)))
  (define-values (opcode ra rb rc da) (decompose-instr (IR)))
  (define-values (M proc) (opcode->M/proc opcode))
  (when (show-instructions)
    (printf "~a : ~s : ~a ~a ~a ~a ~a : ~a : ~s~n"
      (ALIGN k)
      M
      (~hx opcode 2)
      (~hx ra 1)
      (~hx rb 1)
      (~hx rc 1)
      (~hx da D-digits)
      (~hx pc A-digits)
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

(define (assemble source-code #:offset (offset 0) #:target (target 'reset))

  (set! offset (AND A-mask offset))

  (unless (and (list? source-code) (andmap list? source-code))
    (raise-argument-error 'assemble "(listof list?)" source-code))

  (unless (memq target '(reset memory list))
    (raise-argument-error 'assemble "(or/c 'reset 'memory 'list)" target))

  (define (addr? a) (and (symbol? a) (not (memq a register-names))))
  (define addr-hash (make-hasheq))
  (define (datum? d) (or (addr? d) (INTEGER? d)))
  (define (instr-error instr) (error 'assemble "incorrect instuction: ~s" instr))

  (define (set-addr! addr k)
    (when (hash-ref addr-hash addr #f) (error 'assemble "duplicate address: ~s" addr))
    (hash-set! addr-hash addr (+ k offset)))

  (define da (length registers))

  (define (get-datum a)
    (if (INTEGER? a)
      (AND D-mask a)
      (hash-ref addr-hash a (λ () (error 'assemble "unknown address: ~s" a)))))

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
    (for/list ((instr (in-list source-code)) (k (in-naturals offset)))
      (define assembled-instr (assemble-instr instr))
      (when (show-binary-code)
        (define-values (opcode ra rb rc da) (decompose-instr assembled-instr))
        (printf "~a : ~a ~a ~a ~a ~a : ~s~n"
          (~ax k)
          (~hx opcode 2)
          (~hx ra 1)
          (~hx rb 1)
          (~hx rc 1)
          (~hx da D-digits)
          (W-sign-extend assembled-instr)))
      (cons k assembled-instr)))

  (define (assemble-instr instr)
    (match instr
      ((list 'DATUM datum)
       (unless (datum? datum) (instr-error instr))
       (if (symbol? datum)
         (hash-ref addr-hash datum (λ () (error 'DATUM "unknown address: ~s" datum)))
         (AND W-mask datum)))
      ((list M)
       (define opcode (mnemonic->opcode M))
       (compose-instr opcode 0 0 0 0))
      ((list M R)
       (define opcode (mnemonic->opcode M))
       (if (memq R register-names)
         (compose-instr opcode (R->r R) 0 0 0)
         (compose-instr opcode da 0 0 (get-datum R))))
      ((list M Ra Rb)
       (define opcode (mnemonic->opcode M))
       (if (memq Rb register-names)
         (compose-instr opcode (R->r Ra) (R->r Rb) 0 0)
         (compose-instr opcode (R->r Ra) da 0 (get-datum Rb))))
      ((list M Ra Rb Rc)
       (define opcode (mnemonic->opcode M))
       (if (memq Rc register-names)
         (compose-instr opcode (R->r Ra) (R->r Rb) (R->r Rc) 0)
         (compose-instr opcode (R->r Ra) (R->r Rb) da (get-datum Rc))))))

  (when (show-source-code)
    (printf "~nSource-code~n")
    (for ((instr (in-list source-code)) (k (in-naturals offset)))
      (printf "~a : ~a~n" (~ax k) instr)))

  (define binary-code (assemble-phase2 0 (assemble-phase1 0 source-code)))

  (case target
    ((reset)
     (M-reset)
     (for ((instr (in-list binary-code)))
       (M-set! (car instr) (cdr instr))))
    ((list) binary-code)
    ((memory)
     (for ((instr (in-list binary-code)))
       (M-set! (car instr) (cdr instr))))))

;═════════════════════════════════════════════════════════════════════════════════════════════════════

(reset)

;═════════════════════════════════════════════════════════════════════════════════════════════════════

