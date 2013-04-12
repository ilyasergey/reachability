;; A Scheme-to-C compiler.

;; Author: Matthew Might
;; Site:   http://matt.might.net/
;;         http://www.ucombinator.org/

;; The purpose of this compiler is to demonstrate
;; the most direct possible mapping of Scheme into C.
;; Toward that end, the compiler uses only two
;; intermediate transformations: mutable-variable
;; elimination and closure-conversion.

;; To run the compiler:

;;  $ interp this-file.scm < program.scm > out.c
;;  $ gcc -o out out.c

;; (It's useful to compare this compiler to the 
;; Scheme-to-Java compiler that started from the 
;; same codebase.)

;; The compiler handles Core Scheme and some extras.
;; With a macro system like syntax-rules and some
;; more primitives, all of R5RS could be supported.

;; Unlike the Java version, this compiler handles
;; recursion using a lets+sets transformation.

;; The compilation proceeds from Core Scheme plus
;; sugar through three intermediate languages:

;; Core Scheme + Sugar

;;    =[desugar]=>

;; Core Scheme 

;;    =[mutable variable elimination]=>

;; Intermediate Scheme (1) 

;;    =[closure conversion]=>

;; Intermediate Scheme (2) 

;;    =[code emission]=>

;; C


;; Core input language:

;; <exp> ::= <const>
;;        |  <prim>
;;        |  <var>
;;        |  (lambda (<var> ...) <exp>)
;;        |  (if <exp> <exp> <exp>)
;;        |  (set! <var> <exp>)
;;        |  (<exp> <exp> ...)

;; <const> ::= <int>
;;          |  #f 

;; Syntactic sugar:

;; <exp> ::+ (let ((<var> <exp>) ...) <exp>)
;;        |  (letrec ((<var> <exp>) ...) <exp>)
;;        |  (begin <exp> ...)

;; Intermediate language (1)

;; <exp> ::+ (cell <exp>)
;;        |  (cell-get <exp>)
;;        |  (set-cell! <exp> <value>)

;; Intermediate language (2)

;; <exp> ::+ (closure <lambda-exp> <env-exp>)
;;        |  (env-make <env-num> (<symbol> <exp>) ...)
;;        |  (env-get <env-num> <symbol> <exp>)




;; Utilities.

(define (cadr p) (car (cdr p)))
(define (cddr p) (cdr (cdr p)))
(define (caadr p) (car (car (cdr p))))
(define (caddr p) (car (cdr (cdr p))))
(define (cadddr p) (car (cdr (cdr (cdr p)))))

(define (map f lst) 
  (if (pair? lst)
      (cons (f (car lst))
            (map f (cdr lst)))
      '()))


(define (for-each f lst) 
  (begin
    (if (pair? lst)
        (cons (f (car lst))
              (map f (cdr lst)))
        '())
    (void)))
  

(define (append lst1 lst2)
  (if (not (pair? lst1))
      lst2
      (cons (car lst1) 
            (append (cdr lst1) lst2))))

(define (string->list s)
  (define (f i)
    (if (< i (string-length s))
        (cons (string-ref s i)
              (f (+ i 1)))
        '()))
  (f 0))


(define (apply f l)
  (let ((len (length l)))
  (cond
    ((= len 0) (f))
    ((= len 1) (f (car l)))
    ((= len 2) (f (car l) (cadr l)))
    ((= len 3) (f (car l) (cadr l) (caddr l)))
    ((= len 4) (f (car l) (cadr l) (caddr l) (cadddr l))))))

(define (assv x f)
  (if (pair? f)
      (if (eqv? (car (car f)) x)
          (car f)
          (assv x (cdr f)))
      #f))


(define (assq x f)
  (if (pair? f)
      (if (eq? (car (car f)) x)
          (car f)
          (assv x (cdr f)))
      #f))


; void : -> void
(define (void) (if #f #t))

; tagged-list? : symbol value -> boolean
(define (tagged-list? tag l)
  (and (pair? l)
       (eq? tag (car l))))

; char->natural : char -> natural
(define (char->natural c)
  (let ((i (char->integer c)))
    (if (< i 0)
        (* -2 i)
        (+ (* 2 i) 1))))

; integer->char-list : integer -> string
(define (integer->char-list n)
  (string->list (number->string n)))

; gensym-count : integer
(define gensym-count 0)

; gensym : symbol -> symbol
(define gensym (lambda (name)
                 (begin
                   (set! gensym-count (+ gensym-count 1))
                   (string->symbol (string-append 
                                    (if (symbol? name)
                                        (symbol->string name)
                                        name)
                                    "$"
                                    (number->string gensym-count))))))

; member : symbol sorted-set[symbol] -> boolean
(define (member sym S)
  (if (not (pair? S))
      #f
      (if (eq? sym (car S))
          #t
          (member sym (cdr S)))))

; symbol<? : symbol symobl -> boolean
(define (symbol<? sym1 sym2)
  (string<? (symbol->string sym1)
            (symbol->string sym2)))

; insert : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (insert sym S)
  (if (not (pair? S))
      (cons sym '())
      (cond
        ((eq? sym (car S))       S)
        ((symbol<? sym (car S))  (cons sym S))
        (else (cons (car S) (insert sym (cdr S)))))))

; remove : symbol sorted-set[symbol] -> sorted-set[symbol]
(define (remove sym S)
  (if (not (pair? S))
      '()
      (if (eq? (car S) sym)
          (cdr S)
          (cons (car S) (remove sym (cdr S))))))
          
; union : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (union set1 set2)
  ; NOTE: This should be implemented as merge for efficiency.
  (if (not (pair? set1))
      set2
      (insert (car set1) (union (cdr set1) set2))))

; difference : sorted-set[symbol] sorted-set[symbol] -> sorted-set[symbol]
(define (difference set1 set2)
  ; NOTE: This can be similarly optimized.
  (if (not (pair? set2))
      set1
      (difference (remove (car set2) set1) (cdr set2))))

; reduce : (A A -> A) list[A] A -> A
(define (reduce f lst init)
  (if (not (pair? lst))
      init
      (reduce f (cdr lst) (f (car lst) init))))

; azip : list[A] list[B] -> alist[A,B]
(define (azip list1 list2)
  (if (and (pair? list1) (pair? list2))
      (cons (cons (car list1) (cons (car list2) '()))
            (azip (cdr list1) (cdr list2)))
      '()))

; assq-remove-key : alist[A,B] A -> alist[A,B]
(define (assq-remove-key env key)
  (if (not (pair? env))
      '()
      (if (eq? (car (car env)) key)
          (assq-remove-key (cdr env) key)
          (cons (car env) (assq-remove-key (cdr env) key)))))

; assq-remove-keys : alist[A,B] list[A] -> alist[A,B]
(define (assq-remove-keys env keys)
  (if (not (pair? keys))
      env
      (assq-remove-keys (assq-remove-key env (car keys)) (cdr keys))))




;; Data type predicates and accessors.

; const? : exp -> boolean
(define (const? exp)
  (or (integer? exp)
      (boolean? exp)))

; ref? : exp -> boolean
(define (ref? exp)
  (symbol? exp))

; let? : exp -> boolean
(define (let? exp)
  (tagged-list? 'let exp))

; let->bindings : let-exp -> alist[symbol,exp]
(define (let->bindings exp)
  (cadr exp))

; let->exp : let-exp -> exp
(define (let->exp exp)
  (caddr exp))

; let->bound-vars : let-exp -> list[symbol]
(define (let->bound-vars exp)
  (map car (cadr exp)))

; let->args : let-exp -> list[exp]
(define (let->args exp)
  (map cadr (cadr exp)))

; letrec? : exp -> boolean
(define (letrec? exp)
  (tagged-list? 'letrec exp))

; letrec->bindings : letrec-exp -> alist[symbol,exp]
(define (letrec->bindings exp)
  (cadr exp))

; letrec->exp : letrec-exp -> exp
(define (letrec->exp exp)
  (caddr exp))

; letrec->exp : letrec-exp -> list[symbol]
(define (letrec->bound-vars exp)
  (map car (cadr exp)))

; letrec->exp : letrec-exp -> list[exp]
(define (letrec->args exp)
  (map cadr (cadr exp)))

; lambda? : exp -> boolean
(define (lambda? exp)
  (tagged-list? 'lambda exp))

; lambda->formals : lambda-exp -> list[symbol]
(define (lambda->formals exp)
  (cadr exp))

; lambda->exp : lambda-exp -> exp
(define (lambda->exp exp)
  (caddr exp))

; if? : exp -> boolean
(define (if? exp)
  (tagged-list? 'if exp))

; if->condition : if-exp -> exp
(define (if->condition exp)
  (cadr exp))

; if->then : if-exp -> exp
(define (if->then exp)
  (caddr exp))

; if->else : if-exp -> exp
(define (if->else exp)
  (cadddr exp))

; app? : exp -> boolean
(define (app? exp)
  (pair? exp))

; app->fun : app-exp -> exp
(define (app->fun exp)
  (car exp))

; app->args : app-exp -> list[exp]
(define (app->args exp)
  (cdr exp))
  
; prim? : exp -> boolean
(define (prim? exp)
  (or (eq? exp '+)
      (eq? exp '-)
      (eq? exp '*)
      (eq? exp '=)
      (eq? exp 'display)))

; begin? : exp -> boolean
(define (begin? exp) 
  (tagged-list? 'begin exp))

; begin->exps : begin-exp -> list[exp]
(define (begin->exps exp)
  (cdr exp))

; set! : exp -> boolean
(define (set!? exp)
  (tagged-list? 'set! exp))

; set!->var : set!-exp -> var
(define (set!->var exp)
  (cadr exp))

; set!->exp : set!-exp -> exp
(define (set!->exp exp)
  (caddr exp))

; closure? : exp -> boolean
(define (closure? exp) 
  (tagged-list? 'closure exp))

; closure->lam : closure-exp -> exp
(define (closure->lam exp) 
  (cadr exp))

; closure->env : closure-exp -> exp
(define (closure->env exp) 
  (caddr exp))

; env-make? : exp -> boolean
(define (env-make? exp) 
  (tagged-list? 'env-make exp))

; env-make->id : env-make-exp -> env-id
(define (env-make->id exp)
  (cadr exp))

; env-make->fields : env-make-exp -> list[symbol]
(define (env-make->fields exp)
  (map car (cddr exp)))
  
; env-make->values : env-make-exp -> list[exp]
(define (env-make->values exp)
  (map cadr (cddr exp)))

; env-get? : exp -> boolen
(define (env-get? exp)
  (tagged-list? 'env-get exp))

; env-get->id : env-get-exp -> env-id
(define (env-get->id exp)
  (cadr exp))
  
; env-get->field : env-get-exp -> symbol
(define (env-get->field exp)
  (caddr exp))

; env-get->env : env-get-exp -> exp
(define (env-get->env exp)
  (cadddr exp)) 

; set-cell!? : set-cell!-exp -> boolean
(define (set-cell!? exp)
  (tagged-list? 'set-cell! exp))

; set-cell!->cell : set-cell!-exp -> exp
(define (set-cell!->cell exp)
  (cadr exp))

; set-cell!->value : set-cell!-exp -> exp
(define (set-cell!->value exp)
  (caddr exp))

; cell? : exp -> boolean
(define (cell? exp)
  (tagged-list? 'cell exp))

; cell->value : cell-exp -> exp
(define (cell->value exp)
  (cadr exp))

; cell-get? : exp -> boolean
(define (cell-get? exp)
  (tagged-list? 'cell-get exp))

; cell-get->cell : cell-exp -> exp
(define (cell-get->cell exp)
  (cadr exp))



;; Syntax manipulation.

; substitute-var : alist[var,exp] ref-exp -> exp
(define (substitute-var env var)
  (let ((sub (assq var env)))
    (if sub
        (cadr sub)
        var)))

; substitute : alist[var,exp] exp -> exp
(define (substitute env exp)
  
  (define (substitute-with env)
    (lambda (exp)
      (substitute env exp)))

  (cond
    ; Core forms:    
    ((null? env)        exp)
    ((const? exp)       exp)
    ((prim? exp)        exp)
    ((ref? exp)         (substitute-var env exp))
    ((lambda? exp)      `(lambda ,(lambda->formals exp)
                           ,(substitute (assq-remove-keys env (lambda->formals exp)) 
                                        (lambda->exp exp))))
    ((set!? exp)        `(set! ,(substitute-var env (set!->var exp))
                               ,(substitute env (set!->exp exp))))
    ((if? exp)          `(if ,(substitute env (if->condition exp))
                             ,(substitute env (if->then exp))
                             ,(substitute env (if->else exp))))
    
    ; Sugar:
    ((let? exp)         `(let ,(azip (let->bound-vars exp)
                                     (map (substitute-with env) (let->args exp)))
                           ,(substitute (assq-remove-keys env (let->bound-vars exp))
                                        (let->exp exp))))
    ((letrec? exp)      (let ((new-env (assq-remove-keys env (letrec->bound-vars exp))))
                          `(letrec ,(azip (letrec->bound-vars exp) 
                                          (map (substitute-with new-env) 
                                               (letrec->args exp)))
                             ,(substitute new-env (letrec->exp exp)))))
    ((begin? exp)       (cons 'begin (map (substitute-with env) (begin->exps exp))))

    ; IR (1):
    ((cell? exp)        `(cell ,(substitute env (cell->value exp))))
    ((cell-get? exp)    `(cell-get ,(substitute env (cell-get->cell exp))))
    ((set-cell!? exp)   `(set-cell! ,(substitute env (set-cell!->cell exp))
                                    ,(substitute env (set-cell!->value exp))))
    
    ; IR (2):
    ((closure? exp)     `(closure ,(closure->lam exp) ,(closure->env exp)))
    ((env-make? exp)    `(env-make ,(env-make->id exp) 
                                   ,@(azip (env-make->fields exp)
                                           (map (substitute-with env)
                                                (env-make->values exp)))))
    ((env-get? exp)     `(env-get ,(env-get->id exp)
                                  ,(env-get->field exp)
                                  ,(substitute env (env-get->env exp))))
    
    ; Application:
    ((app? exp)         (map (substitute-with env) exp))
    (else               (error "unhandled expression type in substitution: " exp))))




;; Desugaring.

; let=>lambda : let-exp -> app-exp
(define (let=>lambda exp)
  (if (let? exp)
      (let ((vars (map car (let->bindings exp)))
            (args (map cadr (let->bindings exp))))
        `((lambda (,@vars) ,(let->exp exp)) ,@args))
      exp))

; letrec=>lets+sets : letrec-exp -> exp
(define (letrec=>lets+sets exp)
  (if (letrec? exp)
      (let* ((bindings  (letrec->bindings exp))
             (namings   (map (lambda (b) (cons (car b) 
                                               (cons #f '()))) 
                             bindings))
             (names     (letrec->bound-vars exp))
             (sets      (map (lambda (binding) 
                               (cons 'set! binding))
                             bindings))
             (args      (letrec->args exp)))
        `(let ,namings
           (begin ,@(append sets (cons (letrec->exp exp) '())))))))

; begin=>let : begin-exp -> let-exp
(define (begin=>let exp)
  (define (singlet? l)
    (and (list? l)
         (= (length l) 1)))
  
  (define (dummy-bind exps)
    (cond
      ((singlet? exps)  (car exps))
      
      ((pair? exps)     `(let (($_ ,(car exps)))
                          ,(dummy-bind (cdr exps))))))
  (dummy-bind (begin->exps exp)))

; desugar : exp -> exp
(define (desugar exp)
  (cond
    ; Core forms:
    ((const? exp)      exp)
    ((prim? exp)       exp)
    ((ref? exp)        exp)
    ((lambda? exp)     `(lambda ,(lambda->formals exp)
                          ,(desugar (lambda->exp exp))))
    ((set!? exp)       `(set! ,(set!->var exp) ,(set!->exp exp)))
    ((if? exp)         `(if ,(if->condition exp)
                            ,(if->then exp)
                            ,(if->else exp)))
    
    ; Sugar:
    ((let? exp)        (desugar (let=>lambda exp)))
    ((letrec? exp)     (desugar (letrec=>lets+sets exp)))
    ((begin? exp)      (desugar (begin=>let exp)))
    
    ; IR (1):
    ((cell? exp)       `(cell ,(desugar (cell->value exp))))
    ((cell-get? exp)   `(cell-get ,(desugar (cell-get->cell exp))))
    ((set-cell!? exp)  `(set-cell! ,(desugar (set-cell!->cell exp)) 
                                   ,(desugar (set-cell!->value exp))))
    
    ; IR (2): 
    ((closure? exp)    `(closure ,(desugar (closure->lam exp))
                                 ,(desugar (closure->env exp))))
    ((env-make? exp)   `(env-make ,(env-make->id exp)
                                  ,@(azip (env-make->fields exp)
                                          (map desugar (env-make->values exp)))))
    ((env-get? exp)    `(env-get ,(env-get->id exp)
                                 ,(env-get->field exp)
                                 ,(env-get->env exp)))
    
    ; Applications:
    ((app? exp)        (map desugar exp))    
    (else              (error "unknown exp: " exp))))
    




;; Syntactic analysis.

; free-vars : exp -> sorted-set[var]
(define (free-vars exp)
  (cond
    ; Core forms:
    ((const? exp)    '())
    ((prim? exp)     '())    
    ((ref? exp)      (cons exp '()))
    ((lambda? exp)   (difference (free-vars (lambda->exp exp))
                                 (lambda->formals exp)))
    ((if? exp)       (union (free-vars (if->condition exp))
                            (union (free-vars (if->then exp))
                                   (free-vars (if->else exp)))))
    ((set!? exp)     (union (cons (set!->var exp) '())
                            (free-vars (set!->exp exp))))
    
    ; Sugar:
    ((let? exp)      (free-vars (let=>lambda exp)))
    ((begin? exp)    (reduce union (map free-vars (begin->exps exp)) '()))

    ; IR (1):
    ((cell-get? exp)  (free-vars (cell-get->cell exp)))
    ((cell? exp)      (free-vars (cell->value exp)))
    ((set-cell!? exp) (union (free-vars (set-cell!->cell exp))
                             (free-vars (set-cell!->value exp))))
    
    ; IR (2):
    ((closure? exp)   (union (free-vars (closure->lam exp))
                             (free-vars (closure->env exp))))
    ((env-make? exp)  (reduce union (map free-vars (env-make->values exp)) '()))
    ((env-get? exp)   (free-vars (env-get->env exp)))

    ; Application:
    ((app? exp)       (reduce union (map free-vars exp) '()))
    (else             (error "unknown expression: " exp))))





;; Mutable variable analysis and elimination.

;; Mutables variables analysis and elimination happens
;; on a desugared Intermediate Language (1).

;; Mutable variable analysis turns mutable variables 
;; into heap-allocated cells:

;; For any mutable variable mvar:

;; (lambda (... mvar ...) body) 
;;           =>
;; (lambda (... $v ...) 
;;  (let ((mvar (cell $v)))
;;   body))

;; (set! mvar value) => (set-cell! mvar value)

;; mvar => (cell-get mvar)

; mutable-variables : list[symbol]
(define mutable-variables '())

; mark-mutable : symbol -> void
(define (mark-mutable symbol)
  (set! mutable-variables (cons symbol mutable-variables)))

; is-mutable? : symbol -> boolean
(define (is-mutable? symbol)
  (define (is-in? S)
    (if (not (pair? S))
        #f
        (if (eq? (car S) symbol)
            #t
            (is-in? (cdr S)))))
  (is-in? mutable-variables))

; analyze-mutable-variables : exp -> void
(define (analyze-mutable-variables exp)
  (cond 
    ; Core forms:
    ((const? exp)    (void))
    ((prim? exp)     (void))
    ((ref? exp)      (void))
    ((lambda? exp)   (analyze-mutable-variables (lambda->exp exp)))
    ((set!? exp)     (mark-mutable (set!->var exp)))
    ((if? exp)       (begin
                       (analyze-mutable-variables (if->condition exp))
                       (analyze-mutable-variables (if->then exp))
                       (analyze-mutable-variables (if->else exp))))
    
    ; Sugar:
    ((let? exp)      (begin
                       (map analyze-mutable-variables (map cadr (let->bindings exp)))
                       (analyze-mutable-variables (let->exp exp))))
    ((letrec? exp)   (begin
                       (map analyze-mutable-variables (map cadr (letrec->bindings exp)))
                       (analyze-mutable-variables (letrec->exp exp))))
    ((begin? exp)    (begin
                       (map analyze-mutable-variables (begin->exps exp))
                       (void)))
    
    ; Application:
    ((app? exp)      (begin 
                       (map analyze-mutable-variables exp)
                       (void)))
    (else            (error "unknown expression type: " exp))))


; wrap-mutables : exp -> exp
(define (wrap-mutables exp)
  
  (define (wrap-mutable-formals formals body-exp)
    (if (not (pair? formals))
        body-exp
        (if (is-mutable? (car formals))
            `(let ((,(car formals) (cell ,(car formals))))
               ,(wrap-mutable-formals (cdr formals) body-exp))
            (wrap-mutable-formals (cdr formals) body-exp))))
  
  (cond
    ; Core forms:
    ((const? exp)    exp)
    ((ref? exp)      (if (is-mutable? exp)
                         `(cell-get ,exp)
                         exp))
    ((prim? exp)     exp)
    ((lambda? exp)   `(lambda ,(lambda->formals exp)
                        ,(wrap-mutable-formals (lambda->formals exp)
                                               (wrap-mutables (lambda->exp exp)))))
    ((set!? exp)     `(set-cell! ,(set!->var exp) ,(wrap-mutables (set!->exp exp))))
    ((if? exp)       `(if ,(wrap-mutables (if->condition exp))
                          ,(wrap-mutables (if->then exp))
                          ,(wrap-mutables (if->else exp))))
    
    ; Application:
    ((app? exp)      (map wrap-mutables exp))
    (else            (error "unknown expression type: " exp))))
                        


;; Name-mangling.

;; We have to "mangle" Scheme identifiers into
;; C-compatible identifiers, because names like
;; foo-bar/baz are not identifiers in C.

; mangle : symbol -> string
(define (mangle symbol)
  (define (m chars)
    (if (null? chars)
        '()
        (if (or (and (char-alphabetic? (car chars)) (not (char=? (car chars) #\_)))
                (char-numeric? (car chars)))
            (cons (car chars) (m (cdr chars)))
            (cons #\_ (append (integer->char-list (char->natural (car chars)))
                              (m (cdr chars)))))))
  (list->string (m (string->list (symbol->string symbol)))))





;; Closure-conversion.

;; Closure conversion operates on a desugared
;; Intermediate Language (2).  Closure conversion
;; eliminates all of the free variables from every
;; lambda term.

;; The transform is:

;;  (lambda (v1 ... vn) body)
;;             =>
;;  (closure (lambda ($env v1 ... vn) 
;;                   {xi => (env-get $id xi $env)}body)
;;           (env-make $id (x1 x1) ... (xn xn)))

;;  where x1,...xn are the free variables in the lambda term.



; type env-id = natural

; num-environments : natural
(define num-environments 0)

; environments : alist*[env-id,symbol]
(define environments '())

; allocate-environment : list[symbol] -> env-id
(define (allocate-environment fields)
  (let ((id num-environments))
    (set! num-environments (+ 1 num-environments))
    (set! environments (cons (cons id fields) environments))
    id))

; get-environment : natural -> list[symbol]
(define (get-environment id)
  (cdr (assv id environments)))


; closure-convert : exp -> exp
(define (closure-convert exp)
  (cond
    ((const? exp)        exp)
    ((prim? exp)         exp)
    ((ref? exp)          exp)
    ((lambda? exp)       (let* (($env (gensym 'env))
                                (body  (closure-convert (lambda->exp exp)))
                                (fv    (difference (free-vars body) (lambda->formals exp)))
                                (id    (allocate-environment fv))
                                (sub  (map (lambda (v)
                                             (cons v 
                                                   (cons
                                                    `(env-get ,id 
                                                              ,v 
                                                              ,$env)
                                                    '())))
                                           fv)))
                           `(closure (lambda (,$env ,@(lambda->formals exp))
                                       ,(substitute sub body))
                                     (env-make ,id ,@(azip fv fv)))))
    ((if? exp)           `(if ,(closure-convert (if->condition exp))
                              ,(closure-convert (if->then exp))
                              ,(closure-convert (if->else exp))))
    ((set!? exp)         `(set! ,(set!->var exp)
                                ,(closure-convert (set!->exp exp))))
    
    ; IR (1):
    
    ((cell? exp)         `(cell ,(closure-convert (cell->value exp))))
    ((cell-get? exp)     `(cell-get ,(closure-convert (cell-get->cell exp))))
    ((set-cell!? exp)    `(set-cell! ,(closure-convert (set-cell!->cell exp))
                                     ,(closure-convert (set-cell!->value exp))))
    
    ; Applications:
    ((app? exp)          (map closure-convert exp))
    (else                (error "unhandled exp: " exp))))
    



;; Compilation routines.

; c-compile-program : exp -> string
(define (c-compile-program exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n"))))
         (body (c-compile-exp exp append-preamble)))
    (string-append 
     "int main (int argc, char* argv[]) {\n"
     preamble 
     "  __sum         = MakePrimitive(__prim_sum) ;\n" 
     "  __product     = MakePrimitive(__prim_product) ;\n" 
     "  __difference  = MakePrimitive(__prim_difference) ;\n" 
     "  __display     = MakePrimitive(__prim_display) ;\n" 
     "  __numEqual    = MakePrimitive(__prim_numEqual) ;\n"      
     "  " body " ;\n"
     "  return 0;\n"
     " }\n")))


; c-compile-exp : exp (string -> void) -> string
(define (c-compile-exp exp append-preamble)
  (cond
    ; Core forms:
    ((const? exp)       (c-compile-const exp))
    ((prim?  exp)       (c-compile-prim exp))
    ((ref?   exp)       (c-compile-ref exp))
    ((if? exp)          (c-compile-if exp append-preamble))

    ; IR (1):
    ((cell? exp)        (c-compile-cell exp append-preamble))
    ((cell-get? exp)    (c-compile-cell-get exp append-preamble))
    ((set-cell!? exp)   (c-compile-set-cell! exp append-preamble))
    
    ; IR (2):
    ((closure? exp)     (c-compile-closure exp append-preamble))
    ((env-make? exp)    (c-compile-env-make exp append-preamble))
    ((env-get? exp)     (c-compile-env-get exp append-preamble))
    
    ; Application:      
    ((app? exp)         (c-compile-app exp append-preamble))
    (else               (error "unknown exp in c-compile-exp: " exp))))

; c-compile-const : const-exp -> string
(define (c-compile-const exp)
  (cond
    ((integer? exp) (string-append 
                     "MakeInt(" (number->string exp) ")"))
    ((boolean? exp) (string-append
                     "MakeBoolean(" (if exp "1" "0") ")"))
    (else           (error "unknown constant: " exp))))

; c-compile-prim : prim-exp -> string
(define (c-compile-prim p)
  (cond
    ((eq? '+ p)       "__sum")
    ((eq? '- p)       "__difference")
    ((eq? '* p)       "__product")
    ((eq? '= p)       "__numEqual")
    ((eq? 'display p) "__display")
    (else             (error "unhandled primitive: " p))))

; c-compile-ref : ref-exp -> string
(define (c-compile-ref exp)
  (mangle exp))
  
; c-compile-args : list[exp] (string -> void) -> string
(define (c-compile-args args append-preamble)
  (if (not (pair? args))
      ""
      (string-append
       (c-compile-exp (car args) append-preamble)
       (if (pair? (cdr args))
           (string-append ", " (c-compile-args (cdr args) append-preamble))
           ""))))

; c-compile-app : app-exp (string -> void) -> string
(define (c-compile-app exp append-preamble)
  (let (($tmp (mangle (gensym 'tmp))))
    
    (append-preamble (string-append
                      "Value " $tmp " ; "))
    
    (let* ((args     (app->args exp))
           (fun      (app->fun exp)))
      (string-append
       "("  $tmp " = " (c-compile-exp fun append-preamble) 
       ","
       $tmp ".clo.lam("
       "MakeEnv(" $tmp ".clo.env)"
       (if (null? args) "" ",")
       (c-compile-args args append-preamble) "))"))))
  
; c-compile-if : if-exp -> string
(define (c-compile-if exp append-preamble)
  (string-append
   "(" (c-compile-exp (if->condition exp) append-preamble) ").b.value ? "
   "(" (c-compile-exp (if->then exp) append-preamble)      ") : "
   "(" (c-compile-exp (if->else exp) append-preamble)      ")"))

; c-compile-set-cell! : set-cell!-exp (string -> void) -> string 
(define (c-compile-set-cell! exp append-preamble)
  (string-append
   "(*"
   "(" (c-compile-exp (set-cell!->cell exp) append-preamble) ".cell.addr)" " = "
   (c-compile-exp (set-cell!->value exp) append-preamble)
   ")"))

; c-compile-cell-get : cell-get-exp (string -> void) -> string 
(define (c-compile-cell-get exp append-preamble)
  (string-append
   "(*("
   (c-compile-exp (cell-get->cell exp) append-preamble)
   ".cell.addr"
   "))"))

; c-compile-cell : cell-exp (string -> void) -> string
(define (c-compile-cell exp append-preamble)
  (string-append
   "NewCell(" (c-compile-exp (cell->value exp) append-preamble) ")"))

; c-compile-env-make : env-make-exp (string -> void) -> string
(define (c-compile-env-make exp append-preamble)
  (string-append
   "MakeEnv(__alloc_env" (number->string (env-make->id exp))
   "(" 
   (c-compile-args (env-make->values exp) append-preamble)
   "))"))

; c-compile-env-get : env-get (string -> void) -> string
(define (c-compile-env-get exp append-preamble)
  (string-append
   "((struct __env_"
   (number->string (env-get->id exp)) "*)" 
   (c-compile-exp (env-get->env exp) append-preamble) ".env.env)->" 
   (mangle (env-get->field exp))))




;; Lambda compilation.

;; Lambdas get compiled into procedures that, 
;; once given a C name, produce a C function
;; definition with that name.

;; These procedures are stored up an eventually 
;; emitted.

; type lambda-id = natural

; num-lambdas : natural
(define num-lambdas 0)

; lambdas : alist[lambda-id,string -> string]
(define lambdas '())

; allocate-lambda : (string -> string) -> lambda-id
(define (allocate-lambda lam)
  (let ((id num-lambdas))
    (set! num-lambdas (+ 1 num-lambdas))
    (set! lambdas (cons (cons id (cons lam '())) lambdas))
    id))

; get-lambda : lambda-id -> (symbol -> string)
(define (get-lambda id)
  (cdr (assv id lambdas)))

; c-compile-closure : closure-exp (string -> void) -> string
(define (c-compile-closure exp append-preamble)
  (let* ((lam (closure->lam exp))
         (env (closure->env exp))
         (lid (allocate-lambda (c-compile-lambda lam))))
    (string-append
     "MakeClosure("
     "__lambda_" (number->string lid)
     ","
     (c-compile-exp env append-preamble)
     ")")))

; c-compile-formals : list[symbol] -> string
(define (c-compile-formals formals)
  (if (not (pair? formals))
      ""
      (string-append
       "Value "
       (mangle (car formals))
       (if (pair? (cdr formals))
           (string-append ", " (c-compile-formals (cdr formals)))
           ""))))

; c-compile-lambda : lamda-exp (string -> void) -> (string -> string)
(define (c-compile-lambda exp)
  (let* ((preamble "")
         (append-preamble (lambda (s)
                            (set! preamble (string-append preamble "  " s "\n")))))
    (let ((formals (c-compile-formals (lambda->formals exp)))
          (body    (c-compile-exp     (lambda->exp exp) append-preamble)))
      (lambda (name)
        (string-append "Value " name "(" formals ") {\n"
                       preamble
                       "  return " body " ;\n"
                       "}\n")))))
  
; c-compile-env-struct : list[symbol] -> string
(define (c-compile-env-struct env)
  (let* ((id     (car env))
         (fields (cdr env))
         (sid    (number->string id))
         (tyname (string-append "struct __env_" sid)))
    (string-append 
     "struct __env_" (number->string id) " {\n" 
     (apply string-append (map (lambda (f)
                                 (string-append
                                  " Value "
                                  (mangle f) 
                                  " ; \n"))
                               fields))
     "} ;\n\n"
     tyname "*" " __alloc_env" sid 
     "(" (c-compile-formals fields) ")" "{\n"
     "  " tyname "*" " t = malloc(sizeof(" tyname "))" ";\n"
     (apply string-append 
            (map (lambda (f)
                   (string-append "  t->" (mangle f) " = " (mangle f) ";\n"))
                 fields))
     "  return t;\n"
     "}\n\n"
     )))
    



;; Code emission.
(define (emit line)
  line)
  
; c-compile-and-emit : (string -> A) exp -> void
(define (c-compile-and-emit emit input-program)

  (define compiled-program "")
  
  (set! input-program (desugar input-program))

  (analyze-mutable-variables input-program)

  (set! input-program (desugar (wrap-mutables input-program)))

  (set! input-program (closure-convert input-program))

  (emit "#include <stdlib.h>")
  (emit "#include <stdio.h>")
  (emit "#include \"scheme.h\"")
  
  (emit "")
  
  ; Create storage for primitives:
  (emit "
Value __sum ;
Value __difference ;
Value __product ;
Value __display ;
Value __numEqual ;
")
  
  (for-each 
   (lambda (env)
     (emit (c-compile-env-struct env)))
   environments)

  (set! compiled-program  (c-compile-program input-program))

  ;; Emit primitive procedures:
  (emit 
   "Value __prim_sum(Value e, Value a, Value b) {
  return MakeInt(a.z.value + b.z.value) ;
}")
  
  (emit 
   "Value __prim_product(Value e, Value a, Value b) {
  return MakeInt(a.z.value * b.z.value) ;
}")
  
  (emit 
   "Value __prim_difference(Value e, Value a, Value b) {
  return MakeInt(a.z.value - b.z.value) ;
}")
  
  (emit
   "Value __prim_display(Value e, Value v) {
  printf(\"%i\\n\",v.z.value) ;
  return v ;
}")
  
  (emit
   "Value __prim_numEqual(Value e, Value a, Value b) {
  return MakeBoolean(a.z.value == b.z.value) ;
}")
  
  ;; Emit lambdas:
  ; Print the prototypes:
  (for-each
   (lambda (l)
     (emit (string-append "Value __lambda_" (number->string (car l)) "() ;")))
   lambdas)
  
  (emit "")
  
  ; Print the definitions:
  (for-each
   (lambda (l)
     (emit ((cadr l) (string-append "__lambda_" (number->string (car l))))))
   lambdas)
  
  (emit compiled-program))


;; Compile and emit:
(define the-benchmark-program 3)

(c-compile-and-emit emit the-benchmark-program)
