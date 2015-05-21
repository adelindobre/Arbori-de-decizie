;#lang racket


;; ================================================================================================================ FORMATS


;; În lista dată de atribute, un atribut va fi descris complet printr-o listă care are pe prima poziție numele atributului și
;; în restul listei valorile pentru atribut.
;; Specificarea completă a atributelor se va face deci printr-o listă de forma:
;; '( (atribut1 valoare11 valoare12 valoare13) (atribut2 valoare21 valoare22) (atribut3 valoare31 valoare32 valoare33) )
;; La fel, informațiile despre clasificare vor fi specificate complet prin lista de forma
;; '(nume-atribut-clasă clasă1 clasă2 clasă3)

;; Funcțiile de mai jos pot fi utile pentru a extrage caracteristicile atributelor / clasei:
(define get-attr-name car)
(define get-attr-values cdr)

;; Exemplele vor fi date ca o listă în care fiecare exemplu este o listă de perechi nume_atribut-valoare_atribut.
;; Fiecare exemplu va fi de forma (Atenție! nu este obligatoriu ca atributele să apară în ordinea din lista de atribute):
;; '((atribut1 . valoare12) (atribut2 . valoare22) (atribut3 . valoare33))

;; get-attr-value extrage dintr-un exemplu valoarea pentru atributul cerut
(define get-attr-value (λ (attribute-name example) (cdr (assoc attribute-name example))))


;; [BONUS] Există trei tipuri de noduri speciale, care folosesc stringuri fixe din lista string de mai jos:
;; * rădăcina arborelui poate fi 'no-examples dacă niciun exemplu nu a fost dat la rulare (listă vidă)
;; * dacă într-un nod din arbore lista de exemple este vidă, nodul va avea valoarea '(default . <clasă-default>), cu
;;   clasă-default clasa majoritară a exemplelor din nodul părinte.
;; * dacă într-un nod din arbore sunt exemple cu clase diferite, dar nu mai sunt atribute după care să fie separate, nodul
;;   va avea valoarea '(majority . <clasă-majoritară>), cu clasă-majoritară clasa majoritară a exemplelor.
(define string '(no-examples default majority))


;; ================================================================================================================ PRINTING

;; prints the arguments, separated by spaces, and followed by newline.
;; prints only if 'printing' is enabled and either the first argument is #t (exactly #t) or 'verbose' is enabled.
;; always returns non-#f
;; usage:
;;  (p "the number is" 5 "and the other is" 0) - prints only if 'printing' and 'verbose' are enabled
;;  (p #t "must print this") - prints if 'pinting' is enabled, regardless of the setting for 'verbose'
(define (p . L) (when printing
                  (let* ((must (and (not (null? L)) (equal? #t (car L)))) (LP (if must (cdr L) L)))
                    (when (or must verbose) (map (λ (el) (display el) (display " ")) LP) (newline)) )))
(define printing #t) ; if false, (p ...) never prints
(define verbose #t) ; if false, only (p #t ...) prints





;; ================================================================================================================ OTHER

;; Logaritm în baza 2
(define (log2 x) (/ (log x) (log 2)))





;; ================================================================================================================ TESTING

;; from arguments specifying attribute names, the name of the class attribute, and the examples, produces a test
;; in the form '(  (examples (...)  (class-attr <class-attr-name> <class1> <class2> ...)  (attribute-set (attr-name val1 val2 val3) (attr-name val1 ...) ...)  )
;; used in (get-test name)
(define (make-test attributes classname examples test-examples)
  (let* (
         (allnames (append attributes (list classname)))
         (allvaluesdups (foldl (λ (example acc) (map cons example acc)) (build-list (length allnames) (λ (x) '())) examples))
         (allvalues (map remove-duplicates allvaluesdups))
         (class-attr (append (list 'class-attr classname) (last allvalues)))
         (attribute-set (cons 'attribute-set (map cons attributes (take allvalues (length attributes)))))
         (example-set (cons 'examples (map (λ (example) (map cons allnames example)) examples)))
         (output (list example-set class-attr attribute-set (cons 'test-examples test-examples)))
         )
    output
    ))

;; produce un test care va fi apoi utilizat de (test <nume-test>)
(define (get-test test-name) (apply make-test (cdr (assoc test-name tests))))

;; întoarce arborele rezultat prin aplicarea funcției date în tree-creator argumentelor din testul test-name
(define (get-tree test-name tree-creator)
  (apply tree-creator 
         (map (λ (part) (cdr (assoc part (get-test test-name))))
              '(examples attribute-set class-attr))
         ))

;; obține un arbore chemând funcția din test-creator cu argumentele din test-name
;; și veriică structura arborelui și clasificarea corectă a exemplelor din test.
(define (perform-test functions test-name tree-creator) 
  (let* ((test (get-test test-name))
         (test-examples (cdr (assoc 'test-examples test)))
         (output (get-tree test-name tree-creator))
         (structure-test (check-tree-structure functions output test))
         )
    (if (not (equal? structure-test #t))
        (and (display `(check-tree-structure failed with error: ,structure-test))(newline) #f)
        (andmap (λ (example)
                  (let ((decision (decide-class functions output (car example))))
                    (or (equal? decision (cdr example))
                        (and (display `(example < ,(car example) > classified as < ,decision > instead of < ,(cdr example) >)) #f))
                    )) test-examples)
        )))

;; verifică structura unui arbore (accesat prin funcțiile oferite în fn)
;; și întoarce #t dacă structura este corectă, și un mesaj care descrie prima eroare, altfel (mesajul este o listă).
;; elemente verificate:
;; * structura corectă a nodurilor, a ramurilor și a frunzelor
;; * apartenența valorilor din frunze la mulțimea de valori a atributului clasă
;; * apartenența valorilor din noduri la mulțimea de atribute
;; * apartenența valorilor de pe ramuri la mulțimea de valori ai atributului verificat în nod
;; * corespondența dintre ramurile unui nod și mulțimea valorilor atributului verificat în nod (trebuie să fie 1-la-1)
(define (check-tree-structure fn root test)
  (check-tree-structure-r fn root (cdr (assoc 'attribute-set test)) (cdr (assoc 'class-attr test)) #f))

(define (check-tree-structure-r fn root attributes class-attribute up-branch)
  (let* (
         (Fl? (first fn))   ; is leaf?
         (Fcls (second fn)) ; leaf class
         (Fsl? (third fn))  ; is special leaf?
         (Ftyp (fourth fn)) ; special leaf type
         (Fattr (fifth fn)) ; node attribute
         (Fchld (sixth fn)) ; child node
         (Fn? (if (> (length fn) 6) (seventh fn) identity)) ; is node?
         (render-node (λ (node) 
                        (if (Fl? node) (Fcls node) 
                            (cons (Fattr node)
                                  (filter identity (map (λ (v) (if (Fchld node v) `(,v ->...) #f))
                                                        (if (assoc (Fattr node) attributes) (get-attr-values (assoc (Fattr node) attributes)) '())))))))
         )
    (cond
      ((not (Fn? root)) `(should-be-node element < ,root > is not a node))
      ((Fsl? root) ; node (majority . class) or (default . class)
       (if (member (Ftyp root) (cdr string))
           (or (and (member (Fcls root) (get-attr-values class-attribute)) #t)
               `(in node < ,root > value < ,(Fcls root) > is not an appropriate class))
           `(special leaf node < ,root > has an incorrect type)
           ))
      ((Fl? root) ; node is 'no-examples or a class name
       (or (and (not up-branch) (equal? (Fcls root) (first string)))
           (and (member (Fcls root) (get-attr-values class-attribute)) #t)
           `(leaf node < ,root > has an illegal value)))
      (else  ; node is a normal node, ith attribute and branches
       ;     (if (null? root)
       ;         `(node for value < ,up-branch > is empty)
       (let* ((attribute-name (Fattr root))
              (attribute (assoc attribute-name attributes))
              (attr-values (when attribute (get-attr-values attribute))))
         (if attribute
             (foldl (λ (v acc) 
                      (if (equal? acc #t)
                          (cond
                            ((Fchld root v)
                             (check-tree-structure-r fn (Fchld root v) (remove attribute attributes) class-attribute v))
                            (else `(in node < ,(render-node root) > no branch was found for value < ,v > of attribute < ,attribute-name > ))
                            )
                          acc
                          ))
                    #t attr-values)
             `(attribute < ,attribute-name > in node < ,(render-node root) > is not in the attribute list)
             )));)
      )))
;; checks the correctness of the function above
(define (sanity-check-1) (andmap (λ (test)
                                   ;(p test)
                                   (or ((if (and (> (length test) 2) (third test)) identity not)
                                        (equal? (check-tree-structure base-tree-functions
                                                                      (second test)
                                                                      base-tree-context
                                                                      ) #t)
                                        ) (and (p #t "failed " test) #f)))
                                 basetree))

;; calculează înălțimea arborelui, folosind funcțiile de acces oferite
(define (get-tree-height fn tree test) (get-tree-height-r fn tree (cdr (assoc 'attribute-set test))))
(define (get-tree-height-r fn tree attributes)
  (if ((first fn) tree) 1 (add1 (apply max (map (λ (v) (get-tree-height-r fn ((sixth fn) tree v) attributes)) (get-attr-values (assoc ((fifth fn) tree) attributes)))))))
;; checks the correctness of the function above
(define (sanity-check-2) (andmap (λ (test)
                                   ;(p test)
                                   (if (and (> (length test) 2) (third test))
                                       (or (equal? (fourth test) (get-tree-height base-tree-functions (second test) base-tree-context)) (and (p #t "failed " test) #f))
                                       #t)) basetree))

;; decide clasa exemplului <example>, bazat pe arborele de decizie dat și funcțiile de acces oferite
(define (decide-class fn node example)
  (if ((first fn) node) ((second fn) node) 
      (decide-class fn ((sixth fn) node (get-attr-value ((fifth fn) node) example)) example)))

;; checks the correctness of the function above
(define (sanity-check-3) (andmap (λ (test) (if (and (> (length test) 2) (third test))
                                               (andmap
                                                (λ (ex) (equal? (decide-class base-tree-functions (second test) (car ex)) (cdr ex)))
                                                (fifth test))
                                               #t)) basetree))


;; performs all sanity checks
(define (sanity-checks) (andmap eval
                                (map (λ (c) (list (string->symbol (string-join (list "sanity-check-" (number->string c)) ""))))
                                     '(1 2))
                                ))
;; ================================================================================================================ TESTS



;; Teste pentru temă. Pentru a vedea un test într-o formă aproape de cea dată programului, apelați (get-test <nume-test>), 
;; unde nume-test este 1,2,3, etc (vedeți primul membru din fiecare element din tests).

;; aici, testele sunt date ca:
;; '( <nume-test> <listă-nume-atribute> <nume-atribut-clasă> <listă-exemple> )
;; unde fiecare exemplu este dat ca o listă de n+1 valori: pentru cele n atribute (în ordine) plus clasa exemplului
(define tests '(
                (objects (shape size) class
                         (
                          (round 2 nice)
                          (irregular 4 cumbersome)
                          (irregular 2 ugly)
                          (irregular 1 nice)
                          (square 2 ugly)
                          )(
                            ( ((size . 1) (shape . round)) . nice)
                            ( ((size . 1) (shape . irregular)) . nice)
                            ( ((size . 2) (shape . irregular)) . ugly)
                            ( ((size . 4) (shape . irregular)) . cumbersome)
                            ))
                (food-small (Food Chat Speedy Price Bar) BigTip
                            (
                             (great yes yes adequate no yes)
                             (great no yes adequate no yes)
                             (mediocre yes no high no no)
                             (great yes yes adequate yes yes)
                             )(
                               ( ((Food . great) (Chat . yes) (Speedy . no) (Price . high) (Bar . yes)) . yes)
                               ( ((Food . mediocre) (Chat . yes) (Speedy . no) (Price . high) (Bar . yes)) . no)
                               ( ((Food . great) (Chat . yes) (Speedy . no) (Price . adequate) (Bar . no)) . yes)
                               ( ((Food . mediocre) (Chat . no) (Speedy . yes) (Price . adequate) (Bar . no)) . no)
                               ))
                (food-big (Food Chat Speedy Price Bar) BigTip
                          (
                           (great yes no high no no)
                           (great no no high no yes)
                           (mediocre yes no high no no)
                           (great yes yes adequate yes yes)
                           )(
                             ( ((Food . great) (Chat . yes) (Speedy . no) (Price . high) (Bar . yes)) . no)
                             ( ((Food . mediocre) (Chat . yes) (Speedy . no) (Price . high) (Bar . yes)) . no)
                             ( ((Food . great) (Chat . yes) (Speedy . yes) (Price . adequate) (Bar . no)) . yes)
                             ( ((Food . mediocre) (Chat . no) (Speedy . yes) (Price . adequate) (Bar . no)) . no)
                             ))
                (weather (Outlook Temperature Humidity Wind) Play
                         (
                          (Sunny 	Hot	High 	Weak	No)
                          (Sunny 	Hot	High 	Strong	No)
                          (Overcast 	Hot	High 	Weak	Yes)
                          (Rain 	Mild	High 	Weak	Yes)
                          (Rain 	Cool	Normal 	Weak	Yes)
                          (Rain 	Cool	Normal 	Strong	No)
                          (Overcast 	Cool	Normal 	Strong	Yes)
                          (Sunny 	Mild	High 	Weak	No)
                          (Sunny 	Cool	Normal 	Weak	Yes)
                          (Rain 	Mild	Normal 	Weak	Yes)
                          (Sunny 	Mild	Normal 	Strong	Yes)
                          (Overcast 	Mild	High 	Strong	Yes)
                          (Overcast 	Hot	Normal 	Weak	Yes)
                          (Rain 	Mild	High 	Strong	No)
                          )(
                            ( ((Outlook . Overcast) (Temperature . Hot) (Humidity . High) (Wind . Strong)) . Yes)
                            ( ((Outlook . Rain) (Temperature . Hot) (Humidity . High) (Wind . Strong)) . No)
                            ( ((Outlook . Sunny) (Temperature . Hot) (Humidity . High) (Wind . Strong)) . No)
                            ))
                (bonus (shape size) class
                       (
                        (round 2 nice)
                        (irregular 4 cumbersome)
                        (irregular 1 nice)
                        (square 2 ugly)
                        (square 3 cumbersome)
                        (square 3 ugly)
                        )(
                          ( ((size . 1) (shape . round)) . nice)
                          ( ((size . 3) (shape . irregular)) . ugly)
                          ( ((size . 4) (shape . square)) . cumbersome)
                          ))
                ))

;; sanity tests, for internal use
(define base-tree-context '((class-attr class yes no) (attribute-set (shape square irregular) (size 1 4))))
(define basetree `(
                   (constant-1 a)
                   (constant-2 1)
                   (no-examples ((#f . no-examples)) #t 1 ((#f . no-examples) (((a . 1)) . no-examples)))
                   (pair-1 (a . b))
                   (pair-2 ((a . b)))
                   (valid-pair ((majority . yes)) #t 1 ((#f . yes) (((a . 1)) . yes)))
                   (valid-pair ((default . yes)) #t 1 ((#f . yes) (((a . 1)) . yes)))
                   (no-branches ((#f . shape)))
                   ;(invalid-branch-1 ((#f . shape) (square . yes) (dunno))
                   (invalid-branch-2 ((#f . shape) (square . yes) (dunno . other)))
                   ;(invalid-branch-3 ((#f . shape) (square . yes) (dunno . blank)))
                   (missing-branches ((#f . shape) #f (square . size) #f #f (1 . yes) (4 . no)))
                   (ok-branches ((#f . shape) (square . yes) (irregular . size) #f #f (1 . no) (4 . (majority . yes))) #t 3 
                                (
                                 (((shape . square) (size . 1)) . yes)
                                 (((shape . square) (size . 4)) . yes)
                                 (((shape . irregular) (size . 1)) . no)
                                 (((shape . irregular) (size . 4)) . yes)
                                 ))
                   ))
(define base-tree-functions
  (let ((branchlist (λ (node)
                      (filter (λ (br) (and (not (null? br)) (car br) (pair? br)))
                              (map (λ (child) (map (λ (ref) (list-ref node ref))
                                                   (let build ((L (list child)))
                                                     (cond
                                                       ((null? L) '())
                                                       ((>= (car L) (length node)) (build (cdr L)))
                                                       (else (cons (car L) (build (append (cdr L) (map (λ (chld) (+ chld (* 2 (car L)))) (build-list 2 add1))))))
                                                       ))
                                                   )) (build-list 2 add1))))))
    (list
     (λ (node) (null? (branchlist node))) ; is leaf?
     (λ (node) ((if (pair? (cdar node)) cddar cdar) node)) ; leaf class
     (λ (node) (and ((first base-tree-functions) node) (pair? (cdar node)))) ; is special leaf?
     cadar ; special leaf type
     cdar ; node attribute
     (λ (node value) (let ((branch (filter (λ (br) (equal? (caar br) value)) (branchlist node))))
                       (if (null? branch) #f (car branch)))) ; child node
     (λ (node) (and (list? node) (pair? (car node)))) ; is node?
     )))





