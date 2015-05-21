#lang racket

(require test-engine/racket-tests)
(include "decision-tree-test.rkt")

;(define node (list atribut class specialType children valAtribut) )


;; pentru frunze:
;; primește un nod; întoarce #t dacă acesta este frunză, #f altfel

(define is-leaf?  
  (λ (node) 
    (if (null? (second node)) 
        #f
        #t)
    )
  )

;; primște un nod frunză; întoarce clasa exemplelor din frunză
(define get-leaf-class
  ; TODO
  (λ (node) 
      (second node)
  )
)

;; pentru frunze speciale (BONUS):

;; primște un nod; întoarce #t dacă nodul este frunză specială (frunză fără exemple sau cu exemple de clase diferite), #f altfel
;; dacă nu implementați bonus, trebuie ca funcția să întoarcă #f

(define is-special-leaf?
  ; TODO
  (λ (node) 
    (if (or (equal? (car (third node)) (second string)) (equal? (car (third node)) (third string) ) )
        #t
        #f)
    )
  )

;; primește un nod frunză specială; întoarce tipul frunzei speciale (trebuie să fie unul dintre membrii 2 și 3 ai strings
;; clasa exemplelor din frunza specială va fi verificată tot cu get-leaf-class

(define get-special-leaf-type
  ; TODO
  (λ (node) 
    (car (third node))
    )
  )


;; pentru noduri care nu sunt frunze:

;; primște un nod; întoarce numele atributului verificat în nod
(define get-attribute-name
  ; TODO
  (λ (node) 
    (first node) 
    )
  )

;; primește un nod și o valoare a atributului verificat în nod
;; întoarce nodul copil (nod intern sau frunză) corespunzător valorii date
;(define get-child
  ; TODO
;  (λ (node value) 
;    (first (filter (lambda (x) (if (equal? value (fifth x)) x #f) ) (fourth node)))
;    )
;  )

;(define get-child
;  (λ (node value) 
;    (if (equal? (first (fifth node)) value)
;        (first (fourth node))
;        (second (fourth node))
;        )
;    )
;  )

(define get-child
  (λ (node value) 
   (foldl (λ (el acc) (if (equal? value el) (list-ref (fourth node) (get-list-index (fifth node) el)) acc)) null (fifth node))
    )
 )

;; opțional: verificare nod
;; primește un argument; întoarce #t dacă argumentul este o reprezentare corectă a unui nod (frunză sau nu) din arbore; #f altfel
(define is-node?
  ;TODO
  (λ (node)
    (if (or (not (null? (first node))) (not (null? (second node))) (not (null? (fifth node)))) #t #f)
    )
  )


; asamblare funcții de acces arbore
(define functions (list is-leaf? get-leaf-class is-special-leaf? get-special-leaf-type get-attribute-name get-child is-node?))


;(define node (list atribut class specialType children valAtribut) )



;; TASK (pregătitor):
;; scrieți (manual) în formatul ales un arbore de decizie pentru exemple conținând 1 atribut - shape, care are două valori - round și square
;; un exemplu va fi în clasa "yes" dacă este rotund, și în "no" altfel
;; arborele trebuie să fie astfel:
;;    shape
;;     / \
;; round square
;;   /     \
;; yes     no


(define (change-index-value lst idx val)
  (if (null? lst)
    lst
    (cons
      (if (zero? idx)
        val
        (car lst))
      (change-index-value (cdr lst) (- idx 1) val))))


(define atribut '(shape round square) )
(define clasa '(classname yes no))
(define examples '( ( (shape . round) (classname . yes) )  ( (shape . square) (classname . no) ) )  )

(define node1 (list null (second clasa) '(notype) '() null) )
(define node2 (list null (third clasa) '(notype) '() null) )
(define node3 (list (first atribut) null  '(notype) (list node1 node2) (cdr atribut) ))

;(change-index-value node1 5 node3)
;(change-index-value node2 5 node3)

(define tree-1 
  node3
  )
           
(check-expect (is-node? tree-1) #t)
(check-expect (is-leaf? tree-1) #f)
(check-expect (get-attribute-name tree-1) 'shape)
(check-expect (not (get-child tree-1 'round)) #f)
(check-expect (not (get-child tree-1 'square)) #f)
(check-expect (is-leaf? (get-child tree-1 'round)) #t)
(check-expect (is-leaf? (get-child tree-1 'square)) #t)
(check-expect (is-special-leaf? (get-child tree-1 'round)) #f)
(check-expect (get-leaf-class (get-child tree-1 'round)) 'yes)
(check-expect (get-leaf-class (get-child tree-1 'square)) 'no)


;; TASK
;; scrieți funcția de mai jos pentru a calcula entropia unui set de exemple, fiecare exemplu conținând informație despre clasa sa
;; funcția log2 este implementată în decision-tree-test

;; examples: o listă de exemple (setul S), nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: entropia setului de exemple în raport cu clasa, H(S) = - sumă-peste-clase p(clasă)*log2 p(clasă)
;; unde p(clasă) este numărul de exemple cu clasa <clasă> împărțit la numărul de exemple din setul S


(define cautaClasa
  (λ (examples clasaNume)
    (filter (λ (perX) (if (equal? (car perX) clasaNume) #t #f)) examples)      
    )
  )

(define listClassType
  (λ (examples class-attribute) 
    (if (null? examples)
        '()
    (append (cautaClasa (car examples) (car class-attribute)) (listClassType (cdr examples) class-attribute) )
    )
  )
 )

(define (aparitii examples class-attribute)
  (map (λ (classValue) (foldl  (λ (perX acc) (if (equal? (cdr perX) classValue) (+ acc 1) (+ acc 0)) ) 0 (listClassType examples class-attribute) ) ) (cdr class-attribute) )
  )

(define compute-enthropy
  
  (λ (examples class-attribute)
    (-(apply + (map (λ (frac) (if (equal? frac 0) 0 (* frac (log2 frac))  ) ) (map  (λ (nrApar) (/ nrApar (length examples) ) ) (aparitii examples class-attribute))  ) ))
   )
 )

(define tolerance 0.001)
;(check-within (compute-enthropy '() '(classname yes no)) 0 tolerance) ; expect error
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . yes))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . no)) ((shape . square) (classname . no))) '(classname yes no)) 0 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no))) '(classname yes no)) 1 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . no))) '(classname yes no maybe)) 0.918 tolerance)
(check-within (compute-enthropy '(((shape . round) (classname . yes)) ((shape . square) (classname . no)) ((shape . square) (classname . maybe))) '(classname yes no maybe)) 1.584 tolerance)

;; TASK
;; scrieți funcția de mai jos pentru a calcula câștigul informațional al unui atribut în raport cu clasa, pentru un set de exemple

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attribute: o listă de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: câștigul informațional al atributului, G(S, A) = H(S) - sumă-peste-valori-A p(v)*H(Sv)
;;   unde p(v) este numărul de exemple cu valoarea v pentru A împărțit la numărul de exemple din S
;;   iar Sv este mulțimea exemplelor din S care au valoarea v pentru A

(define cautaAtributValue
  (λ (examples atribut value)
    (filter (λ (perX) (if (and (equal? (car perX) atribut) (equal? (cdr perX) value) )  #t #f)) examples)      
    )
  )

(define nrExempleAtr
  (λ (examples atribut value) 
   (if (null? examples)
       '()
       (if (not (null? (cautaAtributValue (car examples) atribut value)) ) 
           (cons (car examples) (nrExempleAtr (cdr examples) atribut value)) 
           (nrExempleAtr (cdr examples) atribut value)
           )
   )   
  )
 )

(define calculFractie
  (λ (examples attribute)
    (map  (λ (nrApar) (/ nrApar (length examples) ) ) (aparitii examples attribute))
    )
  )

(define elemSum
  (λ (examples attribute class-attribute)
    ( map (λ (frac value) (if (equal? frac 0) 0 (* frac (compute-enthropy (nrExempleAtr examples (car attribute) value) class-attribute) ) ) ) (calculFractie examples attribute) (cdr attribute) )
  )
 )

(define compute-gain
  (λ (examples attribute class-attribute)
    ( - (compute-enthropy examples class-attribute) (apply + (elemSum examples attribute class-attribute) ) )
  )
)

(check-within (compute-gain 
               '(((shape . round) (classname . yes)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . square) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 1 tolerance)
(check-within (compute-gain 
               '(((shape . round) (classname . no)) ((shape . round) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . yes)))
               '(shape round square)
               '(classname yes no)
               ) 0.311 tolerance)
(check-within (compute-gain 
               '(((shape . round) (size . 1) (classname . yes))
                 ((shape . round) (size . 2) (classname . no))
                 ((shape . square) (size . 1) (classname . yes))
                 ((shape . square) (size . 2) (classname . no))
                 ((shape . square) (size . 2) (classname . yes)))
               '(size 1 2)
               '(classname yes no)
               ) 0.419 tolerance)



;; TASK
;; creați un arbore de decizie pentur mulțimea dată de exemple, pentru muțimea dată de atribute, și pentru clasa dată

;; examples: o listă de exemple, nevidă, în care fiecare exemplu este o listă de perechi, una dintre ele fiind (<nume-atribut-clasă> <clasă>)
;; attributes: o listă de liste de forma (<nume-atribut> <valore-1> <valoare-2> <valoare-3>)
;; class-attribute: o listă de forma (<nume-atribut-clasă> <clasă-1> <clasă-2> <clasă-3>)
;; întoarce: un arbore de decizie, în formatul ales, corespunzător cu argumentele date

(define indexElem
  (λ (lista x)
    ( + (- (length lista) (length (member x lista))) 1)
    )
  )

(define sameClass 
  (λ (examples class)
    (foldl (λ (x acc) (if (= x (length examples)) (+ acc (indexElem (aparitii examples class) x) )  acc)) 0 (aparitii examples class))
    )
)

(define (get-list-index l el)
    (if (null? l)
        -1
        (if (equal? (car l) el)
            0
            (let ( (result (get-list-index (cdr l) el)) )
                (if (= result -1)
                    -1
                    (+ 1 result))))))


(define findMax
  (λ (max lista)
    (if (null? lista)
        max
        (if (> (car lista) max)
            (findMax (car lista) (cdr lista))
            (findMax max (cdr lista)))
    )
  )
 )

(define operations
  (λ (examples attributes class-attribute) 
    (get-list-index (map (λ (atribut) (compute-gain examples atribut class-attribute) ) attributes) (findMax 0 (map (λ (atribut) (compute-gain examples atribut class-attribute) ) attributes)) )
  ))

(define deleteItem
  (λ (lista el) 
    (filter (λ (x) (if (equal? el x) #f #t) )  lista)
  ))

(define divideEx
  (λ (examples restAttributes valAtr class-attribute)
    (list (first valAtr) null '(notype) (divideEx2 examples restAttributes valAtr class-attribute) (cdr valAtr))  
    )
  )
  
(define divideEx2
  (λ (examples restAttributes valAtr class-attribute)
    (map  (λ (val) (create-tree (nrExempleAtr examples (car valAtr) val) restAttributes class-attribute) ) (cdr valAtr))
    )
  )  
  

(define create-tree
  (λ (examples attributes class-attribute)
    (cond
      ( (not (= (sameClass examples class-attribute) 0)) (list null (list-ref  class-attribute (sameClass examples class-attribute) ) '(notype) null null) )
      (else (divideEx examples (deleteItem attributes (list-ref attributes (operations examples attributes class-attribute))) (list-ref attributes (operations examples attributes class-attribute)) class-attribute ))
      )
  )
)

(define I-DID-THE-BONUS #f)

(check-expect (perform-test functions 'food-small create-tree) #t)
(check-expect (perform-test functions 'food-big create-tree) #t)
(check-expect (perform-test functions 'objects create-tree) #t)
(check-expect (perform-test functions 'weather create-tree) #t)

(check-expect (and (perform-test functions 'food-small create-tree) (get-tree-height functions (get-tree 'food-small create-tree) (get-test 'food-small))) 2)
(check-expect (and (perform-test functions 'food-big create-tree)   (get-tree-height functions (get-tree 'food-big create-tree) (get-test 'food-big)))   4)
(check-expect (and (perform-test functions 'weather create-tree)    (get-tree-height functions (get-tree 'weather create-tree) (get-test 'weather)))    3)
(check-expect (and (perform-test functions 'objects create-tree)    (get-tree-height functions (get-tree 'objects create-tree) (get-test 'objects)))    3)

(if I-DID-THE-BONUS (display "BONUS DONE\n") (display "bonus not done\n"))
(check-expect (if I-DID-THE-BONUS (perform-test functions 'bonus create-tree) #t) #t)
(when I-DID-THE-BONUS (display (get-tree 'bonus create-tree)) (newline))




(test)