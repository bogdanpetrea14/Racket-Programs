#lang racket
(require "suffix-tree-stream.rkt")
(require "collection.rkt")

(provide (all-defined-out))

;; Vom prelua toate funcțiile din etapele 1-3 (exceptând
;; longest-common-substring, care nu beneficiază de 
;; reprezentarea ca flux întrucât parcurge tot arborele)
;; și le vom adapta la noua reprezentare a unui ST.
;;
;; Pentru că un ST este construit pornind de la o colecție
;; de sufixe și pentru că ne dorim să nu calculăm toate
;; sufixele decât dacă este nevoie, vom modifica toate
;; funcțiile care prelucrau liste de sufixe pentru a
;; prelucra fluxuri de sufixe.
;;
;; Obs: fără această modificare a listelor de sufixe în
;; fluxuri de sufixe, și presupunând că am manipulat
;; arborii de sufixe doar prin interfața definită în
;; fișierul suffix-tree (respectând astfel bariera de 
;; abstractizare), ar trebui să alterăm doar funcția 
;; suffixes->st care este practic un constructor pentru
;; tipul ST.
;; Din cauza transformării listelor de sufixe în fluxuri,
;; avem mult mai multe implementări de modificat.
;; Puteam evita acest lucru? Da, utilizând conceptul de
;; colecție de sufixe de la început (în loc să presupunem
;; că ele vor fi prelucrate ca liste). În loc de cons,
;; car, cdr, map, filter, etc. am fi folosit de fiecare
;; dată collection-cons, collection-first, ... etc. -
;; aceste funcții fiind definite într-o bibliotecă
;; inițială ca fiind echivalentele lor pe liste, și
;; redefinite ulterior în stream-cons, stream-first,
;; ... etc. Operatorii pe colecții de sufixe ar fi 
;; folosit, desigur, doar funcții de tip collection-.
;;
;; Am ales să nu procedăm astfel pentru că ar fi provocat
;; confuzie la momentul respectiv (când chiar operatorii
;; pe liste erau o noutate) și pentru a vă da ocazia să
;; faceți singuri acest "re-design".


; TODO
; Copiați din etapele anterioare implementările funcțiilor
; de mai jos și modificați-le astfel:
; - Toate funcțiile care lucrează cu liste de sufixe vor
;   lucra cu un nou tip de date Collection, ai cărui
;   constructori și operatori vor fi definiți de voi
;   în fișierul collection.rkt.
; - Pentru toate funcțiile, trebuie să vă asigurați că
;   este respectată bariera de abstractizare (atât în 
;   cazul tipului ST cât și în cazul tipului Collection).
; Obs: cu cât mai multe funcții rămân nemodificate, cu atât
; este mai bine (înseamnă că design-ul inițial a fost bun).
         
(define (longest-common-prefix w1 w2)
  (define (aux-function w1 w2 acc)
    (cond
      [(and (collection-empty? w1) (collection-empty? w2)) (list (reverse acc) w1 w2)]
      [(and (not (collection-empty? w1)) (not (collection-empty? w2)) (eq? (collection-first w1) (collection-first w2)))
       (aux-function (collection-rest w1) (collection-rest w2) (cons (collection-first w1) acc))]
      [else (list (reverse acc) w1 w2)]))
  (aux-function w1 w2 '()))



; am schimbat, în numele funcției, cuvântul list în
; cuvântul collection
(define (longest-common-prefix-of-collection words)
    (define (aux-function words acc)
    (cond
      ((collection-empty? words) (collection-first acc))
      (else
        (let ((new-acc (longest-common-prefix (collection-first words) (collection-first acc))))
          (aux-function (collection-rest words) new-acc))
        )
      )
    )
  (aux-function words (list (collection-first words)))
)


(define (match-pattern-with-label st pattern)
  (let* ((branch (get-ch-branch st (car pattern))))
    (if (not branch)
        (list #f '())
        (let* ((label (get-branch-label branch))
               (common-prefix (longest-common-prefix-of-collection (list label pattern))))
          (if (equal? label common-prefix)
              (if (equal? label pattern)
                  #t
                  (list label (drop pattern (length common-prefix)) (get-branch-subtree branch)))
              (if (equal? common-prefix pattern)
                  #t 
                  (list #f common-prefix)))))))


(define (st-has-pattern? st pattern)
  (let ((result (match-pattern-with-label st pattern)))
    (if (eq? result #t)
        #t
        (if (eq? (car result) #f)
            #f
            (st-has-pattern? (caddr result) (cadr result))))))


(define (get-suffixes text)
  (if (not (collection-empty? text))
      (collection-cons text (get-suffixes (collection-rest text)))
      '()
  )
)


(define (get-ch-words words ch)
  (collection-filter
    (lambda (x)
      (and (not (collection-empty? x)) (char=? (collection-first x) ch))
    )
    words
  )
)


(define (ast-func suffixes)
  (cons (list (collection-first (collection-first suffixes)))
    (collection-map 
      (lambda (x)
        (if (collection-empty? x) '() (collection-rest x))
      )
      suffixes
    )
  )
)


(define (cst-func suffixes)
  (let* ((prefix (longest-common-prefix-of-collection suffixes)))
    (cons prefix 
      (collection-map 
        (lambda (x)
          (if (collection-empty? x) '() (drop x (length prefix)))
        )
        suffixes
      ))
  )
)

; considerați că și parametrul alphabet este un flux
; (desigur, și suffixes este un flux, fiind o colecție
; de sufixe)
(define (suffixes->st labeling-func suffixes alphabet)
  (if (collection-empty? suffixes)
      '() 
      (let* ((ch-words (collection-map (lambda (x) (get-ch-words suffixes x)) alphabet))
             (non-empty-ch-words (collection-filter (lambda (x) (not (collection-empty? x))) ch-words)))
        (collection-map (lambda (ch-suffixes)
               (let ((label-result (labeling-func ch-suffixes)))
                 (cons (car label-result)
                       (suffixes->st labeling-func (cdr label-result) alphabet))))
             non-empty-ch-words))))


; nu uitați să convertiți alfabetul într-un flux
(define text->st
  (lambda (text)
    (lambda (labeling-func)
      (suffixes->st labeling-func 
        (get-suffixes (append text '(#\$)))
        (list->stream (sort (remove-duplicates (append text '(#\$))) char<?))
      )
    )
  )
)


(define (text->ast text)
  ((text->st text) ast-func)
)


(define (text->cst text)
  ((text->st text) cst-func))


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (substring? text pattern)
  (let* ((st (text->ast text))
         (result (st-has-pattern? st pattern))
        )
    (if result #t #f)
  )
)


; dacă ați respectat bariera de abstractizare,
; această funcție va rămâne nemodificată.
(define (repeated-substring-of-given-length text len)
  (let* ((tree (text->cst text)))
    (define (find-substring node accumulated)
      (if (collection-empty? node)
          #f  ; Dacă nodul este null, nu am găsit un subșir.
          (let* ((branch (first-branch node))
                 (label (get-branch-label branch))
                 (subtree (get-branch-subtree branch))
                 (combined-label (append accumulated label)))
            (if (and (>= (length combined-label) len) ; Dacă lungimea combinată este >= len și nodul are copii,
                     (not (collection-empty? subtree)))
                (take combined-label len) ; Returnăm primele 'len' elemente ca listă.
                (or (find-substring subtree combined-label) ; Altfel, continuăm în subarbore cu eticheta acumulată.
                    (find-substring (other-branches node) accumulated)))))) ; Sau încercăm următoarea ramură.
    (find-substring tree '())))  ; Începem cu arborele și o listă acumulată goală.
