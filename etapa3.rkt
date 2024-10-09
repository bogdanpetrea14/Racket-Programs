#lang racket
(require "suffix-tree.rkt")
(require "etapa1.rkt")
(require "etapa2.rkt")
(require racket/trace)

(provide (all-defined-out))

;; Această etapă este dedicată aplicațiilor 
;; arborelui de sufixe:
;; - găsirea unui șablon într-un text
;; - cel mai lung subșir comun a două texte
;; - găsirea unui subșir de lungime dată care se
;;   repetă în text
;; Conform convenției din etapele anterioare, un text
;; este întotdeauna reprezentat ca listă de caractere.
;; Rezultatele funcțiilor de mai jos sunt de asemenea
;; reprezentate ca liste de caractere.


; TODO 1
; Implementați funcția substring? care primește un text și
; un șablon nevid și întoarce true dacă șablonul apare în 
; text, respectiv false în caz contrar.
; Observație: ați implementat deja logica principală a
; acestei căutări în etapa 1, în funcția st-has-pattern?,
; care este un operator al tipului ST. Acum aveți toate
; uneltele necesare implementării operatorului corespunzător
; pentru tipul text (pentru că în etapa 2 ați implementat
; construcția arborelui de sufixe asociat unui text).
(define (substring? text pattern)
  (let* ((st (text->cst text))
         (result (st-has-pattern? st pattern))
        )
    (if result #t #f)
  )
)


; TODO 2
; Implementați funcția longest-common-substring care primește
; două texte și determină cel mai lung subșir comun al
; acestora, folosind algoritmul următor:
; 1. Construiește arborele de sufixe ST1 pentru primul text.
; 2. Pentru fiecare sufix din al doilea text (de la cel mai
;    lung la cel mai scurt), găsește cea mai lungă potrivire 
;    cu sufixele din primul text, urmând căile relevante în ST1.
; 3. Rezultatul final este cel mai lung rezultat identificat
;    la pasul 2 (în caz de egalitate a lungimii, păstrăm
;    primul șir găsit).
; Folosiți named let pentru a parcurge sufixele.
; Observație: pentru sufixele din al doilea text nu dorim 
; marcajul de final $ pentru a nu crește artificial lungimea 
; șirului comun cu acest caracter.
; Hint: Revizitați funcția match-pattern-with-label (etapa 1).

(define (longest-common-substring text1 text2)
  (let* ((tree (text->ast text1))  ; Step 1: Construct suffix tree ST1 for text1.
         (suffixes (get-suffixes text2)))  ; Prepare all suffixes of text2, without the ending '$'.
    (define (process-match match suf)
      (cond
        ((eq? match #t) (length suf))  ; Full match, return its length.
        ((and (list? match) (not (eq? (car match) #f)))  ; Partial match, process further.
         (let ((new-pattern (cadr match))
               (subtree (caddr match)))
           (if (> (length new-pattern) 0)
               (+ (length (car match)) (process-match (match-pattern-with-label subtree new-pattern) new-pattern))
               (length (car match)))))
        (else 0)))  ; No match or mismatch.

    (let loop ((suffixes suffixes) (max-suf '()) (max-len 0))
      (if (null? suffixes)
          max-suf  ; Return the longest common substring found.
          (let* ((suf (car suffixes))
                 (match-result (match-pattern-with-label tree suf))
                 (match-len (process-match match-result suf)))
            (if (> match-len max-len)
                (loop (cdr suffixes) (take suf match-len) match-len)
                (loop (cdr suffixes) max-suf max-len)))))))







; TODO 3
; Implementați funcția repeated-substring-of-given-length
; care primește un text și un număr natural len și  
; parcurge arborele de sufixe al textului până găsește un
; subșir de lungime len care se repetă în text.
; Dacă acest subșir nu există, funcția întoarce false.
; Obs: din felul în care este construit arborele de sufixe
; (pe baza alfabetului sortat), rezultatul va fi primul 
; asemenea subșir din punct de vedere alfabetic.
; Ideea este următoarea: orice cale în arborele de sufixe
; compact care se termină cu un nod intern (un nod care 
; are copii, nu este o frunză) reprezintă un subșir care
; se repetă, pentru că orice asemenea cale reprezintă un
; prefix comun pentru două sau mai multe sufixe ale textului.
; Folosiți interfața definită în fișierul suffix-tree
; atunci când manipulați arborele.
(define (repeated-substring-of-given-length text len)
  (let* ((tree (text->cst text)))
    (define (find-substring node accumulated)
      (if (null? node)
          #f  ; Dacă nodul este null, nu am găsit un subșir.
          (let* ((branch (first-branch node))
                 (label (get-branch-label branch))
                 (subtree (get-branch-subtree branch))
                 (combined-label (append accumulated label)))
            (if (and (>= (length combined-label) len) ; Dacă lungimea combinată este >= len și nodul are copii,
                     (not (null? subtree)))
                (take combined-label len) ; Returnăm primele 'len' elemente ca listă.
                (or (find-substring subtree combined-label) ; Altfel, continuăm în subarbore cu eticheta acumulată.
                    (find-substring (other-branches node) accumulated)))))) ; Sau încercăm următoarea ramură.
    (find-substring tree '())))  ; Începem cu arborele și o listă acumulată goală.






