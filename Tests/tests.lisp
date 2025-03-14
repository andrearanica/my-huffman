;; 1. Test con simboli semplici
(defparameter sw-1 (list (cons 'a 8) (cons 'b 3) (cons 'c 1) 
                       (cons 'd 1) (cons '(e) 1) (cons 'f 1) 
                       (cons 'g 1) (cons 'h 1)))
(defparameter HT-1 (hucodec-generate-huffman-tree sw-1))
(defparameter MESSAGE-1 '(A B C A))

(equal MESSAGE-1 (hucodec-decode (hucodec-encode MESSAGE-1 ht-1) ht-1))

;; 2. Test usando liste come simboli

(defparameter sw-2 (list (cons '(a) 8) (cons '(b) 4)
                         (cons '(c) 3)))
(defparameter HT-2 (hucodec-generate-huffman-tree sw-2))
(defparameter MESSAGE-2 '((a) (b) (c) (a)))

(equal MESSAGE-2 (hucodec-decode (hucodec-encode MESSAGE-2 ht-2) ht-2))

;; 3. Test usando struct come simboli

(defstruct person
  name
  age)

(defparameter person-1 (make-person :name "Mario Rossi" :age 50))
(defparameter person-2 (make-person :name "Matteo Salvini" :age 52))
(defparameter person-3 (make-person :name "Ciao mondo" :age 10))
(defparameter sw-3 (list (cons person-1 50)
                    (cons person-2 1)
                    (cons person-3 2)))
(defparameter HT-3 (hucodec-generate-huffman-tree sw-3))
(defparameter MESSAGE-3 (list person-1 person-2 person-3))

(equal MESSAGE-3 (hucodec-decode (hucodec-encode MESSAGE-3 HT-3) HT-3))
(hucodec-generate-symbol-bits-table ht-3)

;; 4. Test tabella dei simboli con liste come simboli

(defparameter sw-4 (list (cons '(a b) 1) (cons '(f g) 2)))
(defparameter ht-4 (hucodec-generate-huffman-tree sw-4))

(hucodec-generate-symbol-bits-table ht-4)

;; 5. Test con empty tree
#|
(defparameter sw-5 (list))
(defparameter ht-5 (hucodec-generate-huffman-tree sw-5))
; (hucodec-encode '(a b c) ht-5) should raise Huffman tree empty
; (hucodec-decode '(0 0 1) ht-5) same
|#

;; 6. Test con simboli null

(defparameter sw-6 (list (cons nil 5) (cons 'a 7) (cons 'b 1)))
(defparameter ht-6 (hucodec-generate-huffman-tree sw-6))
(defparameter MESSAGE-6 (list nil 'a))
(equal MESSAGE-6 (hucodec-decode (hucodec-encode MESSAGE-6 ht-6) ht-6))

;; 7. Test con file

(defparameter sw-7 (list (cons #\a 8) (cons #\b 3) (cons #\c 1) 
                         (cons #\d 1) (cons #\e 1) (cons #\f 1) 
                         (cons #\g 1) (cons #\h 1) (cons #\i 1)
                         (cons #\j 1) (cons #\k 1) (cons #\l 1)
                         (cons #\m 1) (cons #\n 1) (cons #\o 1)
                         (cons #\p 1) (cons #\q 1) (cons #\r 1)
                         (cons #\s 1) (cons #\t 1) (cons #\u 1)
                         (cons #\v 1) (cons #\w 1) (cons #\x 1)
                         (cons #\y 1) (cons #\z 1) (cons #\NewLine 1)
                         (cons #\space 2) (cons #\tab 1)))

(defparameter file-path "/home/andre/Documenti/GitHub/my-huffman/Tests/file.txt")
(defparameter ht-7 (hucodec-generate-huffman-tree sw-7))
(defparameter encoded-message (hucodec-encode-file file-path ht-7))

;; 8. Test con messaggio lungo

(defparameter message-8 (list #\c #\i #\a #\o #\space #\m #\o #\n #\d #\o #\space #\q #\u #\e #\s #\t #\o #\space #\e #\space #\u #\n #\space #\t #\e #\s #\t #\space #\p #\e #\r #\space #\l #\i #\s #\p))

(equal message-8 (hucodec-decode (hucodec-encode message-8 ht-7) ht-7))

;; 9. Test con albero con un solo nodo

(defparameter sw-9 (list (cons #\a 8)))
(defparameter ht-9 (hucodec-generate-huffman-tree sw-9))
(equal (list #\a) (hucodec-decode (hucodec-encode (list #\a) ht-9) ht-9))
(defparameter table-9 (hucodec-generate-symbol-bits-table ht-9))

;; 10. Test con albero grande

(defparameter sw-10 '(
                      (A . 5)
                      (B . 3)
                      (C . 2)
                      (D . 5)
                      (E . 7)
                      (F . 10)
                      (G . 12)
                      (H . 23)
                      (I . 40)
                      (J . 42)
                      (K . 4)
                      (L . 6)
                      (M . 8)
                      (N . 9)
                      (O . 4)
                      (P . 1)
                      (Q . 4)
                      (R . 12)
                      (S . 15)
                      (T . 8)
                      (U . 23)
                      (V . 22)
                      (W . 36)
                      (X . 57)
                      (Y . 99)
                      (Z . 100)
                      (SPACE . 4)))

(defparameter ht-10 (hucodec-generate-huffman-tree sw-10))
(equal (list 'A 'SPACE 'B) (hucodec-decode (hucodec-encode (list 'A 'SPACE 'B) ht-10) ht-10))