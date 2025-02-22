;;; Ranica Andrea 909424

;;; -*- Mode: Lisp -*-

;; Struct che rappresenta il nodo dell'albero di Huffman
(defstruct node
  symbol
  weight
  left 
  right)

;; Costruttore per la struct node
(defun create-node (symbol weight &optional (left nil) (right nil))
  (make-node :symbol symbol :weight weight :left left :right right))

;; Crea una copia dei due nodi
(defun copy-node (node)
  (create-node (node-symbol node)
               (node-weight node)
               (node-left node)
               (node-right node)))

;; Ritorna true se il nodo è una foglia
(defun leaf-p (node)
  (and (null (node-left node))
       (null (node-right node))))

;; Restituisce T se un il primo nodo ha un peso inferiore del secondo
(defun compare-nodes (first-node second-node)
  (< (node-weight first-node) (node-weight second-node)))

;; Ordina i nodi per peso in modo crescente
(defun sort-nodes (nodes)
  (stable-sort nodes 'compare-nodes))

;; Stampa ricorsivamente il nodo dato
(defun print-node (node indentation)
  (format t "~aSymbol: ~a Weight: ~a~%" 
          indentation
          (node-symbol node)
          (node-weight node))
  (cond ((node-left node)
         (print-node (node-left node) 
                     (concatenate 'string indentation "-"))))
  (cond ((node-right node)
         (print-node (node-right node)
                     (concatenate 'string indentation "-")))))

;; Converte una cons simbolo-peso in un nodo foglia
(defun sw-to-node (sw)
  (create-node (car sw) (cdr sw)))

;; Converte una lista di cons simbolo-peso in nodi foglie
(defun sw-list-to-nodes (sw-list)
  (cond ((null sw-list) nil)
        (t (cons (sw-to-node (car sw-list)) 
                 (sw-list-to-nodes (cdr sw-list))))))

;; Restituisce il simbolo del nodo ottenuto combinando i simboli di due nodi
(defun merge-two-nodes-symbols (first-node-symbol second-node-symbol)
  (cond ((listp first-node-symbol) 
         (cond 
          ((listp second-node-symbol) 
           (append first-node-symbol 
                   second-node-symbol))
          (t (append first-node-symbol
                     (list second-node-symbol)))))
        (t (cond ((listp second-node-symbol)
                  (append (list first-node-symbol) second-node-symbol))
                 (t (append (list first-node-symbol) 
                            (list second-node-symbol)))))))

;; Unisce i due nodi in un unico nodo che ha come simbolo i simboli dei
;; due nodi e come peso la somma dei pesi dei due nodi
(defun merge-two-nodes (first-node second-node)
  (let ((first-node-symbol (node-symbol first-node)))
    (let ((second-node-symbol (node-symbol second-node)))
      (create-node (merge-two-nodes-symbols first-node-symbol 
                                            second-node-symbol)
                   (+ (node-weight first-node) (node-weight second-node))
                   first-node
                   second-node))))

;; Unisce ricorsivamente la lista di nodi fornita e restituisce un unico
;; nodo radice
(defun merge-nodes (nodes)
  (cond ((null nodes) nil)
        ((null (cdr nodes)) (copy-node (car nodes)))
        (t (let ((sorted-nodes (sort-nodes nodes)))
             (let ((merged (merge-two-nodes 
                            (car sorted-nodes)
                            (cadr sorted-nodes))))
                   (merge-nodes (cons merged (cddr sorted-nodes))))))))

;; Genera l'albero di Huffman per la lista di cons simbolo-peso fornita
(defun hucodec-generate-huffman-tree (sw-list)
  (let ((nodes (sw-list-to-nodes sw-list)))
      (merge-nodes nodes)))

;; Stampa un albero di Huffman data la sua radice
(defun hucodec-print-huffman-tree (tree)
  (print-node tree ""))

;; Ritorna se l'elemento ï¿½ presente nella lista (analogo a member
;; ma funziona anche per le stringhe)
(defun is-item-in-list (item list)
  (cond ((null list) nil)
        ((equal item (car list)) t)
        (t (is-item-in-list item (cdr list)))))

;; Ritorna quale sottoalbero considerare per codificare un carattere
(defun choose-encoding-branch (symbol huffman-tree)
  (let ((left-tree (node-left huffman-tree))
        (right-tree (node-right huffman-tree)))
    (let ((left-tree-symbol-list 
           (if (listp (node-symbol left-tree)) 
               (node-symbol left-tree)
             (list (node-symbol left-tree))))
          (right-tree-symbol-list
           (if (listp (node-symbol right-tree))
               (node-symbol right-tree)
             (list (node-symbol right-tree)))))
      (cond ((and left-tree 
                  (is-item-in-list symbol left-tree-symbol-list))
             0)
            ((and right-tree 
                  (is-item-in-list symbol right-tree-symbol-list)) 1)
            (t (error (format t "Error: symbol ~A is not defined" symbol)))))))

;; Restituisce la lista che contiene la codifica del carattere fornito 
;; nell'albero di Huffman
(defun get-symbol-encoding (symbol huffman-tree)
  (cond ((null huffman-tree) nil)
        ((equal (node-symbol huffman-tree) symbol) nil)
        (t
         (let ((new-branch (choose-encoding-branch symbol huffman-tree)))
           (if (= new-branch 1)
               (cons 1 (get-symbol-encoding symbol (node-right huffman-tree)))
             (cons 0 (get-symbol-encoding symbol (node-left huffman-tree))))))))

;; Ritorna tutti i simboli presenti nell'albero di Huffman dato
(defun get-symbols-from-huffman-tree (huffman-tree)
  (cond ((null huffman-tree) nil)
        ((and (null (node-left huffman-tree)) (null (node-right huffman-tree)))
         (cons (node-symbol huffman-tree) nil))
        (t (append (get-symbols-from-huffman-tree (node-left huffman-tree))
                   (get-symbols-from-huffman-tree (node-right huffman-tree))))))

;; Ritorna una lista di coppie simbolo-codifica di un albero di Huffman
(defun hucodec-generate-symbol-bits-table (huffman-tree)
  (let ((symbols (get-symbols-from-huffman-tree huffman-tree)))
    (generate-symbol-bits-table symbols huffman-tree)))

;; Ritorna una lista di coppie simbolo-codifica per ogni simbolo passato
(defun generate-symbol-bits-table (symbols huffman-tree)
  (if (null symbols)
      nil
    (cons (list (car symbols)
                (get-symbol-encoding (car symbols) huffman-tree))
          (generate-symbol-bits-table (cdr symbols) huffman-tree))))

;; Ritorna una lista di bit che rappresentano la codifica del messaggio
(defun hucodec-encode (message huffman-tree)
  (cond ((null message) nil)
        (t (append (get-symbol-encoding (car message) huffman-tree)
                   (hucodec-encode (cdr message) huffman-tree)))))

;; Ritorna una lista che contiene il contenuto dello stream passato
(defun read-list-from (input-stream)
  (let ((char (read-char input-stream nil 'eof)))
    (unless (eq char 'eof)
      (cons (if (char= char #\NewLine) "\\n" (string char))
            (read-list-from input-stream)))))

;; Ritorna il contenuto di un file come lista di simboli
(defun get-file-content-list (filename)
  (with-open-file (in filename :direction :input)
    (read-list-from in)))

;; Ritorna la lista di bit che rappresentano la codifica del file
(defun hucodec-encode-file (filename huffman-tree)
  (let ((file-content-list (get-file-content-list filename)))
    (hucodec-encode file-content-list huffman-tree)))

;; Ritorna una lista che rappresenta la stringa codificata dai bit passati
(defun hucodec-decode (bits huffman-tree)
  (bits-to-symbols bits huffman-tree huffman-tree))

;; Converte la lista di bit nei simboli corrispondenti, ritornando ricorsivamente
;; alla radice quando finisce in una foglia
(defun bits-to-symbols (bits node root)
  (cond ((and (leaf-p node) (null bits))
         (cons (node-symbol node) nil))
        ((leaf-p node) 
         (cons (node-symbol node) (bits-to-symbols bits root root)))
        ((null bits) (error "Given encoding is not complete"))
        (t 
         (let ((next-branch (choose-branch (car bits) node)))
           (bits-to-symbols (cdr bits) next-branch root)))))

;; Ritorna quale figlio del nodo seguire per continuare la decodifica
(defun choose-branch (bit node)
  (cond ((= 0 bit) (node-left node))
        ((= 1 bit) (node-right node))
        (t (error "Bit not valid in given encoding"))))

(defparameter sw (list (cons 'a 8) (cons 'b 3) (cons 'c 1) (cons 'd 1) (cons 'e 1) (cons 'f 1) (cons 'g 1) (cons 'h 1)))
; (defparameter sw (list (cons "a" 8) (cons "b" 3) (cons "c" 1) (cons "d" 1) (cons "e" 1) (cons "f" 1) (cons "g" 1) (cons "\\n" 1)))
(defparameter HT (hucodec-generate-huffman-tree sw))
(defparameter MESSAGE '(A B C A))

