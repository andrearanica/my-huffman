;;; -*- Mode: Lisp -*-

;;; 909424 Ranica Andrea

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

;; Ritorna true se il nodo e' una foglia
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
  (cond ((null node) nil)
        (t (format t "~aSymbol: ~a Weight: ~a~%" 
                   indentation
                   (node-symbol node)
                   (node-weight node))
           (cond ((node-left node)
                  (print-node (node-left node) 
                              (concatenate 'string 
                                           indentation 
                                           "-"))))
           (cond ((node-right node)
                  (print-node (node-right node)
                              (concatenate 'string 
                                           indentation
                                           "-")))))))

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
  (cons first-node-symbol (cons second-node-symbol nil)))

;; Unisce i due nodi in un unico nodo che ha come simbolo i simboli dei
;; due nodi e come peso la somma dei pesi dei due nodi
(defun merge-two-nodes (first-node second-node)
  (let ((first-node-symbol (node-symbol first-node)))
    (let ((second-node-symbol (node-symbol second-node)))
      (let ((merged-node-symbol 
             (merge-two-nodes-symbols first-node-symbol
                                      second-node-symbol)))
        (create-node merged-node-symbol
                     (+ (node-weight first-node) (node-weight second-node))
                     first-node
                     second-node)))))

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
  (cond ((equal sw-list nil) (error "Symbols-n-weights list cannot be empty"))
        (t
         (let ((nodes (sw-list-to-nodes sw-list)))
           (merge-nodes nodes)))))

;; Stampa un albero di Huffman data la sua radice
(defun hucodec-print-huffman-tree (tree &optional (indent-level 0))
  (print-node tree (scale-char "-" indent-level)))

;; Ritorna una stringa formata da n volte un carattere
(defun scale-char (char n)
  (cond ((equal n 0) "")
        (t (concatenate 'string char (scale-char char (- n 1))))))

;; Ritorna se l'elemento e' presente nella lista (analogo a member
;; ma funziona anche per le stringhe)
(defun is-item-in-list (item list)
  (cond ((null list) nil)
        ((equal item (car list)) t)
        (t (is-item-in-list item (cdr list)))))

;; Ritorna quale sottoalbero considerare per codificare un carattere
(defun choose-encoding-branch (symbol huffman-tree)
  (let ((left-tree (node-left huffman-tree))
        (right-tree (node-right huffman-tree)))
    (let ((symbols-in-left-tree (get-symbols-from-huffman-tree left-tree))
          (symbols-in-right-tree (get-symbols-from-huffman-tree right-tree)))
      (cond ((is-item-in-list symbol symbols-in-left-tree) 0)
            ((is-item-in-list symbol symbols-in-right-tree) 1)
            (t (error "Symbol ~A is not defined in the given tree" 
                      symbol))))))

;; Restituisce la lista che contiene la codifica del carattere fornito 
;; nell'albero di Huffman
(defun get-symbol-encoding (symbol node root)
  (cond ((null node) (error "Huffman tree is empty"))
        ((and 
          (equal (node-symbol node) symbol)
          (leaf-p root))
         '(none))             ; This case handles ht with single node
        ((equal (node-symbol node) symbol) nil)
        (t
         (let ((new-branch (choose-encoding-branch symbol node)))
           (if (equal new-branch 1)
               (cons 1 (get-symbol-encoding symbol (node-right node) root))
             (cons 0 (get-symbol-encoding symbol (node-left node) root)))))))

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
                (get-symbol-encoding (car symbols) huffman-tree huffman-tree))
          (generate-symbol-bits-table (cdr symbols) huffman-tree))))

;; Ritorna una lista di bit che rappresentano la codifica del messaggio
(defun hucodec-encode (message huffman-tree)
  (cond ((null message) nil)
        ((not (listp message)) (error 
                                "hucodec-encode argument must be a list"))
        (t (append (get-symbol-encoding (car message) huffman-tree huffman-tree)
                   (hucodec-encode (cdr message) huffman-tree)))))

;; Ritorna una lista che contiene il contenuto dello stream passato
(defun read-list-from (input-stream)
  (let ((char (read-char input-stream nil 'eof)))
    (unless (eq char 'eof)
      (cons (if (char= char #\NewLine) #\NewLine char)
            (read-list-from input-stream)))))

;; Ritorna il contenuto di un file come lista di simboli
(defun get-file-content-list (filename)
  (with-open-file (in filename :direction :input)
    (read-list-from in)))

;; Ritorna la lista di bit che rappresentano la codifica del file
(defun hucodec-encode-file (filename huffman-tree)
  (cond ((not (probe-file filename)) (error "File '~A' doesn't exist" filename))
        (t (let ((file-content-list (get-file-content-list filename)))
             (hucodec-encode file-content-list huffman-tree)))))

;; Ritorna una lista che rappresenta la stringa codificata dai bit passati
(defun hucodec-decode (bits huffman-tree)
  (cond ((null huffman-tree) (error "Huffman tree is empty"))
        ((not (listp bits)) (error "hucodec-decode argument must be a list"))
        ((and (equal bits (list))
              (leaf-p huffman-tree)) 
         (bits-to-symbols '(none) huffman-tree huffman-tree))
        ((equal bits (list)) nil)
        (t (bits-to-symbols bits huffman-tree huffman-tree))))

;; Converte la lista di bit nei simboli corrispondenti, ritornando 
;; ricorsivamente alla radice quando finisce in una foglia
(defun bits-to-symbols (bits node root)
  (cond ((and (leaf-p node) (null bits) (not (equal node root)))
         (cons (node-symbol node) nil))
        ((and (leaf-p node) (not (equal node root))) 
         (cons (node-symbol node) (bits-to-symbols bits root root)))
        ((and (null bits) (equal node root)) nil)
        ((and (null bits) (not (equal node root))) 
         (error "Given encoding is not complete"))
        (t 
         (let ((next-branch (choose-branch (car bits) node)))
           (if (equal node next-branch) 
                 (cons (node-symbol node)
                       (bits-to-symbols (cdr bits) next-branch root))
             (bits-to-symbols (cdr bits) next-branch root))))))

;; Ritorna quale figlio del nodo seguire per continuare la decodifica
(defun choose-branch (bit node)
  (cond ((and (equal 'none bit) (leaf-p node)) node)
        ((equal 0 bit) (node-left node))
        ((equal 1 bit) (node-right node))
        (t (error "The given bits is not a valid encoding in current tree"))))
