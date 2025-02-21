(defstruct node
  symbol
  weight
  left 
  right)

(defun create-node (symbol weight &optional (left nil) (right nil) )
  (make-node :symbol symbol :weight weight :left left :right right))

(defun sw-to-node (sw)
  (create-node (car sw) (cdr sw)))

(defun sw-list-to-nodes (sw-list)
  (cond ((null sw-list) nil)
        (t (cons (sw-to-node (car sw-list)) (sw-list-to-nodes (cdr sw-list))))))

(defun compare-nodes (first-node second-node)
  (< (node-weight first-node) (node-weight second-node)))

(defun sort-nodes (nodes)
  (stable-sort nodes 'compare-nodes))

(defun merge-two-nodes (first-node second-node)
  (create-node (cons (node-symbol first-node) (node-symbol second-node))
               (+ (node-weight first-node) (node-weight second-node))
               first-node
               second-node))

(defun copy-node (node)
  (create-node (node-symbol node)
               (node-weight node)
               (node-left node)
               (node-right node)))

(defun merge-nodes (nodes)
  (cond ((null nodes) nil)
        ((null (cdr nodes)) (copy-node (car nodes)))
        (t (let ((sorted-nodes (sort-nodes nodes)))
             (let ((merged (merge-two-nodes (car sorted-nodes) (cadr sorted-nodes))))
                   (merge-nodes (cons merged (cddr sorted-nodes))))))))


(defun hucodec-generate-huffman-tree (sw-list)
  (let ((nodes (sw-list-to-nodes sw-list)))
      (merge-nodes nodes)))

(defun print-node (node indentation)
  (format t "~aSymbol: ~a Weight: ~a~%" 
          indentation
          (node-symbol node)
          (node-weight node))
  (cond ((node-left node)
         (print-node (node-left node) (concatenate 'string indentation "-"))))
  (cond ((node-right node)
         (print-node (node-right node) (concatenate 'string indentation "-")))))

(defun hucodec-print-huffman-tree (tree)
  (print-node tree ""))

; (defparameter sw (list (cons 'a 8) (cons 'b 3) (cons 'c 1) (cons 'd 1) (cons 'e 1) (cons 'f 1) (cons 'g 1) (cons 'h 1)))

; (defparameter nodes (sw-list-to-nodes sw))

