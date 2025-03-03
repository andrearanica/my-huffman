;;; Test per la funzione hucodec-encode-file

;; Funzione per leggere un file ricorsivamente
(defun read-file-chars (stream result)
  "Legge un file carattere per carattere in modo ricorsivo."
  (let ((char (read-char stream nil nil)))
    (if char
        (read-file-chars stream (cons char result))
        (reverse result))))

;; Funzione ausiliaria per contare le frequenze dei caratteri (ricorsiva)
(defun count-char-frequencies (chars result)
  "Conta le frequenze dei caratteri in una lista di caratteri."
  (if (null chars)
      result
      (let ((entry (assoc (first chars) result)))
        (if entry
            (count-char-frequencies (rest chars)
                                   (cons (cons (car entry) (1+ (cdr entry)))
                                         (remove entry result)))
            (count-char-frequencies (rest chars)
                                   (cons (cons (first chars) 1) result))))))

;; Funzione per stampare le frequenze (ricorsiva)
(defun print-frequencies (freq-list)
  "Stampa ricorsivamente una lista di frequenze."
  (if (null freq-list)
      nil
      (progn
        (format t "  ~S: ~A~%" (caar freq-list) (cdar freq-list))
        (print-frequencies (cdr freq-list)))))

;; Funzione che esegue il test di hucodec-encode-file
(defun test-hucodec-encode-file (filename)
  "Testa la funzione hucodec-encode-file su un file specifico."
  (format t "~%TEST: hucodec-encode-file~%")
  (format t "File: ~A~%" filename)
  
  ;; Verifica che il file esista
  (if (probe-file filename)
      (progn
        ;; Leggi il contenuto del file per un confronto
        (with-open-file (stream filename :direction :input)
          (let ((file-content (read-file-chars stream '())))
            
            ;; Crea un albero di Huffman basato sulle frequenze dei caratteri
            (let ((char-frequencies (count-char-frequencies file-content '())))
              (format t "Frequenze dei caratteri nel file:~%")
              (print-frequencies char-frequencies)
              
              ;; Genera l'albero di Huffman
              (let ((huffman-tree (hucodec-generate-huffman-tree char-frequencies)))
                (format t "Albero di Huffman generato per il file~%")
                
                ;; Codifica il file
                (let ((encoded-bits (hucodec-encode-file filename huffman-tree)))
                  (format t "File codificato: ~A bit~%" (length encoded-bits))
                  
                  ;; Confronta con la codifica diretta del contenuto
                  (let ((direct-encoded (hucodec-encode file-content huffman-tree)))
                    (format t "Contenuto codificato direttamente: ~A bit~%" (length direct-encoded))
                    (format t "Corrispondenza tra le due codifiche: ~A~%" (equal encoded-bits direct-encoded))
                    
                    ;; Decodifica per verificare la correttezza
                    (let ((decoded (hucodec-decode encoded-bits huffman-tree)))
                      (format t "Decodifica corretta: ~A~%" (equal decoded file-content))
                      
                      ;; Calcola il tasso di compressione
                      (let* ((original-size (* (length file-content) 8)) ; Assumendo 8 bit per carattere ASCII
                             (compressed-size (length encoded-bits))
                             (compression-ratio (/ compressed-size original-size))
                             (savings-percent (* 100 (- 1 compression-ratio))))
                        (format t "Dimensione originale (8 bit/char): ~A bit~%" original-size)
                        (format t "Dimensione compressa: ~A bit~%" compressed-size)
                        (format t "Rapporto di compressione: ~,2F~%" compression-ratio)
                        (format t "Risparmio: ~,2F%~%" savings-percent))))))))))
      (format t "ERRORE: Il file ~A non esiste.~%" filename)))

;; Esegui il test con un file di esempio (assicurati che esista questo file)
(test-hucodec-encode-file "/home/andre/Documenti/GitHub/my-huffman/Test/file.txt")
