Ranica  Andrea  909424

Il codice Lisp implementa una libreria per codificare e decodificare dati usando
gli alberi di Huffman.

La funzione per generare un albero di Huffman è hucodec-generate-huffman-tree/2,
che prende in input una lista di coppie simbolo-peso e restituisce la radice
dell'albero di Huffman generato.

La funzione per codificare un messaggio è hucodec-encode / 2, che richiede il 
messaggio da codificare (rappresentato da una lista di simboli) e la radice
dell'albero con cui effettuare la codifica; restituisce una lista di bit.
È possibile ottenere la codifica del contenuto di un file con il predicato 
hucodec-encode-file, che funziona in modo analogo. 
Importante: il contenuto del file è espresso come una lista di caratteri, per 
questo è necessario definire i simboli dell'albero come caratteri. Ad esempio,
possiamo definire le coppie come ((#\a 8) (#\b 2) ...)

La funzione per decodificare una lista di bit è hucodec-decode / 2, e restitui-
sce il messaggio decodificato a partire dalla lista di bit. Anche in questo caso
il messaggio è una lista di simboli.

Infine, è possibie stampare a schermo la struttura dell'albero con la funzione
hucodec-print-huffman-tree / 1 e generare la tabella dei simboli di un dato
albero con la funzione hucodec-generate-symbol-bits-table / 1.