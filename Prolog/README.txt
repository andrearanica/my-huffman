Ranica  Andrea  909424

Il codice prolog realizza una libreria per effettuare le operazioni di codifica
e decodifica di dati usando gli alberi di Huffman.

Il predicato per creare un albero di huffman è hucodec_generate_huffman_tree / 3
che prende in input una lista di simboli e pesi e restituisce la radice dell'
albero di Huffman costruito a partire dai simboli e pesi dati.
I simboli e i pesi vanno passati a questa funzione come una lista di predicati
sw(Symbol, Weight).

Il predicato hucodec_encode / 3 permette di codificare un messaggio, che è rap-
presentato come una lista di simboli. Questi simboli possono essere atomi oppure 
simboli più complessi, come liste. Il predicato è vero se il terzo parametro è 
una lista di bit che sono la codifica del messaggio nell'albero fornito.
Il predicato hucodec_encode_file / 3 funziona in modo analogo, ma accetta come
primo parametro una stringa che contiene il percorso al file da codificare.
Attenzione: il predicato considera il contenuto del file come caratteri, quindi
per codificarne il contenuto è necessario definire nelle coppie simbolo-peso i
caratteri contenuti all'interno del file, compresi i caratteri ' ' e '\n'.

È inoltre presente il predicato hucodec_decode che è vero se il terzo parametro
è il messaggio decodificato dei bit forniti nell'albero di Huffman passato come
secondo parametro. Anche in questo caso, il messaggio e i bit sono rappresentati
come liste di simboli.

Infine, è possibile mostrare a schermo un albero di Huffman attraverso il predi-
cato hucodec_print_huffman_tree / 1 e generare la tabella dei simboli di un 
albero con il predicato hucodec_generate_symbol_bits_table / 2.