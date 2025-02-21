%%% -*- Mode: Prolog -*-

%%% symbols_n_weights / 1
%%% Predicato che definisce dei simboli con i relativi pesi da usare come tests

symbols_n_weights([
    sw(a, 8),
    sw(b, 3),
    sw(c, 1),
    sw(d, 1),
    sw(e, 1),
    sw(f, 1),
    sw(g, 1),
    sw(h, 1)
]).

%%% message / 1
%%% Predicato che definisce dei messaggi di prova da usare come test

message([a, b, c]).

%%% test

symbols_n_weights(SWs),
   message(M),
   hucodec_generate_huffman_tree(SWs, HT),
   hucodec_print_huffman_tree(HT),          % Stampa l'albero
   hucodec_encode(M, HT, Bits),             % Codifica M in Bits
   hucodec_decode(Bits, HT, Decoded),       % Decodifica Bits in Decoded
   writeln('Bits = ':Bits),
   writeln('Decoded = ':Decoded).