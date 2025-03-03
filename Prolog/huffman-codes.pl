%%%% -*- Mode: Prolog -*-

%%% 909424 Ranica Andrea

%%% hucodec_generate_huffman_tree / 2
%%% Predicato che genera un albero di Huffman prendendo in input una lista di
%%% coppie simbolo-peso

hucodec_generate_huffman_tree(SymbolsNWeights, HuffmanTree) :-
    SymbolsNWeights \= [],
    sw_to_nodes(SymbolsNWeights, Nodes),
    merge_sort(Nodes, SortedNodes),
    merge_nodes(SortedNodes, HuffmanTree).

%%% hucodec_print_huffman_tree / 1
%%% Predicato che stampa a schermo un albero di Huffman passato come parametro

hucodec_print_huffman_tree(HuffmanTree) :-
    print_node(HuffmanTree, '').

%%% sw_to_nodes / 2
%%% Predicato che converte una lista di coppie in un insieme di nodi foglie, per
%%% poter generare l\'albero corrispondente 

sw_to_nodes([], []) :- !.
sw_to_nodes([sw(S, W) | Xs], [node(S, W, void, void) | Zs]) :-
    sw_to_nodes(Xs, Zs).

%%% merge_nodes / 2
%%% Predicato che prende una lista di nodi e li unisce in una lista con un solo
%%% nodo, che conterrà come simbolo una lista con tutti i simboli e come peso la
%%% somma dei pesi dei nodi

merge_nodes([], void) :- !.
merge_nodes([X], X) :- !.
merge_nodes([FirstNode,SecondNode | RemainingNodes], ListWithMergedNodes) :- 
    merge_two_nodes(FirstNode, SecondNode, MergedNode), 
    ListWithTwoMergedNodes = [MergedNode | RemainingNodes],
    merge_sort(ListWithTwoMergedNodes, SortedListWithTwoMergedNodes),
    merge_nodes(SortedListWithTwoMergedNodes, ListWithMergedNodes),
    !.

%%% min_weight_in_sw_list / 2
%%% Predicato che ritorna 2 nodi con peso minimo nella lista fornita

min_weight_in_nodes([node(_, Weight, _, _)], Weight) :- !.
min_weight_in_nodes([FirstNode | OtherNodes], MinWeight) :-
    FirstNode = node(_, Weight, _, _),
    !,
    min_weight_in_nodes(OtherNodes, OtherNodesMinWeight),
    min(Weight, OtherNodesMinWeight, MinWeight).

%%% min / 3
%%% Predicato che restituisce il minimo tra due atomi (interi)

min(X, Y, X) :-
    X =< Y, !.
min(X, Y, Y) :- 
    X > Y, !.

%%% merge_nodes / 3
%%% Predicato che prende due nodi e ne crea uno solo che abbia i due nodi come
%%% figli sinistri e destri

merge_two_nodes(FirstNode, SecondNode, MergedNode) :-
    FirstNode = node(_, FirstNodeWeight, _, _),
    SecondNode = node(_, SecondNodeWeight, _, _),
    MergedNodeWeight is FirstNodeWeight + SecondNodeWeight,
    merge_nodes_names(FirstNode, SecondNode, MergedNodeName),
    MergedNode = node(MergedNodeName, MergedNodeWeight, FirstNode, SecondNode).

%%% merged_node_name / 3
%%% Predicato che prende due simboli e li unisce in una lista che li contiene
%%% entrambi, sia che siano atomi che liste

merge_nodes_names(FirstNode, SecondNode, MergedNodeName) :-
    FirstNode = node(FirstNodeName, _, void, void),
    SecondNode = node(SecondNodeName, _, void, void),
    append([FirstNodeName], [SecondNodeName], MergedNodeName).

merge_nodes_names(FirstNode, SecondNode, MergedNodeName) :-
    FirstNode = node(FirstNodeName, _, void, void),
    SecondNode = node(SecondNodeName, _, _, _),
    append([FirstNodeName], SecondNodeName, MergedNodeName).

merge_nodes_names(FirstNode, SecondNode, MergedNodeName) :-
    FirstNode = node(FirstNodeName, _, _, _),
    SecondNode = node(SecondNodeName, _, void, void),
    append(FirstNodeName, [SecondNodeName], MergedNodeName).

merge_nodes_names(FirstNode, SecondNode, MergedNodeName) :-
    FirstNode = node(FirstNodeName, _, _, _),
    SecondNode = node(SecondNodeName, _, _, _),
    MergedNodeName = [FirstNodeName, SecondNodeName].

%%% node / 4
%%% Predicato che rappresenta un nodo con simbolo, peso, sottoalbero sinistro e
%%% sottoalbero destro

node(_, _, void, void).

%%% is_leaf / 1

is_leaf(Node) :-
    Node = node(_, _, void, void).

%%% print_node / 2
%%% Predicato ausiliario per stampare un singolo nodo con una precisa
%%% indentazione

print_node(void, _) :- !.
print_node(node(X, K, void, void), Indentation) :-
    !,
    write(Indentation),
    write(X),
    write(':'),
    write(K),
    nl.
print_node(node(X, K, XLeft, XRight), Indentation) :-
    write(Indentation), write(X), write(':'), write(K), nl,
    atom_concat(Indentation, '-', NewIndentation),
    print_node(XLeft, NewIndentation),
    print_node(XRight, NewIndentation).

%%% sb / 2
%%% Predicato che associa un simbolo alla sua codifica

sb(_, _).

%%% hucodec_generate_symbol_bits_table / 2
%%% Predicato che genera la tabella dei simboli per l'albero fornito

hucodec_generate_symbol_bits_table(Tree, SymbolBitsTable) :-
    tree_to_symbols(Tree, Symbols),
    symbols_to_symbols_table(Symbols, Tree, SymbolBitsTable).

%%% tree_to_symbols / 2
%%% Predicato che restituisce la lista di simboli da cui è composto un albero

tree_to_symbols(void, []) :- !.
tree_to_symbols(node(Symbol, _, void, void), [Symbol]) :- !.
tree_to_symbols(node(_, _, Left, Right), Symbols) :-
    !,
    tree_to_symbols(Left, LeftSymbols),
    tree_to_symbols(Right, RightSymbols),
    append(LeftSymbols, RightSymbols, Symbols).

%%% symbols_to_symbols_table / 3
%%% Predicato che converte una lista di simboli in istanze di sb nell'albero

symbols_to_symbols_table([], _, []).
symbols_to_symbols_table([S], Tree, [sb(S, Bits)]) :-
    !,
    symbol_to_bits_in_tree(S, Tree, Bits, Tree).
symbols_to_symbols_table([S | Ss], Tree, [sb(S, Bits) | Sbs]) :-
    symbol_to_bits_in_tree(S, Tree, Bits, Tree),
    symbols_to_symbols_table(Ss, Tree, Sbs).

%%% symbol_to_bits_in_tree / 3
%%% Predicato che trova la codifica binaria di un simbolo percorrendo un albero

symbol_to_bits_in_tree(Symbol, Node, [none], Node) :-
    Node = node(Symbol, _, void, void),
    !.
symbol_to_bits_in_tree(Symbol, Node, [], Root) :-
    Node \= Root,
    Node = node(Symbol, _, void, void),
    !.
symbol_to_bits_in_tree(Symbol, Node, Encoding, Root) :-
    Node = node(_, _, Left, _),
    symbol_to_bits_in_tree(Symbol, Left, PartialEncoding, Root),
    append([0], PartialEncoding, Encoding),
    !.
symbol_to_bits_in_tree(Symbol, Node, Encoding, Root) :-
    Node = node(_, _, _, Right),
    symbol_to_bits_in_tree(Symbol, Right, PartialEncoding, Root),
    append([1], PartialEncoding, Encoding),
    !.

%%% hucodec_encode / 3
%%% Predicato che genera la codifica di un messaggio seguendo un albero

hucodec_encode(MessageList, Tree, Bits) :-
    is_list(MessageList),
    encode_list(MessageList, Tree, Bits).

%%% encode_list / 3
%%% Predicato che restituisce le codifiche degli elementi della lista fornita

encode_list([], _, []) :- !.
encode_list([Symbol | Symbols], Tree, EncodedList) :-
    symbol_to_bits_in_tree(Symbol, Tree, FirstSymbleBits, Tree),
    encode_list(Symbols, Tree, OtherSymblesBits),
    append(FirstSymbleBits, OtherSymblesBits, EncodedList).

%%% hucodec_encode_file / 3
%%% Predicato che codifica un file seguendo un albero

hucodec_encode_file(FileName, Tree, EncodedFile) :-
    exists_file(FileName),
    file_content(FileName, FileContent),
    hucodec_encode(FileContent, Tree, EncodedFile).

%%% file_content / 2
%%% Predicato che restituisce il contenuto del file passato come primo parametro

file_content(FileName, ListFileContent) :-
    open(FileName, read, Stream),
    read_stream(Stream, FileContent),
    atom_chars(FileContent, ListFileContent),
    close(Stream).

%%% read_file / 2
%%% Predicato che legge un file e ne restituisce il contenuto

read_stream(Stream, Content) :-
    \+ at_end_of_stream(Stream),
    !,
    get_char(Stream, Char),
    read_stream(Stream, OtherChars),
    atom_concat(Char, OtherChars, Content).
read_stream(Stream, '') :- 
    at_end_of_stream(Stream).

%%% hucodec_decode / 3
%%% Predicato che decodifica una lista di bit nei corrispondenti simboli all'
%%% interno dell'albero fornito

hucodec_decode(Bits, Tree, Symbols) :-
    bits_to_symbols(Bits, Tree, Symbols, Tree).

%%% bits_to_symbols / 4
%%% Predicato che converte una lista di bit nei corrispondenti simboli, avendo
%%% sia il nodo corrente in cui cercare sia la radice dell'intero albero

bits_to_symbols([], Node, [Symbol], Root) :-
    Node \= Root,
    !,
    Node = node(Symbol, _, void, void).
bits_to_symbols([], Node, [], Root) :-
    Node = Root,
    !.
bits_to_symbols([none | Bits], Node, Symbols, Node) :-
    !,
    Node = node(Symbol, _, _, _),
    bits_to_symbols(Bits, Node, OtherSymbols, Node),
    append([Symbol], OtherSymbols, Symbols).
bits_to_symbols(Bits, Node, Symbols, Root) :-
    Node = node(Symbol, _, void, void),
    !,
    bits_to_symbols(Bits, Root, OtherSymbols, Root),
    append([Symbol], OtherSymbols, Symbols).
bits_to_symbols([0 | Bits], Node, Symbols, Root) :-
    Node = node(_, _, Left, _),
    !,
    bits_to_symbols(Bits, Left, Symbols, Root).
bits_to_symbols([1 | Bits], Node, Symbols, Root) :-
    Node = node(_, _, _, Right),
    !,
    bits_to_symbols(Bits, Right, Symbols, Root).

%%% merge_sort / 2
%%% true if the second list is the first one sorted

merge_sort([], []) :- !.
merge_sort([X], [X]) :- !.
merge_sort(List, Sorted) :-
    divide(List, L1, L2),
    merge_sort(L1, Sorted1),
    merge_sort(L2, Sorted2),
    !,
    merge(Sorted1, Sorted2, Sorted).

% divide / 3
% true if the first list is the first one splitted in two lists

divide([], [], []).
divide([X], [X], []).
divide([X, Y | Rest], [X | L1], [Y | L2]) :-
    divide(Rest, L1, L2).

% merge / 3
% true if the third list is the sorted merge of the first two lists

merge([], L, L).
merge(L, [], L).
merge([node(X, N, XLeft, XRight) | T1], [node(Y, M, YLeft, YRight) | T2],
      [node(X, N, XLeft, XRight) | T]) :-
    N =< M,
    merge(T1, [node(Y, M, YLeft, YRight) | T2], T),
    !.
merge([node(X, N, XLeft, XRight) | T1], [node(Y, M, YLeft, YRight) | T2],
      [node(Y, M, YLeft, YRight) | T]) :-
    N > M,
    merge([node(X, N, XLeft, XRight) | T1], T2, T),
    !.

%%% 1. Test con simboli semplici

symbols_n_weights_1([
    sw(a, 8),
    sw(b, 3),
    sw(c, 1),
    sw(d, 1),
    sw(e, 1),
    sw(f, 1),
    sw(g, 1),
    sw(h, 1)
]).

message_1([a, b, c]).

:- symbols_n_weights_1(X),
    message_1(M),
    hucodec_generate_huffman_tree(X, Tree),
    hucodec_encode(M, Tree, EncodedMessage),
    hucodec_decode(EncodedMessage, Tree, M).

%%% 2. Test con liste come simboli

symbols_n_weights_2([
    sw([a], 8),
    sw([b], 3),
    sw([], 1)
]).

message_2([[a], [b], [], [a]]).

:- symbols_n_weights_2(X),
    message_2(M),
    hucodec_generate_huffman_tree(X, Tree),
    hucodec_encode(M, Tree, EncodedMessage),
    hucodec_decode(EncodedMessage, Tree, M).

%%% 3. Test con albero vuoto (non va bene, deve ritornare false)

/*symbols_n_weights_3([]).
:- symbols_n_weights_3(X),
    hucodec_generate_huffman_tree(X, _).*/

%%% 4. Test con file

symbols_n_weights_4([
    sw('a', 8),
    sw(b, 3),
    sw(c, 1),
    sw('\n', 1),
    sw(' ', 1)
]).

:- symbols_n_weights_4(X),
    hucodec_generate_huffman_tree(X, Tree),
    hucodec_encode_file('../Tests/file.txt', Tree, _).

%%% 5. Test con albero con un solo nodo

symbols_n_weights_5([
    sw([a], 8)
]).

message_5([[a], [a]]).

:- symbols_n_weights_5(X),
    hucodec_generate_huffman_tree(X, Tree),
    message_5(Message),
    hucodec_encode(Message, Tree, EncodedMessage),
    hucodec_decode(EncodedMessage, Tree, Message).