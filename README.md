# My Huffman

This repository contains an implementation of the Huffman encoding and decoding algorithm in Prolog and Lisp, for the Programming Languages course at University of Milano-Bicocca.  
The goal of the two libraries is to create a Huffman Tree, using a list of couples symbol-weight. The library can encode both messages, represented with lists of symbols, and files.

## Prolog

The predicates that can be used to perform operations are the following.
- hucodec_decode/3 Bits HuffmanTree Message
- hucodec_encode/3 Message HuffmanTree Bits
- hucodec_encode_file/3 Filename HuffmanTree Bits
- hucodec_generate_huffman_tree/2 SymbolsAndWeights HuffmanTree
- hucodec_generate_symbol_bits_table/2 HuffmanTree SymbolBitsTable
- hucodec_print_huffman_tree/1 HuffmanTree

## Lisp

The functions that can be used to perform operations are the following.
- hucodec-decode bits huffman-tree -> message
- hucodec-encode message huffman-tree -> bits
- hucodec-encode-file filename huffman-tree -> bits
- hucodec-generate-huffman-tree symbols-n-weights -> huffman-tree
- hucodec-generate-symbol-bits-table huffman-tree -> symbol-bits-table
- hucodec-print-huffman-tree huffman-tree &optional (indent-level 0) -> NIL
