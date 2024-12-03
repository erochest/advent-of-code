! Copyright (C) 2024 Eric Rochester.
! See https://factorcode.org/license.txt for BSD license.
USING: advent.io arrays assocs hashtables kernel math sequences
       sorting splitting ;
IN: advent.y2024.day01

: read-data ( path -- seqs ) (file-lines) [ split-numbers ] map ;

: sort-data ( seqs -- sorted-seqs )
    [ [ first ] map sort ] [ [ second ] map sort ] bi 2array ;

: compute-distances ( seq -- seq )
    [ first ] [ second ] bi
    [ - abs ] 2map ;

: find-site-distances ( seqs -- checksum )
    sort-data compute-distances sum ;

: (frequencies) ( prev elt -- next ) over inc-at ;
: frequencies ( seq -- freqs )
    H{ } clone [ (frequencies) ] reduce ;

: similarity ( node freq -- score ) * ;

: (frequencies-by-product)
    ( frequencies item -- frequences score )
    dup pick at [ 0 ] unless* 
    similarity ;

: frequencies-by-product ( frequencies seq -- seq )
    [ (frequencies-by-product) ] map nip ;

: find-similarity-scores ( seqs -- checksum )
    [ [ second ] map frequencies ] [ [ first ] map ] bi
    frequencies-by-product
    sum ;
