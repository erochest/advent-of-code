max-line 16 * constant max-paragraph
create paragraph-buffer max-paragraph 2 + allot

: clear-paragraph-buffer ( -- ) paragraph-buffer max-paragraph blank ;
: is-end-of-paragraph? ( flag length -- flag ) 0= swap invert or ;
: line->paragraph ( paragraph-cursor length -- )
    line-buffer paragraph-buffer 2swap -rot + swap cmove ;
: cursor+ ( cursor0 length -- cursor1 ) + 1+ ;

: read-paragraph ( -- n )
    \ Reads one empty-line paragraph from fd-in. Return the length of data in paragraph-buffer.
    clear-paragraph-buffer
    0 begin ( cursor )
        read-input-line ( cursor length flag )
        over is-end-of-paragraph? invert while ( cursor length )
        2dup line->paragraph ( cursor length )
        cursor+ ( cursor1 )
    repeat ( cursor length )
    drop ( cursor )
    ;
