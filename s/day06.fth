s" files.fth" included
s" debug.fth" included
s" stack.fth" included
s" bytes.fth" included
s" paragraphs.fth" included

27 0 2constant declaration-bounds

: is-space? ( c -- f ) 32 = ;

: set-declaration ( decl0 c -- decl1 ) [char] a - set-position ;

: set-declarations ( a u -- bytes )
    0 -rot bounds u+do
        i c@ dup is-space? if drop
        else set-declaration
        then
    loop
    ;

: count-declarations ( bytes -- u )
    { declarations }
    0 declaration-bounds u+do
        declarations i bit-set? if
            1+
        then
    loop  
    ;

: (day06a) ( a u -- n )
    open-input
    0 begin
        read-paragraph dup 0<> while
        paragraph-buffer swap
        set-declarations count-declarations
        +
    repeat
    drop
    close-input
    ;

: day06a ( -- ) s" d/day06.txt" (day06a) . cr ;

: (day06b) ( a u -- n )
    open-input
    0 -1 begin ( total group )
        read-input-line while ( total group-byes line-len )
        ?dup if line-buffer swap set-declarations and  \ when there is a line group
        else count-declarations + -1  \ group break
        then
    repeat 
    drop close-input
    count-declarations + \ process any last item
    ;

: day06b ( -- ) s" d/day06.txt" (day06b) . cr ;
