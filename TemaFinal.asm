    ;functie introducere caracter 
    putc    macro   char
            push    ax
            mov     al, char
            mov     ah, 0eh
            int     10h     
            pop     ax
    putc    endm
    
    org 100h
    ;setare mod video
    ; curata ecran 
    mov ax,0600h     
    mov bh,07       
    mov cx,0000      
    mov dx,184fh     
    int 10h           
    mov ah,00        
    mov al,13h       
    int 10h
              
    ;Program desenat casa 
    ;linie tavan
    mov cx,130           ;pozitionare pe coloana 
    mov dx,75            ;pozitionare pe rand
    Tavan: 
        mov ah,0ch       ;desenare linie
        mov al,07h       ;culoare linie 
        int 10h          
        inc cx           ;incrementare pozitie linie 
        cmp cx,216       ;limita linie
    jnz Tavan 
    
    ;linie podea
    mov cx,130           ;pozitionare pe coloana
    mov dx,125           ;pozitionare pe rand 
    Podea:
        mov ah,0ch       ;desenare linie
        mov al,07h       ;culoare linie  
        int 10h         
        inc cx           ;incrementare pozitie linie  
        cmp cx,216       ;limita linie
    jnz Podea  
    
    ;perete stangg
    mov cx,130           ;pozitionare pe coloana
    mov dx,75            ;pozitionare pe rand
    PereteSt:               
        mov ah,0ch       ;desenare linie
        mov al,07h       ;culoare linie 
        int 10h          
        inc dx           ;incrementare pozitie linie
        cmp dx,125       ;limita linie
    jnz PereteSt 
    
    ;perete drept        
    mov cx,216           ;pozitionare pe coloana
    mov dx,75            ;pozitionare pe rand
    PereteDr: 
        mov ah,0ch       ;desenare linie
        mov al,07h       ;culoare linie 
        int 10h         
        inc dx           ;incrementare pozitie linie
        cmp dx,126       ;limita linie
    jnz PereteDr 
    
    ;acoperis stang
    mov cx,130           ;pozitionare pe coloana
    mov dx,75            ;pozitionare pe rand
    AcoperisSt: 
        mov ah,0ch       ;desenare linie
        mov al,0ch       ;culoare linie 
        int 10h          
        inc cx           ;incrementare pozitie linie
        dec dx           ;pe diagonala
        cmp cx,173       ;limita linie   
        cmp dx,32        ;pe diagonala
    jnz AcoperisSt 
    
    ;acoperis drept
    mov cx,173           ;pozitionare pe coloana
    mov dx,32            ;pozitionare pe rand
    AcoperisDr: 
        mov ah,0ch       ;desenare linie
        mov al,0ch       ;culoare linie 
        int 10h         
        inc cx           ;incrementare pozitie linie
        inc dx           ;pe diagonala
        cmp cx,216       ;limita linie   
        cmp dx,75        ;pe diagonala
    jnz AcoperisDr
    
    ;Desenare Usa
    ;linie stanga
    mov cx,164           ;pozitionare pe coloana
    mov dx,125           ;pozitionare pe rand
    UsaSt: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie 
        int 10h         
        dec dx           ;incrementare pozitie linie
        cmp dx,100       ;limita linie
    jnz UsaSt 
    
    ;linie dreapta
    mov cx,182           ;pozitionare pe coloana
    mov dx,125           ;pozitionare pe rand
    UsaDr: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie 
        int 10h          
        dec dx           ;incrementare pozitie linie
        cmp dx,100       ;limita linie
    jnz UsaDr 
    
    ;linie sus
    mov cx,164           ;pozitionare pe coloana
    mov dx,100           ;pozitionare pe rand    
    UsaS:
         mov ah,0ch      ;desenare linie
         mov al,06h      ;culoare linie 
         int 10h 
         inc cx          ;incrementare pozitie linie
         cmp cx,183      ;limita linie  
    jnz UsaS 
    
    ;Desenare ferestre
    ;Linii verticale fereastra stanga
    ;lini 1
    mov cx,136           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind1V1: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h        
        inc dx           ;incrementare pozitie linie
        cmp dx,105       ;limita linie
    jnz Wind1V1
    
    ;linie 2             ;pozitionare pe coloana
    mov cx,146           ;pozitionare pe rand
    mov dx,85        
    Wind1V2:             
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie
        int 10h          
        inc dx           ;incrementare pozitie linie
        cmp dx,105       ;limita linie
    jnz Wind1V2 
    
    ;linie 3
    mov cx,156           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind1V3: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h          
        inc dx           ;incrementare pozitie linie
        cmp dx,105       ;limita linie
    jnz Wind1V3
    
    ;Linii verticale fereastra dreapta
    ;linie 1
    mov cx,190           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind2V1:
         mov ah,0ch      ;desenare linie
         mov al,01h      ;culoare linie  
         int 10h         
         inc dx          ;incrementare pozitie linie
         cmp dx,105      ;limita linie
    jnz Wind2V1
    
    ;linie 2
    mov cx,200           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind2V2: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h          
        inc dx           ;incrementare pozitie linie
        cmp dx,105       ;limita linie
    jnz Wind2V2
    
    ;linie 3
    mov cx,210           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind2V3: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie
        int 10h          
        inc dx           ;incrementare pozitie linie
        cmp dx,105       ;limita linie
    jnz Wind2V3  
    
    ;Linii orizontale fereastra stanga
    ;linie 1
    mov cx,136           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind1H1: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h          
        inc cx           ;incrementare pozitie linie
        cmp cx,156       ;limita linie
    jnz Wind1H1
    
    ;linie 2
    mov cx,136           ;pozitionare pe coloana
    mov dx,95            ;pozitionare pe rand
    Wind1H2: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie
        int 10h          
        inc cx           ;incrementare pozitie linie
        cmp cx,156       ;limita linie
    jnz Wind1H2
    
    ;linie 3
    mov cx,136           ;pozitionare pe coloana
    mov dx,105           ;pozitionare pe rand
    Wind1H3: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h          
        inc cx           ;incrementare pozitie linie
        cmp cx,157       ;limita linie
    jnz Wind1H3
    
    ;Linii orizontale fereastra dreapta
    ;linie 1 
    mov cx,190           ;pozitionare pe coloana
    mov dx,85            ;pozitionare pe rand
    Wind2H1: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h          
        inc cx           ;incrementare pozitie linie
        cmp cx,210       ;limita linie
    jnz Wind2H1  
    
    ;linie 2
    mov cx,190           ;pozitionare pe coloana
    mov dx,95            ;pozitionare pe rand
    Wind2H2: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie 
        int 10h         
        inc cx           ;incrementare pozitie linie
        cmp cx,210       ;limita linie
    jnz Wind2H2
    
    ;linie 3
    mov cx,190           ;pozitionare pe coloana
    mov dx,105           ;pozitionare pe rand
    Wind2H3: 
        mov ah,0ch       ;desenare linie
        mov al,01h       ;culoare linie  
        int 10h          
        inc cx           ;incrementare pozitie linie
        cmp cx,211       ;limita linie
    jnz Wind2H3 
    
    ;Sfarsit desenat casa!
    ; -------------------------------------------;
    
    ;Desenare copac
    ;Desenare trunchi
    ;linie stanga 
    mov cx,55            ;pozitionare pe coloana
    mov dx,120           ;pozitionare pe rand
    TruncSt: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie
        int 10h          
        dec dx           ;incrementare pozitie linie
        cmp dx,80        ;limita linie
    jnz TruncSt
    
    ;linie dreapta
    mov cx,71            ;pozitionare pe coloana
    mov dx,120           ;pozitionare pe rand
    TruncDr: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie
        int 10h         
        dec dx           ;incrementare pozitie linie
        cmp dx,80        ;limita linie
    jnz TruncDr
    
    ;linie jos
    mov cx,71            ;pozitionare pe coloana
    mov dx,120           ;pozitionare pe rand
    TruncJ: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie
        int 10h         
        dec cx           ;incrementare pozitie linie
        cmp cx,55        ;limita linie
    jnz TruncJ 
    
    ;linie sus
    mov cx,71            ;pozitionare pe coloana
    mov dx,80            ;pozitionare pe rand
    TruncS: 
        mov ah,0ch       ;desenare linie
        mov al,06h       ;culoare linie
        int 10h         
        dec cx           ;incrementare pozitie linie
        cmp cx,55        ;limita linie
    jnz TruncS 
    
    ;Desenare coroana copac
    ;partea stanga
    mov cx,20           ;pozitionare pe coloana
    mov dx,80           ;pozitionare pe rand
    CorSt: 
        mov ah,0ch      ;desenare linie
        mov al,02h      ;culoare linie 
        int 10h          
        inc cx          ;incrementare pozitie linie
        dec dx          ;pe diagonala
        cmp cx,63       ;limita linie   
        cmp dx,37       ;pe diagonala
    jnz CorSt 
    
    ;partea dreapta
    mov cx,63           ;pozitionare pe coloana
    mov dx,37           ;pozitionare pe rand
    CorDr: 
        mov ah,0ch      ;desenare linie
        mov al,02h      ;culoare linie 
        int 10h         
        inc cx          ;incrementare pozitie linie
        inc dx          ;pe diagonala
        cmp cx,106      ;limita linie   
        cmp dx,80       ;pe diagonala
    jnz CorDr 
    
    ;partea jos
    mov cx,20           ;pozitionare pe coloana
    mov dx,80           ;pozitionare pe rand
    CorJ: 
        mov ah,0ch      ;desenare linie
        mov al,02h      ;culoare linie 
        int 10h         
        inc cx          ;incrementare pozitie linie
        cmp cx,106      ;limita linie   
    jnz CorJ
    ;Sfarsit desenare copac!
    ;---------------------------------------------;
    
    ;Desenare sosea 
    ;Linii orizontale
    ;linie 1
    mov cx,0            ;pozitionare pe coloana 
    mov dx,150          ;pozitionare pe rand
    Sos1: 
        mov ah,0ch      ;desenare linie
        mov al,07h      ;culoare linie 
        int 10h          
        inc cx          ;incrementare pozitie linie 
        cmp cx,320      ;limita linie
    jnz Sos1
    
    ;linie 2
    mov cx,0            ;pozitionare pe coloana 
    mov dx,190          ;pozitionare pe rand
    Sos2: 
        mov ah,0ch      ;desenare linie
        mov al,07h      ;culoare linie 
        int 10h          
        inc cx          ;incrementare pozitie linie 
        cmp cx,320      ;limita linie
    jnz Sos2
    
    ;Sfarsit sosea
    ;----------------------------------------------;
    
    ;Produce sunet
    ;Acesta va semnala faptul ca poti incepe folosi mouse-ul pentru desenat
    
    mov ah, 02
    mov dl, 07h      ;07h preia valoare pentru a produce sunet
    int 21h          ;emitere sunet  
    int 21h 
    int 21h 
    int 21h 
    int 21h 
    
    ;Sfarsit producere sunet
    ;----------------------------------------------------;
    
    ;Desenare folosind mouse-ul               
    jmp start
    
    oldX dw -1         ;coordonate intiale axa OX
    oldY dw 0          ;coordonate initiale axa OY
    ;resetare mouse si determinare status
    start:
        mov ax, 0
        int 33h
        cmp ax, 0
    ;verificare daca buton mouse este apasat
    check_mouse_button:
        mov ax, 3
        int 33h
        shr cx, 1       ; impartire prin 2
        cmp bx, 1
    ;desenare pixel de culoare galben    
    jne xor_cursor:
        mov al, 0eh     ; culoare pixel
        jmp draw_pixel
    ;    
    xor_cursor:
        cmp oldX, -1    ;compara valoare noua cu cea initiala
        je not_required
        push cx          ;stocare date noi
        push dx
        mov cx, oldX
        mov dx, oldY
        mov ah, 0dh     ;preluare pixel
        int 10h
        xor al, 0eh     ;culoare pixel
        mov ah, 0ch     ;setare pixel
        int 10h
        pop dx          ;eliberare 
        pop cx
    ;    
    not_required:
        mov ah, 0dh     ;preluare pixel
        int 10h
        xor al, 0eh     ;culoare pixel
        mov oldX, cx    ; preia valoare noua coloana
        mov oldY, dx    ;preia valoare noua rand
    ;afisare pixel    
    draw_pixel:
        mov ah, 0ch     ; setare pixel
        int 10h
    ; iesire din bucla daca tasta ESC este apasata    
    check_esc_key:
        mov dl, 255
        mov ah, 6
        int 21h
        cmp al, 27      ; comparare cu tasta esc
        jne check_mouse_button
    ;Sfarsit desenare folosind mouse
    ;------------------------------------------------------------------------;   
    
    ;Afisare mesaje de introducere a numelui;
    ;Introducere nume caracter cu caracter si afisare a acestuia
    jmp mesaj  ; jump la mesaj
    
    buffer db "empty buffer --- empty buffer"
    size = $ - offset buffer 
    msg1   db "Autor: ", 0
    
    mesaj:
    ;Afisare mesaj informativ
    lea     si, msg1
    call    print_string
    
    ;preluare caracter
    lea     di, buffer      ; buffer offset.
    mov     dx, size        ; buffer size.
    call    get_string
    
    putc    0Dh
    putc    10 ;linie noua
    
    ;Afisare caracter
    mov     si, di
    call    print_string
    
    ;Sfarsit introducere nume de la tastatura 
    ;---------------------------- -----------------------; 
    
    ;Start animatie
    jmp     start_animatie
    
    ;lungime initiala
    s_size  equ     7
    
    ; LSB este stanga
    snake dw s_size dup(0)
    
    tail    dw      ?
    
    ; constante directii
    left    equ     4bh
    right   equ     4dh
    
    ; directia curenta dreapta
    cur_dir db      right
    
    ;timp asteptare
    wait_time dw    0
    
    start_animatie:
    
    ; asteapta tasta
    mov ah, 00h
    int 16h
    
    ; ascunde cursor
    mov     ah, 1
    mov     ch, 2bh
    mov     cl, 0bh
    int     10h           
    
    
    game_loop:
    
    ;arata cap
    mov     dx, snake[0]
    
    ; pozitie
    mov     ah, 02h
    mov     dh,15h
    int     10h
    
    ; print '*' la locatie
    mov     al, '*'
    mov     ah, 09h
    mov     bl, 0ch ; culoare
    mov     cx, 1   
    int     10h
    
    ; mentine coada
    mov     ax, snake[s_size * 2 - 2]
    mov     tail, ax
    
    call    move_snake
    
    ;ascunde coada veche
    mov     dx, tail
    
    ;pozitie
    mov     ah, 02h 
    mov     dh,15h     
    int     10h
    
    ; print ' ' la locatie
    mov     al, ' '
    mov     ah, 09h
    mov     bl, 0eh ; attribute.
    mov     cx, 1   ; single char.
    int     10h
    
    check_for_key:
    
    ;verifica comanda
    mov     ah, 01h
    int     16h
    jz      no_key
    
    mov     ah, 00h
    int     16h
    
    cmp     al, 1bh    ; esc - key?
    je      stop_game  ;
    
    mov     cur_dir, ah
    
    no_key:
    
    mov     ah, 00h
    int     1ah
    cmp     dx, wait_time
    jb      check_for_key
    add     dx, 4
    mov     wait_time, dx
    
    jmp     game_loop
    
    stop_game:
    
    ; arata cursor
    mov     ah, 1
    mov     ch, 0bh
    mov     cl, 0bh
    int     10h
    ret
    
    ;functie pentru preluare caracter
    get_string      proc    near
    push    ax
    push    cx
    push    di
    push    dx
    
    mov     cx, 0                   ; numara caractere.
    
    cmp     dx, 1                   ;verificare lungime buffer suficienta
    jbe     empty_buffer            ;
    
    dec     dx                      ; rezerva pentru ultimul bit
    
    ;asteapta apasare tasta 
    wait_for_key:
    
    mov     ah, 0                   ; preia tasta apasata
    int     16h
    
    cmp     al, 0Dh                 ;return caracter 
    jz      exit
    
    
    cmp     al, 8                   ; verifica daca backspace este apasat
    jne     add_to_buffer
    jcxz    wait_for_key            ; nimic de sters
    dec     cx
    dec     di
    putc    8                       
    putc    ' '                     ; ceva de sters
    putc    8                       
    jmp     wait_for_key
    
    ;adaugare caracter in buffer
    add_to_buffer:
    
            cmp     cx, dx          ; verifica daca buffer este full
            jae     wait_for_key    ; asteapta comanda de return sau stergere
    
            mov     [di], al
            inc     di
            inc     cx
            
    
            mov     ah, 0eh         ;print caracter 
            int     10h
    
    jmp     wait_for_key           ;jump
    
    ;daca nu se apasa nimic
    exit:
        mov     [di], 0
    
    ;elibereaza buffer
    empty_buffer:
           pop     dx
           pop     di
           pop     cx
           pop     ax
    ret
    
    get_string      endp
    
    ;Afisare caracter 
    print_string proc near
        push    ax      ; memorare in registru
        push    si      
        
    ;trecere la urmatorul caracter
    next_char:      
            mov     al, [si]
            cmp     al, 0
            jz      printed
            inc     si
            mov     ah, 0eh 
            int     10h
            jmp     next_char
            
    ;verifica daca a fost printat caracterul        
    printed:
        pop     si      
        pop     ax      
    
    ret
    
    print_string endp   
    
    ;----------------------------------------;
    ;Functii necesare pentru animatii
    move_snake proc near
      
    mov     ax, 40h
    mov     es, ax
    ;distanta de la punct la coada
      mov   di, s_size * 2 - 2
      mov   cx, s_size-1
      
    move_array:
      mov   ax, snake[di-2]
      mov   snake[di], ax
      sub   di, 2
      loop  move_array
    ;comenzi sageti
    cmp     cur_dir, left
      je    move_left
    cmp     cur_dir, right
      je    move_right
    
    jmp     stop_move       ;nicio directie
    
    move_left:
      mov   al, b.snake[0]
      dec   al
      mov   b.snake[0], al
      cmp   al, -1
      jne   stop_move       
      mov   al, es:[4ah]    ; coloana numar.
      dec   al
      mov   b.snake[0], al  ; intoarcere dreapta.
      jmp   stop_move
    
    move_right:
      mov   al, b.snake[0]
      inc   al
      mov   b.snake[0], al
      cmp   al, es:[4ah]    ; numar coloana   
      jb    stop_move
      mov   b.snake[0], 0   ; intoarcere stanga
      
      jmp   stop_move
      
      stop_move:
      ret
      
    move_snake endp