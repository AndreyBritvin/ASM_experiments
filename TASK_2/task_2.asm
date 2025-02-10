.model tiny
.code
org 100h
Start: jmp MAIN

;-----------------------------------------
; Initialise screen address to es
; Return: nothing
; Destr: es
;-----------------------------------------
INIT_SCREEN proc
    push bx
    mov bx, 0b800h
    mov es, bx
    pop bx
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Put string at es:[di], where first  symbol ds:[si]
;                              second, etc   ds:[si + 1] and print it cx times
;                              last is       ds:[si + 2]
; Destr:    al, cx, di, si
;-----------------------------------------
DRAW_LINE proc
    push di
    push cx
    lodsb
    stosw
    lodsb
    rep stosw
    lodsb
    stosw
    pop cx
    pop di
    add di, 80 * 2
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Put frame at es:[di], where first line   ds:[si]     --- ds:[si + 2]
;                             second, etc  ds:[si + 3] --- ds:[si + 5] and print it cx times
;                             last is      ds:[si + 6] --- ds:[si + 8]
; Destr:    al, bx, cx, di
;-----------------------------------------
DRAW_FRAME proc
    call DRAW_LINE

    DRAW_N_LINES:
    test bx, bx
    jz DRAW_N_LINES_END
    push si
    call DRAW_LINE
    pop si
    dec bx
    jmp DRAW_N_LINES
    DRAW_N_LINES_END:

    add si, 3
    call DRAW_LINE
    ret
    endp
;-----------------------------------------


MAIN:
    cld                            ; for correct work string functions
    call INIT_SCREEN

    mov si, offset FRAME_PATTERN ; set character data

    mov di, (5 * 80 * 2) + 3 * 2 ; initial offset
    mov ah, 1101010b        ; set color mode
    mov bx, 5 ; width
    mov cx, 8 ; height

    sub bx, 2 ;
    sub cx, 2 ; decrease to include border in number

    call DRAW_FRAME

    ; Finish Programm
    mov ax, 4c00h			; ax = 4c00h
	int 21h

FRAME_PATTERN: db '123456789'

end Start
