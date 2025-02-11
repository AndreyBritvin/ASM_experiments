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
; Destr:    al, di, si
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
;                             second, etc  ds:[si + 3] --- ds:[si + 5] and print it bx times
;                             last is      ds:[si + 6] --- ds:[si + 8]
; Destr:    al, bx, cx, di, si
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

;-----------------------------------------
; Read number from ds:[si] and put integer result to ax
; Destr ax, si
;-----------------------------------------
ATOI proc
    push cx
    xor ax, ax      ; ax = 0
    mov ch, 10

    ATOI_READ_SYMBOL:
    mov cl, [si]
    cmp cl, '0'
    jb ATOI_END
    cmp cl, '9'
    ja ATOI_END
    sub cl, '0'
    mul ch
    add al, cl
    inc si
    jmp ATOI_READ_SYMBOL
    ATOI_END:
    pop cx
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Read number from ds:[si] and put integer hex result to ax
; Destr ax, si
;-----------------------------------------
ATOIHEX proc
    push cx
    xor ax, ax      ; ax = 0
    mov ch, 16      ; TODO: fix via shifting
    ATOIHEX_READ_SYMBOL:
    mov cl, [si]
    cmp cl, '0'
    jb ATOIHEX_CHECK_LETTER
    cmp cl, '9'
    ja ATOIHEX_CHECK_LETTER
    jmp ATOIHEX_DIGIT

    ATOIHEX_CHECK_LETTER:
    cmp cl, 'a'
    jb ATOIHEX_END
    cmp cl, 'h'
    ja ATOIHEX_END
    jmp ATOIHEX_LETTER

    ATOIHEX_DIGIT:
    sub cl, '0'
    jmp ATOIHEX_MULT

    ATOIHEX_LETTER:
    sub cl, 'a' - 10

    ATOIHEX_MULT:
    mul ch
    add al, cl
    inc si
    jmp ATOIHEX_READ_SYMBOL

    ATOIHEX_END:
    pop cx
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Skip spaces at ds:[si] by incrementing si
; Destr: si
;-----------------------------------------
SKIP_SPACES proc
    push cx
    SKIP_SPACES_BEGIN:
    mov cl, [si]
    cmp cl, ' '
    jne ATOI_END
    inc si
    jmp SKIP_SPACES_BEGIN
    SKIP_SPACES_END:
    pop cx
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Read string from ds:[si]
; Ret: ah - color scheme
;      bx - height
;      cx - width
;      dx - addr of text_str
;      si - addr of frame_pattern
; Destr: ax, bx, cx, dx, si
;-----------------------------------------
PARSE_COMMAND_LINE proc
    push di
    mov si, 81h

    call SKIP_SPACES
    call ATOI
    mov bx, ax          ; get width TODO: move to another function

    call SKIP_SPACES
    call ATOI
    mov cx, ax          ; get height

    call SKIP_SPACES
    call ATOIHEX
    shl ax, 8   ; shift color scheme to ah
    push ax     ; save ax with color scheme
    mov dx, si  ; save si in dx

    call SKIP_SPACES
    call ATOI
    cmp ax, 0
    je COMM_LINE_PATTERN

    sub ax, 1                       ; if ax != 0
    mov di, ax                      ; di = ax
    shl di, 3                       ; di *= 8
    add di, ax                      ; di += ax => di *= 9
    lea si, [FRAME_PATTERN + di]    ; address to string
    jmp COMM_LINE_END
    COMM_LINE_PATTERN:  ; if ax == 0
    call SKIP_SPACES    ; si = first not space symbol
    COMM_LINE_END:

    push si             ; save si with frame pattern address
    mov si, dx          ; return si index from dx to th
    call SKIP_SPACES    ; TODO: make offset after frame pattern
    mov dx, si          ; save addr of string to dx

    pop si

    pop ax
    pop di
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Put string at es:[di] with length of cx
; Destr: al, cx, di, si
;-----------------------------------------
PRINT_STRING proc
    PRINT_STRING_LOOP:
    lodsb
    stosw
    loop PRINT_STRING_LOOP
    ret
    endp
;-----------------------------------------


MAIN:
    cld                            ; for correct work string functions
    call INIT_SCREEN
    call PARSE_COMMAND_LINE
    ; mov si, offset FRAME_PATTERN ; set character data

    mov di, (5 * 80 * 2) + 3 * 2 ; initial offset
    ; mov ah, 1101010b        ; set color mode
    ; mov bx, 5 ; width
    ; mov cx, 8 ; height

    sub bx, 2 ;
    sub cx, 2 ; decrease to include border in number

    call DRAW_FRAME

    mov cx, 6
    mov di, (7 * 80 * 2) + 5 * 2
    mov si, offset FRAME_PATTERN + 9
    call PRINT_STRING
    ; Finish Programm
    mov ax, 4c00h			; ax = 4c00h
	int 21h

FRAME_PATTERN: db '123456789'
               db '+-+| |+-+'
               db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch
end Start
