.model tiny
.code
org 100h
Start: jmp MAIN
;-----------------------------------------
;
; Req: bx - string address
;      cx - coordinates, where
;           ch - x coord
;           cl - y coord
;       dh - color mode
; Assumes: es = 0b800h
; Destr: bx, cx, si
;
;-----------------------------------------
DRAW_FRAME proc
    push si
    call DRAW_LINE

    DR_FRAME_DRAW_MIDDLE_BEGIN:
    test ch, ch
    jz DR_FRAME_DRAW_MIDDLE_END ; while ch > 0
    add di, 80 * 2              ; di += 80 * 2
    mov si, bx
    call DRAW_LINE              ; draw_line()
    mov bx, si
    sub ch, 1                   ; ch--;

    jmp DR_FRAME_DRAW_MIDDLE_BEGIN
    DR_FRAME_DRAW_MIDDLE_END:

    add bx, 3
    call DRAW_LINE
    pop si
    ret
    endp
;-----------------------------------------


;-----------------------------------------
; Req:
;-----------------------------------------
DRAW_LINE proc
    push cx
    push di

    mov dl, [bx]        ; dx = current symbol
    mov es:[di], dx
    add di, 2           ; di += 2
    add bx, 1           ; bx += 1, next symbol

    mov dl, [bx]        ; dx = current symbol

    DR_LINE_LOOP_BEGIN:
    test cl, cl
    jz DR_LINE_END_LOOP        ; while cl > 0
    mov es:[di], dx            ; put at [di] symbol in dx
    add di, 2                  ; di += 2
    sub cl, 1                  ; cl--
    jmp DR_LINE_LOOP_BEGIN     ; loop
    DR_LINE_END_LOOP:

    add bx, 1

    mov dl, [bx]        ; dx = current symbol
    mov es:[di], dx
    add bx, 1           ; bx += 1, next symbol

    pop di
    pop cx
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Initialise screen address to es
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

MAIN:
    call INIT_SCREEN

    mov bx, offset FRAME_PATTERN ; set character data
    mov di, (5 * 80 * 2) + 3 * 2 ; initial offset
    mov dh, 1101010b        ; set color mode
    mov ch, 3               ; set width
    mov cl, 5               ; set height
    call DRAW_FRAME

    ; Finish Programm
    mov ax, 4c00h			; ax = 4c00h
	int 21h

FRAME_PATTERN: db '123456789'

end Start
