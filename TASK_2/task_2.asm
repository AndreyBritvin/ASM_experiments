.model tiny
.code
org 100h
locals @@
Start: jmp MAIN
;@@, macro endm, equ,

;-------------CONSTANTS_BEGIN-------------------
VIDEO_MEMORY_SEGMENT_ADDR   equ 0b800h
BYTES_PER_SYMBOL            equ 2
SCREEN_WIDTH                equ 80
PATTERN_OFFSET              equ 3
COMMAND_LINE_BEGIN_ADDRESS  equ 81h
REGISTERS_AMOUNT            equ 10
TOGGLE_SCAN_CODE            equ 12h
FRAME_X_POSITION            equ 5
FRAME_Y_POSITION            equ 3
;--------------CONSTANTS_END--------------------

;-----------------------------------------
; Calls on int 09h, checks if scan-code is 'e' and enable/disable frame
; Return: nothing
; Destr: nothing
;-----------------------------------------
FRAME_ENABLE_INT proc
    push ax
    ; mov ax, VIDEO_MEMORY_SEGMENT_ADDR
    ; mov es, ax
    ; mov ah, 4eh
    ; mov bx, 5 * SCREEN_WIDTH * BYTES_PER_SYMBOL + 40 * BYTES_PER_SYMBOL
    in al, 60h

    cmp al, TOGGLE_SCAN_CODE                ; if SCAN-CODE IS ... then toggle IS_FRAME_ACTIVE
    jne @@ANOTHER_BUTTON
    ;mov es:[bx], ax
    mov al, byte ptr cs:IS_FRAME_ACTIVE
    xor al, 1
    mov byte ptr cs:IS_FRAME_ACTIVE, al
    @@ANOTHER_BUTTON:

    pop ax
    db 0eah
Original_int09h_handler_offset:
    dw 0
Original_int09h_handler_segment:
    dw 0
IS_FRAME_ACTIVE: db 0
    endp
;-----------------------------------------

;-----------------------------------------
; Calls on int 08h, updates frame if enabled every 55ms
; Return: nothing
; Destr: nothing
;-----------------------------------------
FRAME_UPDATE_INT proc
    push ax
    mov al, byte ptr cs:IS_FRAME_ACTIVE
    cmp al, 1
    jne @@DONT_SHOW_FRAME           ; if IS_FRAME_ACTIVE

    push bx cx dx di si
    push bp es ds ss                ; save all registers (for correct interrupt and for print)

    push cs
    pop ds                          ; ds = cs

    cld                             ; for correct work string functions
    mov ax, 1003h                   ; set video memory highest bit for blinking or for high contrast
    mov bl, 0h                      ; for high constast
    int 10h

    call INIT_SCREEN
    mov si, offset FRAME_PATTERN + 3 * PATTERN_OFFSET * 1 ; set frame_pattern

    mov di, ( FRAME_X_POSITION * SCREEN_WIDTH * BYTES_PER_SYMBOL)    \
            + FRAME_Y_POSITION                * BYTES_PER_SYMBOL   ; initial offset
    mov ah, 1101010b                                ; set color mode
    mov bx, REGISTERS_AMOUNT + 2                    ; height
    mov cx, 9                                       ; width |ax ABCD|
                                                    ;       012345678 7+2

    sub bx, 2 ;
    sub cx, 2 ; decrease to include border in number

    call DRAW_FRAME

    mov di, ( (FRAME_X_POSITION + 1) * SCREEN_WIDTH * BYTES_PER_SYMBOL) \
            + (FRAME_Y_POSITION + 1)                * BYTES_PER_SYMBOL ; initial offset
    call PRINT_REGISTERS

    pop ss ds es bp
    pop si di dx cx bx  ; restore values
    @@DONT_SHOW_FRAME:
    pop ax
    db 0eah
Original_int08h_handler_offset:
    dw 0
Original_int08h_handler_segment:
    dw 0
    endp
;-----------------------------------------

;-----------------------------------------
; Prints every register value
; Return: nothing
; Destr: al, cx, si, di
;-----------------------------------------
PRINT_REGISTERS proc
    mov cx, REGISTERS_AMOUNT
    mov si, offset REG_PATTERN
    mov bp, sp
    add bp, REGISTERS_AMOUNT * 2 ; to go from top to bottom bc registers pushed in such order
@@PRINT_REG:
    push di
    lodsb
    stosw
    lodsb
    stosw
    lodsb
    stosw       ; print ax_ (space)

    mov bx, ss:[bp]
    dec bp
    dec bp      ; get next value
    call PRINT_REG_VALUE

    pop di
    add di, SCREEN_WIDTH * BYTES_PER_SYMBOL
    loop @@PRINT_REG

    ret
    endp
REG_PATTERN: db "ax bx cx dx di si bp es ds ss "
;-----------------------------------------

;-----------------------------------------
; Prints register value (from bx) to es:[di] in hex mode, aka itoa
; Destr: di
;-----------------------------------------
PRINT_REG_VALUE proc
    push ax cx dx
    ; mov bx, cs
    mov cx, 4   ; in 16 bit register _4_ parts of 4 bits
    @@GET_DIGIT:
    mov dx, bx                              ; save in dx
    and bx, 1111000000000000b               ; mask first 4 bits
    shr bx, 12                              ; delete zeros (bc little endian)
    mov al, [bx + offset HEX_TO_ASCCI_ARR]  ; get ascii character
    shl dx, 4                               ; delete first 4 bits and replace new value
    mov bx, dx                              ; resave dx to bx
    stosw
    loop @@GET_DIGIT
    pop dx cx ax
    ret
HEX_TO_ASCCI_ARR:
    db '0123456789ABCDEF'
    endp
;-----------------------------------------

;-----------------------------------------
; Initialise screen address to es
; Return: nothing
; Destr: es
;-----------------------------------------
INIT_SCREEN proc
    push bx
    mov bx, VIDEO_MEMORY_SEGMENT_ADDR
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
    add di, SCREEN_WIDTH * BYTES_PER_SYMBOL
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

    add si, PATTERN_OFFSET
    call DRAW_LINE
    ret
    endp
;-----------------------------------------

FRAME_PATTERN: db '123456789'                                           ; debug
               db '+-+| |+-+'                                           ; cool
               db 0c9h, 0cdh, 0bbh, 0bah, ' ', 0bah, 0c8h, 0cdh, 0bch   ; stripes
               db 04h, 03h, 04h, 03h, ' ', 03h, 04h, 03h, 04h           ; hearts
END_OF_RESIDENT:            ; All before will save as resident. Below will destroy after program finishes

;-----------------------------------------
; Read number from ds:[si] and put integer result to ax
; Destr ax, si
;-----------------------------------------
ATOI proc
    push cx
    xor ax, ax      ; ax = 0
    mov ch, 10      ; multiplyer

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
    shl ax, 4
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
    jne SKIP_SPACES_END
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
    mov si, COMMAND_LINE_BEGIN_ADDRESS

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

    call SKIP_SPACES
    call ATOI                       ; get pattern number (0 - custom, next - as in programm at FRAME_PATTERN)
    mov dx, si  ; save si in dx
    cmp ax, 0
    je COMM_LINE_PATTERN

    dec ax                          ; if ax != 0 : ax-- to get correct offset
    mov di, ax                      ; di = ax
    shl di, 3                       ; di *= 8
    add di, ax                      ; di += ax => di *= 9
    lea si, [FRAME_PATTERN + di]    ; address to constant (hardcoded) string with pattern
    inc ax                          ; return ax to non zero to be sure for correct algorithm next
    jmp COMM_LINE_END

    COMM_LINE_PATTERN:  ; if ax == 0 === custom frame_pattern
    call SKIP_SPACES    ; si = first not space symbol

    COMM_LINE_END:

    push si             ; save si with frame pattern address

    ; now need to find string which will be inside frame
    mov si, dx          ; return si index from dx to si to continue command line parsing
    call SKIP_SPACES    ; skip spaces to first nonspaceable
    cmp ax, 0           ;
    je COMM_LINE_CUSTOM_PATTERN
    call SKIP_SPACES          ; if ax != 0 it means that printable string exactly after the offset-number
    jmp COMM_LINE_CUSTOM_PATTERN_END

    COMM_LINE_CUSTOM_PATTERN: ; if ax == 0 (custom line)
    mov al, ' '
    call STRLEN               ; find length of pattern (usually 9, but can be more)
    add si, cx                ; add this length offset to si
    call SKIP_SPACES          ; and skip spaces to next nonspaceable symbol
    COMM_LINE_CUSTOM_PATTERN_END:

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
    push ax             ; to save color scheme
    mov al, 0dh         ; 0d = \n TODO: fix this??? Not universal function
    call STRLEN         ; put length in cx
    pop ax
    dec cx              ; not print \n
    PRINT_STRING_LOOP:
    lodsb
    stosw
    loop PRINT_STRING_LOOP
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Count length of string from ds:[si] and puts it in cx, terminal symbol should be in al
; Ret: cx - length of string
; Destr: ax, cx
;-----------------------------------------
STRLEN proc
    push es
    push di
    push bx
    mov di, si      ; di = si, for scasb
    mov bx, ds
    mov es, bx      ; es = ds, for scasb
    mov cx, -1      ; cx = FFFF
    repne scasb     ; while (cx-- != 0 && ZF == 0): ZF = (al == ES:[DI++])
    neg cx
    dec cx          ; return cx to normal positive value
    pop bx
    pop di
    pop es
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Fills offset and segment of interrupt #al to [di] and [di + 2], and rewrites
; in interrupt table with ds:[dx]. So dx should contain offset of our function
; Destr: ah, bx, es
;-----------------------------------------
CREATE_ISR_CHAIN proc
    mov ah, 35h                 ; call DOS Fn(35h) - to get current address
    int 21h                     ; es:[bx] is current interrupt function
    mov [di], bx                ; save address ofset
    mov [di + 2], es            ; save segment
    mov ah, 25h                 ;
    int 21h                     ; call DOS Fn(25h), to put in interrupt table ds:[dx] address
    ret
    endp
;-----------------------------------------

;-----------------------------------------
; Makes our programm resident and terminate programm
; Destr: ax, dx
;-----------------------------------------
MAKE_RESIDENT proc
    mov dx, offset END_OF_RESIDENT      ; select area to save
    shr dx, 4                           ; bc in dx paragraphs of 16 bytes, so divide by 16
    inc dx                              ; for some cases where last bytes could lost
    mov ax, 3100h                       ; create resident
    int 21h
    ret
    endp
;-----------------------------------------

MAIN:
    mov al, 09h
    mov dx, offset FRAME_ENABLE_INT
    mov di, offset Original_int09h_handler_offset
    call CREATE_ISR_CHAIN                           ; create my isr handler for 09h

    mov al, 08h
    mov dx, offset FRAME_UPDATE_INT
    mov di, offset Original_int08h_handler_offset
    call CREATE_ISR_CHAIN                           ; create my isr handler for 08h

    ; Finish Programm
    call MAKE_RESIDENT

end Start
