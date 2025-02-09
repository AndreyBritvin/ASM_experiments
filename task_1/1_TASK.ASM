.model tiny
.code
org 100h
start:
	mov dx, offset MyStr		; dx = &MyStr

	mov ah, 09h			; ah = 09h
	int 21h				; DOS Fn 09h = puts(dx)

	mov ax, 4c00h			; ax = 4c00h
	int 21h				; DOS Fn 4ch = exit(al)

MyStr: db "Hello World$"
end start
))
Hihihiha
	