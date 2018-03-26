
[org 0x0100]
jmp start

s:dw 0
tickcount: dw 0
oldisr: dd 0
plate: db '                '
lengthplate: dw 16
extreme_left: dw 0
extreme_right: dw 64
xpos: dw 30
ball: db '0'
space: db ' '
lengthball: dw 1
level: db 'Level:' ; string to be printed
length: dw 6
score:db 'Score:'
lives:db 'Lives:'
first_time: db 1
xball: dw 38
yball:dw 19
prev: dw 0
next :dw 0
scorenum:dw 0
changedirection:dw 0
botleft:dw 0
lifecount:dw 3
uprightdirection:dw 0
checkhit: dw 0  ;initail value
initial: dw 0
upleft:dw 0
special:dw 0
special2:dw 0
upleftrd:dw 0
botright2:dw 0



clrscr: push es
		push ax
		push cx
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		xor di, di ; point di to top left column
		mov ax, 0x0720 ; space char in normal attribute
		mov cx, 2000 ; number of screen locations
		cld ; auto increment mode
		rep stosw ; clear the whole screen
		pop di
		pop cx
		pop ax
		pop es
		ret
		
printnum: push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov ax, [bp+4] ; load number in ax
mov bx, 10 ; use base 10 for division
mov cx, 0 ; initialize count of digits
nextdigit: mov dx, 0 ; zero upper half of dividend
div bx ; divide by 10
add dl, 0x30 ; convert digit into ascii value
push dx ; save ascii value on stack
inc cx ; increment count of values
cmp ax, 0 ; is the quotient zero
jnz nextdigit ; if no divide it again
mov di, 3902 ; point di to top left column
nextpos: pop dx ; remove a digit from the stack
mov dh, 0x02 ; use normal attribute
mov [es:di], dx ; print char on screen
add di, 2 ; move to next screen location
loop nextpos ; repeat for all digits on stack
pop di
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 2
		
clrplate:	push bp
		mov bp, sp	
		push es
		push ax
		push cx
		push si
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov al, 80 ; load al with columns per row
		mul byte [bp+10] ; multiply with y position
		add ax, [bp+12] ; add x position
		shl ax, 1 ; turn into byte offset
		mov di,ax ; point di to required location
		mov si, [bp+6] ; point si to plate string
		mov cx, [bp+4] ; load length of plate in cx
		mov ax,0x0720
		cld ; auto increment mode
		
nextchar2: mov ah,0x07
lodsb ; load next char in al
		stosw ; print char/attribute pair
		loop nextchar2 ; repeat for the whole string
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10	
		
printBrick1:
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push si
push di

mov ax, 0xb800
mov es, ax ; point es to video base
;calculating  left
mov al, 80 ; load al with columns per row
mul byte [bp+10] ; multiply with y position
add ax, [bp+8] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location

;calculating  right
mov al, 80 ; load al with columns per row
mul byte [bp+6] ; multiply with y position
add ax, [bp+4] ; add x position
shl ax, 1 ; turn into byte offset
mov si,ax ; point di to required location 

mov ax,0xDD20
loopit:
mov [es:di], ax ; show this char on screen
add di, 2
cmp di,si
jne loopit


pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 8

printBrick2:
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push si
push di

mov ax, 0xb800
mov es, ax ; point es to video base
;calculating  left
mov al, 80 ; load al with columns per row
mul byte [bp+10] ; multiply with y position
add ax, [bp+8] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location

;calculating  right
mov al, 80 ; load al with columns per row
mul byte [bp+6] ; multiply with y position
add ax, [bp+4] ; add x position
shl ax, 1 ; turn into byte offset
mov si,ax ; point di to required location 

mov ax,0x2220
loopit2:
mov [es:di], ax ; show this char on screen
add di, 2
cmp di,si
jne loopit2


pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 8

printBrick3:
push bp
mov bp, sp
push es
push ax
push bx
push cx
push dx
push si
push di

mov ax, 0xb800
mov es, ax ; point es to video base
;calculating  left
mov al, 80 ; load al with columns per row
mul byte [bp+10] ; multiply with y position
add ax, [bp+8] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location

;calculating  right
mov al, 80 ; load al with columns per row
mul byte [bp+6] ; multiply with y position
add ax, [bp+4] ; add x position
shl ax, 1 ; turn into byte offset
mov si,ax ; point di to required location 

mov ax,0xEE20
loopit3:
mov [es:di], ax ; show this char on screen
add di, 2
cmp di,si
jne loopit3


pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop es
pop bp
ret 8

printInfo:
push bp
mov bp, sp
push es
push ax
push cx
push si
push di
mov ax, 0xb800
mov es, ax ; point es to video base
mov al, 80 ; load al with columns per row
mul byte [bp+10] ; multiply with y position
add ax, [bp+12] ; add x position
shl ax, 1 ; turn into byte offset
mov di,ax ; point di to required location
mov si, [bp+6] ; point si to string
mov cx, [bp+4] ; load length of string in cx
mov ah, 0x02 ; load attribute in ah
nextchar: mov al, [si] ; load next char of string
mov [es:di], ax ; show this char on screen
add di, 2
add si, 1 ; move to next char in string
loop nextchar ; repeat the operation cx times
mov al,[bp+8]
add al,0x30
mov ah,0x02
mov [es:di],ax

pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 10	

right:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	;mov word[cs:changedirection],0
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	cmp ax,72
	ja yes11
	add ax,5         ;increment in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	add bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp exitttt
	yes11:
	mov word[cs:botleft],1
	
	jmp exitttt
	exitttt:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;;;;;;left direction

left2:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	cmp ax,6
	jna yes11left
	sub ax,5         ;increment in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	add bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp exittttleft
	yes11left:
	
	mov word[cs:botright2],1
	jmp exittttleft
	
	exittttleft:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;;;;;;;;;;;;;;;;;;;;leftdown of right side

leftdown:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	mov word[cs:changedirection],0
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	
	l2:
	mov ax,[cs:xball]
    sub ax,4           ;increment in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	
	add bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	push si
	call checkhitboard
	cmp word[cs:initial],1 
	je bu    ;if not a miss then change direction
    
	l3:
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp exittttt
	
	bu:
	mov word [cs:next],0x0100
	mov word[cs:upleft],1
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	
	;;;;;;this part is done for proper bounce as soon as ball hits the board and changes direction
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	
	exittttt:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;;;;;;;;;;;rightdown of left side
rightdown:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	;mov word[cs:changedirection],0
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	
	l2rd:
	mov ax,[cs:xball]
    add ax,4           ;increment in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	
	add bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	push si
	call checkhitboard
	cmp word[cs:initial],1 
	je burd    ;if not a miss then change direction
    
	l3rd:
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp exitttttrd
	
	burd:
	mov word [cs:next],0x0100
	mov word[cs:upleftrd],1
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	;jmp exitttttrd
	;;;;;;this part is done for proper bounce as soon as ball hits the board and changes direction
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	
	exitttttrd:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

checkhitboard:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	
	mov ax,[bp+4]
	mov cx,3200
	mov dx, 3358
	
	mov bx,cx
	
	again:
	cmp ax,bx
	je compared
	add bx,2
	cmp bx,dx
	jne again
	jmp e
	compared:
	mov ax,[es:bx]
	cmp ax,0x0720
	je empty
	jne notempty
	jmp e
	notempty:
	mov word[cs:checkhit],1  ;not empty
	mov word[cs:initial],1
	jmp e 
	empty:
	mov word[cs:checkhit],0  ;empty
mov ax, 35
push ax ; push x position
mov ax, 24
push ax ; push y position
dec word[cs:lifecount]
mov ax,[cs:lifecount]
push ax ; push attribute
mov ax, lives
push ax ; push address of lives string
push word [length] ; push lives length
call printInfo ; call the printInfo subroutine

	e:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret 2

;;;;;;;;;;;upleft of left side
up_left:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	cmp ax,6
	jna yes22
	sub ax,5         ;decrement in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp ex11
	yes22:
	mov word[cs:special],1
	;call up1
	ex11:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;upright of right side
up_right2:
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	cmp ax,72
	jae yes222
	add ax,5         ;decrement in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	mov si,[cs:next]     ;printing
	
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:si],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp ex11222
	yes222:
	mov ah,0x07
	mov al,1
	mov word[es:0],ax
	mov word[cs:special2],1
	
	ex11222:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret


;;;;;;;;;;;;upleft of right side

upleft2:   
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	
	
	sub ax,5         ;add in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	
	mov bx,[cs:next]
	mov dx,[es:bx]
	;push dx
	;call printnum
	cmp dx,0x0720
	jne hitit2
	
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:bx],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp ex1112
	
	hitit2:
		push bx
		call checkrange
		mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
		jmp ex1112
		
	ex1112:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

;;;;;;;;;;;;;;;;;;;;;;;;;upright of left side

upright:   
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	
	mov bx,[cs:next]   ;move next value in previous value
	mov [cs:prev],bx
	mov ax,[cs:xball]
	
	
	add ax,5         ;add in x position
	push ax
	mov word[cs:xball],ax ;store the new result back
	mov bx,[cs:yball]     ;similarily with y
	sub bx,1
	push bx
	mov word[cs:yball],bx
	mov dx, 1 ; blue on black attribute
	push dx ; push attribute
	mov bx, ball
	push bx ; push address of plate string
	push word [lengthball] ; push plate length       ;calculate the new next position
	call printball ; call the printplate subroutine
	
	
	mov bx,[cs:next]
	mov dx,[es:bx]
	;push dx
	;call printnum
	cmp dx,0x0720
	jne hitit
	
	mov ah, 0x04         ;red color
	mov al,0x02 ;extended ascii character
	mov word[es:bx],ax
	
	mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
	jmp ex111
	
	hitit:
        mov word[changedirection],1
		push bx
		call checkrange
		mov si,[cs:prev]
	mov ax,0x0720
	mov word[es:si],ax
		jmp ex111
		
	ex111:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret


up1:   ;straight direction
push bp
	mov bp, sp
	push es
	push ax
	push cx
	push si
	push di
	push bx
	push dx

	mov ax, 0xb800
	mov es, ax
	mov bx,[cs:next]
	
	mov byte[cs:ball],0x02
	    mov al,[cs:ball]
		sub bx,160
		mov dx,[es:bx]
		cmp dx,0x0720
		jne diff
		mov ah, 0x04
		mov [es:bx],ax
		add bx,160
		mov word[es:bx],0x0720
		
		mov bx,[cs:next]
		sub bx,160
		mov [cs:next],bx
		dec word[cs:yball]
		jmp exitt
		
		diff:
		mov word[cs:changedirection],1 ;means ball has hit the brick
		push bx
		call checkrange
		
		exitt:
pop dx
pop bx
pop di
pop si
pop cx
pop ax
pop es
pop bp
ret

; timer interrupt service routine
timer: 
push ax
inc word [cs:tickcount]; increment tick count
inc word [cs:s]
;cmp word [cs:tickcount],2160      ;after 2 min =120 sec* 18times=2160 times


cmp word [cs:tickcount],9
jne skip
mov word [cs:tickcount],0000
cmp word [cs:s],2160
jae Sb
l1:
cmp word[cs:upleftrd],1
je pathchange3
cmp word[cs:special],1
je pathchange4
cmp word[cs:special2],1
je pathchange5
cmp word[cs:upleft],1
je pathchange3
cmp word[cs:botright2],1
je pathchange6
cmp word[cs:botleft],1
je pathchange2
cmp word[cs:changedirection],1
je pathchange
cmp word[cs:upleftrd],1
je pathchange3

call up1 ;straight up
jmp skip

pathchange:
;call left2
call right     ;to completely change direction
jmp skip

pathchange2:
call leftdown
jmp skip

pathchange3:
call up_left
;call up_right2  ;to completely change direction
jmp skip

pathchange4:
call upright
jmp skip

pathchange5:
call upleft2   ;upleft of right side
jmp skip

pathchange6:
call rightdown   ;upleft of right side
jmp skip

Sb:
call ScoreBonus
jmp l1

skip:
mov al, 0x20
out 0x20, al ; end of interrupt
pop ax
iret ; return from interrupt

ScoreBonus:
push bp
mov bp,sp
push ax
push bx
push cx
push dx
push si
push di

cmp word [cs:scorenum],42
;cmp word [cs:scorenum],42
jne en
mov ax, 25
push ax ; push x position
mov ax, 24
push ax ; push y position
add word[cs:scorenum],20
mov ax, word[cs:scorenum]
push ax ; push attribute
mov ax, score
push ax ; push address of score string
push word [length] ; push score length
call printInfo ; call the printInfo subroutine
mov ax, word[cs:scorenum]
push ax ; push attribute
call printnum


en:
pop di
pop si
pop dx
pop cx
pop bx
pop ax
pop bp
ret


cmp word [cs:scorenum],2
;cmp word [cs:scorenum],42
jne l1
mov ax, 25
push ax ; push x position
mov ax, 24
push ax ; push y position
add word[cs:scorenum],20
mov ax, word[cs:scorenum]
push ax ; push attribute
mov ax, score
push ax ; push address of score string
push word [length] ; push score length
call printInfo ; call the printInfo subroutine

checkrange:
		push bp
        mov bp, sp	
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		mov ax, 0xb800
		mov es, ax 
		mov ax,[bp+4]
		
		
		mov cx,320 ;initial number saved
		mov dx,338 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b1:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b1
		;jmp endd
		
		mov cx,342 ;initial number saved
		mov dx,360 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b2:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b2
		
		mov cx,364 ;initial number saved
		mov dx,382 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b3:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b3
		
		mov cx,386 ;initial number saved
		mov dx,404 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b4:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b4
		
		mov cx,408 ;initial number saved
		mov dx,426 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b5:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b5
		
		mov cx,430 ;initial number saved
		mov dx,448 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b6:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b6
		
		mov cx,452 ;initial number saved
		mov dx,472 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b7:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b7
		
		mov cx,640 ;initial number saved
		mov dx,658 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b8:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b8
		
		mov cx,662 ;initial number saved
		mov dx,680 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b9:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b9
		
		mov cx,684 ;initial number saved
		mov dx,702 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b10:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b10
		
		mov cx,706 ;initial number saved
		mov dx,724 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b11:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b11
		
		mov cx,728 ;initial number saved
		mov dx,746 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b12:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b12
		
		mov cx,750 ;initial number saved
		mov dx,768 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b13:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b13
		
		mov cx,772 ;initial number saved
		mov dx,792 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b14:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b14
		
		mov cx,960 ;initial number saved
		mov dx,978 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b15:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b15
		
		mov cx,982 ;initial number saved
		mov dx,1000 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b16:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b16
		
		mov cx,1004 ;initial number saved
		mov dx,1022 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b17:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b17
		
		
		mov cx,1026 ;initial number saved
		mov dx,1044 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b18:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b18
		jmp endd
		
		mov cx,1048 ;initial number saved
		mov dx,1066 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b19:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b19
		
		mov cx,1070 ;initial number saved
		mov dx,1088 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b20:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b20
		
		mov cx,1092 ;initial number saved
		mov dx,1112 ;final number saved
		mov bx,cx   ;copy of initial number to start the loop
		b21:
		cmp ax,bx
		je yess
		add bx,2
		cmp bx,dx
		jne b21
		
		yess:
		push cx
		push dx
		call vanishBrick
		
		endd:
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 2
		
		vanishBrick:
		push bp
        mov bp, sp	
		push es
		push ax
		push bx
		push cx
		push dx
		push si
		push di
		mov ax, 0xb800
		mov es, ax 
		mov cx,[bp+6]
		mov dx,[bp+4]
		
		mov di,cx
		loopb:
		mov word [es:di],0x0720
		add di,2
		cmp di,dx
		jne loopb
		
mov ax, 25
push ax ; push x position
mov ax, 24
push ax ; push y position

add word[cs:scorenum],2
mov ax, word[cs:scorenum]
push ax ; push attribute
mov ax, score
push ax ; push address of score string
push word [length] ; push score length
call printInfo ; call the printInfo subroutine
mov ax, word[cs:scorenum]
push ax ; push attribute
call printnum
		enddd:
		pop di
		pop si
		pop dx
		pop cx
		pop bx
		pop ax
		pop es
		pop bp
		ret 4
		
		
		
		
printball:push bp
		mov bp, sp	
		push es
		push ax
		push cx
		push si
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov al, 80 ; load al with columns per row
		mul byte [bp+10] ; multiply with y position
		add ax, [bp+12] ; add x position
		shl ax, 1 ; turn into byte offset
		mov di,ax ; point di to required location
		mov [next],di ;store the current value in a global variable
		mov si, [bp+6] ; point si to plate string
		mov cx, [bp+4] ; load length of plate in cx
		mov ah,[bp+8]
		
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10		
	
printplate:push bp
		mov bp, sp	
		push es
		push ax
		push cx
		push si
		push di
		mov ax, 0xb800
		mov es, ax ; point es to video base
		mov al, 80 ; load al with columns per row
		mul byte [bp+10] ; multiply with y position
		add ax, [bp+12] ; add x position
		shl ax, 1 ; turn into byte offset
		mov di,ax ; point di to required location
		mov si, [bp+6] ; point si to plate string
		mov cx, [bp+4] ; load length of plate in cx
		mov ah,1Eh
		cld ; auto increment mode
		
nextchar1: lodsb ; load next char in al
		stosw ; print char/attribute pair
		loop nextchar1 ; repeat for the whole string
		pop di
		pop si
		pop cx
		pop ax
		pop es
		pop bp
		ret 10
					
		
kbisr: push ax
		push es
		mov ax, 0xb800
		mov es, ax ; point es to video memory
		in al, 0x60 ; read a char from keyboard port
		cmp al,0x4B ; is left cursor movement key pressed
		jne nextcmp		
		
		mov dx, [xpos]
		push dx ; push x position
		mov dx, 20
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, plate
		push bx ; push address of plate string
		push word [lengthplate] ; push plate length
		
		call clrplate
		
		mov dx,[xpos]
		cmp dx,[extreme_left]
		je remainSame	
		sub word [xpos],1
		mov dx, [xpos]
		
remainSame:push dx ; push x position
		mov dx, 20
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, plate
		push bx ; push address of plate string
		push word [lengthplate] ; push plate length
		call printplate ; call the printplate subroutine
		jmp nomatch ; leave interrupt routine
		
nextcmp:cmp al,0x4D ; is right cursor movement key pressed

		jne nomatch
		
		mov dx, [xpos]
		push dx ; push x position
		mov dx, 20
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, plate
		push bx ; push address of plate string
		push word [lengthplate] ; push plate length
		
		call clrplate
		
		mov dx,[xpos]
		cmp dx,[extreme_right]
		je remainSame1		
		add word [xpos],1
		mov dx, [xpos]
		
remainSame1:push dx ; push x position
		mov dx, 20
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, plate
		push bx ; push address of plate string
		push word [lengthplate] ; push plate length
		call printplate ; call the printplate subroutine

nomatch:pop es
		pop ax
		jmp far [cs:oldisr] ; call the original ISR		

		
start:	call clrscr 

		;-------------------------------
;brick1
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,0
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,9 ;y2
push ax
call printBrick1
;---------------------------------
;brick2
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,11
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,20 ;y2
push ax
call printBrick1
;---------------------------------
;brick3
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,22
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,31 ;y2
push ax
call printBrick1
;---------------------------------
;brick4
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,33
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,42 ;y2
push ax
call printBrick1
;---------------------------------
;brick5
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,44
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,53 ;y2
push ax
call printBrick1
;---------------------------------
;brick6
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,55
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,64 ;y2
push ax
call printBrick1
;---------------------------------
;brick7
; right coordinates of brick1
mov ax,2  ;x1
push ax
mov ax,66
push ax ; y1
; left coordinates of brick1
mov ax,2 ;x1
push ax
mov ax,76 ;y2
push ax
call printBrick1
;-------------------------------2nd row
;brick8
; right coordinates of brick8
mov ax,4  ;x1
push ax
mov ax,0
push ax ; y1
; left coordinates of brick8
mov ax,4 ;x1
push ax
mov ax,9 ;y2
push ax
call printBrick2
;---------------------------------
;brick9
; right coordinates of brick9
mov ax,4  ;x1
push ax
mov ax,11
push ax ; y1
; left coordinates of brick9
mov ax,4 ;x1
push ax
mov ax,20 ;y2
push ax
call printBrick2
;---------------------------------
;brick10
; right coordinates of brick10
mov ax,4  ;x1
push ax
mov ax,22
push ax ; y1
; left coordinates of brick10
mov ax,4 ;x1
push ax
mov ax,31 ;y2
push ax
call printBrick2
;---------------------------------
;brick11
; right coordinates of brick11
mov ax,4  ;x1
push ax
mov ax,33
push ax ; y1
; left coordinates of brick11
mov ax,4 ;x1
push ax
mov ax,42 ;y2
push ax
call printBrick2
;---------------------------------
;brick12
; right coordinates of brick12
mov ax,4  ;x1
push ax
mov ax,44
push ax ; y1
; left coordinates of brick12
mov ax,4 ;x1
push ax
mov ax,53 ;y2
push ax
call printBrick2
;---------------------------------
;brick13
; right coordinates of brick13
mov ax,4  ;x1
push ax
mov ax,55
push ax ; y1
; left coordinates of brick13
mov ax,4 ;x1
push ax
mov ax,64 ;y2
push ax
call printBrick2
;---------------------------------
;brick14
; right coordinates of brick14
mov ax,4  ;x1
push ax
mov ax,66
push ax ; y1
; left coordinates of brick14
mov ax,4 ;x1
push ax
mov ax,76 ;y2
push ax
call printBrick2
;-------------------------------3rd row
;brick15
; right coordinates of brick15
mov ax,6  ;x1
push ax
mov ax,0
push ax ; y1
; left coordinates of brick15
mov ax,6 ;x1
push ax
mov ax,9 ;y2
push ax
call printBrick3
;---------------------------------
;brick16
; right coordinates of brick16
mov ax,6  ;x1
push ax
mov ax,11
push ax ; y1
; left coordinates of brick16
mov ax,6 ;x1
push ax
mov ax,20 ;y2
push ax
call printBrick3
;---------------------------------
;brick17
; right coordinates of brick17
mov ax,6  ;x1
push ax
mov ax,22
push ax ; y1
; left coordinates of brick17
mov ax,6 ;x1
push ax
mov ax,31 ;y2
push ax
call printBrick3
;---------------------------------
;brick18
; right coordinates of brick18
mov ax,6  ;x1
push ax
mov ax,33
push ax ; y1
; left coordinates of brick18
mov ax,6 ;x1
push ax
mov ax,42 ;y2
push ax
call printBrick3
;---------------------------------
;brick19
; right coordinates of brick19
mov ax,6  ;x1
push ax
mov ax,44
push ax ; y1
; left coordinates of brick19
mov ax,6 ;x1
push ax
mov ax,53 ;y2
push ax
call printBrick3
;---------------------------------
;brick20
; right coordinates of brick20
mov ax,6  ;x1
push ax
mov ax,55
push ax ; y1
; left coordinates of brick20
mov ax,6 ;x1
push ax
mov ax,64 ;y2
push ax
call printBrick3
;---------------------------------
;brick21
; right coordinates of brick21
mov ax,6  ;x1
push ax
mov ax,66
push ax ; y1
; left coordinates of brick21
mov ax,6 ;x1
push ax
mov ax,76 ;y2
push ax
call printBrick3
;---------------------------------
mov ax, 15
push ax ; push x position
mov ax, 24
push ax ; push y position
mov ax, 0 ; parameter to print
push ax ; push attribute
mov ax, level
push ax ; push address of level string
push word [length] ; push level length
call printInfo ; call the printInfo subroutine
;--------------------------------
mov ax, 25
push ax ; push x position
mov ax, 24
push ax ; push y position
mov ax, 0 ; parameter to print
push ax ; push attribute
mov ax, score
push ax ; push address of score string
push word [length] ; push score length
call printInfo ; call the printInfo subroutine

;--------------------------------
mov ax, 35
push ax ; push x position
mov ax, 24
push ax ; push y position
mov ax, 3 ; parameter to print
push ax ; push attribute
mov ax, lives
push ax ; push address of lives string
push word [length] ; push lives length
call printInfo ; call the printInfo subroutine
;------------terminating condition



		mov dx, 30
		push dx ; push x position
		mov dx, 20
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, plate
		push bx ; push address of plate string
		push word [lengthplate] ; push plate length
		call printplate ; call the printplate subroutine
		
		mov dx, [xball]
		push dx ; push x position
		mov dx, [yball]
		push dx ; push y position
		mov dx, 1 ; blue on black attribute
		push dx ; push attribute
		mov bx, ball
		push bx ; push address of plate string
		push word [lengthball] ; push plate length
		call printball ; call the printplate subroutine
		
		
		xor ax, ax
		mov es, ax ; point es to IVT base
		
		mov ax, [es:9*4]
		mov [oldisr], ax ; save offset of old routine
		mov ax, [es:9*4+2]
		mov [oldisr+2], ax ; save segment of old routine
		
		cli ; disable interrupts
		
		mov word [es:9*4], kbisr ; store offset at n*4
		mov [es:9*4+2], cs ; store segment at n*4+2
		mov word[es:8*4],timer
		mov [es:8*4+2],cs
		sti
		jmp $

terminate: mov ax,0x3100
			int 0x21
