org 100h

jmp start

sound_data:
    incbin "kingsv.wav"
    sound_index: dw 0
surp:times 240 dw 0
; bytes at odd indexes indicate the direction of snake 
; Df=-1 for right,1 for left,2 for up ,3 for down
printf:db 1
tick:dw 0
sec:db 0
l:db 'l',0x06,'i',0x06,'v',0x06,'e',0x06,'s',0x06,':',0x06
lives: dw 0x0e03,0x0e03,0x0e03
countL: dw 3
gOver:db 'g',0x40,'a',0x40,'m',0x40,'e',0x40,'o',0x40,'v',0x40,'e',0x40,'r',0x40
timeLeft: db 't',0x06,'i',0x06,'m',0x06,'e',0x06,' ',0x06,'l',0x06,'e',0x06,'f',0x06,'t',0x06,':',0x06
seconds: db 0
minutes: db 4
ticks: dw 0
grf: db 0
rdf: db 0
a: dw 7
m: db 24
c: db 99	
x: db 2
sc:db 's',0x06,'c',0x06,'o',0x06,'r',0x06,'e',0x06
score: dw 0
prevPos: dw 0
speeder: dw 3600
sSize:db 's',0x06,'i',0x06,'z',0x06,'e',0x06,0,0x07
size: dw 0
placed:db 0
placed1:db 0
Winner:db 'y',0x40,'o',0x40,'u',0x40,' ',0x40,'w',0x40,'i',0x40,'n',0x40,' ',0x40

Hurdle: db 15,0x40,15,0x40,15,0x40,15,0x40,15,0x40,15,0x40,15,0x40,15,0x40
Stages:db 'S',0x06,'t',0x06,'a',0x06,'g',0x06,'e',0x06,' ',0x07
stage:dw 0x0e30
soundtick:db 0
LooseF:db 0
stagesec: db 0
attri: db 0x00
stageUp: db 0

message: db '_____________________________________________________________',0
         db '|++++++++   +      +         +          +       +    +++++++ |',0
         db '|+          +  +   +       +    +       +    +       +       |',0
         db '|++++++++   +    + +     + + + + +      +  +         ++++++  |',0
         db '|       +   +      +    +         +     +   +        +       |',0
         db '|++++++++   +      +   +           +    +     +      +++++++ |',0
         db '|Press Any Key To Play.........................              |',0
         db '|____________________________________________________________|',0

PrintMessage:

push ax 
push bx
push cx 
push dx 
push si 
push di 
push es
push si

push word 0xb800
pop es 
mov di,654
mov ax,61
mov bl,8
mul bl

mov cx,ax
mov si,message

mov dx,0
mov ah,0x5e
length:
cld
lodsb
stosw
inc dx
cmp dx,61
jne skipLen
mov dx,0
add si,2
add di,36
skipLen:

loop length
pop si 
pop es
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 

ret

printagain:

cmp byte[stageUp],1
jne skipSup
mov byte[stageUp],0
mov byte[rdf],0
mov byte[grf],0
skipSup:

ret

random:
    push bp
    mov bp,sp
	push ax
	push bx

    mov ah,0
    mov al,[a]
    div byte[m]
    mov byte[a],ah
    cmp byte[a],0
    jne vip
    add byte[a],1
    vip:

	mov bl,[a]
	mov al,[x]
	mul bl
	add al,2
	mov bl,[m]
	div bl
	mov byte [x],ah
	mov al,ah
	mov ah,0
	cmp ax,0
    jne .yeah
    add ax,1
    .yeah:
    mov word[bp+4],ax

	pop bx
	pop ax
    pop bp

ret

upStage:


cmp byte[stagesec],30
jne skipStage
mov byte[stagesec],0
cmp byte[attri],0x00
jne changeattri
mov byte[attri],0x70
jmp ohhhachaaa
changeattri:
mov byte[attri],0x00
ohhhachaaa:
mov byte[stageUp],1
call clearScr
call changeColor 
call drawBorder
call prHr
skipStage:
inc byte[stagesec]

ret


prHr:

    push es 
    push ds 
    push si 
    push di     
    push cx 
    push dx 

    
    mov si,Hurdle 
    
    cmp byte[seconds],20
    jne .special
    mov byte [a],14
    .special:
    mov byte [m],20
    push word 0
    call random
    pop dx
    add dx,2

    mov byte [m],38
    push word 0
    call random
    pop cx
    add cx,2

    push word 0
    push cx
    push dx
    call calPos
    pop di 
    
    mov cx,8

    cld 
    push word 0xb800 
    pop es 

    rep movsw

    mov byte[a],3

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es
ret

prSt:

    push es 
    push ds 
    push si 
    push di     
    push cx 
    push dx 

    mov cx,6

    mov si,Stages 
    mov di, 130
    add di,480
    add di,16
    cld 
    push word 0xb800 
    pop es 

    rep movsw

    mov dx,[stage]
    mov word[es:di],dx

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es


ret

mySound:

push ax 
push bx 
push cx
push dx 

mov dx,22ch
mov al,10h
out dx,al

mov si,[sound_index]
mov al,[sound_data+si]
out dx,al

mov cx,500
.delay:
loop .delay

inc word[sound_index]

cmp word[sound_index],51529
jb skipsound
mov word[sound_index],0
skipsound:

pop dx 
pop cx 
pop bx 
pop ax

ret

note_on:
			; change frequency
			mov dx, ax
			mov al, 0b6h
			out 43h, al
			mov ax, dx
			out 42h, al
			mov al, ah
			out 42h, al
 			; start the sound
			in al, 61h
			or al, 3h
			out 61h, al
	ret

note_off:
			in al, 61h
			and al, 0fch
			out 61h, al			
	ret



placeFruit:

    
    push bp
    mov bp,sp 
    add bp,2
    push ax 
    push bx 
    push cx 
    push dx 
    push di 
    push si 
    push es 
    push ds

    mov al,[sec]
    mul byte[score]
    cmp al,0
    jne haw
    mov byte[a],al
    haw:
    mov ax,[bp+2]
    cmp al,0x01
    jne dont
    add byte[a],12
    dont:
    mov byte [m],58
    push word 0
    call random
    pop cx      ;rows

    mov byte [m],23
    push word 0
    call random
    pop dx      ;columns

    push word 0
    push dx
    push cx
    call calPos
    pop di 

    push word 0xb800
    pop es 
    check:
    cmp byte[es:di],0x20
    je done
    mov byte [m],58
    push word 0
    call random
    pop cx      ;rows

    cmp byte[seconds],20
    jne special
    mov byte [a],54
    special:
    add byte[a],bl
    mov byte [m],23
    push word 0
    call random
    pop dx      ;columns

    push word 0
    push dx
    push cx
    call calPos
    pop di 

   
    ;push ax
    ;call placeFruit 
    ;jmp endfr
    jmp check
    done:
    mov ax,[bp+2]
    mov word[es:di],ax
    cmp ah,0x02
    jne red
    mov byte[grf],1
    jmp endfr
    red:
    mov byte[rdf],1
    endfr:
    pop ds
    pop es 
    pop si 
    pop di 
    pop dx 
    pop cx 
    pop bx 
    pop ax 
    pop bp

ret 2

printnum:
	push bp
	mov bp, sp
	push es
	push ax
	push bx
	push cx
	push dx
	push di
	mov ax, 0xb800
	mov es, ax 
	mov ax, [bp+4] 
	mov bx, 10 
	mov cx, 0 
nextdigit: 
	mov dx, 0
	div bx 
	add dl, 0x30 
	push dx 
	inc cx 
	cmp ax, 0 
	jne nextdigit
nextpos:
	pop dx 
	mov dh, 0x0e
	mov [es:di], dx 
	add di, 2
loop nextpos
	pop di
	pop dx
	pop cx
	pop bx
	pop ax
	pop es
	pop bp
ret 2

prTime:
    push es 
    push ds 
    push si 
    push di     
    push cx 
    push dx 

    mov cx,10

    mov si,timeLeft 
    mov di, 130
    cld 
    push word 0xb800 
    pop es 

    rep movsw


    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es

ret

prL:
    push es 
    push ds 
    push si     
    push di 
    push cx 
    push dx 

    mov cx,[countL]
    sub cx,3
    add cx,9

    mov si,l 
    mov di,122
    add di,340
    cld 
    push word 0xb800 
    pop es 

    rep movsw


    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es

ret

prS:

    push es 
    push ds 
    push si 
    push di 
    push cx 
    push dx 

    mov cx,6
    mov si,sc
    mov di,4
    add di,1570
    add di,160
    cld 
    push word 0xb800 
    pop es 

    rep movsw

    mov ax,[score]
    push ax
    call printnum

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es



ret

prSi:

    push es 
    push ds 
    push si 
    push di 
    push cx 
    push dx 

    mov cx,5
    mov si,sSize
    mov di,4
    add di,1570
    add di,160
    add di,160
    cld 
    push word 0xb800 
    pop es 

    rep movsw

    push ds
    mov ax,surp
    push ax 
    call strLen

    mov bl,2
    div bl
    mov word[size],ax
    push ax
    call printnum

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es


ret

prW:
    push es 
    push ds 
    push si 
    push di 
    push cx 
    push dx 

    mov cx,8
    mov si,Winner 
    mov di,4
    add di,1570
    cld 
    push word 0xb800 
    pop es 

    rep movsw

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es

ret


prG:
    push es 
    push ds 
    push si 
    push di 
    push cx 
    push dx 

    mov cx,8
    mov si,gOver 
    mov di,4
    add di,1570
    cld 
    push word 0xb800 
    pop es 

    rep movsw

    pop dx 
    pop cx 
    pop di 
    pop si 
    pop ds 
    pop es

ret


sound:

    ;modified code after taking this from
    ;http://muruganad.com/8086/8086-assembly-language-program-to-play-sound-using-pc-speaker.html

    push ax 
    push bx 
    push cx 
    push dx 


    mov al,182
    out 43h,al

    mov ax,4560

    out 42h,al

    mov al,ah 
    out 42h,al

    in al,61h

    or al,11b
    out 61h,al

    mov bx,25
    pause1:
    mov cx,0x0aaa
    pause2:
    dec cx 
    jne pause2
    dec bx 
    jne pause1 
    in al,61h 

    and al,11111100b

    out 61h,al

    pop dx 
    pop cx 
    pop bx 
    pop ax

ret

changeColor:
    push ax
    push cx 
    push es 
    push di
    push bx

    push word 0xb800
    pop es
    mov al,[attri]
    mov cx,1500
    mov di,1
    mov ah,0
   cld
   change:
    inc ah
    mov al,[es:di]
    and al,0x0f
    add al,[attri]

    stosb
    add di,1
    cmp ah,61
    jl nextline
    mov ah,0
    add di,38
    nextline:
   loop change
    pop bx
    pop di 
    pop es 
    pop cx 
    pop ax

ret

drawBorder:

    push ax
    push cx 
    push di 
    push es 
    push si

    push word 0xb800
    pop es

    mov di,0
    mov ax,0x0705
    mov cx,60
    cld
    rep stosw

    mov di,160
    mov si,278
    mov cx,25
    line:
    mov word[es:di],ax
    mov word[es:si],ax
    add di,160
    add si,160
    loop line
     mov di,3840
    cld
    mov cx,60
    rep stosw

    pop si
    pop es 
    pop di 
    pop cx 
    pop ax


ret

lifeLoss:

    push ax
    push ds 
    push si
    push cx 
    push es 
    push di 


    push ds 
    push word surp
    call strLen

    mov cx,ax
    mov di,surp
    mov ax,0
    push ds
    pop es  
    cld
    rep stosw


    pop di
    pop es
    pop cx
    pop si 
    pop ds 
    pop ax

ret

StopWatch:
	dec byte [seconds]
	cmp byte [seconds],-1
	jne endStopWatch
	mov byte [seconds],59
	dec byte [minutes]
endStopWatch:
ret

apprEx:

push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 
push ds 


push ds
mov ax,surp
push ax 
call strLen

mov cx,ax
sub cx,2

mov bx,surp
add bx,cx
mov bx,[bx]
mov bh,0

push bx
call ExtendSnake


pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 


ret

timeupd:
cmp word[ticks],3600
jne skipSec
mov word[ticks],0
call StopWatch
call upStage
cmp byte[sec],20
jne speed
cmp byte[minutes],1
je prespeed
cmp byte[LooseF],0
jne prespeed
call prHr
prespeed:
inc word[stage]
mov byte[sec],0
shr word[speeder],1
cmp word[speeder],200
jg speed
mov word[speeder],200
speed:
inc byte[sec]
skipSec:
inc word[ticks]
jmp back
iffr:
mov ah,0x02
mov al,[placed]
cmp word[es:di],ax
jne iffg
mov byte[grf],0
inc byte[score]
call apprEx
jmp retur
iffg:
mov ah,0x04
mov al,[placed1]
cmp word[es:di],ax
jne retur
call sound
dec word[countL]
mov dx,0x0117
mov cx,0x00ff
call lifeLoss
call initgame
jmp retur 

timer:

push ax
push di 
push es
push bx


call prTime
jmp timeupd
retur: jmp traversal
back:
inc word[tick]

        mov bx,[speeder]
        cmp word[tick],bx
            jne longj

            cmp word[countL],0
                je longj
                    call prL
                    call prS
                    call prSi
                    call prSt
                    mov word[tick],0

                            push word 0
                            push dx
                            push cx
                            call nextPos
                            pop di

                        push word 0xb800
                        pop es
                        cmp word[es:di],0x6e0f
                        jne notOver
                        call sound
                        dec word[countL]
                        mov dx,0x0117 
                        mov cx,0x00ff
                        call lifeLoss
                        call initgame
                        jmp notOver
                        longj: jmp longj2
                        notOver:
                        cmp word[es:di],0x400f
                        jne negOver
                        call sound
                        dec word[countL]
                        mov dx,0x0117 
                        mov cx,0x00ff
                        call lifeLoss
                        call initgame
                        negOver:
                        jmp iffr
                        traversal:
                        push dx
                        push cx
                        push word surp 
                        call moveSurp
                        call printagain                        
                        skiprint:

                        mov byte[printf],1
	                    mov ah,0
	                    mov al,[seconds]
	                    mov di,156
	                    push ax
	                    call printnum
    
                         mov al,[minutes]
	                    sub di,4
	                    push ax
	                    call printnum

cmp cl,-1
jne righthand

cmp dl,58
jl skipr

call sound
dec word[countL]
mov dx,0x0117
mov cx,0x00ff
call lifeLoss
call initgame
jmp down
skipr:
inc dl
longj2:
jmp down
righthand:
cmp cl,1
jne lefthand

cmp dl,1
jg skipl

call sound
dec word[countL]
mov dx,0x0117
mov cx,0x00ff
call lifeLoss
call initgame


jmp down

skipl:
dec dl
jmp down
lefthand:
cmp cl,2
jne upper

cmp dh,2
jge skipu

call sound
dec word[countL]
mov dx,0x0117
mov cx,0x00ff
call lifeLoss
call initgame

jmp down

skipu:
dec dh
jmp down
upper:
cmp cl,3 
jne down


cmp dh,23
jl skipd

call sound
dec word[countL]
mov dx,0x0117
mov cx,0x00ff
call lifeLoss
call initgame

jmp down
skipd:
inc dh
down:


    cmp byte[grf],0
        jne noprint
            mov byte[m],10
                mov al,[size]
            add al,[seconds]
            mov byte[a],al
            push word 0
            call random
        pop ax
            mov ah,0x02
            mov byte[placed],al
            push ax
        call placeFruit
    noprint:

    cmp byte[rdf],0
        jne noprintred
        mov byte[m],6
        mov al,[score]
        add al,[ticks]
        mov byte[a],al
        push word 0
        call random
        pop ax
        mov ah,0x04
        mov byte[placed1],al
        push ax
        call placeFruit
    noprintred:

    cmp byte[seconds],10
    jge .skipthis
    cmp byte[es:158],0x30
    jne .skipthis
	mov al,' '
	push 0xb800
	pop es
	mov di,158
	stosb
    .skipthis:

cmp word[countL],0
je endall

cmp byte[minutes],0
jne this

cmp byte[seconds],0
je endall

this:
call prTime
cmp byte[LooseF],1
jne skipmsg
call prG
skipmsg:
call mySound
mov al,0x20
out 0x20,al

pop bx
pop es
pop di
pop ax

iret

endall:
mov word[countL],0
cmp word[size],480
jl win
mov byte[LooseF],1
call prW
jmp this
win:
call prG
jmp this

clearScr:
    push ax
    push cx 
    push es 
    push di

    push word 0xb800
    pop es
    mov ax,0x0720
    mov cx,2000
    mov di,0
    cld 
    rep stosw

    pop di 
    pop es 
    pop cx 
    pop ax

ret

strLen:

    push bp
    mov bp,sp

    push es
    push cx 
    push di 

    les di,[bp+4]
    mov cx,0xffff

    xor al,al 

    repne scasb 

    mov ax,0xffff
    sub ax,cx
    dec ax 

    pop di 
    pop cx 
    pop es 
    pop bp

ret 4

initgame:
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push ds 
push es

mov word[surp],0

push word 0xb800
pop es
mov ax,0x0720
mov ah,[attri]
mov di,0
cld
mov cx,2000
rep stosw

;after clearing screen

mov cx,6
cons:

push word 0x00ff     ;initially direction towards right
call ExtendSnake

loop cons

call drawBorder

    push ds
    mov ax,surp
    push ax 
    call strLen
    mov bx,surp
    add bx,ax
    sub bx,2
    mov word[bx],0

push word 0 ;initial direction right
push word 206;intitial position
call printSnake


mov byte[grf],0
mov byte[rdf],0
mov word[stage],0x0e30
push word 0
mov byte[m],6
mov al,[size]
mov byte[c],al
call random
pop ax
mov ah,0x02
mov byte[placed],al
push ax
call placeFruit


pop es 
pop ds 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax

ret 


ExtendSnake:
push bp
mov bp,sp
add bp,2
push es 
push ds
push si 
push di 
push cx
push ax 
push bx 


push ds
mov ax,surp
push ax 
call strLen

mov dx,[bp+2]
mov dh,15
cmp ax,0
je skip
mov bx,ax

mov cx,4
yes:
mov word[bx+surp],dx
add bx,2
loop yes
mov word[surp+bx],0
jmp end
skip:
mov dh,16
mov word[surp],dx

mov word[surp+2],0
;1 interesting 
;30 up
;16 right
;17 left
;31 down

end:
pop bx
pop ax
pop cx 
pop di 
pop si 
pop ds 
pop es 
pop bp

ret 2

printSnake:
push bp
mov bp,sp
add bp,2
push ax 
push bx 
push cx 
push dx 
push si 
push di 
push ds 
push es 


mov di,[bp+2]
mov dx,[bp+4]

mov ax,surp
push ds
push ax 
call strLen
mov cx,ax
mov ax, 0xb800
mov es,ax
shr cx,1
mov bx,surp
mov si,0

cmp dl,-1
jne noright
mov word[surp],0x1000
noright:
cmp dl,1
jne noleft
mov word[surp],0x1100
noleft:
cmp dl,2
jne noup
mov word[surp],0x1d00
noup:
cmp dl,3
jne nothing
mov word[surp],0x1e00
nothing:

mov ax,[surp]
mov al,ah
mov ah,0x6e
mov word[es:di],ax

sub di,2
mov si,3
sub cx,1
s:
;0xaa 
;0xab
mov ah,0x6e
mov al,[surp+si]
mov word[es:di],ax
add si,2
sub di,2
loop s



pop es 
pop ds 
pop di 
pop si 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp

ret 4 

nextPos:

push bp 
mov bp,sp
add bp,2

push ax 
push bx 
push cx 
push dx 
push di 
push si 
push ds 
push es 


mov dx,[bp+2]   ;direction
mov cx,[bp+4]   ;current pos

cmp dl,-1
jne .ifleft

inc cl

.ifleft:
cmp dl,1
jne .ifUp

dec cl

.ifUp:
cmp dl,2
jne .ifDown

dec ch

.ifDown:
cmp dl,3
jne .nodir

inc ch

.nodir:

mov dh,0
mov dl,cl
mov cl,ch
xor ch,ch

push word 0
push cx
push dx
call calPos
pop di
mov [bp+6],di

pop es 
pop ds 
pop si 
pop di 
pop dx 
pop cx 
pop bx 
pop ax 
pop bp

ret 4

moveSurp:

push bp
mov bp,sp 
add bp,2

push ax 
push bx 
push cx 
push dx 
push si 
push di 
push es 
push ds 

mov bx,[bp+2]   ; offset of char with direction
mov ax,[bx]     ; char with direction
mov dx,[bp+4]   ; new direction
mov cx,[bp+6]   ;current rowno in higher byte,current columnno in lower byte

cmp ah,0
je endrecurs

push word 0xb800
pop es

jmp funcbody

endrecurs:
mov word[prevPos],cx
cmp al,-1
jne negoright
cmp cl,0
jle odafa
dec cl
negoright:
cmp al,1
jne negoleft
cmp cl,79
jge odafa
inc cl
negoleft:
cmp al,2
jne negoup
cmp ch,24
jge odafa
inc ch
negoup:
cmp al,3
jne odafa
cmp ch,0
jle odafa
dec ch
odafa:

mov ax,0x0720
mov ah,[attri]
add ah,7
mov dl,cl
mov dh,0
mov cl,ch
xor ch,ch

push word 0
push cx
push dx
call calPos
pop di
mov word[es:di],ax
jmp endfunc


funcbody:
cmp dl,-1
jne ifleft

inc cl

ifleft:
cmp dl,1
jne ifUp

dec cl

ifUp:
cmp dl,2
jne ifDown

dec ch

ifDown:
cmp dl,3
jne nodir

inc ch

nodir:

push ax
push cx
mov al,cl
mov ah,0
mov cl,ch
mov ch,0
;now ax has cols and cx has rows
push word 0
push cx
push ax
call calPos 

pop di  ;offset
pop cx
pop ax

push ax 
mov al,ah
mov ah,0x6e
cmp al,[surp+1]
jne .nothing
cmp dl,-1
jne .noright
mov al,16
.noright:
cmp dl,1
jne .noleft
mov al,17
.noleft:

cmp dl,2
jne .noup
mov al,30
.noup:
cmp dl,3
jne .nothing
mov al,31
.nothing:

mov word [es:di],ax
pop  ax

mov byte[bx],dl


mov ah,0
mov dx,ax
add bx,2

mov cx,[bp+6]
mov ax,[bx]
cmp dl,-1
jne negright
dec cl
negright:
cmp dl,1
jne negleft
inc cl
negleft:
cmp dl,2
jne negup
inc ch
negup:
cmp dl,3
jne dafa
dec ch
dafa:


push cx
push dx
push bx
call moveSurp
endfunc:


pop ds 
pop es 
pop di 
pop si 
pop dx 
pop cx
pop bx 
pop ax 
pop bp

ret 6

calPos:
push bp
mov bp,sp
add bp,2 
push ax 
push bx 
push cx
push dx


mov ax,0
mov bx,0 
mov ax,[bp+4]
mov bx,80
mul bx
mov bx,[bp+2]
add ax,bx 
mov bx,0x02
mul bx
mov word[bp+6],ax
pop dx 
pop cx
pop bx 
pop ax 
pop bp 

ret 4


kbisr

;current position(row,column) is given through main in dx
;prev direction in cx
push ax
push bx
push si 
push di 
push es 
push ds 

in al,0x60

cmp al,0x4d
jne itsleft
cmp byte[surp],1
je ignoreR
mov cx,0x00ff
jmp itsnothing
ignoreR:
jmp itsnothing
itsleft:
cmp al,0x4b
jne itsup
cmp byte[surp],-1
je ignoreL
mov cx,0x0001
jmp itsnothing
ignoreL:
jmp itsnothing
itsup:

cmp al,0x48
jne itsdown
cmp byte[surp],3
je ignoreUp
mov cx,0x0002
jmp itsnothing
ignoreUp:
jmp itsnothing
itsdown:
cmp al,0x50
jne itsnothing
cmp byte[surp],2
je ignoreD
mov cx,0x0003
jmp itsnothing
ignoreD:
itsnothing:


mov al,0x20
out 0x20,al

pop ds 
pop es 
pop di 
pop si 
pop bx 
pop ax

iret

start:

call clearScr
call PrintMessage

mov ah,0
int 0x16
cmp ah,0
je start

call initgame
mov dx,0x0117

;l10:
;push ax
;push word 0x0003    ;down
;push word surp
;call moveSurp
;inc ah
;loop l10
;mov cx,10
;l11:
;push ax
;push word 0x00ff    ;right
;push word surp
;call moveSurp
;inc al
;loop l11

mov ax,331
out 0x40,al
mov al,ah
out 0x40,al

xor ax, ax
mov es, ax

cli
mov word [es:9*4],kbisr
mov [es:9*4+2],cs
mov word [es:8*4],timer
mov [es:8*4+2],cs
sti

jmp $
mov ah,4ch
int 0x21