%include "inc.asm"

org 0x9000

jmp CODE16_SEGMENT

[section .gdt]	; 全局描述符表
; GDT definition
;									段基址	段界限					段属性
GDT_ENTRY		:		Descriptor	0,		0,						0
CODE32_DESC		:		Descriptor	0,		Code32SegLen - 1,		DA_C + DA_32
VIDEO_DESC		:		Descriptor	0xB8000,0x07FFF,				DA_DRWA + DA_32
DATA32_DESC		:		Descriptor	0,		Data32SegLen - 1,		DA_DR + DA_32
STACK_DESC		:		Descriptor	0,		TopOfStackInit,			DA_DRW + DA_32
; GDT end

GdtLen	equ	$ - GDT_ENTRY

GdtPtr:					; 全局描述符表指针
		dw	GdtLen - 1	; 偏移，记录描述符数量
		dd	0			; 全局描述符起始地址，先初始化为0


; GDT Selector

Code32Selector	equ	(0x0001 << 3) + SA_TIG + SA_RPL0	; 0x0001==第二个选择子
VideoSelector	equ (0x0002 << 3) + SA_TIG + SA_RPL0
Data32Selector	equ (0x0003 << 3) + SA_TIG + SA_RPL0
StackSelector	equ (0x0004 << 3) + SA_TIG + SA_RPL0

; end of [section .gdt]

TopOfStackInit equ 0x7c00

[section .dat]						; 32位数据段
[bits 32]
DATA32_SEGMENT:
	DTOS				db "D.T.OS!", 0
	DTOS_OFFSET			equ DTOS - $$
	HELLO_WORLD			db "Hello World!", 0
	HELLO_WORLD_OFFSET	equ HELLO_WORLD - $$

Data32SegLen equ $ - DATA32_SEGMENT

[section .s16]	; 实模式代码段（16bit）
[bits 16]		; 使用16位编译
CODE16_SEGMENT:
    mov ax, cs	; 初始化相关寄存器
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, TopOfStackInit

	; initialize GDT for 32 bits code segment
	mov esi, CODE32_SEGMENT
	mov edi, CODE32_DESC

	call InitDescItem
		
	mov esi, DATA32_SEGMENT
	mov edi, DATA32_DESC

	call InitDescItem

	; initialize GDT pointer struct
	mov eax, 0						; 代码段地址左移4位
	mov ax, ds
	shl eax, 4
	add eax, GDT_ENTRY				; 代码段偏移地址==> 左移过后的代码段+全局描述符表入口地址偏移量
	mov dword [GdtPtr + 2], eax		; 写入全局描述符表指针

	; 1. load GDT
	lgdt [GdtPtr]					; 加载全局描述符表

	; 2. close interrupt
	cli								; 关闭中断

	; 3. open A20
	in al, 0x92						; 通过0x92端口开启A20地址线开关
	or al, 00000010b
	out 0x92, al

	; 4. enter protect mode
	mov eax, cr0					; 设置cr0寄存器，进入保护模式
	or eax, 0x01
	mov cr0, eax

	; 5. jump to 32 bits code
	jmp dword Code32Selector : 0	; 使用jmp跳转到32位代码段选择子的0偏移处


; esi	--> code  segment label
; edi	--> descriptor label
InitDescItem:						; 初始化描述符
	push eax
	
	mov eax, 0						; 代码段地址左移4位
	mov ax, cs
	shl eax, 4
	add eax, esi
	mov word [edi + 2], ax			; 
	shr eax, 16
	mov byte [edi + 4], al
	mov byte [edi + 7], ah

	pop eax
	
	ret


[section .s32]	; 32位代码段
[bits 32]		; 使用32位编译
CODE32_SEGMENT:	; 32位代码段数据
	mov ax, VideoSelector			; 把视频段放到gs寄存器
	mov gs, ax

	mov ax, StackSelector
	mov ss, ax

	mov ax, Data32Selector
	mov ds, ax

	mov ebp, DTOS_OFFSET
	mov bx, 0x0C					; 黑底红字
	mov dh, 12						; 指定行地址
	mov dl, 33						; 指定列地址

	call PrintString

	mov ebp, HELLO_WORLD_OFFSET
	mov bx, 0x0C
	mov dh, 13
	mov dl, 31

	call PrintString

	jmp $

; ds:ebp	--> string address
; bx		--> attribute
; dx		--> dh : row, dl : col
PrintString:
	push ebp
	push eax
	push edi
	push cx
	push dx
print:								; 打印字符串函数
	mov cl, [ds:ebp]
	cmp cl, 0						; 对比字符串结束符号
	je end
	mov eax, 80						; 每行字符数
	mul dh							; 乘以行数
	add al, dl						; 加上列数，最终计算出要显示的位置
	shl eax, 1						; 左移乘以2，计算字节偏移
	mov edi, eax					; 写入显示的偏移地址到edi
	mov ah, bl						; 字符显示属性
	mov al, cl						; 字符
	mov [gs:edi], ax				; 写入显存对应地址
	inc ebp							; 指向下一个字符
	inc dl							; 指向下一个显示的位置
	jmp print

end:
	pop dx
	pop cx
	pop edi
	pop eax
	pop ebp
	ret

Code32SegLen	equ	$ - CODE32_SEGMENT
