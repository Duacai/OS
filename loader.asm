%include "inc.asm"

org 0x9000

jmp CODE16_SEGMENT

[section .gdt]	; 全局描述符表
; GDT definition
;									段基址	段界限					段属性
GDT_ENTRY		:		Descriptor	0,		0,						0
CODE32_DESC		:		Descriptor	0,		Code32SegLen - 1,		DA_C + DA_32
; FDT end

GdtLen	equ	$ - GDT_ENTRY

GdtPtr:					; 全局描述符表指针
		dw	GdtLen - 1	; 偏移，记录描述符数量
		dd	0			; 全局描述符起始地址，先初始化为0


; GDT Selector

Code32Selector	equ	(0x0001 << 3) + SA_TIG + SA_RPL0	; 0x0001==第二个选择子

; end of [section .gdt]


[section .s16]	; 实模式代码段（16bit）
[bits 16]		; 使用16位编译
CODE16_SEGMENT:
    mov ax, cs	; 初始化相关寄存器
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, 0x7c00

	; initialize GDT for 32 bits code segment
	mov eax, 0						; eax清0
	mov ax, cs						; cs寄存器放入ax寄存器
	shl eax, 4						; 左移4位
	add eax, CODE32_SEGMENT			; 32位段基址 ==> cs代码段左移4位加上32位段的偏移
	mov word [CODE32_DESC + 2], ax	; 将ax寄存器的32位段基址写入描述符的低32位的16-31bit(2字节处)
	shr eax, 16						; 将eax右移2字节（上面已经写入了2字节）
	mov byte [CODE32_DESC + 4], al	; 将al寄存器的32位段基址写入描述符的高32位的0-7bit(4字节处)
	mov byte [CODE32_DESC + 7], ah	; 将ah寄存器的32位段基址写入描述符的高32位的24-31bit(7字节处)

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

[section .s32]	; 32位代码段
[bits 32]		; 使用32位编译
CODE32_SEGMENT:	; 32位代码段数据
	mov eax, 0 
	jmp CODE32_SEGMENT

Code32SegLen	equ	$ - CODE32_SEGMENT
