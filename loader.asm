%include "inc.asm"

org 0x9000

jmp ENTRY_SEGMENT

[section .gdt]								; 全局描述符表，部分段基址暂未知地址需使用时再调节
; GDT definition
;						"函数名"	段基址		段界限					段属性
GDT_ENTRY		:		Descriptor	0,			0,						0				; 全局段描述符表第0项不使用
CODE32_DESC		:		Descriptor	0,			Code32SegLen - 1,		DA_C    + DA_32 + DA_DPL0
VIDEO_DESC		:		Descriptor	0xB8000,	0x07FFF,				DA_DRWA + DA_32
DATA32_DESC		:		Descriptor	0,			Data32SegLen - 1,		DA_DR   + DA_32 + DA_DPL3
STACK32_DESC	:		Descriptor	0,			TopOfStack32,			DA_DRW  + DA_32 + DA_DPL0
; GDT end

GdtLen	equ	$ - GDT_ENTRY

GdtPtr:										; 全局描述符表指针
		dw	GdtLen - 1	; 偏移，记录描述符数量
		dd	0			; 全局描述符起始地址，先初始化为0


; GDT Selector
;							TI：全局、局部	RPL：请求权限级别
Code32Selector	equ	(0x0001 << 3) + SA_TIG + SA_RPL0	; 0x0001==第二个选择子
VideoSelector	equ (0x0002 << 3) + SA_TIG + SA_RPL0
Data32Selector	equ (0x0003 << 3) + SA_TIG + SA_RPL0
Stack32Selector	equ (0x0004 << 3) + SA_TIG + SA_RPL0

; end of [section .gdt]

TopOfStack16 equ 0x7c00

[section .dat]								; 32位数据段
[bits 32]
DATA32_SEGMENT:
	DTOS				db "D.T.OS!", 0	; 注意添加字符串结束标记0
	DTOS_OFFSET			equ DTOS - $$

Data32SegLen equ $ - DATA32_SEGMENT

[section .s16]	; 实模式代码段（16bit）
[bits 16]		; 使用16位编译
ENTRY_SEGMENT:								; 16位保护模式入口段
    mov ax, cs					; 初始化相关寄存器
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, TopOfStack16

			; initialize GDT for 32 bits code segment
	mov esi, CODE32_SEGMENT		; 初始化32位保护模式32位代码段描述符
	mov edi, CODE32_DESC

	call InitDescItem
		
	mov esi, DATA32_SEGMENT		; 初始化32位保护模式32位数据段描述符
	mov edi, DATA32_DESC

	call InitDescItem

	mov esi, STACK32_SEGMENT	; 初始化32位保护模式32位栈段描述符
	mov edi, STACK32_DESC

	call InitDescItem

			; initialize GDT pointer struct
	mov eax, 0					; 代码段地址左移4位
	mov ax, ds
	shl eax, 4
	add eax, GDT_ENTRY			; 代码段偏移地址==> 左移过后的代码段+全局描述符表入口地址偏移量
	mov dword [GdtPtr + 2], eax	; 写入全局描述符表指针

			; 1. load GDT
	lgdt [GdtPtr]				; 加载全局描述符表

			; 2. close interrupt
	cli							; 关闭中断

			; 3. open A20
	in al, 0x92					; 通过0x92端口开启A20地址线开关
	or al, 00000010b
	out 0x92, al

			; 4. enter protect mode
	mov eax, cr0				; 设置cr0寄存器，进入保护模式
	or eax, 0x01
	mov cr0, eax

			; 5. jump to 32 bits code
	jmp dword Code32Selector : 0 ; 使用jmp跳转到32位代码段选择子的0偏移处


; esi	--> code segment label
; edi	--> descriptor label
InitDescItem:								; 初始化描述符项目
	push eax
	
	mov eax, 0						; 代码段地址左移4位
	mov ax, cs
	shl eax, 4						; 实地址=段寄存器地址左移4位+偏移地址
	add eax, esi
	mov word [edi + 2], ax			; 将段基址写入描述符2个字节（16位寄存器），低32位的16-31bit（偏移2字节）
	shr eax, 16						; 移除eax实地址中已经写入段基址的2字节数据
	mov byte [edi + 4], al			; 将段基址写入描述符1个字节(8位寄存器)，高32位的0-7bit（偏移4+0=4字节）
	mov byte [edi + 7], ah			; 将段基址写入描述符1个字节(8位寄存器)，高32位的24-31bit（偏移4+3=7字节）

	pop eax
	
	ret

[section .s32]	; 32位代码段
[bits 32]		; 使用32位编译
CODE32_SEGMENT:								; 32位代码段数据
	mov ax, VideoSelector			; 把视频段选择子放到gs全局段寄存器
	mov gs, ax

	mov ax, Stack32Selector
	mov ss, ax

	mov eax, TopOfStack32
	mov esp, eax

	mov ax, Data32Selector
	mov ds, ax

	mov ebp, DTOS_OFFSET				; 在保护模式输出字符串，地址为段偏移
	mov bx, 0x0C					; 黑底淡红字
	mov dh, 12						; 指定行地址，注意行列都是从0开始
	mov dl, 33						; 指定列地址

	call PrintString

	jmp $

; ds:ebp	--> string address
; bx		--> attribute
; dx		--> dh : row, dl : col
PrintString:								; 打印字符串函数
	push ebp
	push eax
	push edi
	push cx
	push dx

print:
	mov cl, [ds:ebp]				; cl记录要打印的字符
	cmp cl, 0						; 对比字符串结束符号
	je end
	mov eax, 80						; 每行字符数
	mul dh							; 乘以行数
	add al, dl						; 加上列数，最终计算出要显示的位置
	shl eax, 1						; 左移乘以2，计算字节偏移
	mov edi, eax					; 写入显示的偏移地址到edi
	mov ah, bl						; 字符属性写入高位
	mov al, cl						; 字符写入低位
	mov [gs:edi], ax				; 写入显存对应地址
	inc ebp							; 指向下一个字符
	inc dl							; 指向屏幕的下一列
	jmp print

end:
	pop dx
	pop cx
	pop edi
	pop eax
	pop ebp

	ret

Code32SegLen	equ	$ - CODE32_SEGMENT

[section .gs]
[bits 32]
STACK32_SEGMENT:							; 32位栈段定义
	times 1024 * 4 db 0

Stack32SegLen equ $ - STACK32_SEGMENT
TopOfStack32 equ Stack32SegLen - 1
