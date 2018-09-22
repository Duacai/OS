%include "inc.asm"							; 加载头文件，一些常量、设置函数

org 0x9000									; 内存加载地址

jmp ENTRY_SEGMENT							; 跳转到ENTRY_SEGMENT入口处

[section .gdt]								; 全局描述符表，部分段基址未知基址需使用时再调节（InitDescItem）
; GDT definition
;						"函数名"	段基址		段界限					段属性
GDT_ENTRY		:		Descriptor	0,			0,						0				; 全局段描述符表第0项不使用
CODE32_DESC		:		Descriptor	0,			Code32SegLen - 1,		DA_C    + DA_32	+ DA_DPL0
VIDEO_DESC		:		Descriptor	0xB8000,	0x07FFF,				DA_DRWA + DA_32	+ DA_DPL3	; 视频段描述符表设在正确无须初始化
DATA32_DESC		:		Descriptor	0,			Data32SegLen - 1,		DA_DR	+ DA_32	+ DA_DPL0
STACK32_DESC	:		Descriptor	0,			TopOfStack32,			DA_DRW  + DA_32	+ DA_DPL0
FUNCTION_DESC	:		Descriptor	0,			FunctionSegLen -1,		DA_C	+ DA_32 + DA_DPL0
TASK_A_LDT_DESC	:		Descriptor	0,			TaskALdtLen	 - 1,		DA_LDT	+ 		  DA_DPL0
TSS_DESC		:		Descriptor	0,			TSSLen - 1,				DA_386TSS + 	  DA_DPL0
; Call Gate
;										选择子				偏移			参数个数		属性
FUNC_PRINTSTRING_DESC	:	Gate	FunctionSelector,	PrintString,	0,			DA_386CGate + DA_DPL3
; GDT end

GdtLen	equ	$ - GDT_ENTRY

GdtPtr:										; 全局描述符表指针
		dw	GdtLen - 1	; 偏移，记录描述符数量
		dd	0			; 全局描述符起始地址，先设置为0


; GDT Selector
;								TI：全局、局部	RPL：请求权限级别
Code32Selector		equ	(0x0001 << 3) + SA_TIG + SA_RPL0	; 0x0001==第二个选择子
VideoSelector		equ (0x0002 << 3) + SA_TIG + SA_RPL3	; 显存特权级低只会影响显示，对系统安全无影响
Data32Selector		equ (0x0003 << 3) + SA_TIG + SA_RPL0
Stack32Selector		equ (0x0004 << 3) + SA_TIG + SA_RPL0
FunctionSelector	equ (0x0005 << 3) + SA_TIG + SA_RPL0
TaskALdtSelector	equ (0x0006 << 3) + SA_TIG + SA_RPL0
TSSSelector			equ (0x0007 << 3) + SA_TIG + SA_RPL0
; Gate Selector
FuncPrintStringSelector		equ (0x0008 << 3) + SA_TIG + SA_RPL3

; end of [section .gdt]

[section .tss]					; TSS任务段104字节，另可附加额外信息
[bits 32]
TSS_SEGMENT:
		dd	0					; 保留前一个TSS段选择子，由CPU填写，高位填0
		dd	TopOfStack32		; 0特权级栈指针
		dd	Stack32Selector		; 0特权级栈段选择子，只能用低2个字节，高位填0
		dd	0					; 1特权级
		dd	0					;
		dd	0					; 2特权级
		dd	0					;
		times 4 * 18 dd 0		; 用于切换寄存器的值，由CPU填写，共18个dd类型
		dw	0					; 最低位为调试陷阱标志T，其余为0
		dw	$ - TSS_SEGMENT + 2	; I/O Map Base Address
		db	0xFF				; 结束标记，属于额外信息
		
TSSLen	equ	$ - TSS_SEGMENT

TopOfStack16 equ 0x7c00

[section .s16]	; 实模式代码段（16bit）
[bits 16]		; 使用16位编译
ENTRY_SEGMENT:								; 16位保护模式入口段
    mov ax, cs					; 初始化相关寄存器
	mov ds, ax
	mov es, ax
	mov ss, ax
	mov sp, TopOfStack16

			; initialize GDT for 32 bits code segment
	mov esi, CODE32_SEGMENT		; 初始化32位代码段、数据段、栈段描述符
	mov edi, CODE32_DESC

	call InitDescItem
	
	mov esi, DATA32_SEGMENT
	mov edi, DATA32_DESC

	call InitDescItem
		
	mov esi, STACK32_SEGMENT
	mov edi, STACK32_DESC

	call InitDescItem
	; 视频段描述符表设在正确无须初始化
	mov esi, FUNCTION_SEGMENT	; 初始化函数段描述符
	mov edi, FUNCTION_DESC
	
	call InitDescItem
	
	mov esi, TASK_A_LDT_ENTRY	; 初始化任务A局部描述符表
	mov edi, TASK_A_LDT_DESC

	call InitDescItem
	
	mov esi, TASK_A_CODE32_SEGMENT	; 初始化任务A代码段、数据段、栈段局部描述符
	mov edi, TASK_A_CODE32_DESC

	call InitDescItem
	
	mov esi, TASK_A_DATA32_SEGMENT
	mov edi, TASK_A_DATA32_DESC

	call InitDescItem
	
	mov esi, TASK_A_STACK32_SEGMENT
	mov edi, TASK_A_STACK32_DESC

	call InitDescItem
	
	mov esi, TSS_SEGMENT		; 初始化TSS任务段
	mov edi, TSS_DESC

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

[section .dat]
[bits 32]
DATA32_SEGMENT:
	DTOS			db	"D.T.OS!", 0
	DTOS_OFFSET		equ	DTOS - $$
	
Data32SegLen equ $ - DATA32_SEGMENT

	
[section .s32]	; 32位代码段
[bits 32]		; 使用32位编译
CODE32_SEGMENT:								; 32位代码段数据
	mov ax, VideoSelector			; 把视频段选择子放到gs全局段寄存器
	mov gs, ax

	mov ax, Data32Selector			; 设置数据段地址
	mov ds, ax
	
	mov ax, Stack32Selector			; 设置栈段地址
	mov ss, ax

	mov eax, TopOfStack32			; 设置32位栈顶地址
	mov esp, eax

	mov ebp, DTOS_OFFSET			; 全局函数打印字符串，使用选择子：偏移量调用
	mov bx, 0x0c
	mov dh, 12
	mov dl, 33
	
	call FunctionSelector : PrintString
	
	mov ax, TSSSelector				; 加载TSS任务状态段
	
	ltr ax
	
	mov ax, TaskALdtSelector		; 加载局部描述符表
	
	lldt ax
	
	push TaskAStack32Selector		; retf远调用，将栈段选择子、栈顶指针、代码段选择子、代码段偏移量压入栈中
	push TaskATopOfStack32
	push TaskACode32Selector
	push 0
	retf							; pop IP(EIP)，POP CS << == >> retf CS:IP(EIP)
	
	jmp $

Code32SegLen	equ		$ - CODE32_SEGMENT
	
[section .gs]
[bits 32]
STACK32_SEGMENT:							; 32位栈段定义
	times 1024 * 4 db 0

Stack32SegLen equ $ - STACK32_SEGMENT
TopOfStack32 equ Stack32SegLen - 1


; ==========================================
;
;		Global Function Segment
;
; ==========================================

[section .func]
[bits 32]
FUNCTION_SEGMENT:

; ds:ebp		--> string address
; bx			--> attribute
; dx			--> dh : row, dl : col
PrintStringFunc:
	push ebp
	push eax
	push edi
	push cx
	push dx
	
Print:
	mov cl, [ds:ebp]
	cmp cl, 0
	je end
	mov eax, 80
	mul dh
	add al, dl
	shl eax, 1
	mov edi, eax
	mov ah, bl
	mov al, cl
	mov [gs:edi], ax
	inc ebp
	inc dl
	jmp Print
	
end:
	pop dx
	pop cx
	pop edi
	pop eax
	pop ebp

	retf
	
PrintString		equ PrintStringFunc - $$

FunctionSegLen	equ $ - FUNCTION_SEGMENT

; =======================================
;
;			Task A code Segment
;
; =======================================

[section .task-a-ldt]
; Task A LDT definition
;								段基址，			段界限，				段属性
TASK_A_LDT_ENTRY:
TASK_A_CODE32_DESC		:	Descriptor	0,		TaskACode32SegLen - 1,		DA_C   + DA_32 + DA_DPL3
TASK_A_DATA32_DESC		:	Descriptor	0,		TaskAData32SegLen - 1,		DA_DR  + DA_32 + DA_DPL3
TASK_A_STACK32_DESC		:	Descriptor	0,		TaskAStack32SegLen - 1,		DA_DRW + DA_32 + DA_DPL3

TaskALdtLen	equ $ - TASK_A_LDT_ENTRY

; Task A LDT Selector
TaskACode32Selector		equ	(0x0000 << 3) + SA_TIL + SA_RPL3
TaskAData32Selector		equ (0x0001 << 3) + SA_TIL + SA_RPL3
TaskAStack32Selector	equ (0x0002 << 3) + SA_TIL + SA_RPL3

[section .task-a-dat]
[bits 32]
TASK_A_DATA32_SEGMENT:
	TASK_A_STRING			db "This is Task A!", 0
	TASK_A_STRING_OFFSET	equ TASK_A_STRING - $$

TaskAData32SegLen equ $ - TASK_A_DATA32_SEGMENT

[section .task-a-gs]
[bits 32]
TASK_A_STACK32_SEGMENT:
	times 1024 db 0

TaskAStack32SegLen equ $ - TASK_A_STACK32_SEGMENT
TaskATopOfStack32 equ TaskAStack32SegLen - 1

[section .task-a-s32]
[bits 32]
TASK_A_CODE32_SEGMENT:
	mov ax, TaskAData32Selector
	mov ds, ax
	
	mov ebp, TASK_A_STRING_OFFSET	; 使用调用门打印字符串，使用调用门：0调用
	mov bx, 0x0c
	mov dh, 14
	mov dl, 29
	
	call FuncPrintStringSelector : 0
	
	jmp $


TaskACode32SegLen	equ $ - TASK_A_CODE32_SEGMENT
