%include "inc.asm"							; 加载头文件，一些常量、设置函数

PageDirBase		equ		0x200000
PageTblBase		equ		0x201000

org 0x9000									; 内存加载地址

jmp ENTRY_SEGMENT							; 跳转到ENTRY_SEGMENT入口处

[section .gdt]								; 全局描述符表，部分段基址未知基址需使用时再调节（InitDescItem）
; GDT definition		8Byte 64bit
;							"函数名"		段基址			段界限						段属性
GDT_ENTRY			:		Descriptor	0,				0,							0							; 全局段描述符表第0项不使用
CODE32_DESC			:		Descriptor	0,				Code32SegLen		-	1,	DA_C    + DA_32	+ DA_DPL3
VIDEO_DESC			:		Descriptor	0xB8000,		0x07FFF,					DA_DRWA + DA_32	+ DA_DPL3	; 视频段描述符表设置正确无须初始化
DATA32_KERNEL_DESC	:		Descriptor	0,				Data32KernelSegLen	-	1,	DA_DRW	+ DA_32	+ DA_DPL0
DATA32_USER_DESC	:		Descriptor	0,				Data32UserSegLen	-	1,	DA_DRW	+ DA_32	+ DA_DPL3
STACK32_KERNEL_DESC	:		Descriptor	0,				TopOfKernelStack32,			DA_DRW  + DA_32	+ DA_DPL0
STACK32_USER_DESC	:		Descriptor	0,				TopOfUserStack32,			DA_DRW  + DA_32	+ DA_DPL3
TSS_DESC			:		Descriptor	0,				TSSLen				-	1,	DA_386TSS		+ DA_DPL0
FUNCTION_DESC		:		Descriptor	0,				FunctionSegLen		-	1,	DA_C	+ DA_32 + DA_DPL0
PAGE_DIR_DESC		:		Descriptor	PageDirBase,	4095,						DA_DRW  + DA_32
PAGE_TBL_DESC		:		Descriptor	PageTblBase,	1023,						DA_DRW  + DA_LIMIT_4K + DA_32
; Call Gate
;										选择子				偏移			参数个数		属性
FUNC_GETKERNELDATA_DESC	:	Gate	FunctionSelector,	GetKernelData,	0,		DA_386CGate + DA_DPL3
; GDT end

GdtLen	equ	$ - GDT_ENTRY

GdtPtr:										; 全局描述符表指针
		dw	GdtLen - 1	; 偏移，记录描述符数量
		dd	0			; 全局描述符起始地址，先设置为0


; GDT Selector			2Byte 16bit	TI：全局、局部	RPL：请求权限级别
Code32Selector			equ	(0x0001 << 3) + SA_TIG + SA_RPL3	; 0x0001==第二个选择子
VideoSelector			equ (0x0002 << 3) + SA_TIG + SA_RPL3	; 显存特权级低只会影响显示，对系统安全无影响
KernelData32Selector	equ (0x0003 << 3) + SA_TIG + SA_RPL0
UserData32Selector		equ (0x0004 << 3) + SA_TIG + SA_RPL3
KernelStack32Selector	equ (0x0005 << 3) + SA_TIG + SA_RPL0
UserStack32Selector		equ (0x0006 << 3) + SA_TIG + SA_RPL3
TSSSelector				equ (0x0007 << 3) + SA_TIG + SA_RPL0
FunctionSelector		equ (0x0008 << 3) + SA_TIG + SA_RPL0
PageDirSelector			equ (0x0009 << 3) + SA_TIG + SA_RPL0
PageTblSelector			equ (0x000a << 3) + SA_TIG + SA_RPL0
; Gate Selector
GetKernelDataSelector	equ (0x0009 << 3) + SA_TIG + SA_RPL3
; end of [section .gdt]

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
	;  视频段描述符表设置正确无须初始化
	mov esi, DATA32_KERNEL_SEGMENT
	mov edi, DATA32_KERNEL_DESC

	call InitDescItem
	
	mov esi, DATA32_USER_SEGMENT
	mov edi, DATA32_USER_DESC
	
	call InitDescItem
		
	mov esi, STACK32_KERNEL_SEGMENT
	mov edi, STACK32_KERNEL_DESC

	call InitDescItem

	mov esi, STACK32_USER_SEGMENT
	mov edi, STACK32_USER_DESC

	call InitDescItem
	
	mov esi, FUNCTION_SEGMENT
	mov edi, FUNCTION_DESC

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
	
			; 5. load TSS
	mov ax, TSSSelector
	ltr ax

; 			6. jump to 32 bits code
	;jmp word Code32Selector : 0
	push UserStack32Selector		;jmp dword Code32Selector : 0 ; 使用jmp跳转到32位代码段选择子的0偏移处
	push TopOfUserStack32
	push Code32Selector
	push 0
	retf						; 弹出2个栈，分别给IP、CS寄存器
	
	
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

[section .kdat]
[bits 32]
DATA32_KERNEL_SEGMENT:
	KDAT			db	"Kernel Data", 0
	KDAT_LEN		equ $ - KDAT
	KDAT_OFFSET		equ	KDAT - $$
	
Data32KernelSegLen equ $ - DATA32_KERNEL_SEGMENT

[section .udat]
[bits 32]
DATA32_USER_SEGMENT:
	UDAT			times 16 db 0
	UDAT_LEN		equ $ - UDAT
	UDAT_OFFSET		equ	UDAT - $$
	
Data32UserSegLen equ $ - DATA32_USER_SEGMENT

[section .tss]					; TSS任务段104字节，另可附加额外信息
[bits 32]
TSS_SEGMENT:
		dd	0						; 保留前一个TSS段选择子，由CPU填写，高位填0
		dd	TopOfKernelStack32		; 0特权级栈指针
		dd	KernelStack32Selector	; 0特权级栈段选择子，只能用低2个字节，高位填0
		dd	0						; 1特权级
		dd	0						;
		dd	0						; 2特权级
		dd	0						;
		times 4 * 18 dd 0			; 用于切换寄存器的值，由CPU填写，共18个dd类型
		dw	0						; 最低位为调试陷阱标志T，其余为0
		dw	$ - TSS_SEGMENT + 2		; I/O Map Base Address
		db	0xFF					; 结束标记，属于额外信息
		
TSSLen	equ	$ - TSS_SEGMENT



[section .s32]	; 32位代码段
[bits 32]		; 使用32位编译
CODE32_SEGMENT:								; 32位代码段数据
	mov ax, VideoSelector			; 把视频段选择子放到gs全局段寄存器
	mov gs, ax

	mov ax, UserData32Selector		; 设置数据段地址
	mov es, ax
	
	mov di, UDAT_OFFSET
	
	call GetKernelDataSelector : 0	; 使用调用门模拟恶意操作，拷贝内核数据至用户空间
	
	mov ax, UserData32Selector		; 入栈eip ==> eip+当前指令相对上面一条指令的偏移（0x10+7=0x17）
	mov ds, ax
	
	mov ebp, UDAT_OFFSET
	mov bx, 0x0C
	mov dh, 12
	mov dl, 33
		
	call PrintString
	
	call SetupPage

	jmp $

;
;
SetupPage
	push eax
	push ecx
	push edi
	push es

	mov ax, PageDirSelector
	mov es, ax
	mov ecx, 1024	; 1K sub tables
	mov edi, 0
	mov eax, PageTblBase | PG_P | PG_USU | PG_RWW
	
	cld
	
stdir:
	stosd
	add eax, 4096
	loop stdir
	
	mov ax, PageTblSelector
	mov es, ax
	mov ecx, 1024 * 1024	; 1M pages
	mov edi, 0
	mov eax, PG_P | PG_USU | PG_RWW
	
	cld
	
sttbl:
	stosd
	add eax, 4096
	loop sttbl
	
	mov eax, PageDirBase
	mov cr3, eax
	mov eax, cr0
	or  eax, 0x80000000
	mov cr0, eax
	
	pop es
	pop edi
	pop ecx
	pop eax
	
	ret

; ds:ebp		--> string address
; bx			--> attribute
; dx			--> dh : row, dl : col
PrintString:
	push ebp
	push eax
	push edi
	push cx
	push dx
	
Print:
	mov cl, [ds:ebp]		; 取一个字符
	cmp cl, 0				; 结束符判断
	je end
	mov eax, 80				; 每行字符列数
	mul dh					; 乘以行数
    ;add al, dl             ; 总偏移字符数，al太小超过255会溢出（3*80+16==3行16列）
    push dx                 ; 备份dx（dh），供循环调用避免丢失行号
    mov dh, 0               ; dx高位置0，确保dx=dl
    add ax, dx              ; 总偏移字符数（65536个字符）
    pop dx
	shl eax, 1				; 每个字符占2字节
	mov edi, eax
	mov ah, bl				; 字符属性
	mov al, cl				; 字符
	mov [gs:edi], ax		; 写入显存
	inc ebp					; 字符源地址递增
	inc dl					; 显存字符地址递增
	jmp Print
	
end:
	pop dx
	pop cx
	pop edi
	pop eax
	pop ebp
	jmp $
	retf
	
Code32SegLen	equ $ - CODE32_SEGMENT


[section .func]
[bits 32]
FUNCTION_SEGMENT:

;es:di --> data buffer
GetKernelDataFunc:			; 调用门，利用此调用门进入内核拷贝数据
	mov cx, [esp + 4]		; 重写RPL避免使用高权限选择子进入内核程序
	and cx, 0x0003
	mov ax, es
	and ax, 0xFFFC
	or ax, cx
	mov es, ax

	mov ax, KernelData32Selector
	mov ds, ax
	
	mov si, KDAT_OFFSET
	
	mov cx, KDAT_LEN
	
	call KMemCpy
	
	retf
	
; ds:si --> source
; es:di --> destination
; cx	--> length
KMemCpy:					; 内存拷贝
	mov ax, es
	
	call CheckRPL			; 检查RPL避免猜中选择子后进入内核，ax==原有RPL
	
	cmp si, di
	ja btoe
	add si, cx
	add di, cx
	dec si
	dec di
	jmp etob
btoe:						; 源在后从前拷贝
	cmp cx, 0
	jz done
	mov al, [ds:si]
	mov byte [es:di], al
	inc si
	inc di
	dec cx
	jmp btoe
etob:						; 源在前从尾拷贝
	cmp cx, 0
	jz done
	mov al, [ds:si]
	mov byte [es:di], al
	dec si
	dec di
	dec cx
	jmp etob
done:
	ret
	
; ax --> selector value
CheckRPL:
	and ax, 0x0003			; 如果是RPL3则立即退出，否则进入异常
	cmp ax, SA_RPL0
	jz valid
	
	mov ax, 0
	mov fs, ax
	mov byte [fs:0], 0
	
valid:
	ret
	
GetKernelData	equ	GetKernelDataFunc - $$
FunctionSegLen	equ $ - FUNCTION_SEGMENT
	
[section .kgs]
[bits 32]
STACK32_KERNEL_SEGMENT:							; 32位栈段定义
	times 256 * 4 db 0

Stack32KernelSegLen equ $ - STACK32_KERNEL_SEGMENT
TopOfKernelStack32 equ Stack32KernelSegLen - 1

[section .usg]
[bits 32]
STACK32_USER_SEGMENT:							; 32位栈段定义
	times 256 * 4 db 0

Stack32UserSegLen equ $ - STACK32_USER_SEGMENT
TopOfUserStack32 equ Stack32UserSegLen - 1
