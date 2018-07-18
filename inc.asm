
; Segment Attribute	; 一致性是指高特权不能访问低特权，但是低特权能访问高特权，不过特权级别不变
DA_32	equ	0x4000	; 保护模式32位段
DA_DR	equ	0x90	; 只读数据段
DA_DRW	equ 0x92	; 可读写数据段
DA_DRWA	equ 0x93	; 已访问可读写数据段
DA_C	equ	0x98	; 只执行代码段
DA_CR	equ	0x9A	; 可执行可读代码段
DA_CCO	equ	0x9C	; 只执行一致代码段
DA_CCOR	equ	0x9E	; 可执行可读一致代码段

; Selector Attribute
SA_RPL0	equ	0		; 高特权，内核级
SA_RPL1	equ	1		; 中高特权，服务级
SA_RPL2	equ	2		; 中低特权，服务级
SA_RPL3	equ	3		; 低特权，用户级

SA_TIG	equ	0		; GDT
SA_TIL	equ	4		; LDT

; 描述符
; usage: Descriptor Base, Limit, Attr
;		 Base: dd
;		 Limit:dd (low 20 bits available)
;		 Attr: dw (lower 4 bits of higher byte are always 0)
%macro Descriptor 3							; 段基址，段界限，段属性
	dw	%2 & 0xFFFF							; 段界限1
	dw	%1 & 0xFFFF							; 段基址1
	db	(%1 >> 16) & 0xFF					; 段基址2
	dw	((%2 >> 8) & 0xF00) | (%3 & 0xF0FF)	; 属性1 + 段界限2 + 属性2
	db	(%1 >> 24) & 0xFF					; 段基址3
%endmacro									; 共8字节

