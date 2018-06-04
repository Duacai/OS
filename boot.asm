org 0x7c00								; 从0x7c00处开始存储代码

jmp short start							; BS_JmpBoot;短跳指令，jmp指令占用1字节，地址占用1字节;下面的nop空指令占用1字节;共3字节
nop

define:
	BaseOfStack     equ 0x7c00			; 定义栈的起始地址，注意栈是从高到低增长的，先加后入栈，先出后减
	BaseOfLoader	equ 0x9000
	RootEntryOffset equ 19				; 文件数据从19逻辑扇区开始
	RootEntryLength equ 14				; 目录文件项共14扇区
	EntryItemLength	equ 32
	FatEntryOffset	equ 1
	FatEntryLength	equ 9


; ##############################################################################
header:
	BS_OEMName		db "D.T.Soft"		; OEM字符，8个，不足以空格填充
	BPB_BytsPerSec	dw 512				; 每扇区字节数
	BPB_SecPerClus	db 1				; 每簇占用扇区数
	BPB_RsvdSecCnt	dw 1				; Boot占用的扇区数
	BPB_NumFATs		db 2				; FAT表的记录数
	BPB_RootEntCnt	dw 224				; 最大根目录文件数
    BPB_TotSec16	dw 2880				; 逻辑扇区总数
    BPB_Media		db 0xF0				; 媒体描述符
    BPB_FATSz16		dw 9				; 每个FAT占用扇区数
    BPB_SecPerTrk	dw 18				; 每个磁道扇区数
    BPB_NumHeads	dw 2				; 磁头数
    BPB_HiddSec		dd 0				; 隐藏扇区数
    BPB_TotSec32	dd 0				; 如果BPB_TotSec16是0,则在这里记录扇区总数
    BS_DrvNum		db 0				; 中断13的驱动器号
    BS_Reserved1	db 0				; 未使用
    BS_BootSig		db 0x29				; 扩展引导标志
    BS_VolID		dd 0				; 卷序列号
    BS_VolLab		db "D.T.OS-0.01"	; 卷标，必须11个字符，不足以空格填充
    BS_FileSysType	db "FAT12   "		; 文件系统类型，必须使8个字符，不足填充空格


; ##############################################################################
start:									; 汇编起始标号，类似于main()函数
	mov ax, cs							; 设置相关的段寄存器
	mov ss, ax
	mov ds, ax
	mov es, ax
	mov sp, BaseOfStack					; 设置函数调用栈

	;mov ax, 59							; 文件数据起始于59扇区
	;mov cx, 1							; 读取1个扇区
	;mov bx, Buf						; 数据写入的地址

	;call ReadSector					; 调用读扇区函数（数据保存到ES:BX）
	
	;mov si, MsgStr
	;mov di, DEST
	;mov cx, MsgLen
	mov ax, RootEntryOffset				; 记录数据起始扇区
	mov cx, RootEntryLength				; 记录数据占用扇区数
	mov bx, Buf							; 读取的数据存放处

	call ReadSector						; 读取指定扇区数据到Buf缓存
	mov si, Target						; 要查找的文件名
	mov cx, TarLen						; 文件名长度
	mov dx, 0							; 设置默认返回值

	call FindEntry						; 查找文件

	cmp dx, 0							; 如果没有找到文件就输出提示,否則跳到last死循环
	jz output

	;mov si, Target
	;mov di, MsgStr
	;mov di, Buf
	;mov di, si
	;add di, 2
	;sub di, 2
	;mov cx, TarLen

	mov si, bx
	mov di, EntryItem
	mov cx, EntryItemLength

	call MemCpy

	mov ax, FatEntryLength
	mov cx, [BPB_BytsPerSec]
	mul cx
	mov bx, BaseOfLoader
	sub bx, ax

	mov ax, FatEntryOffset
	mov cx, FatEntryLength

	call ReadSector

	mov cx, [EntryItem + 0x1A]

	call FatVec

	jmp last

	;call MemCmp

	;cmp cx, 0
	;jz output
	;jmp last

	;mov bp, Buf						; 字符串段的偏移地址，相对于ES; mov bp, MsgStr    ; 输出Hello, DTOS!
	;mov cx, 29							; 字符串长度    ; mov cx, MsgLen	; Test file for virtual floppy.
output:
	mov bp, MsgStr
	;mov bp, Target
	;add bp, 2
	mov cx, MsgLen
	;mov cx, TarLen
	call Print							; 调用字符串打印函数

last:									; 死循环
	hlt
	jmp last


; ##############################################################################
; cx --> index
; bx --> fat table address
;
; return:
;	dx --> fat[index]
FatVec:
	mov ax, cx
	mov cl, 2
	div cl

	push ax

	mov ah, 0
	mov cx, 3
	mul cx
	mov cx, ax

	pop ax

	cmp ah, 0
	jz even
	jmp odd

even:	; fatVec[j] = ( (Fat[i+1] & 0x0f) << 8 ) | Fat[i];
	mov dx, cx
	add dx, 1
	add dx, bx
	mov bp, dx
	mov dl, byte [bp]
	and dl, 0x0F
	shl dx, 8
	add cx, bx
	mov bp, cx
	or  dl, byte [bp]
	jmp return

odd:	; FatVec[j+1] = (Fat[i+2] << 4) | ( (Fat[i+1] >> 4) & 0x0F );
	mov dx, cx
	add dx, 2
	add dx, bx
	mov bp, dx
	mov dl, byte [bp]
	mov dh, 0
	shl dx, 4
	add cx, 1
	add cx, bx
	mov bp, cx
	mov cl, byte [bp]
	shr cl, 4
	and cl, 0x0F
	mov ch, 0
	or  dx, cx

return:
	ret


; ##############################################################################
; ds:si --> source
; es:di --> destination
; cx	--> length
MemCpy:
	push si
	push di
	push cx
	push ax

	cmp si, di

	ja btoe
	
	add si, cx
	add di, cx
	dec si
	dec di

	jmp etob

btoe:
	cmp cx, 0
	jz done
	mov al, [si]
	mov byte [di], al
	inc si
	inc di
	dec cx
	jmp btoe

etob:
	cmp cx, 0
	jz done
	mov al, [si]
	mov byte [di], al
	dec si
	dec di
	dec cx
	jmp etob

done:
	pop ax
	pop cx
	pop di
	pop si

	ret


; ##############################################################################
; es:bx --> root entry offset address
; ds:si --> target string
; cx    --> target length
;
; return:
;		(dx != 0) ? exist : noexist
;			exist --> bx is the target entry
FindEntry:								; 查找根目录文件
	push di
	push bp
	push cx

	mov dx, [BPB_RootEntCnt]			; 最大根目录文件数
	mov bp, sp							; 栈地址，不能直接将sp栈顶指针赋值给通用寄存器

find:
	cmp dx, 0							; 如果没有文件就跳转到noexist结束
	jz noexist
	mov di, bx							; bx==Buf缓存；ReadSector（）已经读取数据到Buf缓存
	mov cx, [bp]						; 借助中间寄存器获取栈顶指针，此时的bp栈顶指针指向最后入栈的cx寄存器
	call MemCmp							; 内存匹配查找（文件查找）
	cmp cx, 0							; 如果还有文件就继续，否则匹配失败跳转到exist结束
	jz exist
	add bx, 32							; Buf缓存地址加32，每个目录项占用32字节
	dec dx								; dx-1，文件数减1
	jmp find

exist:
noexist:
	pop cx
	pop bp
	pop di

	ret


; ##############################################################################
; ds:si --> source
; es:di --> destination
; cx	--> length
;
; return:
;		(cx == 0) ? equal : noequeal
MemCmp:									; 内存数据对比
	push si
	push di
	push ax

compare:
	cmp cx, 0							; 到末尾（文件名结束符）就跳转到equal
	jz equal
	mov al, [si]						; si源（要查找的文件名）
	cmp al, byte [di]					; di目标（根目录区的文件名）
	jz goon								; 匹配一个字节就跳转到goon继续循环判断
	jmp noequal							; 不匹配就跳转到noequal，函数返回以便查找下一个文件
goon:
	inc si								; 源和目标+1递增
	inc di
	dec cx								; 剩余次数-1递减
	jmp compare							; 跳转到compare继续循环

equal:
noequal:
	pop ax
	pop di
	pop si

	ret


; ##############################################################################
; es:bp --> string address
; cx    --> string length
Print:									; 字符串打印函数
	mov dx, 0
	mov ax, 0x1301						; ah=13,在Teletype电传打字机模式下输出;al=01,字符串只含字符，启用BL属性，改变光标
	mov bx, 0x0007						; bh页码,bl前景色；bl=07,80×25字符的2色文本
	int 0x10							; 打印中断
	ret									; 函数返回


; ##############################################################################
; no parameter
ResetFloppy:							; 重置软盘段
	push ax
	push dx

	mov ah, 0x00						; 磁盘系统复位
	mov dl, [BS_DrvNum]					; 驱动器号（0x00~0x7F软盘，0x80~0x0FF硬盘）
	int 0x13							; 读取磁盘的中断
	
	pop dx
	pop ax

	ret

; ax	--> logic sector number
; cx	--> number of sector
; es:bx	--> target address
ReadSector:								; 读扇区段（函数）
	push bx								; 保存相关寄存器
	push cx
	push dx
	push ax
	
	call ResetFloppy					; 重置软盘
	
	push bx
	push cx

	mov bl, [BPB_SecPerTrk]				; 每柱面（磁道）扇区数；本段代码用于计算柱面、磁头和扇区号以及设置驱动器号
	div bl								; 除法，被除数在AX（或者DX高位+AX地位），商在AL，余数在AH
	mov cl, ah							; 余数，cl记录起始扇区;FAT的扇区从0开始，软盘的扇区从1开始，所以下面+1
	add cl, 1
	mov ch, al							; 商（逻辑柱面或磁道数），ch记录起始柱面（磁道）
	shr ch, 1							; 右移一位表示除以2（当前软盘只有2个磁头），因为扇区编号是由两面的柱面（磁道）组合的，然后再从外往内增大；shr移位大于1时需要借助cl寄存器；
	mov dh, al							; 商，dh记录起始磁头
	and dh, 1							; 如果逻辑柱面（磁道）号是偶数就在0磁头，否则就是1磁头（目前只有2个磁头）
	mov dl, [BS_DrvNum]					; 设置驱动器号

	pop ax								; 还原要读取的扇区数,相当于原来的cx，因为cx是最后入栈的，这里是最先出栈
	pop bx								; bx已设置为指向Buf

	mov ah, 0x02						; 0x02读扇区

read:
	int 0x13							; 读取磁盘的中断
	jc read								; 若进位位（CF）被置位，表示调用失败，需要重新读取

	pop ax
	pop dx
	pop cx
	pop bx

	ret


; ##############################################################################
;MsgStr db "Hello, DTOS!"				; 定义字符串，上面43行注释使用
MsgStr db "No LOADER ..."				; 定义字符串，上面43行注释使用
MsgLen equ ($-MsgStr)					; 定义上面字符串长度标号
;DEST db "Hello, DTOS!"					; 测试目标文件
Target db "LOADER"						; 要查栈的文件名
TarLen equ ($-Target)					; 要查找的文件名长度
EntryItem times EntryItemLength db 0x00
Buf:									; Buf缓存，数据的读取和写入空间;下面两行代码是为mbr准备的，其它读写无用无害
	times 510-($-$$) db 0x00
	db 0x55, 0xaa
