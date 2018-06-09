org 0x7c00								; 从0x7c00处开始存储代码

jmp short start							; BS_JmpBoot，短跳指令（3字节）；jmp指令占用1字节，地址占用1字节;下面的nop空指令占用1字节;共3字节
nop

define:
	BaseOfStack     equ 0x7c00			; 定义栈的起始地址，注意栈是从高到低增长的，先加后入栈，先出后减
	BaseOfLoader	equ 0x9000			; 内存加载地址
	RootEntryOffset equ 19				; 目录文件项从19逻辑扇区开始
	RootEntryLength equ 14				; 目录文件项共14扇区
	EntryItemLength	equ 32				; 根目录区每个目录项大小
	FatEntryOffset	equ 1				; FAT表起始扇区（FAT表记录了文件簇对应的实际地址）
	FatEntryLength	equ 9				; FAT表长度（扇区）


; ##############################################################################
header:									; MBR
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
start:									; 汇编起始标号，类似于main()函数，之前用jmp跳转到此
	mov ax, cs							; 设置相关的段寄存器
	mov ss, ax
	mov ds, ax
	mov es, ax
	mov sp, BaseOfStack					; 设置函数调用栈地址

	mov ax, RootEntryOffset				; 记录目录文件项起始扇区
	mov cx, RootEntryLength				; 记录目录文件项占用扇区数
	mov bx, Buf							; 读取的目录文件项存放处

	call ReadSector						; 	读取指定扇区数据到Buf缓存
	mov si, Target						; 要查找的文件名
	mov cx, TarLen						; 文件名长度
	mov dx, 0							; 设置默认返回值为查找失败

	call FindEntry						; 	查找文件，dx作为返回值

	cmp dx, 0							; 如果没有找到文件就输出提示,否則继续
	jz output

	mov si, bx							; 将读取的数据bx放到si，这里是将目录文件项在Buf的地址作为源
	mov di, EntryItem					; 存储根目录区的内存空间di
	mov cx, EntryItemLength				; 根目录区的大小

	call MemCpy							; 	调用内存拷贝，把目录文件项的第一个目录项从Buf拷贝到EntryItem

	mov ax, FatEntryLength				; Fat表占用扇区数
	mov cx, [BPB_BytsPerSec]			; 每扇区字节数
	mul cx								; mul乘法，al*cx，结果放到AX系列寄存器，此时ax等于Fat表占用字节数
	mov bx, BaseOfLoader				; Fat表内存暂存区地址
	sub bx, ax							; 减法，将读取的数据放到Fat暂存区的前面，注意上面的乘法结果不能溢出，这里的AX没有溢出（9*512=4608，ax=16bit=65535）

	mov ax, FatEntryOffset				; Fat1起始扇区
	mov cx, FatEntryLength				; Fat表的长度

	call ReadSector						; 	读取Fat表，放到bx指向的地址

	mov cx, [EntryItem + 0x1A]			; FAT表内存地址加0x1A写入cx寄存器，即文件开始的簇号（DIR_FstClus）

	;call FatVec							;	获取簇对应的地址

	;jmp last
	mov si, BaseOfLoader

loading:
	mov ax, dx
	add ax, 31
	mov cx, 1
	push dx
	push bx
	mov bx, si
	call ReadSector
	pop bx
	pop cx		; pop cx=pop dx;mov cx, dx;
	call FatVec
	cmp dx, 0xFF7
	jnb output
	add si, 512
	jmp loading

output:
	mov bp, MsgStr
	mov cx, MsgLen

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
FatVec:									; 获取簇对应的地址，bx=fat表的地址，cx文件起始簇号
	mov ax, cx							; 下面是乘以1.5的相关操作（/2*3），因为2个簇占3字节，通过簇号的奇偶校验决定该簇在FAT表的偏移
	mov cl, 2
	div cl								; 文件簇编号除以2，商放al，余数放ah；余数为0时簇号为偶数，相对FAT表的偏移量采用低12bit（低1.5字节），否则采用高12bit(高1.5字节)

	push ax								; ah用于决定截取方式，al保留了除以2的整数值

	mov ah, 0							; ax的高位置0，相当于舍去余数保留商
	mov cx, 3
	mul cx								; ax乘以3
	mov cx, ax							; cx=文件簇在FAT表的位置

	pop ax

	cmp ah, 0							; 对簇地址判断奇偶决定比特组合方式（前12比特还是后12比特）
	jz even								; 如果余数是0就跳转到even截取前1.5字节
	jmp odd								; 否则跳转到odd截取后1.5字节

even:									; 偶数：截取前1.5字节；fatVec[j] = ( (Fat[i+1] & 0x0f) << 8 ) | Fat[i];
	mov dx, cx							; 文件簇偏移放入dx
	add dx, 1							; 偏移到第二个字节，需要截取第二个字节
	add dx, bx							; 文件簇索引+Fat表的地址(得到第二个字节的实际地址)
	mov bp, dx							; 借助bp寄存器读取当前簇的第二个字节
	mov dl, byte [bp]
	and dl, 0x0F						; 保留低4位
	shl dx, 8							; 左移8位使第二字节的低4bit放到dx的高位
	add cx, bx							; 指向第一个字节
	mov bp, cx
	or  dl, byte [bp]					; 读取当前簇的第一个字节放到dx的低位，将2个字节组合起来
	jmp return

odd:									; 奇数：截取后1.5字节；FatVec[j+1] = (Fat[i+2] << 4) | ( (Fat[i+1] >> 4) & 0x0F );
	mov dx, cx
	add dx, 2							; 偏移到第三个字节（最后一个字节）
	add dx, bx							; 文件簇偏移量+Fat表的地址(得到第三个字节的实际地址)
	mov bp, dx
	mov dl, byte [bp]					; dl保存第3个字节的数据
	mov dh, 0							; 将dx的高位置0
	shl dx, 4							; 将读取的最后一个字节左移4位，留出低4位给第二个字节的高4位（注意dx是16bit的，左移4位后dl的高4位移到dh的低4位）
	add cx, 1							; 偏移指向第二个字节
	add cx, bx							; 文件簇偏移量+Fat表的地址(得到第二个字节的实际地址)
	mov bp, cx							; 读取第二个字节的值
	mov cl, byte [bp]
	shr cl, 4							; 将读取的第二个字节右移4位，高位移向低位
	and cl, 0x0F						; 保留右移过后的低位4bit
	mov ch, 0							; 将cx的高位置0
	or  dx, cx							; 将2个字节组合起来，最后一字节（存储在2字节的寄存器）左移4位 | 第二字节右移4bit

return:
	ret


; ##############################################################################
; ds:si --> source
; es:di --> destination
; cx	--> length
MemCpy:									; 内存拷贝
	push si
	push di
	push cx
	push ax

	cmp si, di							; 比较内存地址大小，因为两个内存地址空间如果有重复拷贝方式将不同

	ja btoe								; jump if above，如果源地址大于目的地址就从前向后拷贝，否则从后向前拷贝；避免两个内存空间有重复地址时覆盖未拷贝的源数据。
	
	add si, cx							; 从后向前拷贝需要源和目标地址都挪到最后，
	add di, cx
	dec si								; 上面的add使指针指向了最后一个字节的后面，所以需要回退一个字节
	dec di

	jmp etob

btoe:
	cmp cx, 0							; 如果长度为0就停止
	jz done
	mov al, [si]						; 借助al寄存器将源地址的一个字节（al=8bit）拷贝到目的地址
	mov byte [di], al
	inc si								; inc递增指令，拷贝完一个字节后源和目标地址指向下一个
	inc di
	dec cx								; 长度减1
	jmp btoe							; 循环拷贝

etob:									; end to begin
	cmp cx, 0							; 如果拷贝完毕就结束
	jz done
	mov al, [si]
	mov byte [di], al
	dec si								; 源和目标地址递减，注意是从后向前拷贝的
	dec di
	dec cx
	jmp etob							; 循环拷贝

done:									; 内存拷贝完毕还原寄存器数据
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
	call MemCmp							; 内存匹配查找（文件查找），返回cx表示匹配的字符数（为0表示没有文件或匹配成功）
	cmp cx, 0							; 返回0表示匹配成功或根目录没有文件，否则继续查找
	jz exist
	add bx, EntryItemLength				; Buf缓存地址加32，每个目录项占用32字节
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
	cmp cx, 0							; 到末尾（文件名结束符）就跳转到equal，目录项的第一段是文件名
	jz equal
	mov al, [si]						; si源（要查找的文件名）
	cmp al, byte [di]					; di目标（根目录区的文件名）
	jz goon								; 匹配一个字节就跳转到goon继续循环判断
	jmp noequal							; 不匹配就跳转到noequal，函数返回以便查找下一个文件
goon:
	inc si								; 源和目标+1递增
	inc di
	dec cx								; 剩余字符串长度递减
	jmp compare							; 跳转到compare继续循环判断

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
	mov dx, 0							; dx打印的起始行列
	mov ax, 0x1301						; ah=0x13,在Teletype电传打字机模式下输出;al=0x01,字符串只含字符，启用BL属性，光标跟随移动
	mov bx, 0x0007						; bh页码,bl前景色；bl=07，黑底白字
	int 0x10							; 打印中断
	ret									; 函数返回


; ##############################################################################
; no parameter
ResetFloppy:							; 重置软盘
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
ReadSector:								; 读扇区（函数）
	push bx								; 保存相关寄存器
	push cx
	push dx
	push ax
	
	call ResetFloppy					; 重置软盘
	
	push bx
	push cx

	mov bl, [BPB_SecPerTrk]				; 每柱面（磁道）扇区数；本段代码用于计算柱面、磁头和扇区号以及设置驱动器号
	div bl								; 除法，用于计算要读取的数据的起始柱面和扇区偏移；被除数在AX（或者DX高位+AX地位），商在AL，余数在AH
	mov cl, ah							; FAT的扇区从0开始，软盘的扇区从1开始
	add cl, 1							; cl记录要读取的数据相对柱面的扇区偏移，注意FAT扇区和软盘扇区的起始编号不同
	mov ch, al							; 读取的数据起始柱面号
	shr ch, 1							; 计算数据所在柱面偏移量；右移一位表示除以2（当前软盘只有上下2个磁头）；shr移位大于1时需要借助cl寄存器；
	mov dh, al							; 商，dh记录起始磁头
	and dh, 1							; 如果逻辑柱面（磁道）号是偶数就在0磁头，否则就是1磁头（目前只有2个磁头）
	mov dl, [BS_DrvNum]					; 设置驱动器号

	pop ax								; 还原要读取的扇区数,相当于原来的cx，因为cx是最后入栈的，这里是最先出栈
	pop bx								; bx已设置为指向Buf

	mov ah, 0x02						; 0x02读扇区，磁头、驱动器号、柱面（磁道）、扇区-->dh、dl、ch、cl；长度、Buf-->ax、bx

read:
	int 0x13							; 读取磁盘的中断
	jc read								; 若进位位（CF）被置位，表示调用失败，需要重新读取

	pop ax
	pop dx
	pop cx
	pop bx

	ret


; ##############################################################################
MsgStr db "No LOADER ..."				; 定义字符串
MsgLen equ ($-MsgStr)					; 定义上面字符串长度标号

Target db "LOADER     "						; 要查栈的文件名
TarLen equ ($-Target)					; 要查找的文件名长度

EntryItem times EntryItemLength db 0x00	; 目录项空间，且用0填充根目录区，32字节

Buf:									; Buf空间，数据的读取和写入空间;下面两行代码是为mbr准备的
	times 510-($-$$) db 0x00			; 用0填充mbr中的剩余部分（注意留出0x55aa两字节空间）
	db 0x55, 0xaa						; mbr的最后两个字节用0x55AA结尾
