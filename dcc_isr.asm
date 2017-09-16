;;======================================================================;;
;;			DCC FUNCTION DECODER				;;
;;======================================================================;;
;;									;;
;; Program:         DCC_ISR -- DCC function decoder			;;
;; Code:            Paco Cañada						;;
;; Platform:        Microchip PIC12F629, 4 Mhz				;;
;; Date:            14.07.2006						;;
;; First release:   14.07.2006						;;
;; LastDate:        14.07.2006						;;
;;									;;
;;======================================================================;;
;
; Minimal external components, uses internal oscilator at 4 MHz

; This program is distributed as is but WITHOUT ANY WARRANTY
; I hope you enjoy!!
;
; Revisions:
; 14.07.2006	Start of writting code
; 16.07.2006	Added fluorescent lights
; 18.07.2006	Added multiplexed mode
; 09.08.2006	Added PoM
; 30.04.2007	Added analog mode

; ----- Definitions

#define		__VERNUM	D'2'
#define		__VERDAY	0x30
#define		__VERMONTH	0x04
#define		__VERYEAR	0x07


	LIST	   p=12F629	; target processor



	errorlevel -305,-302



	#include p12F629.inc





	__CONFIG  _BODEN_ON & _CP_OFF & _WDT_OFF & _MCLRE_OFF & _PWRTE_ON & _INTRC_OSC_NOCLKOUT 

				; Make sure that internal osc. is calibrated
				; Value has to be read before reprogramming the device.



; --- Macros

#define		DNOP		goto	$+1


; --- Constant values

FXTAL		equ	D'4000000'

GP_TRIS         equ     0x04			; GP3: input
GP_INI          equ     0x00			; all zero
OPTION_INI	equ	0x88			; Option register: no pull-up, falling GP2, no prescaler, wdt 1:1
WPU_INI		equ	0x33			; Weak pull-up enable. default, no pull-ups

INTC_INI	equ	0x88			; GIE, GPIE enable, PEIE disable
PIE1_INI	equ	0x00			; no interrupts


#define		OUT1	0			; 
#define		OUT2	1			; 
#define		DCCIN	GPIO,2			; DCC input pin
#define		OUT3	3			; not implemented
#define		OUT4	4			; 
#define		OUT5	5			; 


; --- EEPROM Section

#define		EE_INI		0x00

E_CV1		equ	EE_INI+0x00		; CV513	Primary Adress low
E_CV7		equ	EE_INI+0x01		; Manufacturer Version
E_CV8		equ	EE_INI+0x02		; Manufacturer ID
E_CV13		equ	EE_INI+0x03		; Analog F1..F8
E_CV14		equ	EE_INI+0x04		; Analog FL,FR (F9..F12)
E_CV17		equ	EE_INI+0x05		; Extended Adress Low Byte
E_CV18		equ	EE_INI+0x06		; Extended Adress High Byte
E_CV19		equ	EE_INI+0x07		; Consist Adress
E_CV29		equ	EE_INI+0x08		; config
E_CV33		equ	EE_INI+0x09		; front light -> GPIO (xx54x210)
E_CV34		equ	EE_INI+0x0A		; back light  -> GPIO (xx54x210)
E_CV35		equ	EE_INI+0x0B		; F1 -> GPIO (xx54x210)
E_CV36		equ	EE_INI+0x0C		; F2 -> GPIO (xx54x210)
E_CV37		equ	EE_INI+0x0D		; F3 -> GPIO (xx54x210)
E_CV38		equ	EE_INI+0x0E		; F4 -> GPIO (xx54x210)
E_CV39		equ	EE_INI+0x0F		; F5 -> GPIO (xx54x210)
E_CV40		equ	EE_INI+0x10		; F6 -> GPIO (xx54x210)
E_CV41		equ	EE_INI+0x11		; F7 -> GPIO (xx54x210)
E_CV42		equ	EE_INI+0x12		; F8 -> GPIO (xx54x210)
E_CV50		equ	EE_INI+0x13		; Normal or fluorescent, multiplexed

; ----- Variables

; --- Internal RAM Section

#define		RAMINI0		0x020		; 64 bytes

INT_W		equ	RAMINI0+0x00		; interrupt context registers
INT_STAT	equ	RAMINI0+0x01

SHIFT0		equ	RAMINI0+0x02
DATA1		equ	RAMINI0+0x03		; interrupt shift register
DATA2		equ	RAMINI0+0x04
DATA3		equ	RAMINI0+0x05
DATA4		equ	RAMINI0+0x06
DATA5		equ	RAMINI0+0x07
DATA6		equ	RAMINI0+0x08

PREAMBLE	equ	RAMINI0+0x09
STATE		equ	RAMINI0+0x0A
DCCBYTE		equ	RAMINI0+0x0B

F1F8SET		equ	RAMINI0+0x0C
F1F8OLD		equ	RAMINI0+0x0D
FSET		equ	RAMINI0+0x0E
FCHG		equ	RAMINI0+0x0F

CV1		equ	RAMINI0+0x10		; 
CV7		equ	RAMINI0+0x11		; 
CV8		equ	RAMINI0+0x12		; 
CV13		equ	RAMINI0+0x13		; 
CV14		equ	RAMINI0+0x14		; 
CV17		equ	RAMINI0+0x15		; 
CV18		equ	RAMINI0+0x16		; 
CV19		equ	RAMINI0+0x17		; 
CV29		equ	RAMINI0+0x18		; 
CV33		equ	RAMINI0+0x19		; 
CV34		equ	RAMINI0+0x1A		; 
CV35		equ	RAMINI0+0x1B		; 
CV36		equ	RAMINI0+0x1C		; 
CV37		equ	RAMINI0+0x1D		; 
CV38		equ	RAMINI0+0x1E		; 
CV39		equ	RAMINI0+0x1F		; 
CV40		equ	RAMINI0+0x20		; 
CV41		equ	RAMINI0+0x21		; 
CV42		equ	RAMINI0+0x22		; 
CV50		equ	RAMINI0+0x23		; 

TEMP		equ	RAMINI0+0x28
COUNT		equ	RAMINI0+0x29
FLAGS		equ	RAMINI0+0x2A
FLAGS2		equ	RAMINI0+0x2B
EEDATA0		equ	RAMINI0+0x2C		; data to write in EEPROM
EEADR0		equ	RAMINI0+0x2D
PAGEREG		equ	RAMINI0+0x2E		; Page register
OUTPUT		equ	RAMINI0+0x2F		; output buffer

FLCNT0		equ	RAMINI0+0x30		; flash counter
FLCNT1		equ	RAMINI0+0x31
FLCNT2		equ	RAMINI0+0x32
FLCNT3		equ	RAMINI0+0x33

FLTIME0		equ	RAMINI0+0x34		; flashing time
FLTIME1		equ	RAMINI0+0x35
FLTIME2		equ	RAMINI0+0x36
FLTIME3		equ	RAMINI0+0x37

SHOOTCNT	equ	RAMINI0+0x38		; shooting prescaler
ANALOGCNT	equ	RAMINI0+0x39		; analog detector timeout

RANDOM0		equ	RAMINI0+0x3C		; RANDOM generator
RANDOM1		equ	RAMINI0+0x3D
RANDOM2		equ	RAMINI0+0x3E
RANDOM3		equ	RAMINI0+0x3F


; --- Flags
						; FLAGS
#define		NEW_PACKET	FLAGS,0		; New packet received
#define		NOCV		FLAGS,1		; No CV finded
#define		RDONLY		FLAGS,2		; CV read only
#define		DCC4BYTE	FLAGS,3		; DCC command 4 or 5 bytes
#define		CONSIST		FLAGS,4
#define		PROG_2X		FLAGS,6		; 2x prog
#define		RESET_FLG	FLAGS,7		; reset packet

						; FLAGS2
#define		DIR		FLAGS2,0
#define		LASTDIR		FLAGS2,1
#define		LASTLT		FLAGS2,2
#define		FLSHOOT0	FLAGS2,4	; doing flashing sequence
#define		FLSHOOT1	FLAGS2,5
#define		FLSHOOT2	FLAGS2,6
#define		FLSHOOT3	FLAGS2,7
						; CV29
#define		DIRINV		CV29,0		; direction inverted
#define		FS28		CV29,1		; 14/28 speed step
#define		PWRSRC		CV29,2		; anolog and DCC
#define		LADRE		CV29,5		; long adress enable

						; CV50
#define		FLUORESC	CV50,0		; 4 fluorescent lights
#define		MUX		CV50,1		; multiplexed output


; --------------- Program Section --------------------------------------


		org	0x000

PowerUp:
		clrf	STATUS			; Bank 0 default
		clrf	INTCON			; Disable all interrupts
		clrf	PCLATH			; tables on page 0
		goto	INIT

; ----------------------------------------------------------------------

		org	0x004

Interrupt:
		movwf	INT_W			; save context registers		;1
		swapf	STATUS,w							;2
		movwf	INT_STAT							;3
		clrf	STATUS			; interrupt uses bank 0			;4

		btfss	DCCIN								;5
		goto	Int_Low_Half							;6,7

Int_High_Half:
		movf	STATE,w								; 8
		addwf	PCL,f								; 9

		goto	Preamble							; 10,11
		goto	WaitLow
		goto	ReadBit
		goto	ReadBit
		goto	ReadBit
		goto	ReadBit
		goto	ReadBit
		goto	ReadBit
		goto	ReadBit
		goto	ReadLastBit
		goto	EndByte1
		goto	EndByte2
		goto	EndByte3
		goto	EndByte4
		goto	EndByte5
		goto	EndByte6


Int_Low_Half:
		movlw	d'256' - d'77'		; 77us: between 64us (one) and 90us (zero);8
		movwf	TMR0								;9
		bcf	INTCON,T0IF		; clear overflow flag for counting	;10
EndHighHalf:
		bcf	INTCON,GPIF						;11	;21
EndInt:
		swapf	INT_STAT,w		; restore context registers	;12	;22
		movwf	STATUS							;13	;23
		swapf	INT_W,f							;14	;24
		swapf	INT_W,w							;15	;25
		retfie								;16,17	;26,27


Preamble:
		btfss	NEW_PACKET		; wait until last decoded		;12
		incf	PREAMBLE,f		;					;13
		btfsc	INTCON,T0IF		; if timer 0 overflows then is a DCC zero;14
		clrf	PREAMBLE		;					;15
		movlw	0xF6			; 10 preamble bits?			;16
		addwf	PREAMBLE,w		;					;17
		btfsc	STATUS,C		;					;18
		incf	STATE,f			; yes, next state			;19
		goto	EndHighHalf		;					;20,21
		

WaitLow:
		btfsc	INTCON,T0IF		; if timer 0 overflows then is a DCC zero;12
		incf	STATE,f			; then state				;13
		clrf	DCCBYTE			;					;14
		clrf	PREAMBLE		;					;15
		clrf	DATA4			;					;16
		clrf	DATA5			;					;17
		clrf	DATA6			;					;18
		goto	EndHighHalf		;					;19,20


ReadBit:
		bsf	STATUS,C							;12
		btfsc	INTCON,T0IF		; if timer 0 overflows then is a DCC zero;13
		bcf	STATUS,C							;14
		rlf	SHIFT0,f		; receiver shift register		;15
		incf	STATE,f			;					;16
		goto	EndHighHalf		;					;17,18
			
ReadLastBit:
		bsf	STATUS,C							;12
		btfsc	INTCON,T0IF		; if timer 0 overflows then is a DCC zero;13
		bcf	STATUS,C							;14
		rlf	SHIFT0,f		; receiver shift register		;15
		incf	DCCBYTE,w							;16
		addwf	STATE,f								;17
		goto	EndHighHalf		;					;18,19

EndByte1:
		movlw	0x00			;					;12
		btfsc	INTCON,T0IF		; End bit=1, invalid packet		;13
		movlw	0x02			;					;14
		movwf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA1			;					;17
		incf	DCCBYTE,f		;					;18
		goto	EndHighHalf		;					;19,20

EndByte2:
		movlw	0x00			;					;12
		btfsc	INTCON,T0IF		; End bit=1, invalid packet		;13
		movlw	0x02			;					;14
		movwf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA2			;					;17
		incf	DCCBYTE,f		;					;18
		goto	EndHighHalf		;					;19,20


EndByte3:
		btfss	INTCON,T0IF		; End bit=1, end of packet		;12
		goto	EndByte3x		;					;13,14
		movlw	0x02			;					;14
		movwf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA3			;					;17
		incf	DCCBYTE,f		;					;18
		bsf	DCC4BYTE		;					;19
		goto	EndHighHalf		;					;20,21
EndByte3x:
		clrf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA3			;					;17
		bsf	NEW_PACKET		;					;18
		bcf	DCC4BYTE		;					;19
		goto	EndHighHalf		;					;20,21
		
EndByte4:
		btfss	INTCON,T0IF		; End bit=1, end of packet		;12
		goto	EndByte4x		;					;13,14
		movlw	0x02			;					;14
		movwf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA4			;					;17
		incf	DCCBYTE,f		;					;18
		goto	EndHighHalf		;					;19,20
EndByte4x:
		clrf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA4			;					;17
		bsf	NEW_PACKET		;					;18
		goto	EndHighHalf		;					;19,20

EndByte5:
		btfss	INTCON,T0IF		; End bit=1, end of packet		;12
		goto	EndByte5x		;					;13,14
		movlw	0x02			;					;14
		movwf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA5			;					;17
		incf	DCCBYTE,f		;					;18
		goto	EndHighHalf		;					;19,20
EndByte5x:
		clrf	STATE			;					;15
		movf	SHIFT0,w		;					;16
		movwf	DATA5			;					;17
		bsf	NEW_PACKET		;					;18
		goto	EndHighHalf		;					;19,20

EndByte6:
		clrf	STATE			;					;12
		btfsc	INTCON,T0IF		; End bit=1, end of packet		;13
		goto	EndInt			; End bit=0, invalid packet		;14,15
		movf	SHIFT0,w		;					;15
		movwf	DATA6			;					;16
		bsf	NEW_PACKET		;					;17
		goto	EndHighHalf		;					;18,19


; ----- Tables on first 256 bytes --------------------------------------

BitPos:
		clrf	PCLATH
		addwf PCL,f
		retlw	b'00000001'		; 0	
		retlw	b'00000010'		; 1	
		retlw	b'00000100'		; 2	
		retlw	b'00001000'		; 3	
		retlw	b'00010000'		; 4	
		retlw	b'00100000'		; 5	
		retlw	b'01000000'		; 6	
		retlw	b'10000000'		; 7	

DecodeCommand:
		swapf	DATA2,w
		movwf	TEMP
		rrf	TEMP,w
		andlw	0x07
		addwf	PCL,f

		goto	DecControl		; 000	Decoder Control
		goto	Advanced		; 001	Advanced operations (4 bytes packet)
		goto	Forward			; 010	Forward
		goto	Reverse			; 011	Reverse
		goto	Function		; 100	Function FL,F1..F4
		goto	Function1		; 101	Function F1..F8
		return				; 110
		goto	PoM			; 111	PoM

; ----------------------------------------------------------------------

INIT:
		clrf	GPIO
		movlw	0x07
		movwf	CMCON			; set GP2:0 to digital I/O

		bsf	STATUS,RP0		; bank 1
		movlw	GP_TRIS
		movwf	TRISIO
		call	0x3FF			; get OSCCAL value
		movwf	OSCCAL
		movlw	WPU_INI			; pull-ups
		movwf	WPU
		movlw	b'00000100'
		movwf	IOC			; interrupt on change on GP3
		clrf	VRCON			; voltage reference off
		movlw	OPTION_INI		; Option register: no pull-up, falling GP2, no prescaler, wdt 1:1
		movwf	OPTION_REG
		movlw	PIE1_INI
		movwf	PIE1
		bcf	STATUS,RP0		; bank 0
		clrf	PIR1
		movlw	0x01			; Timer 1 on, 1:1
		movwf	T1CON

		movlw	0x20			; clear RAM
		movwf	FSR
ClearRAM:
		clrf	INDF
		incf	FSR,f
		movlw	0x60
		xorwf	FSR,w
		btfss	STATUS,Z
		goto	ClearRAM

		movlw	INTC_INI
		movwf	INTCON			; enable interrupt on change

		clrf	PAGEREG			; page register default
		bsf	RANDOM0,0		; to be sure RANDOM shift reg. isn't zero
		bsf	SHOOTCNT,2		; init shooting prescaler

		call	LoadCV			; load CV values


; ----------------------------------------------------------------------

MainLoop:
		btfsc	NEW_PACKET		; new packet?
		call	Decode			; yes, decode

Loop:
		btfsc	RESET_FLG		; no output on reset
		goto	MainLoop

		swapf	RANDOM0,w		; RANDOM shift register
		xorwf	RANDOM1,w		;
		movwf	TEMP			;
		rrf	TEMP,w			;
		rlf	RANDOM3,f		;
		rlf	RANDOM2,f		;
		rlf	RANDOM1,f		;
		rlf	RANDOM0,f		;

		btfss	PIR1,TMR1IF		; end timer?
		goto	MainLoop

		bcf	PIR1,TMR1IF		; yes, reload
		movlw	0xF1
		movwf	TMR1H

		btfsc	PWRSRC			; works in analog?
		decfsz	ANALOGCNT,f		; yes, check analog signal
		goto	Mux			; 
		call	Analog			; no DCC signal present

Mux:
		btfss	MUX			; multiplexed mode?
		goto	Flashing

		clrf	GPIO			; all off
Mux0:
		movf	SHOOTCNT,w
		xorlw	0x04
		btfss	STATUS,Z
		goto	Mux1
		btfsc	OUTPUT,OUT1
		bsf	GPIO,OUT1
Mux1:
		xorlw	(0x04 ^ 0x03)
		btfss	STATUS,Z
		goto	Mux2
		btfsc	OUTPUT,OUT2
		bsf	GPIO,OUT2
Mux2:
		xorlw	(0x03 ^ 0x02)
		btfss	STATUS,Z
		goto	Mux3
		btfsc	OUTPUT,OUT4
		bsf	GPIO,OUT4
Mux3:
		xorlw	(0x02 ^ 0x01)
		btfss	STATUS,Z
		goto	Flashing
		btfsc	OUTPUT,OUT5
		bsf	GPIO,OUT5
		

Flashing:
		decfsz	SHOOTCNT,f		; prescaler
		goto	MainLoop
		bsf	SHOOTCNT,2

Flashing0:
		btfss	FLSHOOT0		; shooting sequence?
		goto	Flashing1

		decfsz	FLTIME0,f		; yes, wait time
		goto	Flashing1

		movf	RANDOM0,w		; new time
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME0
FlashingOn0:
		btfss	OUTPUT,OUT1
		goto	FlashingOff0
		bcf	OUTPUT,OUT1
		bcf	GPIO,OUT1
		goto	Flashing1
FlashingOff0:
		bsf	OUTPUT,OUT1
		btfss	MUX
		bsf	GPIO,OUT1
		decfsz	FLCNT0,f
		goto	Flashing1
		bcf	FLSHOOT0

Flashing1:
		btfss	FLSHOOT1		; shooting sequence?
		goto	Flashing2

		decfsz	FLTIME1,f		; yes, wait time
		goto	Flashing2

		movf	RANDOM1,w		; new time
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME1
FlashingOn1:
		btfss	OUTPUT,OUT2
		goto	FlashingOff1
		bcf	OUTPUT,OUT2
		bcf	GPIO,OUT2
		goto	Flashing2
FlashingOff1:
		bsf	OUTPUT,OUT2
		btfss	MUX
		bsf	GPIO,OUT2
		decfsz	FLCNT1,f
		goto	Flashing2
		bcf	FLSHOOT1

Flashing2:
		btfss	FLSHOOT2		; shooting sequence?
		goto	Flashing3

		decfsz	FLTIME2,f		; yes, wait time
		goto	Flashing3

		movf	RANDOM2,w		; new time
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME2
FlashingOn2:
		btfss	OUTPUT,OUT4
		goto	FlashingOff2
		bcf	OUTPUT,OUT4
		bcf	GPIO,OUT4
		goto	Flashing3
FlashingOff2:
		bsf	OUTPUT,OUT4
		btfss	MUX
		bsf	GPIO,OUT4
		decfsz	FLCNT2,f
		goto	Flashing3
		bcf	FLSHOOT2

Flashing3:
		btfss	FLSHOOT3		; shooting sequence?
		goto	Flashing4

		decfsz	FLTIME3,f		; yes, wait time
		goto	Flashing4

		movf	RANDOM3,w		; new time
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME3
FlashingOn3:
		btfss	OUTPUT,OUT5
		goto	FlashingOff3
		bcf	OUTPUT,OUT5
		bcf	GPIO,OUT5
		goto	Flashing4
FlashingOff3:
		bsf	OUTPUT,OUT5
		btfss	MUX
		bsf	GPIO,OUT5
		decfsz	FLCNT3,f
		goto	Flashing4
		bcf	FLSHOOT3

Flashing4:
		goto	MainLoop


; ----------------------------------------------------------------------

Analog:
		bsf	ANALOGCNT,3
		movf	CV13,w			; F1..F4 analog
		call	FuncAnalog
		movf	CV13,w			; F5..F8 analog
		call	Func1Analog
AnalogLight:
		btfsc	FLUORESC		; light analog
		goto	AnalogFluor		; fluorescent analog

		btfss	DCCIN			; direction
		goto	AnLightBkw
AnLightFwd:
		btfss	CV14,0
		goto	NoLight
		goto	FwdLight
AnLightBkw:
		btfss	CV14,1
		goto	NoLight
		goto	BackLight

AnalogFluor:
		btfss	DCCIN			; direction
		goto	AnFluorBkw
AnFluorFwd:
		btfss	CV14,0
		goto	FluorOff
		goto	FluorOn

AnFluorBkw:
		btfss	CV14,1
		goto	FluorOff
		goto	FluorOn

; ----------------------------------------------------------------------
		
Decode:
		bcf	INTCON,GIE		; disable interrupts for more speed
		bcf	NEW_PACKET		; prepare for next packet

		movf	DATA1,w			; exclusive or check
		xorwf	DATA2,w
		xorwf	DATA3,w
		xorwf	DATA4,w
		xorwf	DATA5,w
		xorwf	DATA6,w

		btfss	STATUS,Z		; valid packet?
		goto	ExitDecode		; no, return

		clrf	ANALOGCNT		; clear analog timeout

		movf	DATA1,w	
		btfss	DATA1,7			; address = '1XXXXXXX' ?
		goto	ShortAddr

		xorwf	CV17,w			; '11AAAAAA''AAAAAAAA'
		btfss	STATUS,Z
		goto	ExitDecode
		movf	DATA2,w
		xorwf	CV18,w
		btfss	STATUS,Z
		goto	ExitDecode

		btfss	LADRE
		goto	ExitDecode
		btfsc	CONSIST
		goto	ExitDecode

		movf	DATA3,w			; put correct order
		movwf	DATA2
		movf	DATA4,w
		movwf	DATA3
		movf	DATA5,w
		movwf	DATA4
		movf	DATA6,w
		movwf	DATA5
		goto	DecodeCommand

ChkCons:
		btfsc	CONSIST			; only if no consist
		goto	ExitDecode
		goto	Broadcast

ShortAddr:
		btfsc	STATUS,Z
		goto	Broadcast		; address = '00000000' ?	

		btfss	LADRE
		xorwf	CV1,w
		btfsc	STATUS,Z
		goto	ChkCons

		movf	CV19,w
		andlw	0x7F
		bcf	CONSIST			; set consist flag
		btfss	STATUS,Z
		bsf	CONSIST
		xorwf	DATA1,w
Broadcast:
		btfss	STATUS,Z
		goto	ChkProg
		goto	DecodeCommand

ChkProg:
		movf	DATA1,w
		andlw	0xF0
		xorlw	0x70			; '0111xxxx'?
		btfsc	STATUS,Z
		goto	CheckSM			; yes, may be service mode

		incfsz	DATA1,w			;'11111111' idle packet
		goto	ExitDecode
		bsf	INTCON,GIE		; yes, don't clear reset flag
		return


; ----------------------------------------------------------------------

DecControl:
		btfsc	DATA2,4			; 000 1 XXXX?
		goto	ConsistControl
		movf	DATA2,w			; reset packet? 000 00000
		btfsc	STATUS,Z
		goto	ResetDec
ExitDecode:
		bcf	RESET_FLG
		bsf	INTCON,GIE		; enable interrupts
		return
		
ResetDec:
		bcf	PROG_2X
		bsf	RESET_FLG
						; reset decoder
		clrf	GPIO

		bsf	INTCON,GIE		; enable interrupts
		return

ConsistControl:
		movf	DATA3,w			; '0001001X' 'ADDR'
		movwf	CV19
		movf	DATA2,w
		xorlw	b'00010010'
		btfsc	STATUS,Z
		bcf	CV19,7
		xorlw	(b'00010010' ^ b'00010011')
		btfsc	STATUS,Z
		bsf	CV19,7
		movf	CV19,w
		movwf	EEDATA0
		movlw	E_CV19
		call	SetParm
		goto	ExitDecode
		
; ----------------------------------------------------------------------

Advanced:
		movf	DATA2,w			; 128 steps
		xorlw	b'00111111'
		btfss	STATUS,Z
		goto	ExitDecode
		btfsc	DATA3,7
		goto	AdvBack
AdvForw:
		btfsc	DIRINV			; CV29,0 dir invert
		goto	AdvBack1
AdvForw1:
		btfsc	CV19,7			; CV19,7 dir invert
		goto	AdvBack2
AdvForw2:
		btfsc	DIR
		goto	ForwExchg
		goto	ExitDecode

AdvBack:
		btfsc	DIRINV			; CV29,0 dir invert
		goto	AdvForw1
AdvBack1:
		btfsc	CV19,7			; CV19,7 dir invert
		goto	AdvForw2
AdvBack2:
		btfss	DIR
		goto	ReverExchg
		goto	ExitDecode

; ----------------------------------------------------------------------

Forward:
		btfsc	DIRINV			; CV29,0 dir invert
		goto	Rever1
Forw1:
		btfsc	CV19,7			; CV19,7 dir invert
		goto	Rever2
Forw2:
		btfss	DIR
		goto	Speed
ForwExchg:
		bcf	DIR
		goto	ExitDecode
Speed:
		btfss	FS28
		call	Light
		goto	ExitDecode
; ----------------------------------------------------------------------

Reverse:
		btfsc	DIRINV			; CV29,0 dir invert
		goto	Forw1
Rever1:
		btfsc	CV19,7			; CV19,7 dir invert
		goto	Forw2
Rever2:
		btfsc	DIR
		goto	Speed
ReverExchg:
		bsf	DIR
		goto	ExitDecode

; ----------------------------------------------------------------------

Light:
		btfsc	FLUORESC			; fluorescent?
		goto	Fluorescent		; yes

		btfss	DATA2,4			; XXXLSSSS
		goto	NoLight
		btfsc	DIR
		goto	BackLight
FwdLight:
		btfss	LASTDIR
		goto	FwdLight1
		btfsc	LASTLT
		return
FwdLight1:
		comf	CV34,w
		andwf	OUTPUT,f		; off
		movf	CV33,w
		iorwf	OUTPUT,f		; on
		bsf	LASTDIR
		bsf	LASTLT
		return

BackLight:
		btfsc	LASTDIR
		goto	BackLight1
		btfsc	LASTLT
		return
BackLight1:
		comf	CV33,w
		andwf	OUTPUT,f		; off
		movf	CV34,w
		iorwf	OUTPUT,f		; on
		bcf	LASTDIR
		bsf	LASTLT
		return

NoLight:
		btfss	LASTLT
		return
		comf	CV33,w			; off
		andwf	OUTPUT,f
		comf	CV34,w			; off
		andwf	OUTPUT,f
		bcf	LASTLT
		return


; ----------------------------------------------------------------------

Function:
		btfsc	FS28			; 28/128 steps?
		call	Light			; yes
		movf	DATA2,w
FuncAnalog:
		andlw	0x0F
		iorwf	F1F8SET,f
		xorlw	0xF0

Func2:
		andwf	F1F8SET,w
		movwf	F1F8SET
		movwf	FSET			; actual F1..F8
		xorwf	F1F8OLD,w
		movwf	FCHG			; changes

		btfsc	FLUORESC		; fluorescent?
		goto	ExitDecode		; yes

		clrf	COUNT
FuncSet:
		movf	COUNT,w
		call	BitPos
		andwf	FCHG,w
		btfss	STATUS,Z
		goto	FuncSet1
FuncSet2:
		incf	COUNT,f
		btfss	COUNT,3
		goto	FuncSet
		movf	OUTPUT,w
		btfss	MUX
		movwf	GPIO
		movf	F1F8SET,w
		movwf	F1F8OLD
		goto	ExitDecode
FuncSet1:
		movlw	CV35
		addwf	COUNT,w
		movwf	FSR
		movf	COUNT,w
		call	BitPos
		andwf	FSET,w
		btfsc	STATUS,Z
		goto	FuncOff
FuncOn:
		movf	INDF,w
		iorwf	OUTPUT,f
		goto	FuncSet2

FuncOff:
		comf	INDF,w
		andwf	OUTPUT,f
		goto	FuncSet2

; ----------------------------------------------------------------------

Function1:
		btfss	DATA2,4			; 1011FFFF
		goto	Function2
		swapf	DATA2,w
Func1Analog:
		andlw	0xF0
		iorwf	F1F8SET,f
		xorlw	0x0F
		goto	Func2


Function2:
						; 1010FFFF
		goto	ExitDecode


; --------------------------------------------------------------------------

Fluorescent:
		btfss	DATA2,4			; XXXLSSSS
		goto	FluorOff
FluorOn:
		btfsc	LASTLT
		return

		clrf	OUTPUT

		movf	RANDOM0,w		; number of flashes
		andlw	0x03
		addlw	0x02
		movwf	FLCNT0
		movf	RANDOM0,w		; time on
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME0
		btfsc	RANDOM0,7
		bsf	OUTPUT,OUT1
		bsf	FLSHOOT0		; start shooting sequence

		movf	RANDOM1,w		; number of flashes
		andlw	0x03
		addlw	0x02
		movwf	FLCNT1
		movf	RANDOM1,w		; time on
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME1
		btfsc	RANDOM1,7
		bsf	OUTPUT,OUT2
		bsf	FLSHOOT1		; start shooting sequence

		movf	RANDOM2,w		; number of flashes
		andlw	0x03
		addlw	0x02
		movwf	FLCNT2
		movf	RANDOM2,w		; time on
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME2
		btfsc	RANDOM2,7
		bsf	OUTPUT,OUT4
		bsf	FLSHOOT2		; start shooting sequence

		movf	RANDOM3,w		; number of flashes
		andlw	0x03
		addlw	0x02
		movwf	FLCNT3
		movf	RANDOM3,w		; time on
		andlw	0x3C
		addlw	0x04
		movwf	FLTIME3
		btfsc	RANDOM3,7
		bsf	OUTPUT,OUT5
		bsf	FLSHOOT3		; start shooting sequence

		movf	OUTPUT,w
		movwf	GPIO
		bsf	LASTLT
		return

FluorOff:
		btfss	LASTLT
		return
		clrf	GPIO			; all off
		clrf	OUTPUT
		bcf	FLSHOOT0
		bcf	FLSHOOT1
		bcf	FLSHOOT2
		bcf	FLSHOOT3
		bcf	LASTLT
		return


; --------------------------------------------------------------------------

; LLLLLLLL 1110CCAA AAAAAAAA DDDDDDDD EEEEEEEE	; CC=11 write byte
;  DATA1    DATA2    DATA3    DATA4    DATA5

PoM:
		btfss	PROG_2X
		goto	SetSM_Flag

		bcf	PROG_2X
		movf	DATA4,w			; save data
		movwf	EEDATA0

		movlw	b'11101100'		; CC=11 write byte
		xorwf	DATA2,w
		btfss	STATUS,Z
		goto	ExitProg
		
		movf	DATA3,w			; CV1 not valid
		btfss	CONSIST			; consist address not valid
		btfsc	STATUS,Z
		goto	ExitProg
		call	FindCV
		btfsc	NOCV
		goto	ExitProg

WritePoM:					;11 Write byte
		btfsc	RDONLY
		goto	CheckCV8PoM
		call	SetParm
		call	LoadCV
		goto	ExitProg

CheckCV8PoM:
		xorlw	E_CV8			; CV8?
		btfss	STATUS,Z
		goto	ExitProg
CheckResetCVPoM:
		movlw	d'33'			; CV8 = 33 -> reset CV
		xorwf	DATA4,w
		btfss	STATUS,Z
		goto	ExitProg
		call	ResetCV			; program CV defaults
		call	LoadCV
		goto	ExitProg


; --------------------------------------------------------------------------


;************* SM Mode *******************************************
; Service Mode

CheckSM:
		btfss	RESET_FLG		; check for SM, reset packet has to come first
		goto	ServModeError
		btfss	PROG_2X
		goto	SetSM_Flag

		btfsc	DCC4BYTE		; 3 or 4 byte packet?
		goto	ChkProg4

; 0111CRRR DDDDDDDD EEEEEEEE

		bcf	PROG_2X
		movf	DATA2,w				; save data
		movwf	EEDATA0

		movf	DATA1,w				; 3 byte programming
		andlw	b'11110111'
		xorlw	b'01110101'			; Reg6
		btfsc	STATUS,Z		
		goto	REG6
		xorlw	(b'01110101')^(b'01110100')	; Reg5
		btfsc	STATUS,Z		
		goto	REG5
		xorlw	(b'01110100')^(b'01110110')	; reg7
		btfsc	STATUS,Z
		goto	REG7	
		xorlw	(b'01110110')^(b'01110111')	; reg8
		btfsc	STATUS,Z
		goto	REG8	

		movf	DATA1,w
		andlw	0x03
		addwf	PAGEREG,w
		call	FindCV
		btfsc	NOCV
		goto	ExitProg
ProgReg:
		btfss	DATA1,3
		goto	EEVERI
		goto	EEPROG

REG5:
		movlw	E_CV29			; CV29 configuration
		goto	ProgReg

REG6:
		btfss	DATA1,3			; read or write
		goto	REG6RD
		decf	DATA2,f			; Page register
		rlf	DATA2,f
		rlf	DATA2,w
		andlw	b'11111100'		; page 1 and 129 are the same. CV1 & CV513
		movwf	PAGEREG
		goto	ExitProg
REG6RD:
		decf	DATA2,f			; read page register
		rlf	DATA2,f
		rlf	DATA2,w
		andlw	b'11111100'		; page 1 and 129 are the same. CV1 & CV513
		xorwf	PAGEREG,w
		goto	EEVERIP

REG7:
		movlw	E_CV7			; only read
		btfss	DATA1,3
		goto	EEVERI
		goto	ExitProg

REG8:
		movlw	E_CV8			; only read
		btfss	DATA1,3
		goto	EEVERI
		goto	CheckResetCV		; if CV8 = 33 reset CV

	
EEPROG:
		btfsc	RDONLY
		goto	CheckCV8
		call	SetParm			; program EEPROM
		call	AckPulse		; do ACK
		bcf	PROG_2X
		bcf	NEW_PACKET
		call	LoadCV
		goto	ExitProg


EEVERI:
		call	EE_Read			; check data
		xorwf	DATA2,w
EEVERIP:
		btfss	STATUS,Z
		goto	ExitProg
DoAck:
		call	AckPulse		; equal, do ACK
		bcf	PROG_2X
;		bcf	NEW_PACKET
		goto	ExitProg


SetSM_Flag:
		bsf	PROG_2X
		goto	ExitProg

ServModeError:
		bcf	RESET_FLG
		bcf	PROG_2X
		goto	ExitProg

CheckCV8:
		xorlw	E_CV8			; CV8?
		btfss	STATUS,Z
		goto	ExitProg
CheckResetCV:
		movlw	d'33'			; CV8 = 33 -> reset CV
		xorwf	DATA2,w
		btfss	STATUS,Z
		goto	ExitProg
CheckResetCVP:
		call	ResetCV			; program CV defaults
		call	AckPulse		; do ACK
		bcf	PROG_2X
		bcf	NEW_PACKET
		call	LoadCV
		goto	ExitProg

ExitProg:
		bsf	INTCON,GIE		; enable interrupts
		return
		
; -----------------------------------------------------------------------------------

AckPulse:
		movlw	b'00110111'		; all lights on
		movwf	GPIO
		movlw	d'6'			; 6ms pulse
		movwf	TEMP
		movlw	0x00
AckNext:
		addlw	0xFF			;1
		btfss	STATUS,Z		;2
		goto	$-2			;3,4
		decfsz	TEMP,f
		goto	AckNext
		clrf	GPIO			; all light off
		return

; -----------------------------------------------------------------------------------

; 0111CCAA AAAAAAAA DDDDDDDD EEEEEEEE

ChkProg4:
		bcf	PROG_2X
;		bcf	RESET_FLG
		movf	DATA3,w			; save data
		movwf	EEDATA0

		btfsc	DATA1,0			; CV513.. or CV1..
		goto	ExitProg

		movf	DATA2,w			; x0AAAAAAAA
		call	FindCV
		btfsc	NOCV
		goto	ExitProg
ProgDirect:
		btfsc	DATA1,3	
		goto	RomNxt
		btfss	DATA1,2
		goto	ExitProg		;00 not defined
RomNxt:
		btfss	DATA1,2
		goto	BitMan			;10 Bit Manipulation
		btfss	DATA1,3
		goto	EEVERI4			;01 Verify byte
WriteDirect:					;11 Write byte
		btfsc	RDONLY
		goto	CheckCV8D
		call	SetParm
		call	AckPulse
		bcf	PROG_2X
		bcf	NEW_PACKET
		call	LoadCV
		goto	ExitProg

EEVERI4:
		call	EE_Read			; check data
		xorwf	DATA3,w
		goto	EEVERIP

CheckCV8D:
		movlw	d'33'			; CV8 = 33 -> reset CV
		xorwf	DATA3,w
		btfss	STATUS,Z
		goto	ExitProg
		goto	CheckResetCVP


; 0111CCAA AAAAAAAA 111KDBBB EEEEEEEE

BitMan:
		call	EE_Read
		movwf	EEDATA0
		movlw	EEDATA0
		movwf	FSR
		movlw	b'00000111'
		andwf	DATA3,w
		call	BitPos
		btfss	DATA3,4			; K
		goto	Vbit			; K=0,verify bit
		btfsc	DATA3,3
		iorwf	INDF,f			; D=1,set bit
		xorlw	0xFF
		btfss	DATA3,3
		andwf	INDF,f			; D=0,clear bit
		movf	DATA2,w
		call	FindCV
		goto	ProgDirect		; write complete byte

Vbit:
		andwf	INDF,w
		btfsc	STATUS,Z
		goto	BitClear
BitSet:
		btfsc	DATA3,3			;D=0
		goto	DoAck			;D=1, ack
		goto	ExitProg
BitClear:
		btfss	DATA3,3			;D=1
		goto	DoAck			;D=0, ack
		goto	ExitProg

; -----------------------------------------------------------------------------------

LoadCV:
		movlw	CV1			; first CV to read
		movwf	FSR
		movlw	E_CV1
		movwf	EEADR0
LoadNext:
		movf	EEADR0,w
		call	EE_Read
		movwf	INDF
		movlw	CV50			; last CV to read
		xorwf	FSR,w
		btfsc	STATUS,Z
		return
		incf	FSR,f
		incf	EEADR0,f
		goto	LoadNext

; -----------------------------------------------------------------------------------

FindCV:
		bcf	NOCV			; w = CV - 1
		bcf	RDONLY

		xorlw	0x00			; CV1
		btfsc	STATUS,Z
		retlw	E_CV1
		xorlw	(0x00 ^ 0x0C)		; CV13
		btfsc	STATUS,Z
		retlw	E_CV13
		xorlw	(0x0C ^ 0x0D)		; CV14
		btfsc	STATUS,Z
		retlw	E_CV14
		xorlw	(0x0D ^ 0x10)		; CV17
		btfsc	STATUS,Z
		retlw	E_CV17
		xorlw	(0x10 ^ 0x11)		; CV18
		btfsc	STATUS,Z
		retlw	E_CV18
		xorlw	(0x11 ^ 0x12)		; CV19
		btfsc	STATUS,Z
		retlw	E_CV19
		xorlw	(0x12 ^ 0x1C)		; CV29
		btfsc	STATUS,Z
		retlw	E_CV29

		xorlw	(0x1C ^ 0x20)		; CV33
		btfsc	STATUS,Z
		retlw	E_CV33
		xorlw	(0x20 ^ 0x21)		; CV34
		btfsc	STATUS,Z
		retlw	E_CV34
		xorlw	(0x21 ^ 0x22)		; CV35
		btfsc	STATUS,Z
		retlw	E_CV35
		xorlw	(0x22 ^ 0x23)		; CV36
		btfsc	STATUS,Z
		retlw	E_CV36
		xorlw	(0x23 ^ 0x24)		; CV37
		btfsc	STATUS,Z
		retlw	E_CV37
		xorlw	(0x24 ^ 0x25)		; CV38
		btfsc	STATUS,Z
		retlw	E_CV38
		xorlw	(0x25 ^ 0x26)		; CV39
		btfsc	STATUS,Z
		retlw	E_CV39
		xorlw	(0x26 ^ 0x27)		; CV40
		btfsc	STATUS,Z
		retlw	E_CV40
		xorlw	(0x27 ^ 0x28)		; CV41
		btfsc	STATUS,Z
		retlw	E_CV41
		xorlw	(0x28 ^ 0x29)		; CV42
		btfsc	STATUS,Z
		retlw	E_CV42
		xorlw	(0x29 ^ 0x31)		; CV50
		btfsc	STATUS,Z
		retlw	E_CV50

		bsf	RDONLY
		xorlw	(0x31 ^ 0x06)		; CV7
		btfsc	STATUS,Z
		retlw	E_CV7
		xorlw	(0x06 ^ 0x07)		; CV8
		btfsc	STATUS,Z
		retlw	E_CV8

		bsf	NOCV			; CV not finded
		retlw	0x7F			; return last location

;---------------------------------------------------------------------------

ResetCV:
		movlw	0x03			; reset CV to default values
		movwf	EEDATA0
		movlw	E_CV1
		call	SetParm

		movlw	0xC0
		movwf	EEDATA0
		movlw	E_CV17
		call	SetParm
		movlw	0x03
		movwf	EEDATA0
		movlw	E_CV18
		call	SetParm
		movlw	E_CV14
		call	SetParm
		movlw	0x00
		movwf	EEDATA0
		movlw	E_CV19
		call	SetParm
		movlw	0x06
		movwf	EEDATA0
		movlw	E_CV29
		call	SetParm

		movlw	0x01
		movwf	EEDATA0
		movlw	E_CV33
		call	SetParm
		movlw	0x02
		movwf	EEDATA0
		movlw	E_CV34
		call	SetParm

		movlw	0x10
		movwf	EEDATA0
		movlw	E_CV35
		call	SetParm
		movlw	0x20
		movwf	EEDATA0
		movlw	E_CV36
		call	SetParm
		movlw	0x00
		movwf	EEDATA0
		movlw	E_CV13
		call	SetParm
		movlw	E_CV37
		call	SetParm
		movlw	E_CV38
		call	SetParm
		movlw	E_CV39
		call	SetParm
		movlw	E_CV40
		call	SetParm
		movlw	E_CV41
		call	SetParm
		movlw	E_CV42
		call	SetParm
		movlw	E_CV50
		call	SetParm

		return


;---------------------------------------------------------------------------

EE_Read:
		bsf	STATUS,RP0		; w=ADR
		movwf	EEADR
		bsf	EECON1,RD
		movf	EEDATA,w
		bcf	STATUS,RP0
		return

SetParm:
		call	EE_Read			; w=ADR, EEDATA0=data. Write only changes
		xorwf	EEDATA0,w
		btfsc	STATUS,Z
		return
EE_Write:		
		movf	EEDATA0,w
		bsf	STATUS,RP0
		movwf	EEDATA
		bsf	EECON1,WREN
		bcf	INTCON,GIE
		movlw	0x55
		movwf	EECON2
		movlw	0xAA
		movwf	EECON2
		bsf	EECON1,WR
		bsf	INTCON,GIE
		bcf	EECON1,WREN
EEWrite0:
		btfsc	EECON1,WR
		goto	EEWrite0
		bcf	STATUS,RP0
		return



; ----- EEPROM default values

		org	0x2100


		dw	0x03			; CV1	Primary Adress
		dw	0x14			; CV7	Manufacturer Version
		dw	0x0D			; CV8	Manufacturer ID
		dw	0x00			; CV13	Analog F1..F8
		dw	0x03			; CV14	Analog FL,FR (F9..F12)
		dw	0xC0			; CV17	Extended Adress High Byte
		dw	0x03			; CV18	Extended Adress Low Byte
		dw	0x00			; CV19	Consist Adress
		dw	0x06			; CV29	0=DIR 1=14/28 2=PWRSRC 3=ADVACK 4=SPDTAB 5=LADRE 6= 7=ACCDEC
		dw	0x01			; CV33	front light -> 	GPIO (xx54x210)
		dw	0x02			; CV34	back light  -> 	GPIO (xx54x210)
		dw	0x10			; CV35	F1 -> 		GPIO (xx54x210)
		dw	0x20			; CV36	F2 -> 		GPIO (xx54x210)
		dw	0x00			; CV37	F3 -> 		GPIO (xx54x210)
		dw	0x00			; CV38	F4 -> 		GPIO (xx54x210)
		dw	0x00			; CV39	F5 -> 		GPIO (xx54x210)
		dw	0x00			; CV40	F6 -> 		GPIO (xx54x210)
		dw	0x00			; CV41	F7 -> 		GPIO (xx54x210)
		dw	0x00			; CV42	F8 -> 		GPIO (xx54x210)
		dw	0x00			; CV50	Config



		org	0x2130

		dt	"dcc_func"
		dt	"F.Cañada"
		dt	(__VERDAY   >> 4)  +0x30
		dt	(__VERDAY   & 0x0F)+0x30,"/"
		dt	(__VERMONTH >> 4)  +0x30
		dt	(__VERMONTH & 0x0F)+0x30,"/"
		dt	(__VERYEAR  >> 4)  +0x30
		dt	(__VERYEAR  & 0x0F)+0x30


	end
