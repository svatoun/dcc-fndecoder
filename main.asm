; original File = dcc_func_wagon.HEX

#undefine OptimizationFix1
#undefine OptimizationFix2
#undefine ProgrammingLock

; original File = Z:\home\sdedic\vlacky\FuncDec.X\dcc_func_wagon.HEX

    processor 12F629
    #include "p12f629.inc"
    __config 0x3FC4
;   _CPD_OFF & _CP_OFF & _BODEN_ON & _MCLRE_OFF & _PWRTE_ON & _WDT_OFF 
;   & _INTRC_OSC_NOCLKOUT 

;   EEPROM-Data
    Org 0x2100
    DE 0x03, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0x1E, 0x0D   ;  ........
    DE 0xFF, 0xFF, 0xFF, 0xFF, 0x00, 0x03, 0xFF, 0xFF   ;  ........
    DE 0xC0, 0x64, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF   ;  .d......
    DE 0xFF, 0xFF, 0xFF, 0xFF, 0x06, 0xFF, 0xFF, 0xFF   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x0F, 0x0F, 0x0F, 0x0F   ;  ........
    DE 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF   ;  ........
    DE 0xFF, 0x0C, 0x0C, 0x0C, 0x0C, 0xFF, 0xFF, 0xFF   ;  ........
    DE 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF   ;  ........
    DE 0x01, 0x02, 0x04, 0x04, 0x08, 0x08, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........
    DE 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00   ;  ........

#define		DCCIN	GPIO,2              ; DCC input pin

#define     CV_ANALOG_ENABLE    CV29,2
#define	    CV_SPEED_28		CV29,1

#define     PACKET_READY LRAM_0x5B,0    ; received packet ready
#define	    IGNORE_CV_FLAG	 LRAM_0x5B,1
#define     RESCV_FLAG   LRAM_0x5B,2    ; received packet ready
#define     FOUR_BYTE   LRAM_0x5B,3     ; four-byte packet ???
#define     SRVMODE_FLAG LRAM_0x5B,4    ; service mode enabled
#define     SRV2X_FLAG   LRAM_0x5B,6    ; one service-mode packet received
#define     RESET_FLAG   LRAM_0x5B,7    ; reset packet received

#define     DCC_HIGH_FLAG   LRAM_0x5C,0     ; Polarity A; exclusive with Polarity B
#define     DCC_LOW_FLAG    LRAM_0x5C,1     ; Polarity B
#define     FLASH_A_STATE   LRAM_0x5C,3     ; state of flash "A"
#define     FLASH_B_STATE   LRAM_0x5C,4     ; state of flash "B"

#define     CURRENT_DIR	    SPEED,7    ; current effective direction. 1 - forward, 0 - backward.

#define     FN_DIRLIGHT_BIT  FSTATE,4


Load_Osccal equ 0x3ff

; RAM-Variable
INT_W		    equ 0x20	    ; save during INTR
INT_STAT	    equ 0x21	    ; save during INTR
INT_FSR		    equ 0x22	    ; save during INTR
INT_READ_BYTE	    equ 0x23
INT_XOR_BYTE	    equ 0x24
DCCSTATE	    equ 0x25
INT_BUF_PTR	    equ 0x26
INT_BIT_COUNT	    equ 0x27

AnalogTimeCounter   equ 0x28
Maybe_PAGEREG	    equ 0x2A

DATA1 equ 0x2B
DATA2 equ 0x2C
DATA3 equ 0x2D
DATA4 equ 0x2E
DATA5 equ 0x2F

COMMAND1  equ 0x31
COMMAND2  equ 0x32
COMMAND3  equ 0x33

SPEED     equ 0x34          ; Bit 0 is insignificant, AND to 0x7E to get speed
FSTATE    equ 0x35          ; Functions 0-4. Bits 0..3 are assigned to F1..F4. Bit 4 is F0 (FL/FR)

LRAM_0x36 equ 0x36          ; Functions 5-12
LRAM_0x37 equ 0x37          ; Functions 13-20
LRAM_0x38 equ 0x38          ; Functions 21-28

LRAM_0x39 equ 0x39
LRAM_0x3A equ 0x3A
LRAM_0x3B equ 0x3B

BRIGHT_FA equ 0x3C          ; "nejaky counter" s FA
BRIGHT_FB equ 0x3D          ; "nejaky counter" s FB
BRIGHT_FC equ 0x3E          ; "nejaky counter" s FC
BRIGHT_FD equ 0x3F          ; "nejaky counter" s FD
BRIGHT_BASE equ BRIGHT_FA

LRAM_0x40 equ 0x40
INCREMENT equ 0x41
FLASH_A_COUNTER equ 0x42
FLASH_B_COUNTER equ 0x43

; 0x44, 6   = FA OFF
; 0x44, 7   = FA ON

; lower 4 bits of 0x44 = state for dispatch (whatever it means)

; 0x45, 6   = FB OFF
; 0x45, 7   = FB ON
; 0x46, 6   = FC OFF
; 0x46, 7   = FC ON
; 0x47, 6   = FD OFF
; 0x47, 7   = FD ON
TODO_FA equ 0x44
TODO_FB equ 0x45
TODO_FC equ 0x46
TODO_FD equ 0x47
TODO_BASE equ TODO_FA

UNUSED_FE equ 0x48
UNUSED_FF equ 0x49
UNUSED_FG equ 0x4A
UNUSED_FI equ 0x4B
UNUSED_FJ equ 0x4C
UNUSED_FK equ 0x4D
UNUSED_FL equ 0x4E
CV_INDEX equ 0x4F	; temporary; CV index during lock check

CV1 equ 0x50
CV17 equ 0x51
CV18 equ 0x52
CV29 equ 0x53
CV15 equ 0x54
CV16 equ 0x55

TEMP_VAR equ 0x59	; temporary variable, saving intermediate results
FUNCTION_ID equ 0x5A
LRAM_0x5B equ 0x5B
LRAM_0x5C equ 0x5C

EEDATA0 equ 0x5E
EEPROM_WRITE_START equ 0x5F

; EEPROM contains CV values, starting from 0 = CV1.
EEPROM_CV1      equ 0x00
EEPROM_CV13     equ 0x0C
EEPROM_CV15	equ 0x0E
EEPROM_CV16	equ 0x0F
EEPROM_CV17     equ 0x10
EEPROM_CV18     equ 0x11
EEPROM_CV29     equ 0x1C

EEPROM_CV37     equ 0x24
EEPROM_CV50     equ 0x31

EEPROM_CV120    equ 0x40
EEPROM_CV124    equ 0x44

EEPROM_OUTCONF_BASE equ EEPROM_CV120

; Program

    Org 0x0000

;   Reset-Vector
    CLRF STATUS
    CLRF INTCON
    CLRF PCLATH          ; !!Bank Program-Page-Select
    GOTO INIT

;   Interrupt-Vector
Interrupt:
    MOVWF INT_W          ; 1
    SWAPF STATUS,W       ; 2
    CLRF  STATUS         ; 3
    MOVWF INT_STAT       ; 4
    BTFSC INTCON,T0IF    ; 5
    GOTO In_LongBit     ; 6,7
    MOVLW 0xB3           ; (256 - 77us): between 64us (one) and 90us (zero);8
    MOVWF TMR0           ; !!Bank!! TMR0 - OPTION_REG
    MOVF DCCSTATE,W
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO IN_StartPreamble     ; 0x00
    GOTO IN_PreambleBit       ; 0x01    reading up to 0x16 short changes = 11 preamble bits
    GOTO In_ExitInterrupt     ; 0x02    ignore following longs, regardless of high/low
    GOTO IN_ResetReader       ; 0x03    bail out on malformed start/stop bit (0/1)
    GOTO IN_OneFirst          ; 0x04    dec cnt, shift to 6. On terminal bit, transition to 7
    GOTO IN_ResetReader       ; 0x05    reset, first change = 0, 2nd change = 1, invalid bit.
    GOTO IN_ShiftBit_1        ; 0x06    shit bit, go to 4
    GOTO IN_TerminalBitOK     ; 0x07    terminal bit received

In_LongBit:
    MOVLW 0xB3           ;   b'10110011'  d'179'
    MOVWF TMR0           ; 77us: between 64us (one) and 90us (zero);8
    MOVF DCCSTATE,W
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO In_ExitInterrupt   ;   0x00     looking for preamble
    GOTO IN_ResetReader     ;   0x01     short preamble, reset
    GOTO IN_StartBitFirst   ;   0x02     start bit, 1st change
    GOTO IN_StartBitSecond  ;   0x03     start bit, complete. shift to state 4, still cnt = 9
    GOTO IN_ZeroFirst       ;   0x04     0, first change. shift to state 5, dec cnt, don't store bit, but store byte if complete
    GOTO IN_ShiftBit_0      ;   0x05     0, complete. shift to state 4, shift bit.
    GOTO IN_ResetReader     ;   0x06     reset, first change = 1, 2nd change = 0, invalid bit.
    GOTO IN_ResetReader     ;   0x07     reset, invalid terminal bit (1/0)

IN_ResetReader:
    CLRF DCCSTATE
In_ExitInterrupt:
    MOVF GPIO,W          ; !!Bank!! GPIO - TRISIO
    BCF INTCON,GPIF
    BCF INTCON,T0IF
    SWAPF INT_STAT,W
    MOVWF STATUS
    SWAPF INT_W,F
    SWAPF INT_W,W
    RETFIE
IN_StartPreamble:
    MOVLW 0x16           ;   b'00010110'  d'022'
    MOVWF INT_BIT_COUNT
    INCF DCCSTATE,F
    GOTO In_ExitInterrupt
IN_OneFirst:
    BSF DCCSTATE,1       ; DCCSTATE = 6
IN_PreambleBit:
    DECFSZ INT_BIT_COUNT,F
    GOTO In_ExitInterrupt
IN_StartBitSecond:
IN_PreambleOK:
    INCF DCCSTATE,F
    GOTO In_ExitInterrupt
IN_StartBitFirst:
    ; Start bit (0) was already received. The next bit will be MSB of the 1st data byte.
    MOVLW DATA1           ;   2B, 2C, 2D, 2E, 2F - 5 byte buffer b'00101011'  d'043'  "+"
    MOVWF INT_BUF_PTR
    CLRF INT_XOR_BYTE
    GOTO IN_ExpectStartBitEnd

; This routine is ALSO called when the 1st change of 9th bit (separator) is read.
; In the case a regular bit first 0 change is read, it's just ignored/accepted,
; store is done in 2nd change (verification). If the bit is final (start/stop),
; the result byte is stored in buffer and XORed.
IN_ZeroFirst:
    INCF DCCSTATE,F       ; = 5
    DECFSZ INT_BIT_COUNT,F
    GOTO In_ExitInterrupt
    BTFSS INT_BUF_PTR,3   ; buffer is 2B .. 2F, 5 byte
    GOTO IN_ResetReader
    MOVF FSR,W
    MOVWF   INT_FSR
    MOVF    INT_BUF_PTR,W
    MOVWF   FSR
    MOVF    INT_READ_BYTE,W
    MOVWF   INDF
    XORWF   INT_XOR_BYTE,F
    INCF INT_BUF_PTR,F
    MOVF INT_FSR,W
    MOVWF FSR
IN_ExpectStartBitEnd:
    MOVLW 0x09           ;   b'00001001'  d'009'
    MOVWF INT_BIT_COUNT
    MOVLW 0x03           ;   b'00000011'  d'003'
    MOVWF DCCSTATE
    GOTO In_ExitInterrupt
IN_ShiftBit_0:
    BCF STATUS,C
    RLF INT_READ_BYTE,F
    DECF DCCSTATE,F
    GOTO In_ExitInterrupt
IN_ShiftBit_1:             ; Shifts a bit into the current byte
    BSF STATUS,C
    RLF INT_READ_BYTE,F
    BCF DCCSTATE,1
    GOTO In_ExitInterrupt
Bit_GetValue:
    ADDWF PCL,F          ; !!Program-Counter-Modification
    RETLW 0x01           ;   b'00000001'  d'001'
    RETLW 0x02           ;   b'00000010'  d'002'
    RETLW 0x04           ;   b'00000100'  d'004'
    RETLW 0x08           ;   b'00001000'  d'008'
    RETLW 0x10           ;   b'00010000'  d'016'
    RETLW 0x20           ;   b'00100000'  d'032'  " "
    RETLW 0x40           ;   b'01000000'  d'064'  "@"
    RETLW 0x80           ;   b'10000000'  d'128'
Process_Command:
    SWAPF COMMAND1,W
    MOVWF TEMP_VAR
    RRF TEMP_VAR,W
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDWF PCL,F          ; !!Program-Counter-Modification
    RETURN              ; 0x00  - Decoder operation, Consist control. Ignore ??
    GOTO LADR_0x013C    ; 0x01  - Advanced operation
    GOTO LADR_0x0152    ; 0x02  - Speed and Direction reverse
    GOTO LADR_0x0145    ; 0x03  - Speed and Direction forward
    GOTO Function_Group1    ; 0x04  - Function group 1
    GOTO Function_Group2    ; 0x05  - Function group 2
    GOTO Function_Extended   ; 0x06  - Functions 13-28
    GOTO Command_POM    ; 0x07  - POM

    ; OPTIMIZATION: can save bytes; the following checks if LRAM_0x40 < 4, if so,
    ; it jumps with the value to Read_Effect; otherwise just returns the value.
LADR_0x006A:
    MOVF LRAM_0x40,W	 
#ifdef OptimizationFix2
    ADDLW 0xFC		 ; 0x00 .. 0x03 does not overflow
    BTFSS STATUS,C
    GOTO Read_Effect
    ADDLW 0xF8		 ; 0x0c + 0xfc = 0x08. Original 0x00..0x0b does NOT overflow
    BTFSS STATUS,C	 ; LRAM_0x40 = 0x0c
    RETURN
    ; 7 instructions
#else
    ADDWF PCL,F          ; !!Program-Counter-Modification           ADDLW   0xFC
    GOTO Read_Effect     ; 0x00                                     BTFSS   STATUS,C
    GOTO Read_Effect     ; 0x01                                     GOTO    Read_Effect
    GOTO Read_Effect     ; 0x02                                     ADDLW   (256 -4 - 12)
    GOTO Read_Effect     ; 0x03                                     BTFSS   STATUS,C
    RETURN               ; 0x04                                     RETURN
    RETURN               ; 0x05                                     ; MOVF LRAM_0x40,W at Read_Effect
    RETURN               ; 0x06                                     ; save1
    RETURN               ; 0x07                                     ; save2
    RETURN               ; 0x08                                     ; save3
    RETURN               ; 0x09                                     ; save4
    RETURN               ; 0x0a                                     ; save5
    RETURN               ; 0x0b                                     ; save6
    ; 14 instructions
    ; 0x0c
#endif
    MOVLW 0xFF           ; NOTE: 0xFF will be INCREMENTED aftr return to 0.
    MOVWF LRAM_0x40
    DECFSZ FLASH_A_COUNTER,F
    GOTO FlashB
    MOVLW 0x31           ;   CV 50 - Flash "A" active period
    BTFSC FLASH_A_STATE
    MOVLW 0x32           ;   CV 51 - Flash "A" inactive period
    CALL EE_Read
    MOVWF FLASH_A_COUNTER
    MOVLW 0x08           ;   
    XORWF LRAM_0x5C,F    ;   Change flash phase
FlashB:
    DECFSZ FLASH_B_COUNTER,F
    RETURN
    MOVLW 0x33           ;   CV 52 - Flash "B" active period
    BTFSC FLASH_B_STATE
    MOVLW 0x34           ;   CV 52 - Flash "B" inactive period
    CALL EE_Read
    MOVWF FLASH_B_COUNTER
    MOVLW 0x10           ;   
    XORWF LRAM_0x5C,F
    RETURN
Read_Effect:
    ADDLW 0x20           ;   b'00100000'  d'032'  " "
    CALL EE_Read
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO Effect_Incandesc
    GOTO Effect_Fluor
    GOTO Effect_Fluor_Broken
    GOTO Effect_Fluor_EOL
    GOTO Effect_FlashA
    GOTO Effect_FlashA_Neg
    GOTO Effect_FlashB
    GOTO Effect_FlashB_Neg

IN_TerminalBitOK:
    MOVF INT_READ_BYTE,W
    XORWF INT_XOR_BYTE,F
    BTFSS STATUS,Z
    GOTO IN_ResetReader ; reset, invalid checksum
    BTFSS DCCIN         ; WTF ??????
    ; this is after receiving TWO "1" signal changes. Check of DCC signal polarity ??
    GOTO IN_DccInLow    ; Jump if DCCIN is low
    BSF DCC_HIGH_FLAG     ; DCCIN is high
    BCF DCC_LOW_FLAG
    GOTO IN_ProcessPacket
IN_DccInLow:
    BCF DCC_HIGH_FLAG     ; DCCIN is low
    BSF DCC_LOW_FLAG
IN_ProcessPacket:
    CLRF AnalogTimeCounter
    MOVF DATA1,W
    BTFSC STATUS,Z       ; skip if NONzero = not RESET packet
    GOTO IN_MaybeReset
    ; nonzero address/1st data packeet
    ANDLW 0xF0           
    XORLW 0x70           ; check of 0x0111????
    BTFSC STATUS,Z       ; skip of other
    GOTO IN_CheckServiceMode     ; received byte / address was 0x0111????
    BCF RESET_FLAG
    BCF SRVMODE_FLAG
    MOVF DATA1,W
    BTFSS DATA1,7        ; bit 7 = long address ?
    BTFSC CV29,5         ; skip when SHORT address is used (CV1)
    GOTO IN_CheckLongAddress
    XORWF CV1,W
    BTFSS STATUS,Z
    GOTO IN_ResetReader  ; not OUR packet, skip.
IN_DecoderCommand:
    MOVF DATA2,W
    MOVWF COMMAND1
    MOVF DATA3,W
    MOVWF COMMAND2
    MOVF DATA4,W
    MOVWF COMMAND3
    BSF   PACKET_READY
    GOTO IN_ResetReader
IN_CheckLongAddress:
    XORWF CV17,W
    BTFSC DATA1,7       
    BTFSS STATUS,Z      ; zero = LSB matches
    GOTO IN_ResetReader ; 10xxxx - accessory packet, bail out
    MOVF DATA2,W
    XORWF CV18,W
    BTFSC CV29,5        ; check for long address flag again
    BTFSS STATUS,Z
    GOTO IN_ResetReader 
    MOVF DATA3,W        ; accept data ??
    MOVWF COMMAND1
    MOVF DATA4,W
    MOVWF COMMAND2
    MOVF DATA5,W
    MOVWF COMMAND3
    BSF PACKET_READY
    GOTO IN_ResetReader
IN_CheckServiceMode:
    BTFSS RESET_FLAG
    GOTO IN_ServModeFail
    BTFSS SRV2X_FLAG
    GOTO IN_ServModeFirst
    BCF SRV2X_FLAG
    BSF SRVMODE_FLAG
    BCF FOUR_BYTE
    MOVF INT_BUF_PTR,W
    XORLW 0x2D           ;   (address) + 0x2B, 0x2C = 3 byte packet
    BTFSS STATUS,Z  
    BSF FOUR_BYTE	 ; 2-byte or 4 byte packet
    MOVF DATA1,W
    MOVWF COMMAND1
    MOVF DATA2,W
    MOVWF COMMAND2
    MOVF DATA3,W
    MOVWF COMMAND3
    BSF PACKET_READY
    GOTO IN_ResetReader
IN_ServModeFirst:
    BSF SRV2X_FLAG
    GOTO IN_ResetReader
IN_ServModeFail:
    BCF RESET_FLAG
    BCF SRV2X_FLAG
    GOTO IN_ResetReader
IN_MaybeReset:
    BCF RESET_FLAG
    BCF SRVMODE_FLAG
    MOVF DATA2,W
    BTFSS STATUS,Z
    GOTO IN_DecoderCommand  ; packet's 2nd byte is != 0, normal command
    BSF RESET_FLAG
    BCF SRV2X_FLAG
    GOTO IN_ResetReader

INIT:
    CLRF GPIO            ;  GPIO
    MOVLW 0x07           ;   b'00000111'  d'007'
    MOVWF CMCON          ; Comparators OFF
    BSF STATUS,RP0        
    MOVLW 0x04           ; Input pin mask
    MOVWF TRISIO         ; GPIO,2 = in, all others = out
    CALL Load_Osccal
    MOVWF OSCCAL         
    MOVLW 0x33           ; Disable weak pull-up for GPIO,2 ???
    MOVWF WPU            ; !!Bank!! Unimplemented - WPU
    MOVLW 0x04           ;   b'00000100'  d'004'
    MOVWF IOC            ; Enable interrupt on GPIO,2 change
    CLRF VRCON           ; !!Bank!! CMCON - VRCON
    MOVLW 0x88           ;   b'10001000'  d'136'
    MOVWF OPTION_REG     ; Option register: no pull-up, falling GP2, no prescaler, wdt 1:1
    CLRF PIE1            ; Disable all interrupts
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    CLRF PIR1            ; !!Bank!! PIR1 - PIE1
    MOVLW 0x01           
    MOVWF T1CON          ; Timer 1 on, 1:1
    MOVLW 0x20           ;   b'00100000'  d'032'  " "
    MOVWF FSR
Clear_RAM:
    CLRF INDF
    INCF FSR,F
    MOVLW 0x60           ;   b'01100000'  d'096'  "`"
    XORWF FSR,W
    BTFSS STATUS,Z
    GOTO Clear_RAM

    MOVLW 0x88           ;   GIE = ON, GPIE = ON
    MOVWF INTCON
    ADDLW 0xC2           ;   0x4A
    MOVWF EEDATA0        ;   will send to EE ??
    CLRF Maybe_PAGEREG
    ADDLW 0xBD           ;   = 0x07
    MOVWF FLASH_A_COUNTER
    MOVWF FLASH_B_COUNTER
    INCF INCREMENT,F     ;   = 0x01 (increment from 0)
    CALL EE_Write        ;   EEPROM(0x07) := 0x4A ???
    CALL LoadAddress
MainLoop:
    BTFSC PACKET_READY
    CALL Process_Packet
    CALL Sub_Function_Bits
    BTFSS RESET_FLAG     ; goto main loop on reset flag, ignore timer
    BTFSS PIR1,0         
    GOTO MainLoop        ; loop if timer did not fire

    ; Some data ... ? Overwrites in "W"
    MOVLW 0x8F           ;   b'10001111'  d'143'
    MOVLW 0x0E           ;   b'00001110'  d'014'
    MOVLW 0xFD           ;   3 * 0x100; if TMR1 runs @ 4MHz, then 5,2x / sec = ~200ms
    MOVWF TMR1H          ;
    BCF PIR1,0

    CALL LADR_0x0270
    BTFSC CV_ANALOG_ENABLE
    DECFSZ AnalogTimeCounter,F   ;   decrement in analog-enable mode. 256 cycles after packet, 32 cycles when "idle"
    GOTO MainLoop
    CALL AnalogOperation
    GOTO MainLoop
Process_Packet:
    BCF PACKET_READY
    BTFSC SRVMODE_FLAG
    GOTO Process_ServiceMode
    GOTO Process_Command
Command_POM:
    BTFSS SRV2X_FLAG
    GOTO Set_POM_Flag
    BCF  SRV2X_FLAG
    MOVF COMMAND3,W      ;   save potential write or compare byte
    MOVWF EEDATA0
    MOVLW 0xEC           ;   0x11101100 (0x111???? = POM, 0x???11 = write byte)
    XORWF COMMAND1,W
    BTFSS STATUS,Z
    RETURN
    ; POM Write CV
    MOVF COMMAND2,W      ;   CV number
    CALL Write_CV_Start
    BTFSC IGNORE_CV_FLAG
    RETURN
    BTFSC RESCV_FLAG
    GOTO Check_ResetCommand
    GOTO Real_CV_Write
Set_POM_Flag:
    BSF SRV2X_FLAG
    RETURN
LADR_0x013C
    MOVF COMMAND1,W
    XORLW 0x3F           ;   b'00111111'  d'063'  "?"
    BTFSS STATUS,Z
    RETURN
    MOVF COMMAND2,W
Sub_Set_SpeedDir:
    BTFSC CV29,0         
    XORLW 0x80           ;   invert direction bit on reverse direction
    MOVWF SPEED
    RETURN
LADR_0x0145
    MOVLW 0x80		 ;   forward speed       
LADR_0x0146
    CALL Sub_Set_SpeedDir
    MOVF COMMAND1,W
    ANDLW 0x1F           
    BTFSS CV_SPEED_28
    ANDLW 0x0F           ;  16 speed steps; bit 4 of speed controls the light.
    IORWF SPEED,F
    BTFSC CV_SPEED_28
    RETURN
Set_FL:
    BCF FN_DIRLIGHT_BIT
    BTFSC COMMAND1,4
    BSF FN_DIRLIGHT_BIT
    RETURN
LADR_0x0152
    MOVLW 0x00           ;   b'00000000'  d'000'
    GOTO LADR_0x0146
AnalogOperation:
    BSF	 AnalogTimeCounter,5      ;   set to 32
    MOVLW 0x82           ;   Going Forward, speed = 2
    BTFSS DCCIN          ;   Detect polarity
    MOVLW 0x02           ;   Going Backward, speed = 2
    CALL Sub_Set_SpeedDir

    MOVLW 0x0C           ;   load CV 13, analog active functions
    CALL EE_Read
    MOVWF TEMP_VAR      
    ANDLW 0x0F           
    MOVWF FSTATE         ;  Bits 0..3 correspond to F1..F4 state
    SWAPF TEMP_VAR,W
    ANDLW 0x0F           ;  Functions F5 - F8
    MOVWF LRAM_0x36
    MOVLW 0x0D           ;  read CV 14
    CALL EE_Read
    MOVWF TEMP_VAR
    BTFSC CURRENT_DIR
    BTFSS TEMP_VAR,0    ;  Going forward
    GOTO Maybe_Set_RevLight_Analog     ;  Going reverse, OR (going forward AND CV14/FL = 0)
    BSF FN_DIRLIGHT_BIT  ;  Going forward AND CV14/FL = 1
Maybe_Set_RevLight_Analog:
    BTFSS CURRENT_DIR
    BTFSS TEMP_VAR,1    ;  Going reverse
    GOTO Finish_Set_Light_Analog     ;  Going forward, OR (going reverse AND CV14/FR = 0)
    BSF FN_DIRLIGHT_BIT  ;  Going reverse AND CV14/FR = 1
Finish_Set_Light_Analog:
    RLF TEMP_VAR,F      
    RLF TEMP_VAR,W      ;  Rotate by 2 bits, F12 is in 7th bit
    ANDLW 0xF0           
    IORWF LRAM_0x36,F    ;  Merge with functions F5-F8
    CLRF LRAM_0x37       ;  No functions F13-F20
    CLRF LRAM_0x38       ;  No functions F21-F28
    BCF DCC_HIGH_FLAG    ;  DCC not detected (neither polarity)
    BCF DCC_LOW_FLAG
    RETURN
Function_Group1:
    BTFSC CV_SPEED_28    ; skip if CV29 = 0, 4th bit of control directs FL
    CALL Set_FL          ; if CV29 = 1, get FL from 4th bit of Function group 1
    MOVLW 0xF0           
    ANDWF FSTATE,F
    MOVF COMMAND1,W
    ANDLW 0x0F           
    IORWF FSTATE,F
    RETURN
Function_Group2:
    BTFSS COMMAND1,4
    GOTO Set_F9_12
    MOVLW 0xF0           ;   b'11110000'  d'240'
    ANDWF LRAM_0x36,F
    MOVF COMMAND1,W
    ANDLW 0x0F           ;   b'00001111'  d'015'
    IORWF LRAM_0x36,F
    RETURN
Set_F9_12:
    MOVLW 0x0F           ;   b'00001111'  d'015'
    ANDWF LRAM_0x36,F
    SWAPF COMMAND1,W
    ANDLW 0xF0           ;   b'11110000'  d'240'
    IORWF LRAM_0x36,F
    RETURN
Function_Extended:
    MOVF COMMAND1,W
    XORLW 0xDE           ;   Extended command 11110
    BTFSS STATUS,Z
    GOTO Function_F21_Check
    MOVF COMMAND2,W
    MOVWF LRAM_0x37      ;   State of F13-20
    RETURN
Function_F21_Check:
    XORLW 0x01           ;   Extended command 11111
    BTFSS STATUS,Z
    RETURN
    MOVF COMMAND2,W
    MOVWF LRAM_0x38      ;   State of F21-28
    RETURN
    
Process_ServiceMode:
    BTFSC FOUR_BYTE	 
    GOTO Service_Direct
    
    ; 3-byte service mode packets
    MOVF COMMAND2,W
    MOVWF EEDATA0        ;   .. command data ??
    MOVF COMMAND1,W      ;   CV number ??
    ; XXX should check decoder CV lock.
    ANDLW 0xF7           ;   ignore "C" (read/write flag)
    XORLW 0x75           ;   0111-x101, undefined ??
    BTFSC STATUS,Z
    GOTO Set_PageRegister
    XORLW 0x01           ;   0111-x100, basic configuration register
    BTFSC STATUS,Z
    GOTO Phys_CV29
    XORLW 0x02           ;   b'0111?110'  --- version number, CV7
    BTFSC STATUS,Z
    GOTO LADR_0x01BF
    XORLW 0x01           ;   b'0111?111'  --- manufacture, CV8
    BTFSC STATUS,Z
    GOTO LADR_0x01C1
    MOVF COMMAND1,W
    ANDLW 0x03           ;   b'00000011'  d'003'
    ADDWF Maybe_PAGEREG,W
    CALL Write_CV_Start
    BTFSC IGNORE_CV_FLAG
    RETURN
Phys_Operation:
    ; !!! must check lock
    BTFSS COMMAND1,3
    GOTO Phys_VerifyCV
    GOTO Real_CV_Write
Phys_CV29:
    MOVLW 0x1C           ;   b'00011100'  d'028'
    GOTO Phys_Operation
Set_PageRegister:
    DECF COMMAND2,F
    RLF COMMAND2,F
    RLF COMMAND2,W
    ANDLW 0xFC           ;   b'11111100'  d'252'
    BTFSS COMMAND1,3
    GOTO Verify_PageReg
    MOVWF Maybe_PAGEREG
    RETURN
Verify_PageReg:
#ifdef ProgrammingLock
    MOVWF   TEMP_VAR		; save CV number
    MOVF    CV15,W		; load CV15, check for zero
    BTFSC   STATUS,Z
    GOTO    Verify_Pagereg_Unlocked	; Ignore lock if CV15 = 0
    XORWF   CV16,W
    BTFSS   STATUS,Z		; skip if CV15 = CV16, otherwise raise ignore flag
    RETURN
Verify_Pagereg_Unlocked:
    MOVF   TEMP_VAR, W
#endif
    XORWF Maybe_PAGEREG,W
    GOTO ACK_If_Zero
LADR_0x01BF
    MOVLW 0x06           ;   b'00000110'  d'006'
    GOTO Phys_Operation
LADR_0x01C1
    MOVF COMMAND2,W
    GOTO Check_ResetTrigger
Phys_VerifyCV:
    CALL EE_Read
    XORWF COMMAND2,W
ACK_If_Zero:
    BTFSS STATUS,Z
    RETURN
Blink_Led:
    MOVLW 0x33           ;   All outputs ON
    MOVWF GPIO           
    MOVLW 0x06           ;   b'00000110'  d'006'
    MOVWF TEMP_VAR
    ; XXX instruction can be saved: Do not initialize to 0, 1st outer cycle will
    ; last 6 inner cycles less. Increase outer cycle by 1
    MOVLW 0x00           
Blink_Led_wait:
    ADDLW 0xFF          
    BTFSS STATUS,Z
    GOTO Blink_Led_wait
    DECFSZ TEMP_VAR,F
    GOTO Blink_Led_wait
    CLRF GPIO            ; turn off LEDs
    RETURN
    
    ; !!!!! Must process
Service_Direct:
    MOVF COMMAND3,W	 ; value of CV to set or verify
    MOVWF EEDATA0
    BTFSS COMMAND1,1	 
    BTFSC COMMAND1,0	 
    RETURN		 ; filter out CVs with address > 0x100
    ;; Direct mode, bits COMMAND1,2-3 has the instruction, COMMAND2 is the CV#
    MOVF COMMAND2,W	 
    CALL Write_CV_Start
    BTFSC IGNORE_CV_FLAG
    RETURN		    ; invalid CV#
    BTFSC COMMAND1,3	    ; skip if NOT 0x08
    GOTO Service_Direct2    ; 11 write byte or 10 bit manipulation
    BTFSS COMMAND1,2	    
    RETURN		    
Service_Direct2:	    
    BTFSS COMMAND1,2	    
    GOTO Service_BitOper    ; 10 - bit manipulation
    BTFSS COMMAND1,3	    
    GOTO Verify_CV_Byte	    
    ; 0x0c..0x0f
Real_CV_Write:
    BTFSC RESCV_FLAG
    GOTO Check_ResetCommand
    CALL EE_Write
    CALL Blink_Led
    CALL LoadAddress
    RETURN
Verify_CV_Byte:
    CALL EE_Read
    XORWF COMMAND3,W
    GOTO ACK_If_Zero
Check_ResetCommand:
    MOVF COMMAND3,W
Check_ResetTrigger:
    XORLW 0x21           ;   b'00100001'  d'033'  "!"
    BTFSS STATUS,Z
    RETURN
    GOTO Reset_CVs
Service_BitOper:
    CALL EE_Read	 ;  read the current CV
    MOVWF EEDATA0
    MOVLW EEDATA0        
    MOVWF FSR		 ;  prepare for bit operation
    MOVLW 0x07           
    ANDWF COMMAND3,W	 ;  bit number in COMMAND3
    CALL Bit_GetValue
    BTFSS COMMAND3,4	 
    GOTO Service_BitVerify
    BTFSC COMMAND3,3	 
    IORWF INDF,F	 ; bit value in W
    XORLW 0xFF           
    BTFSS COMMAND3,3
    ANDWF INDF,F	 ; clear bit
    MOVF COMMAND2,W
    CALL Write_CV_Start
    GOTO Real_CV_Write
Service_BitVerify:
    ANDWF INDF,W
    BTFSC STATUS,Z
    GOTO Service_BitVerify_0
    BTFSC COMMAND3,3
    GOTO Blink_Led
    RETURN
Service_BitVerify_0:
    BTFSS COMMAND3,3
    GOTO Blink_Led
    RETURN
Write_CV_Start:
    BCF IGNORE_CV_FLAG
    BCF RESCV_FLAG
    MOVWF   TEMP_VAR
#ifdef ProgrammingLock
    XORLW   14
    BTFSC   STATUS,Z
    GOTO    Write_CV_Unlocked	; CV15 can be always written to
    MOVF    CV15,W		; load CV15, check for zero
    BTFSC   STATUS,Z
    GOTO    Write_CV_Unlocked	; Ignore lock if CV15 = 0
    XORWF   CV16,W
    BTFSS   STATUS,Z		; skip if CV15 = CV16, otherwise raise ignore flag
    BSF IGNORE_CV_FLAG
Write_CV_Unlocked:
    MOVF    TEMP_VAR,W
#endif
    BTFSS TEMP_VAR,7    
    BTFSC TEMP_VAR,6
    GOTO Check_High_CV_Addr     ; CVs > 0x3F (64 +)

    ; CVs <= 0x3f
    XORLW 0x06           ; check for CV-7
    BTFSC STATUS,Z
    BSF RESCV_FLAG      ; if  TEMP_VAR == 0x06
    XORLW 0x01           ; check for CV-8
    BTFSC STATUS,Z
    BSF RESCV_FLAG      ; if  TEMP_VAR == 0x07
    MOVF TEMP_VAR,W
    RETURN
Check_High_CV_Addr:
    ADDLW 0x89           ;   CV 120 will correspond to 0x00
    BTFSS STATUS,C
    BSF IGNORE_CV_FLAG   ;   Ignore if CV < 120, below func table
    ADDLW 0xC0           ;   If CV >= 120 && CV < 184 => status,c := 0
    BTFSC STATUS,C
    BSF IGNORE_CV_FLAG   ;   Ignore if CV > 184
    ADDLW 0x40           
    ADDLW 0x40           ;   Adjust so that CV120 will become 0x040 = eeprom address
    RETURN
Reset_CVs:
    MOVLW 0x0F           
    MOVWF EEDATA0
    MOVLW EEPROM_CV37    
    MOVWF EEPROM_WRITE_START
    MOVLW 0x04           
    CALL Fill_eeprom_bytes     ;   write 0x0f to EEPROM 0x24-0x27, CV37-40
    MOVLW 0xC0              
    MOVWF EEDATA0
    MOVLW EEPROM_CV17	       ;    CV17 := 0xc0
    CALL EE_Write
    SWAPF EEDATA0,F      ;   prepare to write 0x0C
    MOVLW EEPROM_CV50
    MOVWF EEPROM_WRITE_START
    MOVLW 0x04           
    CALL Fill_eeprom_bytes  ;   write 0x0C to CV50-53
    ; XXX Instrunction can be saved by
    ; BCF EEDATA0,2      ; 0x0C --> 0x08
    ; 
    MOVLW 0x64           
    MOVWF EEDATA0
    MOVLW EEPROM_CV18
    CALL EE_Write

    MOVLW 0x06           
    MOVWF EEDATA0
    MOVLW EEPROM_CV29
    CALL EE_Write

    ; fill CV124-125
    MOVLW 0x08       
    MOVWF EEDATA0
    MOVLW EEPROM_CV124
    MOVWF EEPROM_WRITE_START
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL Fill_eeprom_bytes

    ; Initialize with zeroes
    ; 0x0C, 0x20-23, 0x46-7F
    ; XXXX several instructions can be saved by setting ALL eeprom content to zero
    ; save 6 instructions
    CLRF EEDATA0
    MOVLW EEPROM_CV13
    CALL EE_Write
    
    MOVLW 0x20           
    MOVWF EEPROM_WRITE_START
    MOVLW 0x04           ;   b'00000100'  d'004'
    CALL Fill_eeprom_bytes

    MOVLW 0x46           ;   b'01000110'  d'070'  "F"
    MOVWF EEPROM_WRITE_START
    MOVLW 0x3A           ;   b'00111010'  d'058'  ":"
    CALL Fill_eeprom_bytes
#ifdef ProgrammingLock
    MOVLW EEPROM_CV15
    MOVWF EEPROM_WRITE_START
    MOVLW 2
    CALL Fill_eeprom_bytes  ; erase the decoder lock (both bytes to 0)
#endif
    
    ; Write 1
    INCF EEDATA0,F
    MOVLW 0x40           ;   b'01000000'  d'064'  "@"
    CALL EE_Write

    ; Write 2
    INCF EEDATA0,F
    MOVLW 0x41           ;   b'01000001'  d'065'  "A"
    CALL EE_Write

    ; Write 3
    INCF EEDATA0,F
    MOVLW 0x00           ;   b'00000000'  d'000'
    CALL EE_Write
    MOVLW 0x0D           ;   b'00001101'  d'013'
    CALL EE_Write

    ; Write 4
    INCF EEDATA0,F
    MOVLW 0x42           ;   b'01000010'  d'066'  "B"
    MOVWF EEPROM_WRITE_START
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL Fill_eeprom_bytes
    
    CALL Blink_Led

    ; Fall through
LoadAddress:
    MOVLW EEPROM_CV1
    CALL EE_Read         ; Read EEPROM(0)
    MOVWF CV1
    MOVLW EEPROM_CV17
    CALL EE_Read         ; Read EEPROM(0x10)
    MOVWF CV17
    MOVLW EEPROM_CV18
    CALL EE_Read         ; Read EEPROM(0x11)
    MOVWF CV18
    MOVLW EEPROM_CV29
    CALL EE_Read         ; Read EEPROM(0x1C)
    MOVWF CV29
#ifdef ProgrammingLock
    MOVLW EEPROM_CV15
    CALL EE_Read
    MOVWF CV15
    MOVLW EEPROM_CV16
    CALL EE_Read
    MOVWF CV16
#endif
    RETURN
Fill_eeprom_bytes:
    MOVWF FUNCTION_ID
Fill_eeprom_loop:
    MOVF EEPROM_WRITE_START,W
    CALL EE_Write
    INCF EEPROM_WRITE_START,F
    DECFSZ FUNCTION_ID,F
    GOTO Fill_eeprom_loop
    RETURN

LADR_0x0270
    MOVLW 0x10           ;   b'00010000'  d'016'
    ADDWF INCREMENT,F    ; Series 0x01, 0x11, 0x21, ... 0xf1
    CLRF TEMP_VAR       ; Initially zero
    MOVF INCREMENT,W
    ADDWF BRIGHT_FA,W    ; Add to brightness. Brightness 0x80 will stay on for 0x81..0xf1, off for 0x01..0x71 (8 states from 8).
    BTFSC STATUS,C       ; Brightness 0x00 nevers turn on. Brightness 0xff will stay on even for 0x01.
    BSF TEMP_VAR,0
    MOVF INCREMENT,W
    ADDWF BRIGHT_FB,W
    BTFSC STATUS,C
    BSF TEMP_VAR,1
    MOVF INCREMENT,W
    ADDWF BRIGHT_FC,W
    BTFSC STATUS,C
    BSF TEMP_VAR,4
    MOVF INCREMENT,W
    ADDWF BRIGHT_FD,W
    BTFSC STATUS,C
    BSF TEMP_VAR,5
    MOVF TEMP_VAR,W
    MOVWF GPIO           ; !!Bank!! GPIO - TRISIO
    CALL LADR_0x006A
    INCF LRAM_0x40,F
    RETURN

Sub_Function_Bits:
    CLRF FUNCTION_ID        ; F0
    CLRF LRAM_0x39          ; Set enabled outputs to NONE initially

    BTFSC FSTATE,4       ; F0
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F
    BTFSC FSTATE,0       ; F1
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F
    BTFSC FSTATE,1       ; F2
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F
    BTFSC FSTATE,2       ; F3
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F
    BTFSC FSTATE,3       ; F4
    CALL Load_Function_Cfg

    ; OPTIMIZATION: save instrunctions, by using
    ; FSR + INDF (0x036 - 0x038). Increment at the start of the loop. End when
    ; FSR bit #3 is set = 0x038
    ; 29 instrs. vs. 72 instrs.
#ifdef OptimizationFix1
    MOVLW   0x35
    MOVWF   FSR
Next_Function_Group:
    INCF    FSR,F
    INCF FUNCTION_ID,F   ; F5
    BTFSC INDF,0
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F6
    BTFSC INDF,1
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F7
    BTFSC INDF,2
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F8
    BTFSC INDF,3
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F9
    BTFSC INDF,4
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F10
    BTFSC INDF,5
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F11
    BTFSC INDF,6
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F12
    BTFSC INDF,7
    CALL Load_Function_Cfg
    BTFSS  FSR,3
    GOTO   Next_Function_Group
#else
    INCF FUNCTION_ID,F   ; F5
    BTFSC LRAM_0x36,0
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F6
    BTFSC LRAM_0x36,1
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F7
    BTFSC LRAM_0x36,2
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F8
    BTFSC LRAM_0x36,3
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F9
    BTFSC LRAM_0x36,4
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F10
    BTFSC LRAM_0x36,5
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F11
    BTFSC LRAM_0x36,6
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F12
    BTFSC LRAM_0x36,7
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F13
    BTFSC LRAM_0x37,0
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F14
    BTFSC LRAM_0x37,1
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F15
    BTFSC LRAM_0x37,2
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F16
    BTFSC LRAM_0x37,3
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F17
    BTFSC LRAM_0x37,4
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F18
    BTFSC LRAM_0x37,5
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F19
    BTFSC LRAM_0x37,6
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F20
    BTFSC LRAM_0x37,7
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F21
    BTFSC LRAM_0x38,0
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F22
    BTFSC LRAM_0x38,1
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F23
    BTFSC LRAM_0x38,2
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F24
    BTFSC LRAM_0x38,3
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F25
    BTFSC LRAM_0x38,4
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F26
    BTFSC LRAM_0x38,5
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F27
    BTFSC LRAM_0x38,6
    CALL Load_Function_Cfg
    INCF FUNCTION_ID,F   ; F28
    BTFSC LRAM_0x38,7
    CALL Load_Function_Cfg
#endif

    INCF FUNCTION_ID,F   ; F29 :)) == Stop FWD/BWD
    MOVF SPEED,W
    ANDLW 0x7E           ;   b'01111110'  d'126'  "~"
    BTFSS STATUS,Z
    INCF FUNCTION_ID,F   ; F29 :)) == Moving FWD/BWD
    CALL Load_Function_Cfg

    BTFSC DCC_HIGH_FLAG
    MOVLW 0x7E           ; DCC HIGH on = 0x7E
    BTFSC DCC_LOW_FLAG
    MOVLW 0x7F           ; DCC LOW on = 0x7F
    BTFSS DCC_HIGH_FLAG
    BTFSC DCC_LOW_FLAG
    CALL Load_Eeprom_Bits ; read CV182 when DCC HIGH, CV 183 when DCC LOW

    MOVF LRAM_0x39,W
    XORWF LRAM_0x3A,F     ; 3A = changes, W = enabled bits

    BTFSS LRAM_0x3A,0     ; change on FA
    GOTO Proc_Change_FB
    BTFSS LRAM_0x39,0     ; new state = 1 -> jump
    BSF TODO_FA,6       ; new FA = OFF
    BTFSC LRAM_0x39,0
    BSF TODO_FA,7       ; new FA = ON

Proc_Change_FB:
    BTFSS LRAM_0x3A,1
    GOTO Proc_Change_FC
    BTFSS LRAM_0x39,1
    BSF TODO_FB,6
    BTFSC LRAM_0x39,1
    BSF TODO_FB,7

Proc_Change_FC:
    BTFSS LRAM_0x3A,2
    GOTO Proc_Change_FD
    BTFSS LRAM_0x39,2
    BSF TODO_FC,6
    BTFSC LRAM_0x39,2
    BSF TODO_FC,7

Proc_Change_FD:
    BTFSS LRAM_0x3A,3
    GOTO Proc_Change_End
    BTFSS LRAM_0x39,3
    BSF TODO_FD,6
    BTFSC LRAM_0x39,3
    BSF TODO_FD,7

Proc_Change_End:
    MOVWF LRAM_0x3A        ; 0x3A := enabled bits
    RETURN
;---------------------------------------------------------------------

    ; turn ON outputs according to configuration in EEPROM. Desired Function ID in FUNCTION_ID
Load_Function_Cfg:
    BCF STATUS,C
    BTFSS CURRENT_DIR
    BSF STATUS,C
    RLF FUNCTION_ID,W
    ADDLW EEPROM_OUTCONF_BASE
Load_Eeprom_Bits:
    CALL EE_Read
    IORWF LRAM_0x39,F
    RETURN
Final_State_FSR:
    CALL Fetch_TODO_FSR
    BTFSC INDF,7
    MOVLW 0x10           ;   Desired state = ON, set 4th bit, clear others.
    BTFSC INDF,6
    MOVLW 0x00           ;   Desired state = OFF, clear 4th bit (and others)
    MOVWF INDF
    RETURN
Fetch_State_Or_Off:
    CALL Fetch_TODO_FSR
    BTFSC INDF,6
    MOVLW 0x00           ;   Desired state = OFF, clear. Otherwise KEEP existing state
    MOVWF INDF
    ANDLW 0x0F           ;   Return current state/phase ??
    RETURN
Effect_Incandesc:
    CALL Final_State_FSR
    MOVLW 0x00           ;   set brightness to 0, if bit 4 = 0
    BTFSC INDF,4
    CALL Load_Brightness
Set_Cur_Brightness:
    MOVWF TEMP_VAR
    MOVF LRAM_0x40,W
    ADDLW BRIGHT_BASE
    MOVWF FSR
    MOVF TEMP_VAR,W
    MOVWF INDF
    RETURN
Effect_Fluor_EOL:
    CALL Fetch_State_Or_Off
    BTFSS STATUS,Z       ; Zero if turned OFF, or when state == 0
    GOTO LADR_0x033C
    BSF INDF,4           ; Set 4th bit, indicator "ON"
    BSF INDF,5           ; ???
    GOTO LADR_0x0337
Effect_Fluor_Broken:
    CALL Fetch_State_Or_Off
    BTFSS STATUS,Z
    GOTO LADR_0x033C     ; goto if not off
    BSF INDF,4           ; set "lit" flag on ?
    GOTO LADR_0x0337
Effect_Fluor:
    CALL Fetch_State_Or_Off
    BTFSS STATUS,Z
    GOTO LADR_0x033C
LADR_0x0337:
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSS INDF,7         ;   bit 7 means turn ON
    GOTO Set_Cur_Brightness     ;   erase (0x00) to reg (0x3c + [0x40]), unless 7 bit.
    MOVLW 0x01           ;   Initial flip will make the value in W 0; then Func State will increase to 2.
    CALL Set_Func_State
LADR_0x033C:
    XORLW 0x01           ;   flip '1' in state
    BTFSS STATUS,Z
    GOTO LADR_0x0348
    CALL Inc_Func_State  ;   every other state change, will increment.
    CALL Random
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x03           ;   b'00000011'  d'003'
    CALL Set_Func_0x48
    MOVLW 0x0A           ;   b'00001010'  d'010'
    CALL Set_Func_0x4c
LADR_0x0346:
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
LADR_0x0348:
    XORLW 0x03           ;   b'00000011'  d'003'
    BTFSS STATUS,Z
    GOTO LADR_0x0363
    CALL Load_Func_0x4c
    DECFSZ INDF,F
    RETURN
    CALL Load_Func_0x48
    DECFSZ INDF,F
    GOTO LADR_0x0354
    CALL Inc_Func_State
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
LADR_0x0354
    MOVF LRAM_0x40,W
    ADDLW 0x3C           ;   b'00111100'  d'060'  "<"
    MOVWF FSR
    CALL Load_Brightness
    XORWF INDF,F
    BTFSS STATUS,Z
    GOTO LADR_0x035F
    CALL Random
    ANDLW 0x1F           ;   b'00011111'  d'031'
    ADDLW 0x14           ;   b'00010100'  d'020'
    GOTO Set_Func_0x4c
LADR_0x035F
    CALL Random
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x06           ;   b'00000110'  d'006'
    GOTO Set_Func_0x4c
LADR_0x0363
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSS STATUS,Z
    GOTO LADR_0x0373
    BTFSS INDF,4
    GOTO LADR_0x0346
    CALL Random
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x03           ;   b'00000011'  d'003'
    CALL Set_Func_0x4c
    MOVLW 0x30           ;   b'00110000'  d'048'  "0"
    CALL Set_Func_0x48
    CALL Inc_Func_State
    BTFSS INDF,5
    GOTO LADR_0x0346
    MOVLW 0x10           ;   b'00010000'  d'016'
    GOTO Set_Cur_Brightness
LADR_0x0373
    CALL Load_Func_0x48
    DECFSZ INDF,F
    RETURN
    MOVLW 0x30           ;   b'00110000'  d'048'  "0"
    CALL Set_Func_0x48
    CALL Load_Func_0x4c
    DECFSZ INDF,F
    RETURN
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL Set_Func_State
    CALL Random
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x05           ;   b'00000101'  d'005'
    CALL Set_Func_0x48
    MOVLW 0x0A           ;   b'00001010'  d'010'
    CALL Set_Func_0x4c
    MOVLW 0x00           ;   b'00000000'  d'000'
    GOTO Set_Cur_Brightness
Effect_FlashA
    CALL Final_State_FSR
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSS FLASH_A_STATE
    GOTO Set_Cur_Brightness
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
Effect_FlashA_Neg
    CALL Final_State_FSR
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSC FLASH_A_STATE
    GOTO Set_Cur_Brightness
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
Effect_FlashB
    CALL Final_State_FSR
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSS FLASH_B_STATE
    GOTO Set_Cur_Brightness
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
Effect_FlashB_Neg
    CALL Final_State_FSR
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSC FLASH_B_STATE
    GOTO Set_Cur_Brightness
    CALL Load_Brightness
    GOTO Set_Cur_Brightness
Load_Brightness:
    MOVF LRAM_0x40,W
    ADDLW 0x24           ;   CV 37-40, max brightness
    CALL EE_Read
    MOVWF EEDATA0
    SWAPF EEDATA0,W
    IORLW 0x0F           ;   brightness * 16 + 0x0f
    RETURN
Random:
    MOVF LRAM_0x3B,W
    BTFSC STATUS,Z       ; neni 0 - preskocit
    MOVF TMR0,W          ; Read TMR0 -- random generator ??
    BTFSC STATUS,Z       ; nebyla 0 v 0x3b, neni ani ted
    INCF LRAM_0x3B,W
    MOVWF TEMP_VAR
    RRF TEMP_VAR,F
    SWAPF TEMP_VAR,F
    XORWF TEMP_VAR,F
    RRF TEMP_VAR,F
    RRF LRAM_0x3B,W
    MOVWF LRAM_0x3B
    RETURN
Fetch_TODO_FSR:
    MOVF LRAM_0x40,W
    ADDLW TODO_BASE
    MOVWF FSR
    MOVF INDF,W
    RETURN
Inc_Func_State:
    MOVF LRAM_0x40,W
    ADDLW TODO_BASE
    MOVWF FSR
    INCF INDF,W
Set_Func_State:
    ANDLW 0x0F           ;   State counter (?) goes to lower 4 bits
    MOVWF TEMP_VAR
    MOVF LRAM_0x40,W
    ADDLW TODO_BASE
    MOVWF FSR
    MOVLW 0xF0           ;   Keep existing flags
    ANDWF INDF,F
    MOVF TEMP_VAR,W
    IORWF INDF,F
    RETURN
Set_Func_0x48:
    MOVWF TEMP_VAR
    MOVF LRAM_0x40,W
    ADDLW 0x48           ;   b'01001000'  d'072'  "H"
    MOVWF FSR
    MOVF TEMP_VAR,W
    MOVWF INDF
    RETURN
Load_Func_0x48:
    MOVF LRAM_0x40,W
    ADDLW 0x48           ;   b'01001000'  d'072'  "H"
    MOVWF FSR
    MOVF INDF,W
    RETURN
Set_Func_0x4c:
    MOVWF TEMP_VAR
    MOVF LRAM_0x40,W
    ADDLW 0x4C           ;   b'01001100'  d'076'  "L"
    MOVWF FSR
    MOVF TEMP_VAR,W
    MOVWF INDF
    RETURN
Load_Func_0x4c:
    MOVF LRAM_0x40,W
    ADDLW 0x4C           ;   b'01001100'  d'076'  "L"
    MOVWF FSR
    MOVF INDF,W
    RETURN
EE_Read:
    BSF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    MOVWF EEADR          ; !!Bank!! Unimplemented - EEADR
    BSF EECON1,RD        ; !!Bank!! Unimplemented - EECON1
    MOVF EEDATA,W        ; !!Bank!! Unimplemented - EEDATA
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    RETURN
EE_Write:
    CALL EE_Read
    XORWF EEDATA0,W
    BTFSC STATUS,Z
    RETURN
    MOVF EEDATA0,W
    BSF STATUS,RP0       ; 
    MOVWF EEDATA         ; !!Bank!! Unimplemented - EEDATA
    BSF EECON1,WREN      ; !!Bank!! Unimplemented - EECON1
    BCF INTCON,GIE
    MOVLW 0x55           ;   b'01010101'  d'085'  "U"
    MOVWF EECON2         ; !!Bank!! Unimplemented - EECON2
    MOVLW 0xAA           ;   b'10101010'  d'170'
    MOVWF EECON2         ; !!Bank!! Unimplemented - EECON2
    BSF EECON1,WR        ; !!Bank!! Unimplemented - EECON1
    BSF INTCON,GIE
    BCF EECON1,WREN      ; !!Bank!! Unimplemented - EECON1
EEWrite0:
    BTFSC EECON1,1       ; !!Bank!! Unimplemented - EECON1
    GOTO EEWrite0
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    RETURN

    End
