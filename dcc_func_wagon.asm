; original File = Z:\home\sdedic\old-home\sdedic\vlacky\FuncDec.X\dcc_func_wagon.HEX

    processor 12F629
    #include <P12F629.INC>
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

; RAM-Variable
LRAM_0x20 equ 0x20
LRAM_0x21 equ 0x21
LRAM_0x22 equ 0x22
LRAM_0x23 equ 0x23
LRAM_0x24 equ 0x24
LRAM_0x25 equ 0x25
LRAM_0x26 equ 0x26
LRAM_0x27 equ 0x27
LRAM_0x28 equ 0x28
LRAM_0x2A equ 0x2A
LRAM_0x2B equ 0x2B
LRAM_0x2C equ 0x2C
LRAM_0x2D equ 0x2D
LRAM_0x2E equ 0x2E
LRAM_0x2F equ 0x2F
LRAM_0x31 equ 0x31
LRAM_0x32 equ 0x32
LRAM_0x33 equ 0x33
LRAM_0x34 equ 0x34
LRAM_0x35 equ 0x35
LRAM_0x36 equ 0x36
LRAM_0x37 equ 0x37
LRAM_0x38 equ 0x38
LRAM_0x39 equ 0x39
LRAM_0x3A equ 0x3A
LRAM_0x3B equ 0x3B
LRAM_0x3C equ 0x3C
LRAM_0x3D equ 0x3D
LRAM_0x3E equ 0x3E
LRAM_0x3F equ 0x3F
LRAM_0x40 equ 0x40
LRAM_0x41 equ 0x41
LRAM_0x42 equ 0x42
LRAM_0x43 equ 0x43
LRAM_0x44 equ 0x44
LRAM_0x45 equ 0x45
LRAM_0x46 equ 0x46
LRAM_0x47 equ 0x47
LRAM_0x50 equ 0x50
LRAM_0x51 equ 0x51
LRAM_0x52 equ 0x52
LRAM_0x53 equ 0x53
LRAM_0x59 equ 0x59
LRAM_0x5A equ 0x5A
LRAM_0x5B equ 0x5B
LRAM_0x5C equ 0x5C
LRAM_0x5E equ 0x5E
LRAM_0x5F equ 0x5F

; Program

    Org 0x0000

;   Reset-Vector
    CLRF STATUS
    CLRF INTCON
    CLRF PCLATH          ; !!Bank Program-Page-Select
    GOTO LADR_0x00EE
;   Interrupt-Vector
    MOVWF LRAM_0x20
    SWAPF STATUS,W
    CLRF STATUS
    MOVWF LRAM_0x21
    BTFSC INTCON,T0IF
    GOTO LADR_0x0016
    MOVLW 0xB3           ;   b'10110011'  d'179'
    MOVWF TMR0           ; !!Bank!! TMR0 - OPTION_REG
    MOVF LRAM_0x25,W
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO LADR_0x002B
    GOTO LADR_0x0030
    GOTO LADR_0x0023
    GOTO LADR_0x0022
    GOTO LADR_0x002F
    GOTO LADR_0x0022
    GOTO LADR_0x0050
    GOTO LADR_0x0099
LADR_0x0016
    MOVLW 0xB3           ;   b'10110011'  d'179'
    MOVWF TMR0           ; !!Bank!! TMR0 - OPTION_REG
    MOVF LRAM_0x25,W
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO LADR_0x0023
    GOTO LADR_0x0022
    GOTO LADR_0x0034
    GOTO LADR_0x0032
    GOTO LADR_0x0038
    GOTO LADR_0x004C
    GOTO LADR_0x0022
    GOTO LADR_0x0022
LADR_0x0022
    CLRF LRAM_0x25
LADR_0x0023
    MOVF GPIO,W          ; !!Bank!! GPIO - TRISIO
    BCF INTCON,GPIF
    BCF INTCON,T0IF
    SWAPF LRAM_0x21,W
    MOVWF STATUS
    SWAPF LRAM_0x20,F
    SWAPF LRAM_0x20,W
    RETFIE
LADR_0x002B
    MOVLW 0x16           ;   b'00010110'  d'022'
    MOVWF LRAM_0x27
    INCF LRAM_0x25,F
    GOTO LADR_0x0023
LADR_0x002F
    BSF LRAM_0x25,1
LADR_0x0030
    DECFSZ LRAM_0x27,F
    GOTO LADR_0x0023
LADR_0x0032
    INCF LRAM_0x25,F
    GOTO LADR_0x0023
LADR_0x0034
    MOVLW 0x2B           ;   b'00101011'  d'043'  "+"
    MOVWF LRAM_0x26
    CLRF LRAM_0x24
    GOTO LADR_0x0047
LADR_0x0038
    INCF LRAM_0x25,F
    DECFSZ LRAM_0x27,F
    GOTO LADR_0x0023
    BTFSS LRAM_0x26,3
    GOTO LADR_0x0022
    MOVF FSR,W
    MOVWF LRAM_0x22
    MOVF LRAM_0x26,W
    MOVWF FSR
    MOVF LRAM_0x23,W
    MOVWF INDF
    XORWF LRAM_0x24,F
    INCF LRAM_0x26,F
    MOVF LRAM_0x22,W
    MOVWF FSR
LADR_0x0047
    MOVLW 0x09           ;   b'00001001'  d'009'
    MOVWF LRAM_0x27
    MOVLW 0x03           ;   b'00000011'  d'003'
    MOVWF LRAM_0x25
    GOTO LADR_0x0023
LADR_0x004C
    BCF STATUS,C
    RLF LRAM_0x23,F
    DECF LRAM_0x25,F
    GOTO LADR_0x0023
LADR_0x0050
    BSF STATUS,C
    RLF LRAM_0x23,F
    BCF LRAM_0x25,1
    GOTO LADR_0x0023
LADR_0x0054
    ADDWF PCL,F          ; !!Program-Counter-Modification
    RETLW 0x01           ;   b'00000001'  d'001'
    RETLW 0x02           ;   b'00000010'  d'002'
    RETLW 0x04           ;   b'00000100'  d'004'
    RETLW 0x08           ;   b'00001000'  d'008'
    RETLW 0x10           ;   b'00010000'  d'016'
    RETLW 0x20           ;   b'00100000'  d'032'  " "
    RETLW 0x40           ;   b'01000000'  d'064'  "@"
    RETLW 0x80           ;   b'10000000'  d'128'
LADR_0x005D
    SWAPF LRAM_0x31,W
    MOVWF LRAM_0x59
    RRF LRAM_0x59,W
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDWF PCL,F          ; !!Program-Counter-Modification
    RETURN
    GOTO LADR_0x013C
    GOTO LADR_0x0152
    GOTO LADR_0x0145
    GOTO LADR_0x0175
    GOTO LADR_0x017D
    GOTO LADR_0x018B
    GOTO LADR_0x012A
LADR_0x006A
    MOVF LRAM_0x40,W
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO LADR_0x008D
    GOTO LADR_0x008D
    GOTO LADR_0x008D
    GOTO LADR_0x008D
    RETURN
    RETURN
    RETURN
    RETURN
    RETURN
    RETURN
    RETURN
    RETURN
    MOVLW 0xFF           ;   b'11111111'  d'255'
    MOVWF LRAM_0x40
    DECFSZ LRAM_0x42,F
    GOTO LADR_0x0083
    MOVLW 0x31           ;   b'00110001'  d'049'  "1"
    BTFSC LRAM_0x5C,3
    MOVLW 0x32           ;   b'00110010'  d'050'  "2"
    CALL LADR_0x03E0
    MOVWF LRAM_0x42
    MOVLW 0x08           ;   b'00001000'  d'008'
    XORWF LRAM_0x5C,F
LADR_0x0083
    DECFSZ LRAM_0x43,F
    RETURN
    MOVLW 0x33           ;   b'00110011'  d'051'  "3"
    BTFSC LRAM_0x5C,4
    MOVLW 0x34           ;   b'00110100'  d'052'  "4"
    CALL LADR_0x03E0
    MOVWF LRAM_0x43
    MOVLW 0x10           ;   b'00010000'  d'016'
    XORWF LRAM_0x5C,F
    RETURN
LADR_0x008D
    ADDLW 0x20           ;   b'00100000'  d'032'  " "
    CALL LADR_0x03E0
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDWF PCL,F          ; !!Program-Counter-Modification
    GOTO LADR_0x031E
    GOTO LADR_0x0334
    GOTO LADR_0x032F
    GOTO LADR_0x0329
    GOTO LADR_0x0385
    GOTO LADR_0x038C
    GOTO LADR_0x0393
    GOTO LADR_0x039A
LADR_0x0099
    MOVF LRAM_0x23,W
    XORWF LRAM_0x24,F
    BTFSS STATUS,Z
    GOTO LADR_0x0022
    BTFSS GPIO,2         ; !!Bank!! GPIO - TRISIO
    GOTO LADR_0x00A2
    BSF LRAM_0x5C,0
    BCF LRAM_0x5C,1
    GOTO LADR_0x00A4
LADR_0x00A2
    BCF LRAM_0x5C,0
    BSF LRAM_0x5C,1
LADR_0x00A4
    CLRF LRAM_0x28
    MOVF LRAM_0x2B,W
    BTFSC STATUS,Z
    GOTO LADR_0x00E6
    ANDLW 0xF0           ;   b'11110000'  d'240'
    XORLW 0x70           ;   b'01110000'  d'112'  "p"
    BTFSC STATUS,Z
    GOTO LADR_0x00CE
    BCF LRAM_0x5B,7
    BCF LRAM_0x5B,4
    MOVF LRAM_0x2B,W
    BTFSS LRAM_0x2B,7
    BTFSC LRAM_0x53,5
    GOTO LADR_0x00BD
    XORWF LRAM_0x50,W
    BTFSS STATUS,Z
    GOTO LADR_0x0022
LADR_0x00B5
    MOVF LRAM_0x2C,W
    MOVWF LRAM_0x31
    MOVF LRAM_0x2D,W
    MOVWF LRAM_0x32
    MOVF LRAM_0x2E,W
    MOVWF LRAM_0x33
    BSF LRAM_0x5B,0
    GOTO LADR_0x0022
LADR_0x00BD
    XORWF LRAM_0x51,W
    BTFSC LRAM_0x2B,7
    BTFSS STATUS,Z
    GOTO LADR_0x0022
    MOVF LRAM_0x2C,W
    XORWF LRAM_0x52,W
    BTFSC LRAM_0x53,5
    BTFSS STATUS,Z
    GOTO LADR_0x0022
    MOVF LRAM_0x2D,W
    MOVWF LRAM_0x31
    MOVF LRAM_0x2E,W
    MOVWF LRAM_0x32
    MOVF LRAM_0x2F,W
    MOVWF LRAM_0x33
    BSF LRAM_0x5B,0
    GOTO LADR_0x0022
LADR_0x00CE
    BTFSS LRAM_0x5B,7
    GOTO LADR_0x00E3
    BTFSS LRAM_0x5B,6
    GOTO LADR_0x00E1
    BCF LRAM_0x5B,6
    BSF LRAM_0x5B,4
    BCF LRAM_0x5B,3
    MOVF LRAM_0x26,W
    XORLW 0x2D           ;   b'00101101'  d'045'  "-"
    BTFSS STATUS,Z
    BSF LRAM_0x5B,3
    MOVF LRAM_0x2B,W
    MOVWF LRAM_0x31
    MOVF LRAM_0x2C,W
    MOVWF LRAM_0x32
    MOVF LRAM_0x2D,W
    MOVWF LRAM_0x33
    BSF LRAM_0x5B,0
    GOTO LADR_0x0022
LADR_0x00E1
    BSF LRAM_0x5B,6
    GOTO LADR_0x0022
LADR_0x00E3
    BCF LRAM_0x5B,7
    BCF LRAM_0x5B,6
    GOTO LADR_0x0022
LADR_0x00E6
    BCF LRAM_0x5B,7
    BCF LRAM_0x5B,4
    MOVF LRAM_0x2C,W
    BTFSS STATUS,Z
    GOTO LADR_0x00B5
    BSF LRAM_0x5B,7
    BCF LRAM_0x5B,6
    GOTO LADR_0x0022
LADR_0x00EE
    CLRF GPIO            ; !!Bank!! GPIO - TRISIO
    MOVLW 0x07           ;   b'00000111'  d'007'
    MOVWF CMCON          ; !!Bank!! CMCON - VRCON
    BSF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    MOVLW 0x04           ;   b'00000100'  d'004'
    MOVWF GPIO           ; !!Bank!! GPIO - TRISIO
    CALL LADR_0x03FF
    MOVWF T1CON          ; !!Bank!! T1CON - OSCCAL
    MOVLW 0x33           ;   b'00110011'  d'051'  "3"
    MOVWF WPU            ; !!Bank!! Unimplemented - WPU
    MOVLW 0x04           ;   b'00000100'  d'004'
    MOVWF IOCB           ; !!Bank!! Unimplemented - IOCB
    CLRF CMCON           ; !!Bank!! CMCON - VRCON
    MOVLW 0x88           ;   b'10001000'  d'136'
    MOVWF TMR0           ; !!Bank!! TMR0 - OPTION_REG
    CLRF PIR1            ; !!Bank!! PIR1 - PIE1
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    CLRF PIR1            ; !!Bank!! PIR1 - PIE1
    MOVLW 0x01           ;   b'00000001'  d'001'
    MOVWF T1CON          ; !!Bank!! T1CON - OSCCAL
    MOVLW 0x20           ;   b'00100000'  d'032'  " "
    MOVWF FSR
LADR_0x0104
    CLRF INDF
    INCF FSR,F
    MOVLW 0x60           ;   b'01100000'  d'096'  "`"
    XORWF FSR,W
    BTFSS STATUS,Z
    GOTO LADR_0x0104
    MOVLW 0x88           ;   b'10001000'  d'136'
    MOVWF INTCON
    ADDLW 0xC2           ;   b'11000010'  d'194'
    MOVWF LRAM_0x5E
    CLRF LRAM_0x2A
    ADDLW 0xBD           ;   b'10111101'  d'189'
    MOVWF LRAM_0x42
    MOVWF LRAM_0x43
    INCF LRAM_0x41,F
    CALL LADR_0x03E6
    CALL LADR_0x025C
LADR_0x0115
    BTFSC LRAM_0x5B,0
    CALL LADR_0x0126
    CALL LADR_0x0288
    BTFSS LRAM_0x5B,7
    BTFSS PIR1,0         ; !!Bank!! PIR1 - PIE1
    GOTO LADR_0x0115
    MOVLW 0x8F           ;   b'10001111'  d'143'
    MOVLW 0x0E           ;   b'00001110'  d'014'
    MOVLW 0xFD           ;   b'11111101'  d'253'
    MOVWF TMR1H          ; !!Bank!! TMR1H - Unimplemented
    BCF PIR1,0           ; !!Bank!! PIR1 - PIE1
    CALL LADR_0x0270
    BTFSC LRAM_0x53,2
    DECFSZ LRAM_0x28,F
    GOTO LADR_0x0115
    CALL LADR_0x0154
    GOTO LADR_0x0115
LADR_0x0126
    BCF LRAM_0x5B,0
    BTFSC LRAM_0x5B,4
    GOTO LADR_0x0198
    GOTO LADR_0x005D
LADR_0x012A
    BTFSS LRAM_0x5B,6
    GOTO LADR_0x013A
    BCF LRAM_0x5B,6
    MOVF LRAM_0x33,W
    MOVWF LRAM_0x5E
    MOVLW 0xEC           ;   b'11101100'  d'236'
    XORWF LRAM_0x31,W
    BTFSS STATUS,Z
    RETURN
    MOVF LRAM_0x32,W
    CALL LADR_0x020C
    BTFSC LRAM_0x5B,1
    RETURN
    BTFSC LRAM_0x5B,2
    GOTO LADR_0x01ED
    GOTO LADR_0x01E4
LADR_0x013A
    BSF LRAM_0x5B,6
    RETURN
LADR_0x013C
    MOVF LRAM_0x31,W
    XORLW 0x3F           ;   b'00111111'  d'063'  "?"
    BTFSS STATUS,Z
    RETURN
    MOVF LRAM_0x32,W
LADR_0x0141
    BTFSC LRAM_0x53,0
    XORLW 0x80           ;   b'10000000'  d'128'
    MOVWF LRAM_0x34
    RETURN
LADR_0x0145
    MOVLW 0x80           ;   b'10000000'  d'128'
LADR_0x0146
    CALL LADR_0x0141
    MOVF LRAM_0x31,W
    ANDLW 0x1F           ;   b'00011111'  d'031'
    BTFSS LRAM_0x53,1
    ANDLW 0x0F           ;   b'00001111'  d'015'
    IORWF LRAM_0x34,F
    BTFSC LRAM_0x53,1
    RETURN
LADR_0x014E
    BCF LRAM_0x35,4
    BTFSC LRAM_0x31,4
    BSF LRAM_0x35,4
    RETURN
LADR_0x0152
    MOVLW 0x00           ;   b'00000000'  d'000'
    GOTO LADR_0x0146
LADR_0x0154
    BSF LRAM_0x28,5
    MOVLW 0x82           ;   b'10000010'  d'130'
    BTFSS GPIO,2         ; !!Bank!! GPIO - TRISIO
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL LADR_0x0141
    MOVLW 0x0C           ;   b'00001100'  d'012'
    CALL LADR_0x03E0
    MOVWF LRAM_0x59
    ANDLW 0x0F           ;   b'00001111'  d'015'
    MOVWF LRAM_0x35
    SWAPF LRAM_0x59,W
    ANDLW 0x0F           ;   b'00001111'  d'015'
    MOVWF LRAM_0x36
    MOVLW 0x0D           ;   b'00001101'  d'013'
    CALL LADR_0x03E0
    MOVWF LRAM_0x59
    BTFSC LRAM_0x34,7
    BTFSS LRAM_0x59,0
    GOTO LADR_0x0168
    BSF LRAM_0x35,4
LADR_0x0168
    BTFSS LRAM_0x34,7
    BTFSS LRAM_0x59,1
    GOTO LADR_0x016C
    BSF LRAM_0x35,4
LADR_0x016C
    RLF LRAM_0x59,F
    RLF LRAM_0x59,W
    ANDLW 0xF0           ;   b'11110000'  d'240'
    IORWF LRAM_0x36,F
    CLRF LRAM_0x37
    CLRF LRAM_0x38
    BCF LRAM_0x5C,0
    BCF LRAM_0x5C,1
    RETURN
LADR_0x0175
    BTFSC LRAM_0x53,1
    CALL LADR_0x014E
    MOVLW 0xF0           ;   b'11110000'  d'240'
    ANDWF LRAM_0x35,F
    MOVF LRAM_0x31,W
    ANDLW 0x0F           ;   b'00001111'  d'015'
    IORWF LRAM_0x35,F
    RETURN
LADR_0x017D
    BTFSS LRAM_0x31,4
    GOTO LADR_0x0185
    MOVLW 0xF0           ;   b'11110000'  d'240'
    ANDWF LRAM_0x36,F
    MOVF LRAM_0x31,W
    ANDLW 0x0F           ;   b'00001111'  d'015'
    IORWF LRAM_0x36,F
    RETURN
LADR_0x0185
    MOVLW 0x0F           ;   b'00001111'  d'015'
    ANDWF LRAM_0x36,F
    SWAPF LRAM_0x31,W
    ANDLW 0xF0           ;   b'11110000'  d'240'
    IORWF LRAM_0x36,F
    RETURN
LADR_0x018B
    MOVF LRAM_0x31,W
    XORLW 0xDE           ;   b'11011110'  d'222'
    BTFSS STATUS,Z
    GOTO LADR_0x0192
    MOVF LRAM_0x32,W
    MOVWF LRAM_0x37
    RETURN
LADR_0x0192
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSS STATUS,Z
    RETURN
    MOVF LRAM_0x32,W
    MOVWF LRAM_0x38
    RETURN
LADR_0x0198
    BTFSC LRAM_0x5B,3
    GOTO LADR_0x01D3
    MOVF LRAM_0x32,W
    MOVWF LRAM_0x5E
    MOVF LRAM_0x31,W
    ANDLW 0xF7           ;   b'11110111'  d'247'
    XORLW 0x75           ;   b'01110101'  d'117'  "u"
    BTFSC STATUS,Z
    GOTO LADR_0x01B5
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSC STATUS,Z
    GOTO LADR_0x01B3
    XORLW 0x02           ;   b'00000010'  d'002'
    BTFSC STATUS,Z
    GOTO LADR_0x01BF
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSC STATUS,Z
    GOTO LADR_0x01C1
    MOVF LRAM_0x31,W
    ANDLW 0x03           ;   b'00000011'  d'003'
    ADDWF LRAM_0x2A,W
    CALL LADR_0x020C
    BTFSC LRAM_0x5B,1
    RETURN
LADR_0x01B0
    BTFSS LRAM_0x31,3
    GOTO LADR_0x01C3
    GOTO LADR_0x01E4
LADR_0x01B3
    MOVLW 0x1C           ;   b'00011100'  d'028'
    GOTO LADR_0x01B0
LADR_0x01B5
    DECF LRAM_0x32,F
    RLF LRAM_0x32,F
    RLF LRAM_0x32,W
    ANDLW 0xFC           ;   b'11111100'  d'252'
    BTFSS LRAM_0x31,3
    GOTO LADR_0x01BD
    MOVWF LRAM_0x2A
    RETURN
LADR_0x01BD
    XORWF LRAM_0x2A,W
    GOTO LADR_0x01C5
LADR_0x01BF
    MOVLW 0x06           ;   b'00000110'  d'006'
    GOTO LADR_0x01B0
LADR_0x01C1
    MOVF LRAM_0x32,W
    GOTO LADR_0x01EE
LADR_0x01C3
    CALL LADR_0x03E0
    XORWF LRAM_0x32,W
LADR_0x01C5
    BTFSS STATUS,Z
    RETURN
LADR_0x01C7
    MOVLW 0x33           ;   b'00110011'  d'051'  "3"
    MOVWF GPIO           ; !!Bank!! GPIO - TRISIO
    MOVLW 0x06           ;   b'00000110'  d'006'
    MOVWF LRAM_0x59
    MOVLW 0x00           ;   b'00000000'  d'000'
LADR_0x01CC
    ADDLW 0xFF           ;   b'11111111'  d'255'
    BTFSS STATUS,Z
    GOTO LADR_0x01CC
    DECFSZ LRAM_0x59,F
    GOTO LADR_0x01CC
    CLRF GPIO            ; !!Bank!! GPIO - TRISIO
    RETURN
LADR_0x01D3
    MOVF LRAM_0x33,W
    MOVWF LRAM_0x5E
    BTFSS LRAM_0x31,1
    BTFSC LRAM_0x31,0
    RETURN
    MOVF LRAM_0x32,W
    CALL LADR_0x020C
    BTFSC LRAM_0x5B,1
    RETURN
    BTFSC LRAM_0x31,3
    GOTO LADR_0x01E0
    BTFSS LRAM_0x31,2
    RETURN
LADR_0x01E0
    BTFSS LRAM_0x31,2
    GOTO LADR_0x01F2
    BTFSS LRAM_0x31,3
    GOTO LADR_0x01EA
LADR_0x01E4
    BTFSC LRAM_0x5B,2
    GOTO LADR_0x01ED
    CALL LADR_0x03E6
    CALL LADR_0x01C7
    CALL LADR_0x025C
    RETURN
LADR_0x01EA
    CALL LADR_0x03E0
    XORWF LRAM_0x33,W
    GOTO LADR_0x01C5
LADR_0x01ED
    MOVF LRAM_0x33,W
LADR_0x01EE
    XORLW 0x21           ;   b'00100001'  d'033'  "!"
    BTFSS STATUS,Z
    RETURN
    GOTO LADR_0x0223
LADR_0x01F2
    CALL LADR_0x03E0
    MOVWF LRAM_0x5E
    MOVLW 0x5E           ;   b'01011110'  d'094'  "^"
    MOVWF FSR
    MOVLW 0x07           ;   b'00000111'  d'007'
    ANDWF LRAM_0x33,W
    CALL LADR_0x0054
    BTFSS LRAM_0x33,4
    GOTO LADR_0x0203
    BTFSC LRAM_0x33,3
    IORWF INDF,F
    XORLW 0xFF           ;   b'11111111'  d'255'
    BTFSS LRAM_0x33,3
    ANDWF INDF,F
    MOVF LRAM_0x32,W
    CALL LADR_0x020C
    GOTO LADR_0x01E4
LADR_0x0203
    ANDWF INDF,W
    BTFSC STATUS,Z
    GOTO LADR_0x0209
    BTFSC LRAM_0x33,3
    GOTO LADR_0x01C7
    RETURN
LADR_0x0209
    BTFSS LRAM_0x33,3
    GOTO LADR_0x01C7
    RETURN
LADR_0x020C
    BCF LRAM_0x5B,1
    BCF LRAM_0x5B,2
    MOVWF LRAM_0x59
    BTFSS LRAM_0x59,7
    BTFSC LRAM_0x59,6
    GOTO LADR_0x021A
    XORLW 0x06           ;   b'00000110'  d'006'
    BTFSC STATUS,Z
    BSF LRAM_0x5B,2
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSC STATUS,Z
    BSF LRAM_0x5B,2
    MOVF LRAM_0x59,W
    RETURN
LADR_0x021A
    ADDLW 0x89           ;   b'10001001'  d'137'
    BTFSS STATUS,C
    BSF LRAM_0x5B,1
    ADDLW 0xC0           ;   b'11000000'  d'192'
    BTFSC STATUS,C
    BSF LRAM_0x5B,1
    ADDLW 0x40           ;   b'01000000'  d'064'  "@"
    ADDLW 0x40           ;   b'01000000'  d'064'  "@"
    RETURN
LADR_0x0223
    MOVLW 0x0F           ;   b'00001111'  d'015'
    MOVWF LRAM_0x5E
    MOVLW 0x24           ;   b'00100100'  d'036'  "$"
    MOVWF LRAM_0x5F
    MOVLW 0x04           ;   b'00000100'  d'004'
    CALL LADR_0x0269
    MOVLW 0xC0           ;   b'11000000'  d'192'
    MOVWF LRAM_0x5E
    MOVLW 0x10           ;   b'00010000'  d'016'
    CALL LADR_0x03E6
    SWAPF LRAM_0x5E,F
    MOVLW 0x31           ;   b'00110001'  d'049'  "1"
    MOVWF LRAM_0x5F
    MOVLW 0x04           ;   b'00000100'  d'004'
    CALL LADR_0x0269
    MOVLW 0x64           ;   b'01100100'  d'100'  "d"
    MOVWF LRAM_0x5E
    MOVLW 0x11           ;   b'00010001'  d'017'
    CALL LADR_0x03E6
    MOVLW 0x06           ;   b'00000110'  d'006'
    MOVWF LRAM_0x5E
    MOVLW 0x1C           ;   b'00011100'  d'028'
    CALL LADR_0x03E6
    MOVLW 0x08           ;   b'00001000'  d'008'
    MOVWF LRAM_0x5E
    MOVLW 0x44           ;   b'01000100'  d'068'  "D"
    MOVWF LRAM_0x5F
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL LADR_0x0269
    CLRF LRAM_0x5E
    MOVLW 0x0C           ;   b'00001100'  d'012'
    CALL LADR_0x03E6
    MOVLW 0x20           ;   b'00100000'  d'032'  " "
    MOVWF LRAM_0x5F
    MOVLW 0x04           ;   b'00000100'  d'004'
    CALL LADR_0x0269
    MOVLW 0x46           ;   b'01000110'  d'070'  "F"
    MOVWF LRAM_0x5F
    MOVLW 0x3A           ;   b'00111010'  d'058'  ":"
    CALL LADR_0x0269
    INCF LRAM_0x5E,F
    MOVLW 0x40           ;   b'01000000'  d'064'  "@"
    CALL LADR_0x03E6
    INCF LRAM_0x5E,F
    MOVLW 0x41           ;   b'01000001'  d'065'  "A"
    CALL LADR_0x03E6
    INCF LRAM_0x5E,F
    MOVLW 0x00           ;   b'00000000'  d'000'
    CALL LADR_0x03E6
    MOVLW 0x0D           ;   b'00001101'  d'013'
    CALL LADR_0x03E6
    INCF LRAM_0x5E,F
    MOVLW 0x42           ;   b'01000010'  d'066'  "B"
    MOVWF LRAM_0x5F
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL LADR_0x0269
    CALL LADR_0x01C7
LADR_0x025C
    MOVLW 0x00           ;   b'00000000'  d'000'
    CALL LADR_0x03E0
    MOVWF LRAM_0x50
    MOVLW 0x10           ;   b'00010000'  d'016'
    CALL LADR_0x03E0
    MOVWF LRAM_0x51
    MOVLW 0x11           ;   b'00010001'  d'017'
    CALL LADR_0x03E0
    MOVWF LRAM_0x52
    MOVLW 0x1C           ;   b'00011100'  d'028'
    CALL LADR_0x03E0
    MOVWF LRAM_0x53
    RETURN
LADR_0x0269
    MOVWF LRAM_0x5A
LADR_0x026A
    MOVF LRAM_0x5F,W
    CALL LADR_0x03E6
    INCF LRAM_0x5F,F
    DECFSZ LRAM_0x5A,F
    GOTO LADR_0x026A
    RETURN
LADR_0x0270
    MOVLW 0x10           ;   b'00010000'  d'016'
    ADDWF LRAM_0x41,F
    CLRF LRAM_0x59
    MOVF LRAM_0x41,W
    ADDWF LRAM_0x3C,W
    BTFSC STATUS,C
    BSF LRAM_0x59,0
    MOVF LRAM_0x41,W
    ADDWF LRAM_0x3D,W
    BTFSC STATUS,C
    BSF LRAM_0x59,1
    MOVF LRAM_0x41,W
    ADDWF LRAM_0x3E,W
    BTFSC STATUS,C
    BSF LRAM_0x59,4
    MOVF LRAM_0x41,W
    ADDWF LRAM_0x3F,W
    BTFSC STATUS,C
    BSF LRAM_0x59,5
    MOVF LRAM_0x59,W
    MOVWF GPIO           ; !!Bank!! GPIO - TRISIO
    CALL LADR_0x006A
    INCF LRAM_0x40,F
    RETURN
LADR_0x0288
    CLRF LRAM_0x5A
    CLRF LRAM_0x39
    BTFSC LRAM_0x35,4
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x35,0
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x35,1
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x35,2
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x35,3
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,0
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,1
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,2
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,3
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,4
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,5
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,6
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x36,7
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,0
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,1
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,2
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,3
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,4
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,5
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,6
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x37,7
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,0
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,1
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,2
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,3
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,4
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,5
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,6
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    BTFSC LRAM_0x38,7
    CALL LADR_0x0309
    INCF LRAM_0x5A,F
    MOVF LRAM_0x34,W
    ANDLW 0x7E           ;   b'01111110'  d'126'  "~"
    BTFSS STATUS,Z
    INCF LRAM_0x5A,F
    CALL LADR_0x0309
    BTFSC LRAM_0x5C,0
    MOVLW 0x7E           ;   b'01111110'  d'126'  "~"
    BTFSC LRAM_0x5C,1
    MOVLW 0x7F           ;   b'01111111'  d'127'  ""
    BTFSS LRAM_0x5C,0
    BTFSC LRAM_0x5C,1
    CALL LADR_0x030E
    MOVF LRAM_0x39,W
    XORWF LRAM_0x3A,F
    BTFSS LRAM_0x3A,0
    GOTO LADR_0x02F5
    BTFSS LRAM_0x39,0
    BSF LRAM_0x44,6
    BTFSC LRAM_0x39,0
    BSF LRAM_0x44,7
LADR_0x02F5
    BTFSS LRAM_0x3A,1
    GOTO LADR_0x02FB
    BTFSS LRAM_0x39,1
    BSF LRAM_0x45,6
    BTFSC LRAM_0x39,1
    BSF LRAM_0x45,7
LADR_0x02FB
    BTFSS LRAM_0x3A,2
    GOTO LADR_0x0301
    BTFSS LRAM_0x39,2
    BSF LRAM_0x46,6
    BTFSC LRAM_0x39,2
    BSF LRAM_0x46,7
LADR_0x0301
    BTFSS LRAM_0x3A,3
    GOTO LADR_0x0307
    BTFSS LRAM_0x39,3
    BSF LRAM_0x47,6
    BTFSC LRAM_0x39,3
    BSF LRAM_0x47,7
LADR_0x0307
    MOVWF LRAM_0x3A
    RETURN
LADR_0x0309
    BCF STATUS,C
    BTFSS LRAM_0x34,7
    BSF STATUS,C
    RLF LRAM_0x5A,W
    ADDLW 0x40           ;   b'01000000'  d'064'  "@"
LADR_0x030E
    CALL LADR_0x03E0
    IORWF LRAM_0x39,F
    RETURN
LADR_0x0311
    CALL LADR_0x03B5
    BTFSC INDF,7
    MOVLW 0x10           ;   b'00010000'  d'016'
    BTFSC INDF,6
    MOVLW 0x00           ;   b'00000000'  d'000'
    MOVWF INDF
    RETURN
LADR_0x0318
    CALL LADR_0x03B5
    BTFSC INDF,6
    MOVLW 0x00           ;   b'00000000'  d'000'
    MOVWF INDF
    ANDLW 0x0F           ;   b'00001111'  d'015'
    RETURN
LADR_0x031E
    CALL LADR_0x0311
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    CALL LADR_0x03A1
LADR_0x0322
    MOVWF LRAM_0x59
    MOVF LRAM_0x40,W
    ADDLW 0x3C           ;   b'00111100'  d'060'  "<"
    MOVWF FSR
    MOVF LRAM_0x59,W
    MOVWF INDF
    RETURN
LADR_0x0329
    CALL LADR_0x0318
    BTFSS STATUS,Z
    GOTO LADR_0x033C
    BSF INDF,4
    BSF INDF,5
    GOTO LADR_0x0337
LADR_0x032F
    CALL LADR_0x0318
    BTFSS STATUS,Z
    GOTO LADR_0x033C
    BSF INDF,4
    GOTO LADR_0x0337
LADR_0x0334
    CALL LADR_0x0318
    BTFSS STATUS,Z
    GOTO LADR_0x033C
LADR_0x0337
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSS INDF,7
    GOTO LADR_0x0322
    MOVLW 0x01           ;   b'00000001'  d'001'
    CALL LADR_0x03BE
LADR_0x033C
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSS STATUS,Z
    GOTO LADR_0x0348
    CALL LADR_0x03BA
    CALL LADR_0x03A8
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x03           ;   b'00000011'  d'003'
    CALL LADR_0x03C8
    MOVLW 0x0A           ;   b'00001010'  d'010'
    CALL LADR_0x03D4
LADR_0x0346
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x0348
    XORLW 0x03           ;   b'00000011'  d'003'
    BTFSS STATUS,Z
    GOTO LADR_0x0363
    CALL LADR_0x03DB
    DECFSZ INDF,F
    RETURN
    CALL LADR_0x03CF
    DECFSZ INDF,F
    GOTO LADR_0x0354
    CALL LADR_0x03BA
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x0354
    MOVF LRAM_0x40,W
    ADDLW 0x3C           ;   b'00111100'  d'060'  "<"
    MOVWF FSR
    CALL LADR_0x03A1
    XORWF INDF,F
    BTFSS STATUS,Z
    GOTO LADR_0x035F
    CALL LADR_0x03A8
    ANDLW 0x1F           ;   b'00011111'  d'031'
    ADDLW 0x14           ;   b'00010100'  d'020'
    GOTO LADR_0x03D4
LADR_0x035F
    CALL LADR_0x03A8
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x06           ;   b'00000110'  d'006'
    GOTO LADR_0x03D4
LADR_0x0363
    XORLW 0x01           ;   b'00000001'  d'001'
    BTFSS STATUS,Z
    GOTO LADR_0x0373
    BTFSS INDF,4
    GOTO LADR_0x0346
    CALL LADR_0x03A8
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x03           ;   b'00000011'  d'003'
    CALL LADR_0x03D4
    MOVLW 0x30           ;   b'00110000'  d'048'  "0"
    CALL LADR_0x03C8
    CALL LADR_0x03BA
    BTFSS INDF,5
    GOTO LADR_0x0346
    MOVLW 0x10           ;   b'00010000'  d'016'
    GOTO LADR_0x0322
LADR_0x0373
    CALL LADR_0x03CF
    DECFSZ INDF,F
    RETURN
    MOVLW 0x30           ;   b'00110000'  d'048'  "0"
    CALL LADR_0x03C8
    CALL LADR_0x03DB
    DECFSZ INDF,F
    RETURN
    MOVLW 0x02           ;   b'00000010'  d'002'
    CALL LADR_0x03BE
    CALL LADR_0x03A8
    ANDLW 0x07           ;   b'00000111'  d'007'
    ADDLW 0x05           ;   b'00000101'  d'005'
    CALL LADR_0x03C8
    MOVLW 0x0A           ;   b'00001010'  d'010'
    CALL LADR_0x03D4
    MOVLW 0x00           ;   b'00000000'  d'000'
    GOTO LADR_0x0322
LADR_0x0385
    CALL LADR_0x0311
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSS LRAM_0x5C,3
    GOTO LADR_0x0322
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x038C
    CALL LADR_0x0311
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSC LRAM_0x5C,3
    GOTO LADR_0x0322
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x0393
    CALL LADR_0x0311
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSS LRAM_0x5C,4
    GOTO LADR_0x0322
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x039A
    CALL LADR_0x0311
    MOVLW 0x00           ;   b'00000000'  d'000'
    BTFSC INDF,4
    BTFSC LRAM_0x5C,4
    GOTO LADR_0x0322
    CALL LADR_0x03A1
    GOTO LADR_0x0322
LADR_0x03A1
    MOVF LRAM_0x40,W
    ADDLW 0x24           ;   b'00100100'  d'036'  "$"
    CALL LADR_0x03E0
    MOVWF LRAM_0x5E
    SWAPF LRAM_0x5E,W
    IORLW 0x0F           ;   b'00001111'  d'015'
    RETURN
LADR_0x03A8
    MOVF LRAM_0x3B,W
    BTFSC STATUS,Z
    MOVF TMR0,W          ; !!Bank!! TMR0 - OPTION_REG
    BTFSC STATUS,Z
    INCF LRAM_0x3B,W
    MOVWF LRAM_0x59
    RRF LRAM_0x59,F
    SWAPF LRAM_0x59,F
    XORWF LRAM_0x59,F
    RRF LRAM_0x59,F
    RRF LRAM_0x3B,W
    MOVWF LRAM_0x3B
    RETURN
LADR_0x03B5
    MOVF LRAM_0x40,W
    ADDLW 0x44           ;   b'01000100'  d'068'  "D"
    MOVWF FSR
    MOVF INDF,W
    RETURN
LADR_0x03BA
    MOVF LRAM_0x40,W
    ADDLW 0x44           ;   b'01000100'  d'068'  "D"
    MOVWF FSR
    INCF INDF,W
LADR_0x03BE
    ANDLW 0x0F           ;   b'00001111'  d'015'
    MOVWF LRAM_0x59
    MOVF LRAM_0x40,W
    ADDLW 0x44           ;   b'01000100'  d'068'  "D"
    MOVWF FSR
    MOVLW 0xF0           ;   b'11110000'  d'240'
    ANDWF INDF,F
    MOVF LRAM_0x59,W
    IORWF INDF,F
    RETURN
LADR_0x03C8
    MOVWF LRAM_0x59
    MOVF LRAM_0x40,W
    ADDLW 0x48           ;   b'01001000'  d'072'  "H"
    MOVWF FSR
    MOVF LRAM_0x59,W
    MOVWF INDF
    RETURN
LADR_0x03CF
    MOVF LRAM_0x40,W
    ADDLW 0x48           ;   b'01001000'  d'072'  "H"
    MOVWF FSR
    MOVF INDF,W
    RETURN
LADR_0x03D4
    MOVWF LRAM_0x59
    MOVF LRAM_0x40,W
    ADDLW 0x4C           ;   b'01001100'  d'076'  "L"
    MOVWF FSR
    MOVF LRAM_0x59,W
    MOVWF INDF
    RETURN
LADR_0x03DB
    MOVF LRAM_0x40,W
    ADDLW 0x4C           ;   b'01001100'  d'076'  "L"
    MOVWF FSR
    MOVF INDF,W
    RETURN
LADR_0x03E0
    BSF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    MOVWF EEADR          ; !!Bank!! Unimplemented - EEADR
    BSF EECON1,0         ; !!Bank!! Unimplemented - EECON1
    MOVF EEDATA,W        ; !!Bank!! Unimplemented - EEDATA
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    RETURN
LADR_0x03E6
    CALL LADR_0x03E0
    XORWF LRAM_0x5E,W
    BTFSC STATUS,Z
    RETURN
    MOVF LRAM_0x5E,W
    BSF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    MOVWF EEDATA         ; !!Bank!! Unimplemented - EEDATA
    BSF EECON1,2         ; !!Bank!! Unimplemented - EECON1
    BCF INTCON,GIE
    MOVLW 0x55           ;   b'01010101'  d'085'  "U"
    MOVWF EECON2         ; !!Bank!! Unimplemented - EECON2
    MOVLW 0xAA           ;   b'10101010'  d'170'
    MOVWF EECON2         ; !!Bank!! Unimplemented - EECON2
    BSF EECON1,1         ; !!Bank!! Unimplemented - EECON1
    BSF INTCON,GIE
    BCF EECON1,2         ; !!Bank!! Unimplemented - EECON1
LADR_0x03F6
    BTFSC EECON1,1       ; !!Bank!! Unimplemented - EECON1
    GOTO LADR_0x03F6
    BCF STATUS,RP0       ; !!Bank Register-Bank(0/1)-Select
    RETURN

    End
