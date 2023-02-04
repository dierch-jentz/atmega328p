
; AVRMon-arduino_pro_mini.asm

; The WOZ Monitor for the Apple 1
; Written by Steve Wozniak in 1976

; Ported to the Atmel-AVR in 2021
; Dirk Reismann
; 45711 Datteln

; Memory Area                       Size        Long Address        Short Address
; General purpose register file     32B         0x0000-0x001F       n/a
; I/O register file                 64B         0x0020-0x005F       0x00-0x3f
; Extended I/O register file        160B        0x0060-0x00FF       n/a
; Internal SRAM                     1048x8      0x0100-0x08FF       n/a

;                           Originalcode            Comments

.NOLIST
.INCLUDE "m328Pdef.inc"
.LIST
.DSEG
.ORG    SRAM_START

; $0100
UNUSED: .BYTE     1
XAML:   .BYTE     1         ; XAML .EQ $24          Last "opened" location Low
XAMH:   .BYTE     1         ; XAMH .EQ $25          Last "opened" location High
STL:    .BYTE     1         ; STL .EQ $26           Store address Low
STH:    .BYTE     1         ; STH .EQ $27           Store address High
L:      .BYTE     1         ; L .EQ $28             Hex value parsing Low
H:      .BYTE     1         ; H .EQ $29             Hex value parsing High
YSAV:   .BYTE     1         ; YSAV .EQ $2A          Used to see if hex value is given
MODE:   .BYTE     1         ; MODE .EQ $2B          $00=XAM, $7F=STOR, $AE=BLOCK XAM
; $0109
INPUT:  .BYTE   127         ; IN .EQ $0200,$027F    Input buffer

                            ; KBD .EQ $D010         PIA.A keyboard input
                            ; KBDCR .EQ $D011       PIA.A keyboard control register
                            ; DSP .EQ $D012         PIA.B display output register
                            ; DSPCR .EQ $D013       PIA.B display control register

                            ; KBD b7..b0 are inputs, b6..b0 is ASCII input, b7 is constant high
                            ;     Programmed to respond to low to high KBD strobe
                            ; DSP b6..b0 are outputs, b7 is input
                            ;     CB2 goes low when data is written, returns high when CB1 goes high
                            ; Interrupts are enabled, though not used. KBD can be jumpered to IRQ,
                            ; whereas DSP can be jumpered to NMI.

                            ;-------------------------------------------------------------------------
                            ;  Constants
                            ;-------------------------------------------------------------------------

.CSEG
.DEF    ACCU      = r20
.DEF    XREG      = r21
.DEF    YREG      = r22
.EQU    F_CPU     = 16000000
.EQU    BAUD      = 4800
.EQU    UBRR_VAL  = ((F_CPU+BAUD*8)/(BAUD*16)-1)
.EQU    LF        = $0a
.EQU    BS        = $7f     ; BS .EQ $DF            Backspace key, arrow left key
.EQU    CR        = $0d     ; CR .EQ $8D            Carriage Return
.EQU    ESC       = $1b     ; ESC .EQ $9B           ESC key
.EQU    PROMPT    = '/'     ; PROMPT .EQ "\"        Prompt character

.ORG    INT_VECTORS_SIZE
.ORG    $0000
    rjmp    MAIN
.ORG    INT0addr
    rjmp    INT0addr_
.ORG    INT1addr
    rjmp    INT1addr_
.ORG    PCI0addr
    rjmp    PCI0addr_
.ORG    PCI1addr
    rjmp    PCI1addr_
.ORG    PCI2addr
    rjmp    PCI2addr_
.ORG    WDTaddr
    rjmp    WDTaddr_
.ORG    OC2Aaddr
    rjmp    OC2Aaddr_
.ORG    OC2Baddr
    rjmp    OC2Baddr_
.ORG    OVF2addr
    rjmp    OVF2addr_
.ORG    ICP1addr
    rjmp    ICP1addr_
.ORG    OC1Aaddr
    rjmp    OC1Aaddr_
.ORG    OC1Baddr
    rjmp    OC1Baddr_
.ORG    OVF1addr
    rjmp    OVF1addr_
.ORG    OC0Aaddr
    rjmp    OC0Aaddr_
.ORG    OC0Baddr
    rjmp    OC0Baddr_
.ORG    OVF0addr
    rjmp    OVF0addr_
.ORG    SPIaddr
    rjmp    SPIaddr_
.ORG    URXCaddr
    rjmp    URXCaddr_
.ORG    UDREaddr
    rjmp    UDREaddr_
.ORG    UTXCaddr
    rjmp    UTXCaddr_
.ORG    ADCCaddr
    rjmp    ADCCaddr_
.ORG    ERDYaddr
    rjmp    ERDYaddr_
.ORG    ACIaddr
    rjmp    ACIaddr_
.ORG    TWIaddr
    rjmp    TWIaddr_
.ORG    SPMRaddr
    rjmp    SPMRaddr_

MAIN:
    ldi     r16, HIGH(RAMEND)
    out     SPH, r16
    ldi     r16, LOW(RAMEND)
    out     SPL, r16
    ldi     r16, HIGH(UBRR_VAL)
    sts     UBRR0H, r16
    ldi     r16, LOW(UBRR_VAL)
    sts     UBRR0L, r16
    ldi     r16, (1<<UCSZ01)|(1<<UCSZ00)
    sts     UCSR0C, r16
    ldi     r16, (1<<RXCIE0)|(1<<RXEN0)|(1<<TXEN0)
    sts     UCSR0B, r16
    sei
    clr     r0
    rcall   RESET

LOOP:
    rjmp    LOOP

                            ;-------------------------------------------------------------------------
                            ;  Let's get started
                            ;
                            ;  Remark the RESET routine is only to be entered by asserting the RESET
                            ;  line of the system. This ensures that the data direction registers
                            ;  are selected.
                            ;-------------------------------------------------------------------------

RESET:
                            ; CLD                   Clear decimal arithmetic mode
                            ; CLI
    ldi     YREG, $7f       ; LDY #%0111.1111       Mask for DSP data direction reg
                            ; STY DSP               (DDR mode is assumed after reset)
                            ; LDA #%1010.0111       KBD and DSP control register mask
                            ; STA KBDCR             Enable interrupts, set CA1, CB1 for
                            ; STA DSPCR             positive edge sense/output mode.

                            ; Program falls through to the GETLINE routine to save some program bytes
                            ; Please note that Y still holds $7F, which will cause an automatic Escape

                            ;-------------------------------------------------------------------------
                            ;  The GETLINE process
                            ;-------------------------------------------------------------------------

NOTCR:
    cpi     ACCU, BS        ; CMP #BS               Backspace key?
    breq    BACKSPACE       ; BEQ BACKSPACE         Yes
    cpi     ACCU, ESC       ; CMP #ESC              ESC?
    breq    ESCAPE          ; BEQ ESCAPE            Yes
    inc     YREG            ; INY                   Advance text index
    brpl    NEXTCHAR        ; BPL NEXTCHAR          Auto ESC if line longer than 127

ESCAPE:
    ldi     ACCU, PROMPT    ; LDA #PROMPT           Print prompt character
    rcall   ECHO            ; JSR ECHO              Output it.

GETLINE:
    ldi     ACCU, CR        ; LDA #CR               Send CR
    rcall   ECHO            ; JSR ECHO
    ldi     ACCU, LF
    rcall   ECHO
    ldi     YREG, $00+1     ; LDY #0+1              Start a new input line

BACKSPACE:
    ldi     ACCU, $08
    rcall   ECHO
    ldi     ACCU, ' '
    rcall   ECHO
    ldi     ACCU, $08
    rcall   ECHO
    dec     YREG            ; DEY                   Backup text index
    brmi    GETLINE         ; BMI GETLINE           Oops, line's empty, reinitialize

NEXTCHAR:
    cpi     r17, $00        ; LDA KBDCR             Wait for key press
    breq    NEXTCHAR        ; BPL NEXTCHAR          No key yet!
    mov     XREG, ACCU      ; LDA KBD               Load character. B7 should be '1'
    clr     r17
    ldi     ZL, LOW(INPUT)  ; STA IN,Y              Add to text buffer
    ldi     ZH, HIGH(INPUT)
    add     ZL, YREG
    adc     ZH, r0
    st      Z, ACCU
    rcall   ECHO            ; JSR ECHO              Display character
    cpi     ACCU, CR        ; CMP #CR
    brne    NOTCR           ; BNE NOTCR             It's not CR!

                            ; Line received, now let's parse it

    ldi     YREG, $00-1     ; LDY #-1               Reset text index
    ldi     ACCU, $00       ; LDA #0                Default mode is XAM
    mov     XREG, ACCU      ; TAX                   X=0

SETMODE:
    lsl     ACCU

SETSTOR:
    lsl     ACCU            ; ASL                   Leaves $7B if setting STOR mode
    ldi     ZL, LOW(MODE)   ; STA MODE              Set mode flags
    ldi     ZH, HIGH(MODE)
    st      Z, ACCU

BLSKIP:
    inc     YREG            ; INY                   Advance text index

NEXTITEM:
    ldi     ZL, LOW(INPUT)  ; LDA IN,Y              Get character
    ldi     ZH, HIGH(INPUT)
    add     ZL, YREG
    adc     ZH, r0
    ld      ACCU, Z
    cpi     ACCU, CR        ; CMP #CR
    breq    GETLINE         ; BEQ GETLINE           We're done if it's CR!
    cpi     ACCU, '.'       ; CMP #"."
    brcs    BLSKIP          ; BCC BLSKIP            Ignore everything below "."!
    breq    SETMODE         ; BEQ SETMODE           Set BLOCK XAM mode ("." = $AE)
    cpi     ACCU, ':'       ; CMP #":"
    breq    SETSTOR         ; BEQ SETSTOR           Set STOR mode! $BA will become $7B
    cpi     ACCU, 'R'       ; CMP #"R"
; **** Branch out of range
    brne    NEXTITEM1       ; BEQ RUN
    rcall   RUN
NEXTITEM1:
    ldi     ZL, LOW(L)      ; STX L                 Clear input value (X=0)
    ldi     ZH, HIGH(L)
    st      Z, XREG
    ldi     ZL, LOW(H)      ; STX H
    ldi     ZH, HIGH(H)
    st      Z, XREG
    ldi     ZL, LOW(YSAV)   ; STY YSAV              Save Y for comparison
    ldi     ZH, HIGH(YSAV)
    st      Z, YREG

                            ; Here we're trying to parse a new hex value

NEXTHEX:
    ldi     ZL, LOW(INPUT)  ; LDA IN,Y              Get character for hex test
    ldi     ZH, HIGH(INPUT)
    add     ZL, YREG
    adc     ZH, r0
    ld      ACCU, Z         ; EOR #$B0              Map digits to 0-9
    ldi     r16, $30
    eor     ACCU, r16
    cpi     ACCU, $09+1     ; CMP #9+1              Is it a decimal digit?
    brcs    DIG             ; BCC DIG               Yes!
    ldi     r16, $89        ; ADC #$88              Map letter "A"-"F" to $FA-FF
    adc     ACCU, r16
    cpi     ACCU, $fa       ; CMP #$FA              Hex letter?
    brcs    NOTHEX          ; BCC NOTHEX            No! Character not hex

DIG:
    lsl     ACCU            ; ASL
    lsl     ACCU            ; ASL                   Hex digit to MSD of A
    lsl     ACCU            ; ASL
    lsl     ACCU            ; ASL
    ldi     XREG, $04       ; LDX #4                Shift count

HEXSHIFT:
    lsl     ACCU            ; ASL                   Hex digit left, MSB to carry
    ldi     ZL, LOW(L)      ; ROL L                 Rotate into LSD
    ldi     ZH, HIGH(L)
    ld      r16, Z
    rol     r16
    st      Z, r16
    ldi     ZL, LOW(H)      ; ROL H                 Rotate into MSD's
    ldi     ZH, HIGH(H)
    ld      r16, Z
    rol     r16
    st      Z, r16
    dec     XREG            ; DEX                   Done 4 shifts?
    brne    HEXSHIFT        ; BNE HEXSHIFT          No, loop
    inc     YREG            ; INY                   Advance text index
    brne    NEXTHEX         ; BNE NEXTHEX           Always taken

NOTHEX:
    ldi     ZL, LOW(YSAV)   ; CPY YSAV              Was at least 1 hex digit given?
    ldi     ZH, HIGH(YSAV)
    ld      r16, Z
    cp      YREG, r16
; **** Branch out of range
    brne    NOTHEX1         ; BEQ ESCAPE            No! Ignore all, start from scratch
    rjmp    ESCAPE
NOTHEX1:
    ldi     ZL, LOW(MODE)   ; BIT MODE              Test MODE byte
    ldi     ZH, HIGH(MODE)
    ld      r16, Z
                            ;                       Bit 7 = Negative Flag
    sbrc    r16, $07
    sen
                            ;                       Bit 6 = Overflow Flag
    sbrc    r16, $06
    sev
    brvc    NOTSTOR         ; BVC NOTSTOR           B6=0 is STOR, 1 is XAM or BLOCK XAM

                            ; STOR mode, save LSD of new hex byte

    ldi     ZL, LOW(L)      ; LDA L                 LSD's of hex data
    ldi     ZH, HIGH(L)
    ld      ACCU, Z
    ldi     ZL, LOW(STL)    ; STA (STL,X)           Store current 'store index'(X=0)
    ldi     ZH, HIGH(STL)
    add     ZL, XREG
    adc     ZH, r0
    ld      YL, Z+
    ld      YH, Z
    st      Y, ACCU
    ldi     ZL, LOW(STL)    ; INC STL               Increment store index.
    ldi     ZH, HIGH(STL)
    ld      r16, Z
    inc     r16
    st      Z, r16
; **** Branch out of range
    breq    NOTHEX2         ; BNE NEXTITEM          No carry!
    rjmp    NEXTITEM
NOTHEX2:
    ldi     ZL, LOW(STH)    ; INC STH               Add carry to 'store index' high
    ldi     ZH, HIGH(STH)
    ld      r16, Z
    inc     r16
    st      Z, r16

TONEXTITEM:
    rjmp    NEXTITEM        ; JMP NEXTITEM          Get next command item.

                            ;-------------------------------------------------------------------------
                            ;  RUN user's program from last opened location
                            ;-------------------------------------------------------------------------

RUN:
    clr     ZL
    clr     ZH
    ldi     YL, LOW(XAML)
    ldi     YH, HIGH(XAML)
    ld      ACCU, Y
    ldi     YL, LOW(XAMH)
    ldi     YH, HIGH(XAMH)
    ld      XREG, Y
    add     ZL, ACCU
    adc     ZH, XREG
    icall                   ; JMP (XAML)            Run user's program

                            ;-------------------------------------------------------------------------
                            ;  We're not in Store mode
                            ;-------------------------------------------------------------------------

NOTSTOR:
    brmi    XAMNEXT         ; BMI XAMNEXT           B7 = 0 for XAM, 1 for BLOCK XAM
    ldi     XREG, $02       ; LDX #2                Copy 2 bytes

SETADR:
    ldi     ZL, LOW(L-1)    ; LDA L-1,X             Copy hex data to
    ldi     ZH, HIGH(L-1)
    add     ZL, XREG
    adc     ZH, r0
    ld      ACCU, Z
    ldi     ZL, LOW(STL-1)  ; STA STL-1,X           'store index'
    ldi     ZH, HIGH(STL-1)
    add     ZL, XREG
    adc     ZH, r0
    st      Z, ACCU
    ldi     ZL, LOW(XAML-1) ; STA XAML-1,X          and to 'XAM index'
    ldi     ZH, HIGH(XAML-1)
    add     ZL, XREG
    adc     ZH, r0
    st      Z, ACCU
    dec     XREG            ; DEX                   Next of 2 bytes
    brne    SETADR          ; BNE SETADR            Loop unless X = 0

                            ; Print address and data from this address, fall through next BNE.

NXTPRNT:
    brne    PRDATA          ; BNE PRDATA            NE means no address to print
    ldi     ACCU, CR        ; LDA #CR               Print CR first
    rcall   ECHO            ; JSR ECHO
    ldi     ACCU, LF
    rcall   ECHO
    ldi     ZL, LOW(XAMH)   ; LDA XAMH              Output high-order byte of address
    ldi     ZH, HIGH(XAMH)
    ld      ACCU, Z
    rcall   PRBYTE          ; JSR PRBYTE
    ldi     ZL, LOW(XAML)   ; LDA XAML              Output low-order byte of address
    ldi     ZH, HIGH(XAML)
    ld      ACCU, Z
    rcall   PRBYTE          ; JSR PRBYTE
    ldi     ACCU, ':'       ; LDA #":"              Print colon
    rcall   ECHO            ; JSR ECHO

PRDATA:
    ldi     ACCU, ' '       ; LDA #" "              Print space
    rcall   ECHO            ; JSR ECHO
    ldi     ZL, LOW(XAML)   ; LDA (XAML,X)          Get data from address (X=0)
    ldi     ZH, HIGH(XAML)
    add     ZL, XREG
    adc     ZH, r0
    ld      YL, Z+
    ld      YH, Z
    ld      ACCU, Y
    rcall   PRBYTE          ; JSR PRBYTE            Output it in hex format

XAMNEXT:
    ldi     ZL, LOW(MODE)   ; STX MODE              0 -> MODE (XAM mode).
    ldi     ZH, HIGH(MODE)
    st      Z, XREG
    ldi     ZL, LOW(XAML)   ; LDA XAML              See if there's more to print
    ldi     ZH, HIGH(XAML)
    ld      ACCU, Z
    ldi     ZL, LOW(L)      ; CMP L
    ldi     ZH, HIGH(L)
    ld      r16, Z
    cp      ACCU, r16
    ldi     ZL, LOW(XAMH)   ; LDA XAMH
    ldi     ZH, HIGH(XAMH)
    ld      ACCU, Z
    ldi     ZL, LOW(H)      ; SBC H
    ldi     ZH, HIGH(H)
    ld      r16, Z
    sbc     ACCU, r16
; **** Branch out of range
    brcs    XAMNEXT1        ; BCS TONEXTITEM        Not less! No more data to output
    rjmp    TONEXTITEM
XAMNEXT1:
    ldi     ZL, LOW(XAML)   ; INC XAML              Increment 'examine index'
    ldi     ZH, HIGH(XAML)
    ld      r16, Z
    inc     r16
    st      Z, r16
    brne    MOD8CHK         ; BNE MOD8CHK           No carry!
    ldi     ZL, LOW(XAMH)   ; INC XAMH
    ldi     ZH, HIGH(XAMH)
    ld      r16, Z
    inc     r16
    st      Z, r16

MOD8CHK:
    ldi     ZL, LOW(XAML)   ; LDA XAML              If address MOD 8 = 0 start new line
    ldi     ZH, HIGH(XAML)
    ld      ACCU, Z
    andi    ACCU, $07       ; AND #%0000.0111
    brpl    NXTPRNT         ; BPL NXTPRNT           Always taken.

                            ;-------------------------------------------------------------------------
                            ;  Subroutine to print a byte in A in hex form (destructive)
                            ;-------------------------------------------------------------------------

PRBYTE:
    push    ACCU            ; PHA                   Save A for LSD
    lsr     ACCU            ; LSR
    lsr     ACCU            ; LSR
    lsr     ACCU            ; LSR                   MSD to LSD position
    lsr     ACCU            ; LSR
    rcall   PRHEX           ; JSR PRHEX             Output hex digit
    pop     ACCU            ; PLA                   Restore A

                            ; Fall through to print hex routine

                            ;-------------------------------------------------------------------------
                            ;  Subroutine to print a hexadecimal digit
                            ;-------------------------------------------------------------------------

PRHEX:
    andi    ACCU, $0f       ; AND #%0000.1111       Mask LSD for hex print
    ori     ACCU, '0'       ; ORA #"0"              Add "0"
    cpi     ACCU, '9'+1     ; CMP #"9"+1            Is it a decimal digit?
    brcs    ECHO            ; BCC ECHO              Yes! output it
    ldi     r16, $07        ; ADC #6                Add offset for letter A-F
    adc     ACCU, r16

                            ; Fall through to print routine

                            ;-------------------------------------------------------------------------
                            ;  Subroutine to print a character to the terminal
                            ;-------------------------------------------------------------------------

ECHO:
                            ; BIT DSP               DA bit (B7) cleared yet?
    lds     r16, UCSR0A     ; BMI ECHO              No! Wait for display ready
    sbrs    r16, UDRE0
    rjmp    ECHO
    sts     UDR0, ACCU      ; STA DSP               Output character. Sets DA
    ret

                            ;-------------------------------------------------------------------------
                            ;  Vector area
                            ;-------------------------------------------------------------------------

.DB     $00,$00             ; .DA $0000             Unused, what a pity
                            ; NMI_VEC .DA $0F00     NMI vector
                            ; RESET_VEC .DA RESET   RESET vector
                            ; IRQ_VEC .DA $0000     IRQ vector

INT0addr_:
    reti
INT1addr_:
    reti
PCI0addr_:
    reti
PCI1addr_:
    reti
PCI2addr_:
    reti
WDTaddr_:
    reti
OC2Aaddr_:
    reti
OC2Baddr_:
    reti
OVF2addr_:
    reti
ICP1addr_:
    reti
OC1Aaddr_:
    reti
OC1Baddr_:
    reti
OVF1addr_:
    reti
OC0Aaddr_:
    reti
OC0Baddr_:
    reti
OVF0addr_:
    reti
SPIaddr_:
    reti
URXCaddr_:
    lds     r16, UCSR0A
    sbrs    r16, UDRE0
    rjmp    URXCaddr_
    lds     r17, UDR0
    mov     ACCU, r17
    reti
UDREaddr_:
    reti
UTXCaddr_:
    reti
ADCCaddr_:
    reti
ERDYaddr_:
    reti
ACIaddr_:
    reti
TWIaddr_:
    reti
SPMRaddr_:
    reti

                            ;-------------------------------------------------------------------------

                            ; .LI OFF
