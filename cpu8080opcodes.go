package cpu8080

import (
	"os"
)

// opcodeS comments from:
// http://www.emulator101.com/8080-by-opcode.html

// NOP	1
func (c *CPU8080) opcode0x00(byte1 uint8, byte2 uint8) {
	c.PC++
}

// LXI B,D16	3		B <- byte 3, C <- byte 2
func (c *CPU8080) opcode0x01(byte1 uint8, byte2 uint8) {
	c.B = byte2
	c.C = byte1
	c.PC += 3
}

// STAX B	1		(BC) <- A
func (c *CPU8080) opcode0x02(byte1 uint8, byte2 uint8) {
	address := uint16(c.B)<<8 | uint16(c.C)
	c.writeMem(address, c.A)
	c.PC++
}

// INX B	1		BC <- BC+1
func (c *CPU8080) opcode0x03(byte1 uint8, byte2 uint8) {
	c.C++
	if c.C == 0 {
		c.B++
	}
	c.PC++
}

// INR B	1	Z, S, P, AC	B <- B+1
func (c *CPU8080) opcode0x04(byte1 uint8, byte2 uint8) {
	c.B = c.opAdd1(c.B)
	c.PC++
}

// DCR B	1	Z, S, P, AC	B <- B-1
func (c *CPU8080) opcode0x05(byte1 uint8, byte2 uint8) {
	c.B = c.opSub1(c.B)
	c.PC++
}

// MVI B, D8	2		B <- byte 2
func (c *CPU8080) opcode0x06(byte1 uint8, byte2 uint8) {
	c.B = byte1
	c.PC += 2
}

// RLC	1	CY	A = A << 1; bit 0 = prev bit 7; CY = prev bit 7
func (c *CPU8080) opcode0x07(byte1 uint8, byte2 uint8) {
	x := c.A
	c.A = ((x & 0x80) >> 7) | (x << 1)
	c.cc.cy = bool2uint8(0x80 == (x & 0x80))
	c.PC++
}

// DAD B	1	CY	HL = HL + BC
func (c *CPU8080) opcode0x09(byte1 uint8, byte2 uint8) {
	hl := (uint16(c.H) << 8) | uint16(c.L)
	bc := (uint16(c.B) << 8) | uint16(c.C)
	res := uint32(hl) + uint32(bc)
	c.H = uint8((res & 0xff00) >> 8)
	c.L = uint8(res & 0xff)
	c.cc.cy = bool2uint8(((res & 0xffff0000) != 0))
	c.PC++
}

// LDAX B	1		A <- (BC)
func (c *CPU8080) opcode0x0a(byte1 uint8, byte2 uint8) {
	address := uint16(c.B)<<8 | uint16(c.C)
	c.A = c.Memory[address]
	c.PC++
}

// DCX B	1		BC = BC-1
func (c *CPU8080) opcode0x0b(byte1 uint8, byte2 uint8) {
	c.C--
	if c.C == 0xff {
		c.B--
	}
	c.PC++
}

// INR C	1	Z, S, P, AC	C <- C+1
func (c *CPU8080) opcode0x0c(byte1 uint8, byte2 uint8) {
	c.C = c.opAdd1(c.C)
	c.PC++
}

// DCR C	1	Z, S, P, AC	C <-C-1
func (c *CPU8080) opcode0x0d(byte1 uint8, byte2 uint8) {
	c.C = c.opSub1(c.C)
	c.PC++
}

// MVI C,D8	2		C <- byte 2
func (c *CPU8080) opcode0x0e(byte1 uint8, byte2 uint8) {
	c.C = byte1
	c.PC += 2
}

// RRC	1	CY	A = A >> 1; bit 7 = prev bit 0; CY = prev bit 0
func (c *CPU8080) opcode0x0f(byte1 uint8, byte2 uint8) {
	x := c.A
	c.A = ((x & 1) << 7) | (x >> 1)
	c.cc.cy = bool2uint8(1 == (x & 1))
	c.PC++
}

// -
func (c *CPU8080) opcode0x10(byte1 uint8, byte2 uint8) {
	c.PC++
}

// LXI D,D16	3		D <- byte 3, E <- byte 2
func (c *CPU8080) opcode0x11(byte1 uint8, byte2 uint8) {
	c.D = byte2
	c.E = byte1
	c.PC += 3
}

// STAX D	1		(DE) <- A
func (c *CPU8080) opcode0x12(byte1 uint8, byte2 uint8) {
	address := uint16(c.D)<<8 | uint16(c.E)
	c.writeMem(address, c.A)
	c.PC++
}

// 	INX D	1		DE <- DE + 1
func (c *CPU8080) opcode0x13(byte1 uint8, byte2 uint8) {
	c.E++
	if c.E == 0 {
		c.D++
	}
	c.PC++
}

//	INR D	1	Z, S, P, AC	D <- D+1
func (c *CPU8080) opcode0x14(byte1 uint8, byte2 uint8) {
	c.D = c.opAdd1(c.D)
	c.PC++
}

// DCR D	1	Z, S, P, AC	D <- D-1
func (c *CPU8080) opcode0x15(byte1 uint8, byte2 uint8) {
	c.D = c.opSub1(c.D)
	c.PC++
}

// MVI D, D8	2		D <- byte 2
func (c *CPU8080) opcode0x16(byte1 uint8, byte2 uint8) {
	c.D = byte1
	c.PC += 2
}

// RAL	1	CY	A = A << 1; bit 0 = prev CY; CY = prev bit 7
func (c *CPU8080) opcode0x17(byte1 uint8, byte2 uint8) {
	x := c.A
	c.A = c.cc.cy | (x << 1)
	c.cc.cy = bool2uint8(0x80 == (x & 0x80))
	c.PC++
}

// 	DAD D	1	CY	HL = HL + DE
func (c *CPU8080) opcode0x19(byte1 uint8, byte2 uint8) {
	hl := (uint16(c.H) << 8) | uint16(c.L)
	de := (uint16(c.D) << 8) | uint16(c.E)
	res := uint32(hl) + uint32(de)
	c.H = uint8((res & 0xff00) >> 8)
	c.L = uint8(res & 0xff)
	c.cc.cy = bool2uint8(((res & 0xffff0000) != 0))
	c.PC++
}

// LDAX D	1		A <- (DE)
func (c *CPU8080) opcode0x1a(byte1 uint8, byte2 uint8) {
	address := uint16(c.D)<<8 | uint16(c.E)
	c.A = c.Memory[address]
	c.PC++
}

// DCX D	1		DE = DE-1
func (c *CPU8080) opcode0x1b(byte1 uint8, byte2 uint8) {
	c.E--
	if c.E == 0xff {
		c.D--
	}
	c.PC++
}

// INR E	1	Z, S, P, AC	E <-E+1
func (c *CPU8080) opcode0x1c(byte1 uint8, byte2 uint8) {
	c.E = c.opAdd1(c.E)
	c.PC++
}

// DCR E	1	Z, S, P, AC	E <- E-1
func (c *CPU8080) opcode0x1d(byte1 uint8, byte2 uint8) {
	c.E = c.opSub1(c.E)
	c.PC++
}

// 	MVI E,D8	2		E <- byte 2
func (c *CPU8080) opcode0x1e(byte1 uint8, byte2 uint8) {
	c.E = byte1
	c.PC += 2
}

// RAR	1	CY	A = A >> 1; bit 7 = prev bit 7; CY = prev bit 0
func (c *CPU8080) opcode0x1f(byte1 uint8, byte2 uint8) {
	x := c.A
	c.A = (c.cc.cy << 7) | (x >> 1)
	c.cc.cy = bool2uint8(1 == (x & 1))
	c.PC++
}

// 	RIM	1		special
func (c *CPU8080) opcode0x20(byte1 uint8, byte2 uint8) {
	c.PC++
}

// LXI H,D16	3		H <- byte 3, L <- byte 2
func (c *CPU8080) opcode0x21(byte1 uint8, byte2 uint8) {
	c.H = byte2
	c.L = byte1
	c.PC += 3
}

// SHLD adr	3		(adr) <-L; (adr+1)<-H
func (c *CPU8080) opcode0x22(byte1 uint8, byte2 uint8) {
	address := uint16(byte2)<<8 | uint16(byte1)
	c.writeMem(address, c.L)
	c.writeMem(address+1, c.H)
	c.PC += 3
}

// INX H	1		HL <- HL + 1
func (c *CPU8080) opcode0x23(byte1 uint8, byte2 uint8) {
	c.L++
	if c.L == 0 {
		c.H++
	}
	c.PC++
}

// INR H	1	Z, S, P, AC	H <- H+1
func (c *CPU8080) opcode0x24(byte1 uint8, byte2 uint8) {
	c.H = c.opAdd1(c.H)
	c.PC++
}

// DCR H	1	Z, S, P, AC	H <- H-1
func (c *CPU8080) opcode0x25(byte1 uint8, byte2 uint8) {
	c.H = c.opSub1(c.H)
	c.PC++
}

// 	MVI H,D8	2		L <- byte 2
func (c *CPU8080) opcode0x26(byte1 uint8, byte2 uint8) {
	c.H = byte1
	c.PC += 2
}

// DAA	1		special
func (c *CPU8080) opcode0x27(byte1 uint8, byte2 uint8) {
	/*	lsb := c.A & 0x0F
		if c.cc.ac== 1 || lsb > 9 {
			c.A += 0x06
		}
		c.cc.ac= bool2uint8((lsb + 0x06) > 0xF)
		msb := c.A >> 4
		if c.cc.cy == 1 || msb > 9 {
			c.A += 0x60
		}
		if msb+0x06 > 0xF {
			c.cc.cy = 1
		}
		c.flagsZSP(c.A)
		c.PC++
	*/
	cy := c.cc.cy
	correction := uint8(0)

	lsb := c.A & 0x0F
	msb := c.A >> 4

	if c.cc.ac == 1 || lsb > 9 {
		correction += 0x06
	}

	if c.cc.cy == 1 || msb > 9 || (msb >= 9 && lsb > 9) {
		correction += 0x60
		cy = 1
	}
	c.A = c.opAdd(c.A, correction, 0)
	c.cc.cy = cy
	c.PC++
}

// DAD H	1	CY	HL = HL + HI
func (c *CPU8080) opcode0x29(byte1 uint8, byte2 uint8) {
	hl := (uint16(c.H) << 8) | uint16(c.L)
	res := uint32(hl) + uint32(hl)
	c.H = uint8((res & 0xff00) >> 8)
	c.L = uint8(res & 0xff)
	c.cc.cy = bool2uint8((res & 0xffff0000) != 0)
	c.PC++
}

// LHLD adr	3		L <- (adr); H<-(adr+1)
func (c *CPU8080) opcode0x2a(byte1 uint8, byte2 uint8) {
	address := uint16(byte2)<<8 | uint16(byte1)
	c.L = c.Memory[address]
	c.H = c.Memory[address+1]
	c.PC += 3
}

// 	DCX H	1		HL = HL-1
func (c *CPU8080) opcode0x2b(byte1 uint8, byte2 uint8) {
	c.L--
	if c.L == 0xff {
		c.H--
	}
	c.PC++
}

// INR L	1	Z, S, P, AC	L <- L+1
func (c *CPU8080) opcode0x2c(byte1 uint8, byte2 uint8) {
	c.L = c.opAdd1(c.L)
	c.PC++
}

// 	DCR L	1	Z, S, P, AC	L <- L-1
func (c *CPU8080) opcode0x2d(byte1 uint8, byte2 uint8) {
	c.L = c.opSub1(c.L)
	c.PC++
}

// MVI L, D8	2		L <- byte 2
func (c *CPU8080) opcode0x2e(byte1 uint8, byte2 uint8) {
	c.L = byte1
	c.PC += 2
}

// CMA	1		A <- !A
func (c *CPU8080) opcode0x2f(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ 0xFF
	c.PC++
}

// LXI SP, D16	3		SP.hi <- byte 3, SP.lo <- byte 2
func (c *CPU8080) opcode0x31(byte1 uint8, byte2 uint8) {
	c.sp = uint16(byte2)<<8 | uint16(byte1)
	c.PC += 3
}

// STA adr	3		(adr) <- A
func (c *CPU8080) opcode0x32(byte1 uint8, byte2 uint8) {
	address := uint16(byte2)<<8 | uint16(byte1)
	c.writeMem(address, c.A)
	c.PC += 3
}

// INX SP	1		SP = SP + 1
func (c *CPU8080) opcode0x33(byte1 uint8, byte2 uint8) {
	c.sp++
	c.PC++
}

// INR M	1	Z, S, P, AC	(HL) <- (HL)+1
func (c *CPU8080) opcode0x34(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.opAdd1(c.readFromHL()))
	c.PC++
}

// DCR M	1	Z, S, P, AC	(HL) <- (HL)-1
func (c *CPU8080) opcode0x35(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.opSub1(c.readFromHL()))
	c.PC++
}

// MVI M,D8	2		(HL) <- byte 2
func (c *CPU8080) opcode0x36(byte1 uint8, byte2 uint8) {
	address := uint16(c.H)<<8 | uint16(c.L)
	c.writeMem(address, byte1)
	c.PC += 2
}

// 	STC	1	CY	CY = 1
func (c *CPU8080) opcode0x37(byte1 uint8, byte2 uint8) {
	c.cc.cy = 1
	c.PC++
}

// SPECIAL
func (c *CPU8080) opcode0x38(byte1 uint8, byte2 uint8) {
	os.Exit(1)
	c.PC++
}

// DAD SP	1	CY	HL = HL + SP
func (c *CPU8080) opcode0x39(byte1 uint8, byte2 uint8) {
	hl := (uint16(c.H) << 8) | uint16(c.L)
	res := uint32(hl) + uint32(c.sp)
	c.H = uint8((res & 0xff00) >> 8)
	c.L = uint8(res & 0xff)
	c.cc.cy = bool2uint8(((res & 0xffff0000) != 0))
	c.PC++
}

// 	LDA adr	3		A <- (adr)
func (c *CPU8080) opcode0x3a(byte1 uint8, byte2 uint8) {
	address := uint16(byte2)<<8 | uint16(byte1)
	c.A = c.Memory[address]
	c.PC += 3
}

// DCX SP	1		SP = SP-1
func (c *CPU8080) opcode0x3b(byte1 uint8, byte2 uint8) {
	c.sp--
	c.PC++
}

// INR A	1	Z, S, P, AC	A <- A+1
func (c *CPU8080) opcode0x3c(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd1(c.A)
	c.PC++
}

// DCR A	1	Z, S, P, AC	A <- A-1
func (c *CPU8080) opcode0x3d(byte1 uint8, byte2 uint8) {
	c.A = c.opSub1(c.A)
	c.PC++
}

// MVI A,D8	2		A <- byte 2
func (c *CPU8080) opcode0x3e(byte1 uint8, byte2 uint8) {
	c.A = byte1
	c.PC += 2
}

// CMC	1	CY	CY=!CY
func (c *CPU8080) opcode0x3f(byte1 uint8, byte2 uint8) {
	c.cc.cy = (c.cc.cy ^ 1) & 1
	c.PC++
}

// MOV B,B	1		B <- B
func (c *CPU8080) opcode0x40(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV B,C	1		B <- C
func (c *CPU8080) opcode0x41(byte1 uint8, byte2 uint8) {
	c.B = c.C
	c.PC++
}

// MOV B,D	1		B <- D
func (c *CPU8080) opcode0x42(byte1 uint8, byte2 uint8) {
	c.B = c.D
	c.PC++
}

// MOV B,E	1		B <- E
func (c *CPU8080) opcode0x43(byte1 uint8, byte2 uint8) {
	c.B = c.E
	c.PC++
}

// MOV B,H	1		B <- H
func (c *CPU8080) opcode0x44(byte1 uint8, byte2 uint8) {
	c.B = c.H
	c.PC++
}

// MOV B,L	1		B <- L
func (c *CPU8080) opcode0x45(byte1 uint8, byte2 uint8) {
	c.B = c.L
	c.PC++
}

// MOV B,M	1		B <- (HL)
func (c *CPU8080) opcode0x46(byte1 uint8, byte2 uint8) {
	c.B = c.readFromHL()
	c.PC++
}

// MOV B,A	1		B <- A
func (c *CPU8080) opcode0x47(byte1 uint8, byte2 uint8) {
	c.B = c.A
	c.PC++
}

// MOV C,B	1		C <- B
func (c *CPU8080) opcode0x48(byte1 uint8, byte2 uint8) {
	c.C = c.B
	c.PC++
}

// MOV C,C	1		C <- C
func (c *CPU8080) opcode0x49(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV C,D	1		C <- D
func (c *CPU8080) opcode0x4a(byte1 uint8, byte2 uint8) {
	c.C = c.D
	c.PC++
}

// MOV C,E	1		C <- E
func (c *CPU8080) opcode0x4b(byte1 uint8, byte2 uint8) {
	c.C = c.E
	c.PC++
}

// MOV C,H	1		C <- H
func (c *CPU8080) opcode0x4c(byte1 uint8, byte2 uint8) {
	c.C = c.H
	c.PC++
}

// MOV C,L	1		C <- L
func (c *CPU8080) opcode0x4d(byte1 uint8, byte2 uint8) {
	c.C = c.L
	c.PC++
}

// MOV C,M	1		C <- (HL)
func (c *CPU8080) opcode0x4e(byte1 uint8, byte2 uint8) {
	c.C = c.readFromHL()
	c.PC++
}

// MOV C,A	1		C <- A
func (c *CPU8080) opcode0x4f(byte1 uint8, byte2 uint8) {
	c.C = c.A
	c.PC++
}

// MOV D,B	1		D <- B
func (c *CPU8080) opcode0x50(byte1 uint8, byte2 uint8) {
	c.D = c.B
	c.PC++
}

// MOV D,C	1		D <- C
func (c *CPU8080) opcode0x51(byte1 uint8, byte2 uint8) {
	c.D = c.C
	c.PC++
}

// MOV D,D	1		D <- D
func (c *CPU8080) opcode0x52(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV D,E	1		D <- E
func (c *CPU8080) opcode0x53(byte1 uint8, byte2 uint8) {
	c.D = c.E
	c.PC++
}

// MOV D,H	1		D <- H
func (c *CPU8080) opcode0x54(byte1 uint8, byte2 uint8) {
	c.D = c.H
	c.PC++
}

// MOV D,L	1		D <- L
func (c *CPU8080) opcode0x55(byte1 uint8, byte2 uint8) {
	c.D = c.L
	c.PC++
}

// MOV D,M	1		D <- (HL)
func (c *CPU8080) opcode0x56(byte1 uint8, byte2 uint8) {
	c.D = c.readFromHL()
	c.PC++
}

// MOV D,A	1		D <- A
func (c *CPU8080) opcode0x57(byte1 uint8, byte2 uint8) {
	c.D = c.A
	c.PC++
}

// MOV E,B	1		E <- B
func (c *CPU8080) opcode0x58(byte1 uint8, byte2 uint8) {
	c.E = c.B
	c.PC++
}

// MOV E,C	1		E <- C
func (c *CPU8080) opcode0x59(byte1 uint8, byte2 uint8) {
	c.E = c.C
	c.PC++
}

// MOV E,D	1		E <- D
func (c *CPU8080) opcode0x5a(byte1 uint8, byte2 uint8) {
	c.E = c.D
	c.PC++
}

// MOV E,E	1		E <- E
func (c *CPU8080) opcode0x5b(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV E,H	1		E <- H
func (c *CPU8080) opcode0x5c(byte1 uint8, byte2 uint8) {
	c.E = c.H
	c.PC++
}

// MOV E,L	1		E <- L
func (c *CPU8080) opcode0x5d(byte1 uint8, byte2 uint8) {
	c.E = c.L
	c.PC++
}

// MOV E,M	1		E <- (HL)
func (c *CPU8080) opcode0x5e(byte1 uint8, byte2 uint8) {
	c.E = c.readFromHL()
	c.PC++
}

// MOV E,A	1		E <- A
func (c *CPU8080) opcode0x5f(byte1 uint8, byte2 uint8) {
	c.E = c.A
	c.PC++
}

// MOV H,B	1		H <- B
func (c *CPU8080) opcode0x60(byte1 uint8, byte2 uint8) {
	c.H = c.B
	c.PC++
}

// MOV H,C	1		H <- C
func (c *CPU8080) opcode0x61(byte1 uint8, byte2 uint8) {
	c.H = c.C
	c.PC++
}

// MOV H,D	1		H <- D
func (c *CPU8080) opcode0x62(byte1 uint8, byte2 uint8) {
	c.H = c.D
	c.PC++
}

// MOV H,E	1		H <- E
func (c *CPU8080) opcode0x63(byte1 uint8, byte2 uint8) {
	c.H = c.E
	c.PC++
}

// MOV H,H	1		H <- H
func (c *CPU8080) opcode0x64(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV H,L	1		H <- L
func (c *CPU8080) opcode0x65(byte1 uint8, byte2 uint8) {
	c.H = c.L
	c.PC++
}

// MOV H,M	1		H <- (HL)
func (c *CPU8080) opcode0x66(byte1 uint8, byte2 uint8) {
	c.H = c.readFromHL()
	c.PC++
}

// MOV H,A	1		H <- A
func (c *CPU8080) opcode0x67(byte1 uint8, byte2 uint8) {
	c.H = c.A
	c.PC++
}

// MOV L,B	1		L <- B
func (c *CPU8080) opcode0x68(byte1 uint8, byte2 uint8) {
	c.L = c.B
	c.PC++
}

// MOV L,C	1		L <- C
func (c *CPU8080) opcode0x69(byte1 uint8, byte2 uint8) {
	c.L = c.C
	c.PC++
}

// MOV L,D	1		L <- D
func (c *CPU8080) opcode0x6a(byte1 uint8, byte2 uint8) {
	c.L = c.D
	c.PC++
}

// MOV L,E	1		L <- E
func (c *CPU8080) opcode0x6b(byte1 uint8, byte2 uint8) {
	c.L = c.E
	c.PC++
}

// MOV L,H	1		L <- H
func (c *CPU8080) opcode0x6c(byte1 uint8, byte2 uint8) {
	c.L = c.H
	c.PC++
}

// MOV L,L	1		L <- L
func (c *CPU8080) opcode0x6d(byte1 uint8, byte2 uint8) {
	c.PC++
}

// MOV L,M	1		L <- (HL)
func (c *CPU8080) opcode0x6e(byte1 uint8, byte2 uint8) {
	c.L = c.readFromHL()
	c.PC++
}

// MOV L,A	1		L <- A
func (c *CPU8080) opcode0x6f(byte1 uint8, byte2 uint8) {
	c.L = c.A
	c.PC++
}

// MOV M,B	1		(HL) <- B
func (c *CPU8080) opcode0x70(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.B)
	c.PC++
}

// MOV M,C	1		(HL) <- C
func (c *CPU8080) opcode0x71(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.C)
	c.PC++
}

// MOV M,D	1		(HL) <- D
func (c *CPU8080) opcode0x72(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.D)
	c.PC++
}

// MOV M,E	1		(HL) <- E
func (c *CPU8080) opcode0x73(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.E)
	c.PC++
}

// MOV M,H	1		(HL) <- H
func (c *CPU8080) opcode0x74(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.H)
	c.PC++
}

// MOV M,L	1		(HL) <- L
func (c *CPU8080) opcode0x75(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.L)
	c.PC++
}

// MOV M,A	1		(HL) <- A
func (c *CPU8080) opcode0x77(byte1 uint8, byte2 uint8) {
	c.writeToHL(c.A)
	c.PC++
}

// MOV A,B	1		A <- B
func (c *CPU8080) opcode0x78(byte1 uint8, byte2 uint8) {
	c.A = c.B
	c.PC++
}

// MOV A,C	1		A <- C
func (c *CPU8080) opcode0x79(byte1 uint8, byte2 uint8) {
	c.A = c.C
	c.PC++
}

// MOV A,D	1		A <- D
func (c *CPU8080) opcode0x7a(byte1 uint8, byte2 uint8) {
	c.A = c.D
	c.PC++
}

// MOV A,E	1		A <- E
func (c *CPU8080) opcode0x7b(byte1 uint8, byte2 uint8) {
	c.A = c.E
	c.PC++
}

// MOV A,H	1		A <- H
func (c *CPU8080) opcode0x7c(byte1 uint8, byte2 uint8) {
	c.A = c.H
	c.PC++
}

// MOV A,L	1		A <- L
func (c *CPU8080) opcode0x7d(byte1 uint8, byte2 uint8) {
	c.A = c.L
	c.PC++
}

// MOV A,M	1		A <- (HL)
func (c *CPU8080) opcode0x7e(byte1 uint8, byte2 uint8) {
	c.A = c.readFromHL()
	c.PC++
}

// MOV A,A	1		A <- A
func (c *CPU8080) opcode0x7f(byte1 uint8, byte2 uint8) {
	c.PC++
}

// ADD B	1	Z, S, P, CY, AC	A <- A + B
func (c *CPU8080) opcode0x80(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.B, 0)
	c.PC++
}

// ADD C	1	Z, S, P, CY, AC	A <- A + C
func (c *CPU8080) opcode0x81(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.C, 0)
	c.PC++
}

// ADD D	1	Z, S, P, CY, AC	A <- A + D
func (c *CPU8080) opcode0x82(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.D, 0)
	c.PC++
}

// ADD E	1	Z, S, P, CY, AC	A <- A + E
func (c *CPU8080) opcode0x83(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.E, 0)
	c.PC++
}

// ADD H	1	Z, S, P, CY, AC	A <- A + H
func (c *CPU8080) opcode0x84(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.H, 0)
	c.PC++
}

// ADD L	1	Z, S, P, CY, AC	A <- A + L
func (c *CPU8080) opcode0x85(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.L, 0)
	c.PC++
}

// ADD M	1	Z, S, P, CY, AC	A <- A + (HL)
func (c *CPU8080) opcode0x86(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.readFromHL(), 0)
	c.PC++
}

// ADD A	1	Z, S, P, CY, AC	A <- A + A
func (c *CPU8080) opcode0x87(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.A, 0)
	c.PC++
}

// ADC B	1	Z, S, P, CY, AC	A <- A + B + CY
func (c *CPU8080) opcode0x88(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.B, c.cc.cy)
	c.PC++
}

// ADC C	1	Z, S, P, CY, AC	A <- A + C + CY
func (c *CPU8080) opcode0x89(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.C, c.cc.cy)
	c.PC++
}

// 	ADC D	1	Z, S, P, CY, AC	A <- A + D + CY
func (c *CPU8080) opcode0x8a(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.D, c.cc.cy)
	c.PC++
}

// ADC E	1	Z, S, P, CY, AC	A <- A + E + CY
func (c *CPU8080) opcode0x8b(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.E, c.cc.cy)
	c.PC++
}

// ADC H	1	Z, S, P, CY, AC	A <- A + H + CY
func (c *CPU8080) opcode0x8c(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.H, c.cc.cy)
	c.PC++
}

// ADC L	1	Z, S, P, CY, AC	A <- A + L + CY
func (c *CPU8080) opcode0x8d(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.L, c.cc.cy)
	c.PC++
}

// ADC M	1	Z, S, P, CY, AC	A <- A + (HL) + CY
func (c *CPU8080) opcode0x8e(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.readFromHL(), c.cc.cy)
	c.PC++
}

// ADC A	1	Z, S, P, CY, AC	A <- A + A + CY
func (c *CPU8080) opcode0x8f(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, c.A, c.cc.cy)
	c.PC++
}

// SUB B	1	Z, S, P, CY, AC	A <- A - B
func (c *CPU8080) opcode0x90(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.B, 0)
	c.PC++
}

// SUB C	1	Z, S, P, CY, AC	A <- A - C
func (c *CPU8080) opcode0x91(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.C, 0)
	c.PC++
}

// SUB D	1	Z, S, P, CY, AC	A <- A + D
func (c *CPU8080) opcode0x92(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.D, 0)
	c.PC++
}

// SUB E	1	Z, S, P, CY, AC	A <- A - E
func (c *CPU8080) opcode0x93(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.E, 0)
	c.PC++
}

// SUB H	1	Z, S, P, CY, AC	A <- A - H
func (c *CPU8080) opcode0x94(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.H, 0)
	c.PC++
}

// 	SUB L	1	Z, S, P, CY, AC	A <- A - L
func (c *CPU8080) opcode0x95(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.L, 0)
	c.PC++
}

// SUB M	1	Z, S, P, CY, AC	A <- A + (HL)
func (c *CPU8080) opcode0x96(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.readFromHL(), 0)
	c.PC++
}

// SUB A	1	Z, S, P, CY, AC	A <- A - A
func (c *CPU8080) opcode0x97(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.A, 0)
	c.PC++
}

// SBB B	1	Z, S, P, CY, AC	A <- A - B - CY
func (c *CPU8080) opcode0x98(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.B, c.cc.cy)
	c.PC++
}

// SBB C	1	Z, S, P, CY, AC	A <- A - C - CY
func (c *CPU8080) opcode0x99(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.C, c.cc.cy)
	c.PC++
}

// SBB D	1	Z, S, P, CY, AC	A <- A - D - CY
func (c *CPU8080) opcode0x9a(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.D, c.cc.cy)
	c.PC++
}

// SBB E	1	Z, S, P, CY, AC	A <- A - E - CY
func (c *CPU8080) opcode0x9b(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.E, c.cc.cy)
	c.PC++
}

// SBB H	1	Z, S, P, CY, AC	A <- A - H - CY
func (c *CPU8080) opcode0x9c(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.H, c.cc.cy)
	c.PC++
}

// SBB L	1	Z, S, P, CY, AC	A <- A - L - CY
func (c *CPU8080) opcode0x9d(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.L, c.cc.cy)
	c.PC++
}

// SBB M	1	Z, S, P, CY, AC	A <- A - (HL) - CY
func (c *CPU8080) opcode0x9e(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.readFromHL(), c.cc.cy)
	c.PC++
}

// SBB A	1	Z, S, P, CY, AC	A <- A - A - CY
func (c *CPU8080) opcode0x9f(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, c.A, c.cc.cy)
	c.PC++
}

// ANA B	1	Z, S, P, CY, AC	A <- A & B
func (c *CPU8080) opcode0xa0(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.B)
	c.PC++
}

// ANA C	1	Z, S, P, CY, AC	A <- A & C
func (c *CPU8080) opcode0xa1(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.C)
	c.PC++
}

// ANA D	1	Z, S, P, CY, AC	A <- A & D
func (c *CPU8080) opcode0xa2(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.D)
	c.PC++
}

// ANA E	1	Z, S, P, CY, AC	A <- A & E
func (c *CPU8080) opcode0xa3(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.E)
	c.PC++
}

// ANA H	1	Z, S, P, CY, AC	A <- A & H
func (c *CPU8080) opcode0xa4(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.H)
	c.PC++
}

// ANA L	1	Z, S, P, CY, AC	A <- A & L
func (c *CPU8080) opcode0xa5(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.L)
	c.PC++
}

// ANA M	1	Z, S, P, CY, AC	A <- A & (HL)
func (c *CPU8080) opcode0xa6(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.readFromHL())
	c.PC++
}

// ANA A	1	Z, S, P, CY, AC	A <- A & A
func (c *CPU8080) opcode0xa7(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(c.A)
	c.PC++
}

// XRA B	1	Z, S, P, CY, AC	A <- A ^ B
func (c *CPU8080) opcode0xa8(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.B
	c.logicFlagsA()
	c.PC++
}

// XRA C	1	Z, S, P, CY, AC	A <- A ^ C
func (c *CPU8080) opcode0xa9(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.C
	c.logicFlagsA()
	c.PC++
}

// XRA D	1	Z, S, P, CY, AC	A <- A ^ D
func (c *CPU8080) opcode0xaa(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.D
	c.logicFlagsA()
	c.PC++
}

// XRA E	1	Z, S, P, CY, AC	A <- A ^ E
func (c *CPU8080) opcode0xab(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.E
	c.logicFlagsA()
	c.PC++
}

// XRA H	1	Z, S, P, CY, AC	A <- A ^ H
func (c *CPU8080) opcode0xac(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.H
	c.logicFlagsA()
	c.PC++
}

// XRA L	1	Z, S, P, CY, AC	A <- A ^ L
func (c *CPU8080) opcode0xad(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.L
	c.logicFlagsA()
	c.PC++
}

// XRA M	1	Z, S, P, CY, AC	A <- A ^ (HL)
func (c *CPU8080) opcode0xae(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.readFromHL()
	c.logicFlagsA()
	c.PC++
}

// XRA A	1	Z, S, P, CY, AC	A <- A ^ A
func (c *CPU8080) opcode0xaf(byte1 uint8, byte2 uint8) {
	c.A = c.A ^ c.A
	c.logicFlagsA()
	c.PC++
}

// ORA B	1	Z, S, P, CY, AC	A <- A | B
func (c *CPU8080) opcode0xb0(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.B
	c.logicFlagsA()
	c.PC++
}

// ORA C	1	Z, S, P, CY, AC	A <- A | C
func (c *CPU8080) opcode0xb1(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.C
	c.logicFlagsA()
	c.PC++
}

// ORA D	1	Z, S, P, CY, AC	A <- A | D
func (c *CPU8080) opcode0xb2(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.D
	c.logicFlagsA()
	c.PC++
}

// ORA E	1	Z, S, P, CY, AC	A <- A | E
func (c *CPU8080) opcode0xb3(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.E
	c.logicFlagsA()
	c.PC++
}

// ORA H	1	Z, S, P, CY, AC	A <- A | H
func (c *CPU8080) opcode0xb4(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.H
	c.logicFlagsA()
	c.PC++
}

// ORA L	1	Z, S, P, CY, AC	A <- A | L
func (c *CPU8080) opcode0xb5(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.L
	c.logicFlagsA()
	c.PC++
}

// ORA M	1	Z, S, P, CY, AC	A <- A | (HL)
func (c *CPU8080) opcode0xb6(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.readFromHL()
	c.logicFlagsA()
	c.PC++
}

// ORA A	1	Z, S, P, CY, AC	A <- A | A
func (c *CPU8080) opcode0xb7(byte1 uint8, byte2 uint8) {
	c.A = c.A | c.A
	c.logicFlagsA()
	c.PC++
}

// 	CMP B	1	Z, S, P, CY, AC	A - B
func (c *CPU8080) opcode0xb8(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.B, 0)
	c.PC++
}

// CMP C	1	Z, S, P, CY, AC	A - C
func (c *CPU8080) opcode0xb9(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.C, 0)
	c.PC++
}

// CMP D	1	Z, S, P, CY, AC	A - D
func (c *CPU8080) opcode0xba(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.D, 0)
	c.PC++
}

// CMP E	1	Z, S, P, CY, AC	A - E
func (c *CPU8080) opcode0xbb(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.E, 0)
	c.PC++
}

// CMP H	1	Z, S, P, CY, AC	A - H
func (c *CPU8080) opcode0xbc(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.H, 0)
	c.PC++
}

// CMP L	1	Z, S, P, CY, AC	A - L
func (c *CPU8080) opcode0xbd(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.L, 0)
	c.PC++
}

// CMP M	1	Z, S, P, CY, AC	A - (HL)
func (c *CPU8080) opcode0xbe(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.readFromHL(), 0)
	c.PC++
}

// CMP A	1	Z, S, P, CY, AC	A - A
func (c *CPU8080) opcode0xbf(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, c.A, 0)
	c.PC++
}

// RNZ	1		if NZ, RET
func (c *CPU8080) opcode0xc0(byte1 uint8, byte2 uint8) {
	if c.cc.z == 0 {
		c.PC = uint16(c.Memory[c.sp+1])<<8 | uint16(c.Memory[c.sp])
		c.sp += 2
	} else {
		c.PC++
	}
}

// POP B	1		C <- (sp); B <- (sp+1); sp <- sp+2
func (c *CPU8080) opcode0xc1(byte1 uint8, byte2 uint8) {
	c.C = c.Memory[c.sp]
	c.B = c.Memory[c.sp+1]
	c.sp += 2
	c.PC++
}

// JNZ adr	3		if NZ, PC <- adr
func (c *CPU8080) opcode0xc2(byte1 uint8, byte2 uint8) {
	if c.cc.z == 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// JMP adr	3		PC <= adr
func (c *CPU8080) opcode0xc3(byte1 uint8, byte2 uint8) {
	c.PC = uint16(byte2)<<8 | uint16(byte1)
}

// CNZ adr	3		if NZ, CALL adr
func (c *CPU8080) opcode0xc4(byte1 uint8, byte2 uint8) {
	if c.cc.z == 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// PUSH B	1		(sp-2)<-C; (sp-1)<-B; sp <- sp - 2
func (c *CPU8080) opcode0xc5(byte1 uint8, byte2 uint8) {
	c.writeMem(c.sp-1, c.B)
	c.writeMem(c.sp-2, c.C)
	c.sp -= 2
	c.PC++
}

// ADI D8	2	Z, S, P, CY, AC	A <- A + byte
func (c *CPU8080) opcode0xc6(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, byte1, 0)
	c.PC += 2
}

// RST 0	1		CALL $0
func (c *CPU8080) opcode0xc7(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8((ret & 0xff)))
	c.sp -= 2
	c.PC = 0x0
}

// RZ	1		if Z, RET
func (c *CPU8080) opcode0xc8(byte1 uint8, byte2 uint8) {
	if c.cc.z == 1 {
		c.PC = uint16(c.Memory[c.sp+1])<<8 | uint16(c.Memory[c.sp])
		c.sp += 2
	} else {
		c.PC++
	}
}

// RET	1		Pc.Lo <- (sp); Pc.Hi<-(sp+1); SP <- SP+2
func (c *CPU8080) opcode0xc9(byte1 uint8, byte2 uint8) {
	c.PC = uint16(c.Memory[c.sp+1])<<8 | uint16(c.Memory[c.sp])
	c.sp += 2
}

// JZ adr	3		if Z, PC <- adr
func (c *CPU8080) opcode0xca(byte1 uint8, byte2 uint8) {
	if c.cc.z == 1 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// CZ adr	3		if Z, CALL adr
func (c *CPU8080) opcode0xcc(byte1 uint8, byte2 uint8) {
	if c.cc.z == 1 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8((ret & 0xff)))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// CALL adr	3		(SP-1)<-Pc.Hi;(SP-2)<-Pc.Lo;SP<-SP+2;PC=adr
func (c *CPU8080) opcode0xcd(byte1 uint8, byte2 uint8) {
	ret := c.PC + 3
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8((ret & 0xff)))
	c.sp -= 2
	c.PC = uint16(byte2)<<8 | uint16(byte1)
}

// ACI D8	2	Z, S, P, CY, AC	A <- A + data + CY
func (c *CPU8080) opcode0xce(byte1 uint8, byte2 uint8) {
	c.A = c.opAdd(c.A, byte1, c.cc.cy)
	c.PC += 2
}

// RST 1	1		CALL $8
func (c *CPU8080) opcode0xcf(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8((ret & 0xff)))
	c.sp -= 2
	c.PC = 0x8
}

// RNC	1		if NCY, RET
func (c *CPU8080) opcode0xd0(byte1 uint8, byte2 uint8) {
	if c.cc.cy == 0 {
		c.PC = uint16(c.Memory[c.sp+1])<<8 | uint16(c.Memory[c.sp])
		c.sp += 2
	} else {
		c.PC++
	}
}

// POP D	1		E <- (sp); D <- (sp+1); sp <- sp+2
func (c *CPU8080) opcode0xd1(byte1 uint8, byte2 uint8) {
	c.E = c.Memory[c.sp]
	c.D = c.Memory[c.sp+1]
	c.sp += 2
	c.PC++
}

// JNC adr	3		if NCY, PC<-adr
func (c *CPU8080) opcode0xd2(byte1 uint8, byte2 uint8) {
	if c.cc.cy == 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// OUT D8	2		special
func (c *CPU8080) opcode0xd3(byte1 uint8, byte2 uint8) {
	c.OutputHandler(byte1, c.A)
	c.PC += 2
}

// CNC adr	3		if NCY, CALL adr
func (c *CPU8080) opcode0xd4(byte1 uint8, byte2 uint8) {
	if c.cc.cy == 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// PUSH D	1		(sp-2)<-E; (sp-1)<-D; sp <- sp - 2
func (c *CPU8080) opcode0xd5(byte1 uint8, byte2 uint8) {
	c.writeMem(c.sp-1, c.D)
	c.writeMem(c.sp-2, c.E)
	c.sp -= 2
	c.PC++
}

// SUI D8	2	Z, S, P, CY, AC	A <- A - data
func (c *CPU8080) opcode0xd6(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, byte1, 0)
	c.PC += 2
}

// RST 2	1		CALL $10
func (c *CPU8080) opcode0xd7(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8(ret&0xff))
	c.sp -= 2
	c.PC = 0x10
}

// RC	1		if CY, RET
func (c *CPU8080) opcode0xd8(byte1 uint8, byte2 uint8) {
	if c.cc.cy != 0 {
		c.PC = uint16(c.Memory[c.sp]) | uint16(c.Memory[c.sp+1])<<8
		c.sp += 2
	} else {
		c.PC++
	}
}

// JC adr	3		if CY, PC<-adr
func (c *CPU8080) opcode0xda(byte1 uint8, byte2 uint8) {
	if c.cc.cy != 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// IN D8	2		special
func (c *CPU8080) opcode0xdb(byte1 uint8, byte2 uint8) {
	c.A = c.InputHandler(byte1)
	c.PC += 2
}

// CC adr	3		if CY, CALL adr
func (c *CPU8080) opcode0xdc(byte1 uint8, byte2 uint8) {
	if c.cc.cy != 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// NOP
func (c *CPU8080) opcode0xdd(byte1 uint8, byte2 uint8) {
	c.PC++
}

// SBI D8	2	Z, S, P, CY, AC	A <- A - data - CY
func (c *CPU8080) opcode0xde(byte1 uint8, byte2 uint8) {
	c.A = c.opSub(c.A, byte1, c.cc.cy)
	c.PC += 2
}

// RST 3	1		CALL $18
func (c *CPU8080) opcode0xdf(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8(ret&0xff))
	c.sp -= 2
	c.PC = 0x18
}

// RPO	1		if PO, RET
func (c *CPU8080) opcode0xe0(byte1 uint8, byte2 uint8) {
	if c.cc.p == 0 {
		c.PC = uint16(c.Memory[c.sp]) | uint16(c.Memory[c.sp+1])<<8
		c.sp += 2
	} else {
		c.PC++
	}
}

// POP H	1		L <- (sp); H <- (sp+1); sp <- sp+2
func (c *CPU8080) opcode0xe1(byte1 uint8, byte2 uint8) {
	c.L = c.Memory[c.sp]
	c.H = c.Memory[c.sp+1]
	c.sp += 2
	c.PC++
}

// JPO adr	3		if PO, PC <- adr
func (c *CPU8080) opcode0xe2(byte1 uint8, byte2 uint8) {
	if c.cc.p == 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// XTHL	1		L <-> (SP); H <-> (SP+1)
func (c *CPU8080) opcode0xe3(byte1 uint8, byte2 uint8) {
	l := c.L
	h := c.H
	c.L = c.Memory[c.sp]
	c.H = c.Memory[c.sp+1]
	c.writeMem(c.sp, l)
	c.writeMem(c.sp+1, h)
	c.PC++
}

// CPO adr	3		if PO, CALL adr
func (c *CPU8080) opcode0xe4(byte1 uint8, byte2 uint8) {
	if c.cc.p == 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// PUSH H	1		(sp-2)<-L; (sp-1)<-H; sp <- sp - 2
func (c *CPU8080) opcode0xe5(byte1 uint8, byte2 uint8) {
	c.writeMem(c.sp-1, c.H)
	c.writeMem(c.sp-2, c.L)
	c.sp -= 2
	c.PC++
}

// ANI D8	2	Z, S, P, CY, AC	A <- A & data
func (c *CPU8080) opcode0xe6(byte1 uint8, byte2 uint8) {
	c.A = c.opAna(byte1)
	c.PC += 2
}

// RST 4	1		CALL $20
func (c *CPU8080) opcode0xe7(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8(ret&0xff))
	c.sp -= 2
	c.PC = 0x20
}

// RPE	1		if PE, RET
func (c *CPU8080) opcode0xe8(byte1 uint8, byte2 uint8) {
	if c.cc.p != 0 {
		c.PC = uint16(c.Memory[c.sp]) | uint16(c.Memory[c.sp+1])<<8
		c.sp += 2
	} else {
		c.PC++
	}
}

// PCHL	1		Pc.Hi <- H; Pc.Lo <- L
func (c *CPU8080) opcode0xe9(byte1 uint8, byte2 uint8) {
	c.PC = uint16(c.H)<<8 | uint16(c.L)
}

// JPE adr	3		if PE, PC <- adr
func (c *CPU8080) opcode0xea(byte1 uint8, byte2 uint8) {
	if c.cc.p != 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// XCHG	1		H <-> D; L <-> E
func (c *CPU8080) opcode0xeb(byte1 uint8, byte2 uint8) {
	t := c.D
	c.D = c.H
	c.H = t
	t = c.L
	c.L = c.E
	c.E = t
	c.PC++
}

// CPE adr	3		if PE, CALL adr
func (c *CPU8080) opcode0xec(byte1 uint8, byte2 uint8) {
	if c.cc.p != 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// NOP
func (c *CPU8080) opcode0xed(byte1 uint8, byte2 uint8) {
	c.PC++
}

// XRI D8	2	Z, S, P, CY, AC	A <- A ^ data
func (c *CPU8080) opcode0xee(byte1 uint8, byte2 uint8) {
	x := c.A ^ byte1
	c.flagsZSP(x & 0xff)
	c.cc.cy = 0
	c.cc.ac = 0
	c.A = x
	c.PC += 2
}

// RST 5	1		CALL $28
func (c *CPU8080) opcode0xef(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8(ret&0xff))
	c.sp -= 2
	c.PC = 0x28
}

// RP	1		if P, RET
func (c *CPU8080) opcode0xf0(byte1 uint8, byte2 uint8) {
	if c.cc.s == 0 {
		c.PC = uint16(c.Memory[c.sp]) | uint16(c.Memory[c.sp+1])<<8
		c.sp += 2
	} else {
		c.PC++
	}
}

// POP PSW	1		flags <- (sp); A <- (sp+1); sp <- sp+2
func (c *CPU8080) opcode0xf1(byte1 uint8, byte2 uint8) {
	c.A = c.Memory[c.sp+1]
	psw := c.Memory[c.sp]
	c.sp += 2
	c.cc.cy = bool2uint8((psw & 0x1) != 0)
	c.cc.p = bool2uint8((psw & 0x4) != 0)
	c.cc.ac = bool2uint8((psw & 0x10) != 0)
	c.cc.z = bool2uint8((psw & 0x40) != 0)
	c.cc.s = bool2uint8((psw & 0x80) != 0)
	c.PC++
}

// JP adr	3		if P=1 PC <- adr
func (c *CPU8080) opcode0xf2(byte1 uint8, byte2 uint8) {
	if c.cc.s == 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// DI	1		special
func (c *CPU8080) opcode0xf3(byte1 uint8, byte2 uint8) {
	c.IntEnable = 0
	c.PC++
}

// CP adr	3		if P, PC <- adr
func (c *CPU8080) opcode0xf4(byte1 uint8, byte2 uint8) {
	if c.cc.s == 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// PUSH PSW	1		(sp-2)<-flags; (sp-1)<-A; sp <- sp - 2
func (c *CPU8080) opcode0xf5(byte1 uint8, byte2 uint8) {
	c.writeMem(c.sp-1, c.A)
	psw := (c.cc.cy & 0x01) | //0th position
		(0x02) | //1st
		((c.cc.p & 0x01) << 2) | //2nd
		((c.cc.ac & 0x01) << 4) | //4th
		((c.cc.z & 0x01) << 6) | //6th
		((c.cc.s & 0x01) << 7)
	c.writeMem(c.sp-2, psw)
	c.sp -= 2
	c.PC++
}

// ORI D8	2	Z, S, P, CY, AC	A <- A | data
func (c *CPU8080) opcode0xf6(byte1 uint8, byte2 uint8) {
	x := c.A | byte1
	c.flagsZSP(x)
	c.cc.cy = 0
	c.cc.ac = 0
	c.A = x
	c.PC += 2
}

// RST 6	1		CALL $30
func (c *CPU8080) opcode0xf7(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8(ret&0xff))
	c.sp -= 2
	c.PC = 0x30
}

// RM	1		if M, RET
func (c *CPU8080) opcode0xf8(byte1 uint8, byte2 uint8) {
	if c.cc.s != 0 {
		c.PC = uint16(c.Memory[c.sp]) | uint16(c.Memory[c.sp+1])<<8
		c.sp += 2
	} else {
		c.PC++
	}
}

// SPHL	1		SP=HL
func (c *CPU8080) opcode0xf9(byte1 uint8, byte2 uint8) {
	c.sp = (uint16(c.H) << 8) | uint16(c.L)
	c.PC++
}

// JM adr	3		if M, PC <- adr
func (c *CPU8080) opcode0xfa(byte1 uint8, byte2 uint8) {
	if c.cc.s != 0 {
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// EI	1		special
func (c *CPU8080) opcode0xfb(byte1 uint8, byte2 uint8) {
	c.IntEnable = 1
	c.PC++
}

// CM adr	3		if M, CALL adr
func (c *CPU8080) opcode0xfc(byte1 uint8, byte2 uint8) {
	if c.cc.s != 0 {
		ret := c.PC + 3
		c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
		c.writeMem(c.sp-2, uint8(ret&0xff))
		c.sp -= 2
		c.PC = uint16(byte2)<<8 | uint16(byte1)
	} else {
		c.PC += 3
	}
}

// NOP
func (c *CPU8080) opcode0xfd(byte1 uint8, byte2 uint8) {
	c.PC++
}

// CPI D8	2	Z, S, P, CY, AC	A - data
func (c *CPU8080) opcode0xfe(byte1 uint8, byte2 uint8) {
	c.opSub(c.A, byte1, 0)
	c.PC += 2
}

// RST 7	1		CALL $38
func (c *CPU8080) opcode0xff(byte1 uint8, byte2 uint8) {
	ret := c.PC + 1
	c.writeMem(c.sp-1, uint8((ret>>8)&0xff))
	c.writeMem(c.sp-2, uint8((ret & 0xff)))
	c.sp -= 2
	c.PC = 0x38
}
