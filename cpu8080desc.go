package cpu8080

type conditionCodes struct {
	z  uint8
	s  uint8
	p  uint8
	cy uint8
	ac uint8
}

type opcodeHandler func(byte1 uint8, byte2 uint8)

// CPU8080 is the CPU structure with its methods and registers
type CPU8080 struct {
	Memory        [0xFFFF + 1]uint8
	A             uint8
	B             uint8
	C             uint8
	D             uint8
	E             uint8
	H             uint8
	L             uint8
	PC            uint16
	sp            uint16
	cc            conditionCodes
	IntEnable     int16
	opcodes       map[uint8]opcodeHandler
	InputHandler  func(uint8) uint8
	OutputHandler func(uint8, uint8)
	descOpcodes   map[uint8]string
	RAMUpperLimit uint16
	RAMLowerLimit uint16
}

func (c *CPU8080) registerOpcodeHandlers() {
	c.opcodes = map[uint8]opcodeHandler{
		0x00: c.opcode0x00,
		0x01: c.opcode0x01,
		0x02: c.opcode0x02,
		0x03: c.opcode0x03,
		0x04: c.opcode0x04,
		0x05: c.opcode0x05,
		0x06: c.opcode0x06,
		0x07: c.opcode0x07,
		//0x08: c.opcode0x08,
		0x09: c.opcode0x09,
		0x0a: c.opcode0x0a,
		0x0b: c.opcode0x0b,
		0x0c: c.opcode0x0c,
		0x0d: c.opcode0x0d,
		0x0e: c.opcode0x0e,
		0x0f: c.opcode0x0f,
		//0x10: c.opcode0x10,
		0x11: c.opcode0x11,
		0x12: c.opcode0x12,
		0x13: c.opcode0x13,
		0x14: c.opcode0x14,
		0x15: c.opcode0x15,
		0x16: c.opcode0x16,
		0x17: c.opcode0x17,
		//0x18: c.opcode0x18,
		0x19: c.opcode0x19,
		0x1a: c.opcode0x1a,
		0x1b: c.opcode0x1b,
		0x1c: c.opcode0x1c,
		0x1d: c.opcode0x1d,
		0x1e: c.opcode0x1e,
		0x1f: c.opcode0x1f,
		0x20: c.opcode0x20,
		0x21: c.opcode0x21,
		0x22: c.opcode0x22,
		0x23: c.opcode0x23,
		0x24: c.opcode0x24,
		0x25: c.opcode0x25,
		0x26: c.opcode0x26,
		0x27: c.opcode0x27,
		//0x28: c.opcode0x28,
		0x29: c.opcode0x29,
		0x2a: c.opcode0x2a,
		0x2b: c.opcode0x2b,
		0x2c: c.opcode0x2c,
		0x2d: c.opcode0x2d,
		0x2e: c.opcode0x2e,
		0x2f: c.opcode0x2f,
		//0x30: c.opcode0x30,
		0x31: c.opcode0x31,
		0x32: c.opcode0x32,
		0x33: c.opcode0x33,
		0x34: c.opcode0x34,
		0x35: c.opcode0x35,
		0x36: c.opcode0x36,
		0x37: c.opcode0x37,
		//0x38: c.opcode0x38,
		0x39: c.opcode0x39,
		0x3a: c.opcode0x3a,
		0x3b: c.opcode0x3b,
		0x3c: c.opcode0x3c,
		0x3d: c.opcode0x3d,
		0x3e: c.opcode0x3e,
		0x3f: c.opcode0x3f,
		0x40: c.opcode0x40,
		0x41: c.opcode0x41,
		0x42: c.opcode0x42,
		0x43: c.opcode0x43,
		0x44: c.opcode0x44,
		0x45: c.opcode0x45,
		0x46: c.opcode0x46,
		0x47: c.opcode0x47,
		0x48: c.opcode0x48,
		0x49: c.opcode0x49,
		0x4a: c.opcode0x4a,
		0x4b: c.opcode0x4b,
		0x4c: c.opcode0x4c,
		0x4d: c.opcode0x4d,
		0x4e: c.opcode0x4e,
		0x4f: c.opcode0x4f,
		0x50: c.opcode0x50,
		0x51: c.opcode0x51,
		0x52: c.opcode0x52,
		0x53: c.opcode0x53,
		0x54: c.opcode0x54,
		0x55: c.opcode0x55,
		0x56: c.opcode0x56,
		0x57: c.opcode0x57,
		0x58: c.opcode0x58,
		0x59: c.opcode0x59,
		0x5a: c.opcode0x5a,
		0x5b: c.opcode0x5b,
		0x5c: c.opcode0x5c,
		0x5d: c.opcode0x5d,
		0x5e: c.opcode0x5e,
		0x5f: c.opcode0x5f,
		0x60: c.opcode0x60,
		0x61: c.opcode0x61,
		0x62: c.opcode0x62,
		0x63: c.opcode0x63,
		0x64: c.opcode0x64,
		0x65: c.opcode0x65,
		0x66: c.opcode0x66,
		0x67: c.opcode0x67,
		0x68: c.opcode0x68,
		0x69: c.opcode0x69,
		0x6a: c.opcode0x6a,
		0x6b: c.opcode0x6b,
		0x6c: c.opcode0x6c,
		0x6d: c.opcode0x6d,
		0x6e: c.opcode0x6e,
		0x6f: c.opcode0x6f,
		0x70: c.opcode0x70,
		0x71: c.opcode0x71,
		0x72: c.opcode0x72,
		0x73: c.opcode0x73,
		0x74: c.opcode0x74,
		0x75: c.opcode0x75,
		//0x76: c.opcode0x76,
		0x77: c.opcode0x77,
		0x78: c.opcode0x78,
		0x79: c.opcode0x79,
		0x7a: c.opcode0x7a,
		0x7b: c.opcode0x7b,
		0x7c: c.opcode0x7c,
		0x7d: c.opcode0x7d,
		0x7e: c.opcode0x7e,
		0x7f: c.opcode0x7f,
		0x80: c.opcode0x80,
		0x81: c.opcode0x81,
		0x82: c.opcode0x82,
		0x83: c.opcode0x83,
		0x84: c.opcode0x84,
		0x85: c.opcode0x85,
		0x86: c.opcode0x86,
		0x87: c.opcode0x87,
		0x88: c.opcode0x88,
		0x89: c.opcode0x89,
		0x8a: c.opcode0x8a,
		0x8b: c.opcode0x8b,
		0x8c: c.opcode0x8c,
		0x8d: c.opcode0x8d,
		0x8e: c.opcode0x8e,
		0x8f: c.opcode0x8f,
		0x90: c.opcode0x90,
		0x91: c.opcode0x91,
		0x92: c.opcode0x92,
		0x93: c.opcode0x93,
		0x94: c.opcode0x94,
		0x95: c.opcode0x95,
		0x96: c.opcode0x96,
		0x97: c.opcode0x97,
		0x98: c.opcode0x98,
		0x99: c.opcode0x99,
		0x9a: c.opcode0x9a,
		0x9b: c.opcode0x9b,
		0x9c: c.opcode0x9c,
		0x9d: c.opcode0x9d,
		0x9e: c.opcode0x9e,
		0x9f: c.opcode0x9f,
		0xa0: c.opcode0xa0,
		0xa1: c.opcode0xa1,
		0xa2: c.opcode0xa2,
		0xa3: c.opcode0xa3,
		0xa4: c.opcode0xa4,
		0xa5: c.opcode0xa5,
		0xa6: c.opcode0xa6,
		0xa7: c.opcode0xa7,
		0xa8: c.opcode0xa8,
		0xa9: c.opcode0xa9,
		0xaa: c.opcode0xaa,
		0xab: c.opcode0xab,
		0xac: c.opcode0xac,
		0xad: c.opcode0xad,
		0xae: c.opcode0xae,
		0xaf: c.opcode0xaf,
		0xb0: c.opcode0xb0,
		0xb1: c.opcode0xb1,
		0xb2: c.opcode0xb2,
		0xb3: c.opcode0xb3,
		0xb4: c.opcode0xb4,
		0xb5: c.opcode0xb5,
		0xb6: c.opcode0xb6,
		0xb7: c.opcode0xb7,
		0xb8: c.opcode0xb8,
		0xb9: c.opcode0xb9,
		0xba: c.opcode0xba,
		0xbb: c.opcode0xbb,
		0xbc: c.opcode0xbc,
		0xbd: c.opcode0xbd,
		0xbe: c.opcode0xbe,
		0xbf: c.opcode0xbf,
		0xc0: c.opcode0xc0,
		0xc1: c.opcode0xc1,
		0xc2: c.opcode0xc2,
		0xc3: c.opcode0xc3,
		0xc4: c.opcode0xc4,
		0xc5: c.opcode0xc5,
		0xc6: c.opcode0xc6,
		0xc7: c.opcode0xc7,
		0xc8: c.opcode0xc8,
		0xc9: c.opcode0xc9,
		0xca: c.opcode0xca,
		//0xcb: c.opcode0xcb,
		0xcc: c.opcode0xcc,
		0xcd: c.opcode0xcd,
		0xce: c.opcode0xce,
		0xcf: c.opcode0xcf,
		0xd0: c.opcode0xd0,
		0xd1: c.opcode0xd1,
		0xd2: c.opcode0xd2,
		0xd3: c.opcode0xd3,
		0xd4: c.opcode0xd4,
		0xd5: c.opcode0xd5,
		0xd6: c.opcode0xd6,
		0xd7: c.opcode0xd7,
		0xd8: c.opcode0xd8,
		//0xd9: c.opcode0xd9,
		0xda: c.opcode0xda,
		0xdb: c.opcode0xdb,
		0xdc: c.opcode0xdc,
		0xdd: c.opcode0xdd,
		0xde: c.opcode0xde,
		0xdf: c.opcode0xdf,
		0xe0: c.opcode0xe0,
		0xe1: c.opcode0xe1,
		0xe2: c.opcode0xe2,
		0xe3: c.opcode0xe3,
		0xe4: c.opcode0xe4,
		0xe5: c.opcode0xe5,
		0xe6: c.opcode0xe6,
		0xe7: c.opcode0xe7,
		0xe8: c.opcode0xe8,
		0xe9: c.opcode0xe9,
		0xea: c.opcode0xea,
		0xeb: c.opcode0xeb,
		0xec: c.opcode0xec,
		0xed: c.opcode0xed,
		0xee: c.opcode0xee,
		0xef: c.opcode0xef,
		0xf0: c.opcode0xf0,
		0xf1: c.opcode0xf1,
		0xf2: c.opcode0xf2,
		0xf3: c.opcode0xf3,
		0xf4: c.opcode0xf4,
		0xf5: c.opcode0xf5,
		0xf6: c.opcode0xf6,
		0xf7: c.opcode0xf7,
		0xf8: c.opcode0xf8,
		0xf9: c.opcode0xf9,
		0xfa: c.opcode0xfa,
		0xfb: c.opcode0xfb,
		0xfc: c.opcode0xfc,
		0xfd: c.opcode0xfd,
		0xfe: c.opcode0xfe,
		0xff: c.opcode0xff,
	}
}

func (c *CPU8080) registerOpcodeDescription() {
	c.descOpcodes = map[uint8]string{
		0x00: "NOP",
		0x01: "LXI B,D16",
		0x02: "STAX B",
		0x03: "INX B",
		0x04: "INR B",
		0x05: "DCR B",
		0x06: "MVI B, D8",
		0x07: "RLC",
		0x08: "-",
		0x09: "DAD B",
		0x0a: "LDAX B",
		0x0b: "DCX B",
		0x0c: "INR C",
		0x0d: "DCR C",
		0x0e: "MVI C,D8",
		0x0f: "RRC",
		0x10: "-",
		0x11: "LXI D,D16",
		0x12: "STAX D",
		0x13: "INX D",
		0x14: "INR D",
		0x15: "DCR D",
		0x16: "MVI D, D8",
		0x17: "RAL",
		0x18: "-",
		0x19: "DAD D",
		0x1a: "LDAX D",
		0x1b: "DCX D",
		0x1c: "INR E",
		0x1d: "DCR E",
		0x1e: "MVI E,D8",
		0x1f: "RAR",
		0x20: "RIM",
		0x21: "LXI H,D16",
		0x22: "SHLD adr",
		0x23: "INX H",
		0x24: "INR H",
		0x25: "DCR H",
		0x26: "MVI H,D8",
		0x27: "DAA",
		0x28: "-",
		0x29: "DAD H",
		0x2a: "LHLD adr",
		0x2b: "DCX H",
		0x2c: "INR L",
		0x2d: "DCR L",
		0x2e: "MVI L, D8",
		0x2f: "CMA",
		0x30: "SIM",
		0x31: "LXI SP, D16",
		0x32: "STA adr",
		0x33: "INX SP",
		0x34: "INR M",
		0x35: "DCR M",
		0x36: "MVI M,D8",
		0x37: "STC",
		0x38: "-",
		0x39: "DAD SP",
		0x3a: "LDA adr",
		0x3b: "DCX SP",
		0x3c: "INR A",
		0x3d: "DCR A",
		0x3e: "MVI A,D8",
		0x3f: "CMC",
		0x40: "MOV B,B",
		0x41: "MOV B,C",
		0x42: "MOV B,D",
		0x43: "MOV B,E",
		0x44: "MOV B,H",
		0x45: "MOV B,L",
		0x46: "MOV B,M",
		0x47: "MOV B,A",
		0x48: "MOV C,B",
		0x49: "MOV C,C",
		0x4a: "MOV C,D",
		0x4b: "MOV C,E",
		0x4c: "MOV C,H",
		0x4d: "MOV C,L",
		0x4e: "MOV C,M",
		0x4f: "MOV C,A",
		0x50: "MOV D,B",
		0x51: "MOV D,C",
		0x52: "MOV D,D",
		0x53: "MOV D,E",
		0x54: "MOV D,H",
		0x55: "MOV D,L",
		0x56: "MOV D,M",
		0x57: "MOV D,A",
		0x58: "MOV E,B",
		0x59: "MOV E,C",
		0x5a: "MOV E,D",
		0x5b: "MOV E,E",
		0x5c: "MOV E,H",
		0x5d: "MOV E,L",
		0x5e: "MOV E,M",
		0x5f: "MOV E,A",
		0x60: "MOV H,B",
		0x61: "MOV H,C",
		0x62: "MOV H,D",
		0x63: "MOV H,E",
		0x64: "MOV H,H",
		0x65: "MOV H,L",
		0x66: "MOV H,M",
		0x67: "MOV H,A",
		0x68: "MOV L,B",
		0x69: "MOV L,C",
		0x6a: "MOV L,D",
		0x6b: "MOV L,E",
		0x6c: "MOV L,H",
		0x6d: "MOV L,L",
		0x6e: "MOV L,M",
		0x6f: "MOV L,A",
		0x70: "MOV M,B",
		0x71: "MOV M,C",
		0x72: "MOV M,D",
		0x73: "MOV M,E",
		0x74: "MOV M,H",
		0x75: "MOV M,L",
		0x76: "HLT",
		0x77: "MOV M,A",
		0x78: "MOV A,B",
		0x79: "MOV A,C",
		0x7a: "MOV A,D",
		0x7b: "MOV A,E",
		0x7c: "MOV A,H",
		0x7d: "MOV A,L",
		0x7e: "MOV A,M",
		0x7f: "MOV A,A",
		0x80: "ADD B",
		0x81: "ADD C",
		0x82: "ADD D",
		0x83: "ADD E",
		0x84: "ADD H",
		0x85: "ADD L",
		0x86: "ADD M",
		0x87: "ADD A",
		0x88: "ADC B",
		0x89: "ADC C",
		0x8a: "ADC D",
		0x8b: "ADC E",
		0x8c: "ADC H",
		0x8d: "ADC L",
		0x8e: "ADC M",
		0x8f: "ADC A",
		0x90: "SUB B",
		0x91: "SUB C",
		0x92: "SUB D",
		0x93: "SUB E",
		0x94: "SUB H",
		0x95: "SUB L",
		0x96: "SUB M",
		0x97: "SUB A",
		0x98: "SBB B",
		0x99: "SBB C",
		0x9a: "SBB D",
		0x9b: "SBB E",
		0x9c: "SBB H",
		0x9d: "SBB L",
		0x9e: "SBB M",
		0x9f: "SBB A",
		0xa0: "ANA B",
		0xa1: "ANA C",
		0xa2: "ANA D",
		0xa3: "ANA E",
		0xa4: "ANA H",
		0xa5: "ANA L",
		0xa6: "ANA M",
		0xa7: "ANA A",
		0xa8: "XRA B",
		0xa9: "XRA C",
		0xaa: "XRA D",
		0xab: "XRA E",
		0xac: "XRA H",
		0xad: "XRA L",
		0xae: "XRA M",
		0xaf: "XRA A",
		0xb0: "ORA B",
		0xb1: "ORA C",
		0xb2: "ORA D",
		0xb3: "ORA E",
		0xb4: "ORA H",
		0xb5: "ORA L",
		0xb6: "ORA M",
		0xb7: "ORA A",
		0xb8: "CMP B",
		0xb9: "CMP C",
		0xba: "CMP D",
		0xbb: "CMP E",
		0xbc: "CMP H",
		0xbd: "CMP L",
		0xbe: "CMP M",
		0xbf: "CMP A",
		0xc0: "RNZ",
		0xc1: "POP B",
		0xc2: "JNZ adr",
		0xc3: "JMP adr",
		0xc4: "CNZ adr",
		0xc5: "PUSH B",
		0xc6: "ADI D8",
		0xc7: "RST 0",
		0xc8: "RZ",
		0xc9: "RET",
		0xca: "JZ adr",
		0xcb: "-",
		0xcc: "CZ adr",
		0xcd: "CALL adr",
		0xce: "ACI D8",
		0xcf: "RST 1",
		0xd0: "RNC",
		0xd1: "POP D",
		0xd2: "JNC adr",
		0xd3: "OUT D8",
		0xd4: "CNC adr",
		0xd5: "PUSH D",
		0xd6: "SUI D8",
		0xd7: "RST 2",
		0xd8: "RC",
		0xd9: "-",
		0xda: "JC adr",
		0xdb: "IN D8",
		0xdc: "CC adr",
		0xdd: "-",
		0xde: "SBI D8",
		0xdf: "RST 3",
		0xe0: "RPO",
		0xe1: "POP H",
		0xe2: "JPO adr",
		0xe3: "XTHL",
		0xe4: "CPO adr",
		0xe5: "PUSH H",
		0xe6: "ANI D8",
		0xe7: "RST 4",
		0xe8: "RPE",
		0xe9: "PCHL",
		0xea: "JPE adr",
		0xeb: "XCHG",
		0xec: "CPE adr",
		0xed: "-",
		0xee: "XRI D8",
		0xef: "RST 5",
		0xf0: "RP",
		0xf1: "POP PSW",
		0xf2: "JP adr",
		0xf3: "DI",
		0xf4: "CP adr",
		0xf5: "PUSH PSW",
		0xf6: "ORI D8",
		0xf7: "RST 6",
		0xf8: "RM",
		0xf9: "SPHL",
		0xfa: "JM adr",
		0xfb: "EI",
		0xfc: "CM adr",
		0xfd: "-",
		0xfe: "CPI D8",
		0xff: "RST 7",
	}
}