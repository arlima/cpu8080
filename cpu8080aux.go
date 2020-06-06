package cpu8080

import (
	"bufio"
	"fmt"
	"os"
)

// ExecuteOpCode calls the opcode function related to the opcode in the PC memory position
func (c *CPU8080) ExecuteOpCode(log uint) error {
	var cmd string
	if fn, ok := c.opcodes[c.Memory[c.PC]]; ok {
		if log > 0 {
			cmd = fmt.Sprintf("PC:%04X OPCODE:%02X BYTE1:%02X BYTE2:%02X  CMD:%s\n", c.PC, c.Memory[c.PC], c.Memory[c.PC+1], c.Memory[c.PC+2], c.descOpcodes[c.Memory[c.PC]])
		}
		fn(c.Memory[c.PC+1], c.Memory[c.PC+2])
		if log > 0 {
			fmt.Printf("--------------------------------------------\n")
			fmt.Printf(cmd)
			fmt.Printf("After cmd : A:%02X B:%02X C:%02X D:%02X E:%02X H:%02X L:%02X\n", c.A, c.B, c.C, c.D, c.E, c.H, c.L)
			fmt.Printf("After cmd : S:%01X Z:%01X AC:%01X P:%01X CY:%01X IntEnable:%01X\n", c.cc.s, c.cc.z, c.cc.ac, c.cc.p, c.cc.cy, c.IntEnable)
			fmt.Printf("After cmd : PC:%04X SP:%04X \n", c.PC, c.sp)

			fmt.Printf("--------------------------------------------\n")
		}
	} else {
		return fmt.Errorf("opcode doesn't exist")
	}
	return nil
}

// ReadFileMemory loads a file in a given memory position
func (c *CPU8080) ReadFileMemory(filename string, position int64) error {
	file, err := os.Open(filename)
	if err != nil {
		return err
	}
	defer file.Close()

	stats, statsErr := file.Stat()
	if statsErr != nil {
		return statsErr
	}

	var size int64 = stats.Size()

	if size+position > 0xFFFF {
		return fmt.Errorf("the ROM file is too big")
	}

	bytes := make([]uint8, size)

	bufr := bufio.NewReader(file)
	_, err = bufr.Read(bytes)

	for m := int64(0); m < size; m++ {
		c.Memory[m+position] = bytes[m]
	}

	return nil
}

// Initialize initializes the CPU registers and flags, loads opcodes ands descriptions
func (c *CPU8080) Initialize() {
	c.PC = 0
	c.sp = 0
	c.cc.ac = 0
	c.cc.cy = 0
	c.cc.p = 0
	c.cc.s = 0
	c.cc.z = 0

	c.registerOpcodeHandlers()
	c.registerOpcodeDescription()
}

func (c *CPU8080) logicFlagsA() {
	c.cc.cy = 0
	c.cc.ac = 0
	c.cc.z = bool2uint8(c.A == 0)
	c.cc.s = bool2uint8(0x80 == (c.A & 0x80))
	c.cc.p = parity(c.A, 8)
}

func (c *CPU8080) flagsZSP(value uint8) {
	c.cc.z = bool2uint8(value == 0)
	c.cc.s = bool2uint8(0x80 == (value & 0x80))
	c.cc.p = parity(value, 8)
}

func (c *CPU8080) readFromHL() uint8 {
	offset := (uint16(c.H) << 8) | uint16(c.L)
	return c.Memory[offset]
}

func (c *CPU8080) writeToHL(value uint8) {
	offset := (uint16(c.H) << 8) | uint16(c.L)
	c.writeMem(offset, value)
}

// GenerateInterrupt generates an interrup in the cpu processing
func (c *CPU8080) GenerateInterrupt(interruptnum uint16) {
	//perform "PUSH PC"
	c.writeMem(c.sp-1, uint8((c.PC&0xFF00)>>8))
	c.writeMem(c.sp-2, uint8(c.PC&0xff))
	c.sp -= 2

	//Set the PC to the low memory vector
	c.PC = 8 * interruptnum

	//"DI"
	c.IntEnable = 0
}

func bool2uint8(v bool) uint8 {
	if v {
		return 1
	}
	return 0
}

func parity(x uint8, size int) uint8 {
	p := 0
	x = (x & ((1 << size) - 1))
	for i := 0; i < size; i++ {
		if (x & 0x1) != 0 {
			p++
		}
		x = x >> 1
	}
	return bool2uint8(0 == (p & 0x1))
}

func calcCarry(bits int, a uint8, b uint8, cy uint8) uint8 {
	result := uint16(a) + uint16(b) + uint16(cy)
	carry := uint16(result) ^ uint16(a) ^ uint16(b)
	return bool2uint8((carry & (1 << bits)) > 0)
}

func (c *CPU8080) writeMem(address uint16, val uint8) {
	if address >= c.RAMLowerLimit && address < c.RAMUpperLimit {
		c.Memory[address] = val
	}
}

func (c *CPU8080) opAdd(val uint8, inc uint8, carry uint8) uint8 {
	res := val + inc + carry
	c.flagsZSP(res)
	c.cc.ac = calcCarry(4, val, inc, carry)
	c.cc.cy = calcCarry(8, val, inc, carry)
	return res
}

func (c *CPU8080) opSub(val uint8, inc uint8, carry uint8) uint8 {
	res := c.opAdd(val, ^inc, ^carry&1)
	c.cc.cy = (^c.cc.cy) & 1
	return res
}

func (c *CPU8080) opAdd1(val uint8) uint8 {
	res := val + 1
	c.flagsZSP(res)
	c.cc.ac = bool2uint8((res & 0xF) == 0)
	return res
}

func (c *CPU8080) opSub1(val uint8) uint8 {
	res := val - 1
	c.flagsZSP(res)
	c.cc.ac = bool2uint8(!((res & 0xF) == 0xF))
	return res
}

func (c *CPU8080) opAna(val uint8) uint8 {
	res := c.A & val
	c.cc.cy = 0
	c.cc.ac = bool2uint8(((c.A | val) & 0x08) != 0)
	c.flagsZSP(res)
	return res
}
