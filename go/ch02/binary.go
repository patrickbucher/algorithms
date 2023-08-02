package ch02

import "math"

// AddBinary adds the bits of as and bs, which must be of same length, and
// returns them.
func AddBinary(as, bs []uint8) []uint8 {
	m, n := len(as), len(bs)
	if m != n || m == 0 || n == 0 {
		return []uint8{}
	}
	cs := make([]uint8, n+1)
	for i := range cs {
		cs[i] = 0
	}
	for i := 0; i < n; i++ {
		e := as[i] + bs[i] + cs[i]
		if e == 0 || e == 1 {
			cs[i] = e
		} else if e == 2 {
			cs[i] = 0
			cs[i+1] = 1
		} else if e == 3 {
			cs[i] = 1
			cs[i+1] = 1
		}
	}
	return cs
}

// BinaryToDecimal converts a slice of little endian bits to an unsigned int.
func BinaryToDecimal(bin []uint8) uint {
	var acc uint
	for i, v := range bin {
		acc += uint(v) * uint(math.Pow(2, float64(i)))
	}
	return acc
}

// DecimalToBinary converts an unsigned int to a slice of little endian bits.
func DecimalToBinary(dec uint) []uint8 {
	if dec == 0 {
		return []uint8{0}
	}
	acc := make([]uint8, 0)
	for dec > 0 {
		acc = append(acc, uint8(dec%2))
		dec /= 2
	}
	return acc
}
