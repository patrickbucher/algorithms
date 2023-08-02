package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

type AddBinaryTest struct {
	As []uint8
	Bs []uint8
	Cs []uint8
}

func TestAddBinary(t *testing.T) {
	tests := []AddBinaryTest{
		// NOTE: binary numbers read left (least significant) to right!
		{[]uint8{}, []uint8{}, []uint8{}},
		{[]uint8{0}, []uint8{0}, []uint8{0, 0}},
		{[]uint8{0}, []uint8{1}, []uint8{1, 0}},
		{[]uint8{1}, []uint8{0}, []uint8{1, 0}},
		{[]uint8{1}, []uint8{1}, []uint8{0, 1}},
		{[]uint8{0, 1}, []uint8{1, 1}, []uint8{1, 0, 1}},
		{[]uint8{0, 1, 0, 1, 1, 1}, []uint8{1, 1, 0, 1, 1, 1}, []uint8{1, 0, 1, 0, 1, 1, 1}},
	}
	for _, test := range tests {
		actual := AddBinary(test.As, test.Bs)
		if !sorting.Equal(actual, test.Cs) {
			t.Errorf("expected AddBinary(%v, %v) to be %v, was %v",
				test.As, test.Bs, test.Cs, actual)
		}
	}
}

type BinaryConversionTest struct {
	Binary  []uint8
	Decimal uint
}

func TestBinaryConversion(t *testing.T) {
	tests := []BinaryConversionTest{
		{[]uint8{0}, 0},
		{[]uint8{1}, 1},
		{[]uint8{0, 1}, 2},
		{[]uint8{1, 1}, 3},
		{[]uint8{0, 0, 1}, 4},
		{[]uint8{1, 0, 1}, 5},
		{[]uint8{0, 1, 1}, 6},
		{[]uint8{1, 1, 1}, 7},
		{[]uint8{0, 0, 0, 1}, 8},
	}
	for _, test := range tests {
		actualDec := BinaryToDecimal(test.Binary)
		if actualDec != test.Decimal {
			t.Errorf("expected BinaryToDecimal(%v) to be %d, was %d",
				test.Binary, test.Decimal, actualDec)
		}
		actualBin := DecimalToBinary(test.Decimal)
		if !sorting.Equal(actualBin, test.Binary) {
			t.Errorf("expected DecimalToBinary(%d) to be %v, was %v",
				test.Decimal, test.Binary, actualBin)
		}
	}
}
