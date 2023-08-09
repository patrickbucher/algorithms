package ch02

import (
	"math/rand"
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

func TestBinarySearch(t *testing.T) {
	tests := []SearchTest[int]{
		{[]int{}, 0, -1},
		{[]int{1}, 0, -1},
		{[]int{1}, 1, 0},
		{[]int{1, 2, 3, 4, 5}, 3, 2},
		{[]int{1, 2, 3, 4, 5}, 5, 4},
		{[]int{1, 2, 3, 4, 5}, 6, -1},
	}
	for _, test := range tests {
		actual := BinarySearch(test.Haystack, test.Needle)
		if actual != test.AtIndex {
			t.Errorf("expected %v to be in %v at index %d, was %d",
				test.Needle, test.Haystack, test.AtIndex, actual)
		}
	}
}

func TestBinarySearchBig(t *testing.T) {
	n := 1000
	slice := InsertionSort(sorting.RandomSlice(n, 0, n))
	atPos := rand.Intn(n)
	value := slice[atPos]
	for i := 0; i < n; i++ {
		// make value searched for unique
		if slice[i] == value && i != atPos {
			slice[i] = 0
		}
	}
	actual := BinarySearch(slice, value)
	if actual != atPos {
		t.Errorf("expected to find value %d at index %d, was %d",
			value, atPos, actual)
	}
}
