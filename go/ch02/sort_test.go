package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

func BenchmarkInsertionSort(b *testing.B) {
	numbers := sorting.RandomSlice(1000, 0, 1000)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		InsertionSort(numbers)
	}
}

func BenchmarkSelectionSort(b *testing.B) {
	numbers := sorting.RandomSlice(1000, 0, 1000)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		SelectionSort(numbers)
	}
}
