package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

const TestSize = 10_000

func BenchmarkInsertionSort(b *testing.B) {
	numbers := sorting.RandomSlice(TestSize, 0, TestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		InsertionSort(numbers)
	}
}

func BenchmarkSelectionSort(b *testing.B) {
	numbers := sorting.RandomSlice(TestSize, 0, TestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		SelectionSort(numbers)
	}
}

func BenchmarkMergeSort(b *testing.B) {
	numbers := sorting.RandomSlice(TestSize, 0, TestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		MergeSort(numbers)
	}
}
