package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

const SortTestSize = 10_000

func BenchmarkInsertionSort(b *testing.B) {
	numbers := sorting.RandomSlice(SortTestSize, 0, SortTestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		InsertionSort(numbers)
	}
}

func BenchmarkSelectionSort(b *testing.B) {
	numbers := sorting.RandomSlice(SortTestSize, 0, SortTestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		SelectionSort(numbers)
	}
}

func BenchmarkMergeSort(b *testing.B) {
	numbers := sorting.RandomSlice(SortTestSize, 0, SortTestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		MergeSort(numbers)
	}
}

func BenchmarkParallelMergeSort(b *testing.B) {
	numbers := sorting.RandomSlice(SortTestSize, 0, SortTestSize)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		ParallelMergeSort(numbers)
	}
}
