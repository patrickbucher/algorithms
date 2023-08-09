package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

const SearchTestSize = 10_000

func BenchmarkLinearSearch(b *testing.B) {
	haystack := sorting.RandomSlice(SearchTestSize, 0, SearchTestSize)
	needle := haystack[0]
	haystack = InsertionSort(haystack)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		LinearSearch(haystack, needle)
	}
}

func BenchmarkBinarySearch(b *testing.B) {
	haystack := sorting.RandomSlice(SearchTestSize, 0, SearchTestSize)
	needle := haystack[0]
	haystack = InsertionSort(haystack)
	b.ResetTimer()
	for i := 0; i < b.N; i++ {
		BinarySearch(haystack, needle)
	}
}
