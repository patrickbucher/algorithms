package ch02

import "golang.org/x/exp/constraints"

// InsertionSort returns the given slice in ascending order.
func InsertionSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	ordered := make([]T, n)
	copy(ordered, slice)
	if n <= 1 {
		return ordered
	}
	for i := 1; i < n; i++ {
		key := ordered[i]
		j := i - 1
		for j >= 0 && ordered[j] > key {
			ordered[j+1] = ordered[j]
			j--
		}
		ordered[j+1] = key
	}
	return ordered
}

// RecursiveInsertionSort performs Insertion Sort recursively.
func RecursiveInsertionSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	ordered := make([]T, n)
	copy(ordered, slice)
	if n <= 1 {
		return ordered
	}
	recursiveInsertionSort(ordered, n)
	return ordered
}

func recursiveInsertionSort[T constraints.Ordered](slice []T, n int) {
	if n <= 1 {
		return
	}
	recursiveInsertionSort(slice, n-1)
	e := slice[n-1]
	i := n - 2
	for i >= 0 && slice[i] > e {
		slice[i+1] = slice[i]
		i--
	}
	slice[i+1] = e
}
