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
