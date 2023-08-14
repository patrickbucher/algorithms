package ch02

import "golang.org/x/exp/constraints"

// BubbleSort returns the given slice in ascending order.
func BubbleSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	if n == 0 {
		return slice
	}
	ordered := make([]T, n)
	copy(ordered, slice)
	for i := 0; i < n-1; i++ {
		for j := n - 1; j > i; j-- {
			if ordered[j] < ordered[j-1] {
				ordered[j], ordered[j-1] = ordered[j-1], ordered[j]
			}
		}
	}
	return ordered
}
