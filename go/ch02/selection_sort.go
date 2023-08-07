package ch02

import "golang.org/x/exp/constraints"

func SelectionSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	ordered := make([]T, n)
	copy(ordered, slice)
	if n <= 1 {
		return ordered
	}
	for i := 0; i < n-1; i++ {
		l := ordered[i]
		li := i
		for j := i + 1; j < n; j++ {
			if ordered[j] < l {
				l = ordered[j]
				li = j
			}
		}
		ordered[li] = ordered[i]
		ordered[i] = l
	}
	return ordered
}
