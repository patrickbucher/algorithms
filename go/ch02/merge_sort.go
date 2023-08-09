package ch02

import (
	"math"

	"golang.org/x/exp/constraints"
)

// MergeSort returns a copy of slice sorted ascendingly.
func MergeSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	if n == 0 {
		return slice
	}
	copied := make([]T, len(slice))
	copy(copied, slice)
	mergeSort(copied, 0, n-1)
	return copied
}

func mergeSort[T constraints.Ordered](slice []T, p, r int) {
	if p >= r {
		return
	}
	q := int(math.Floor(float64(p+r) / 2))
	mergeSort(slice, p, q)
	mergeSort(slice, q+1, r)
	merge(slice, p, q, r)
}

func merge[T constraints.Ordered](slice []T, p, q, r int) {
	nl := q - p + 1
	nr := r - q
	left := make([]T, nl)
	right := make([]T, nr)
	copy(left, slice[p:q+1])
	copy(right, slice[q+1:r+1])
	i := 0
	j := 0
	k := p
	for i < nl && j < nr {
		if left[i] < right[j] {
			slice[k] = left[i]
			i++
		} else {
			slice[k] = right[j]
			j++
		}
		k++
	}
	for i < nl {
		slice[k] = left[i]
		i++
		k++
	}
	for j < nr {
		slice[k] = right[j]
		j++
		k++
	}
}
