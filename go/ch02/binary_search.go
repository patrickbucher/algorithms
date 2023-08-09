package ch02

import (
	"golang.org/x/exp/constraints"
)

// BinarySearch searches the needle in the sorted haystack and returns the
// index, or -1, if the needle wasn't found.
func BinarySearch[T constraints.Ordered](haystack []T, needle T) int {
	return binarySearch(haystack, needle, 0, len(haystack))
}

func binarySearch[T constraints.Ordered](haystack []T, needle T, p, r int) int {
	n := r - p
	if n == 0 {
		return -1
	}
	q := p + n/2
	if needle == haystack[q] {
		return q
	} else if needle < haystack[q] {
		return binarySearch(haystack, needle, p, q)
	} else {
		return binarySearch(haystack, needle, q+1, r)
	}
}
