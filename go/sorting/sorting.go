package sorting

import (
	"math/rand"

	"golang.org/x/exp/constraints"
)

// Equal returns true if as and bs contain the same elements in the same order.
func Equal[T comparable](as, bs []T) bool {
	if len(as) != len(bs) {
		return false
	}
	for i, a := range as {
		if a != bs[i] {
			return false
		}
	}
	return true
}

// RandomSlice generates a slice of size n with random values in range [min;max[.
func RandomSlice(n, min, max int) []int {
	if min >= max || n < 1 {
		return []int{}
	}
	slice := make([]int, n)
	for i := 0; i < n; i++ {
		slice[i] = rand.Intn(max-min) + min
	}
	return slice
}

type CmpFunc[T constraints.Ordered] func(T, T) bool

// IsSorted returns true if the slice is sorted according to cmp, and false otherwise.
func IsSorted[T constraints.Ordered](slice []T, cmp CmpFunc[T]) bool {
	if len(slice) < 1 {
		return true
	}
	prev := slice[0]
	for i := 1; i < len(slice); i++ {
		if !cmp(prev, slice[i]) {
			return false
		}
	}
	return true
}

// IsSortedAsc returns true if the slice is sorted in ascending order.
func IsSortedAsc[T constraints.Ordered](slice []T) bool {
	cmp := func(left, right T) bool { return left <= right }
	return IsSorted[T](slice, cmp)
}

// IsSortedDesc returns true if the slice is sorted in descending order.
func IsSortedDesc[T constraints.Ordered](slice []T) bool {
	cmp := func(left, right T) bool { return left >= right }
	return IsSorted[T](slice, cmp)
}
