package sorting

import "golang.org/x/exp/constraints"

// SortTestCase wraps test data for sorting tests.
type SortTestCase[T constraints.Ordered] struct {
	Input    []T
	Expected []T
}

// EqualTestCase wraps test data for slice equality tests.
type EqualTestCase[T comparable] struct {
	Xs []T
	Ys []T
	Eq bool
}

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
