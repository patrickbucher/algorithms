package sorting

import (
	"testing"

	"golang.org/x/exp/constraints"
)

type EqualTestCase[T comparable] struct {
	Xs []T
	Ys []T
	Eq bool
}

func TestEqual(t *testing.T) {
	tests := []EqualTestCase[int]{
		{[]int{1, 2, 3}, []int{1, 2, 3}, true},
	}
	for _, test := range tests {
		actual := Equal(test.Xs, test.Ys)
		if actual != test.Eq {
			t.Errorf("expected Equal(%v, %v) to be %v, was %v",
				test.Xs, test.Ys, test.Eq, actual)
		}
	}
}

type RandomTestCase struct {
	N   int
	Min int
	Max int
}

func TestRandom(t *testing.T) {
	tests := []RandomTestCase{
		{1000, 0, 10},
		{1000, 10, 20},
		{1000, -10, -5},
	}
	for _, test := range tests {
		actual := RandomSlice(test.N, test.Min, test.Max)
		if len(actual) != test.N {
			t.Errorf("expected length to be %d, was %d",
				test.N, len(actual))
		}
		for _, x := range actual {
			if x < test.Min || x >= test.Max {
				t.Errorf("expected value in range [%d;%d[, was %d",
					test.Min, test.Max, x)
			}
		}
	}
}

type SortedTest[T constraints.Ordered] struct {
	Slice  []T
	Cmp    CmpFunc[T]
	Sorted bool
}

func TestIsSortedAscDesc(t *testing.T) {
	asc := func(left, right int) bool { return left <= right }
	desc := func(left, right int) bool { return left >= right }
	tests := []SortedTest[int]{
		{[]int{}, asc, true},
		{[]int{}, desc, true},
		{[]int{1}, asc, true},
		{[]int{1}, desc, true},
		{[]int{1, 2}, asc, true},
		{[]int{1, 2}, desc, false},
		{[]int{2, 1}, asc, false},
		{[]int{2, 1}, desc, true},
		{[]int{0, 0, 0}, asc, true},
		{[]int{0, 0, 0}, desc, true},
		{[]int{1, 2, 3, 4, 5}, asc, true},
		{[]int{1, 2, 3, 4, 5}, desc, false},
		{[]int{5, 4, 3, 2, 1}, asc, false},
		{[]int{5, 4, 3, 2, 1}, desc, true},
	}
	for _, test := range tests {
		actual := IsSorted(test.Slice, test.Cmp)
		if actual != test.Sorted {
			t.Errorf("expected IsSorted(%v, %v) to be %v, was %v",
				test.Slice, test.Cmp, test.Sorted, actual)
		}
	}
}

func TestShuffle(t *testing.T) {
	const n = 100
	slice := make([]int, 0)
	for i := 0; i < n; i++ {
		slice = append(slice, i)
	}
	shuffled := Shuffle(slice)
	if len(shuffled) != n {
		t.Errorf("expected shuffled slice to be of length (%d), was %d",
			n, len(shuffled))
	}
	found := 0
	for _, x := range slice {
		ok := false
		for _, y := range shuffled {
			if x == y {
				ok = true
				found++
				break
			}
		}
		if !ok {
			t.Errorf("missing value %d in shuffled slice", x)
		}
	}
	if found != n {
		t.Errorf("expected to find %d values in shuffled slice, was %d", n, found)
	}
}
