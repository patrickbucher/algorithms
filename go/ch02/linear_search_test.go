package ch02

import "testing"

type LinearSearchTest[T comparable] struct {
	Haystack []T
	Needle   T
	AtIndex  int
}

func TestLinearSearch(t *testing.T) {
	tests := []LinearSearchTest[int]{
		{[]int{}, 0, -1},
		{[]int{1}, 0, -1},
		{[]int{1}, 1, 0},
		{[]int{1, 2, 3, 4, 5}, 5, 4},
		{[]int{1, 2, 3, 4, 5}, 6, -1},
	}
	for _, test := range tests {
		actual := LinearSearch(test.Haystack, test.Needle)
		if actual != test.AtIndex {
			t.Errorf("expected %v to be in %v at index %d, was %d",
				test.Needle, test.Haystack, test.AtIndex, actual)
		}
	}
}
