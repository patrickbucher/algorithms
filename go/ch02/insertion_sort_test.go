package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
	"golang.org/x/exp/constraints"
)

type SortTestCase[T constraints.Ordered] struct {
	Input    []T
	Expected []T
}

func TestInsertionSort(t *testing.T) {
	tests := []SortTestCase[int]{
		{[]int{}, []int{}},
		{[]int{1}, []int{1}},
		{[]int{2, 1}, []int{1, 2}},
		{[]int{3, 1, 2}, []int{1, 2, 3}},
		{[]int{9, 8, 7, 6, 5, 4, 3, 2, 1}, []int{1, 2, 3, 4, 5, 6, 7, 8, 9}},
	}
	for _, test := range tests {
		actual := InsertionSort(test.Input)
		if !sorting.Equal(actual, test.Expected) {
			t.Errorf("expected InsertionSort(%v) to be %v, was %v",
				test.Input, test.Expected, actual)
		}
	}
}
