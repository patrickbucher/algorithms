package ch02

import (
	"testing"

	"github.com/patrickbucher/algorithms/sorting"
)

var tests = []SortTestCase[int]{
	{[]int{}, []int{}},
	{[]int{1}, []int{1}},
	{[]int{2, 1}, []int{1, 2}},
	{[]int{3, 1, 2}, []int{1, 2, 3}},
	{[]int{9, 8, 7, 6, 5, 4, 3, 2, 1}, []int{1, 2, 3, 4, 5, 6, 7, 8, 9}},
}

func TestMergeSort(t *testing.T) {
	for _, test := range tests {
		actual := MergeSort(test.Input)
		if !sorting.Equal(actual, test.Expected) {
			t.Errorf("expected MergeSort(%v) to be %v, was %v",
				test.Input, test.Expected, actual)
		}
	}
}

func TestParallelMergeSort(t *testing.T) {
	for _, test := range tests {
		actual := ParallelMergeSort(test.Input)
		if !sorting.Equal(actual, test.Expected) {
			t.Errorf("expected MergeSort(%v) to be %v, was %v",
				test.Input, test.Expected, actual)
		}
	}
}

func TestBigParallelMergeSort(t *testing.T) {
	n := int(1e6)
	input := sorting.RandomSlice(n, 0, n)
	actual := ParallelMergeSort(input)
	if !sorting.IsSortedAsc(actual) {
		t.Errorf("ParallelMergeSort failed for n=%d", n)
	}
}
