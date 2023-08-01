package sorting

import "testing"

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
