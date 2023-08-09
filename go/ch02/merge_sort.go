package ch02

import (
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
	q := (p + r) / 2
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

// ParallelMergeSort returns a copy of slice sorted in ascending order. The
// sorting is done in parallel.
func ParallelMergeSort[T constraints.Ordered](slice []T) []T {
	n := len(slice)
	if n < 1 {
		return slice
	}
	sorted := make([]T, 0)
	ch := make(chan T)
	go parallelMergeSort(slice, ch)
	for v := range ch {
		sorted = append(sorted, v)
	}
	return sorted
}

func parallelMergeSort[T constraints.Ordered](slice []T, result chan<- T) {
	// TOOD: simplify code
	// additional base case for n == 2
	// initialize bl and br with one read from each channel (both have at least one value)
	// this should simplify the merge loop
	n := len(slice)
	if n < 1 {
		close(result)
		return
	}
	if n == 1 {
		result <- slice[0]
		close(result)
		return
	}
	mid := n / 2
	left := slice[0:mid]
	right := slice[mid:n]
	leftCh := make(chan T)
	rightCh := make(chan T)
	go parallelMergeSort(left, leftCh)
	go parallelMergeSort(right, rightCh)
	nl, nr := mid, n-mid
	i, j := 0, 0
	var bl, br *T = nil, nil
	for i < nl && j < nr {
		if bl == nil {
			l := <-leftCh
			bl = &l
			i++
		}
		if br == nil {
			r := <-rightCh
			br = &r
			j++
		}
		if *bl < *br {
			result <- *bl
			bl = nil
		} else {
			result <- *br
			br = nil
		}
	}
	var leftover *T = nil
	if bl != nil {
		leftover = bl
	} else if br != nil {
		leftover = br
	}
	for i < nl {
		l := <-leftCh
		if leftover != nil {
			if *leftover < l {
				result <- *leftover
				leftover = nil
			}
		}
		result <- l
		i++
	}
	for j < nr {
		r := <-rightCh
		if leftover != nil {
			if *leftover < r {
				result <- *leftover
				leftover = nil
			}
		}
		result <- r
		j++
	}
	if leftover != nil {
		result <- *leftover
	}
	close(result)
}
