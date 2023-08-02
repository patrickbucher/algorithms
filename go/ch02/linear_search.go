package ch02

// LinearSearch searches the needle in the haystack and returns the index, or
// -1, if the needle wasn't found.
func LinearSearch[T comparable](haystack []T, needle T) int {
	for i, e := range haystack {
		if e == needle {
			return i
		}
	}
	return -1
}
