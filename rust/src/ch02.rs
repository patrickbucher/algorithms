pub fn insertion_sort<T: PartialOrd + Copy>(values: &Vec<T>) -> Vec<T> {
    let n = values.len();
    let mut ordered: Vec<T> = Vec::with_capacity(n);
    for i in 0..n {
        ordered.push(values[i]);
    }
    if n <= 1 {
        return ordered;
    }
    for i in 1..n {
        let key = ordered[i];
        let mut insert_at: usize = i;
        for j in (0..i).rev() {
            if ordered[j] > key {
                ordered[j + 1] = ordered[j];
                insert_at = j;
            } else {
                break;
            }
        }
        ordered[insert_at] = key;
    }
    return ordered;
}

pub fn linear_search<T: Eq>(haystack: &Vec<T>, needle: T) -> Option<usize> {
    for i in 0..haystack.len() {
        if haystack[i] == needle {
            return Some(i);
        }
    }
    return None;
}

#[cfg(test)]
mod tests {
    use crate::ch02::insertion_sort;
    use crate::ch02::linear_search;
    use crate::sorting::equal;
    use crate::sorting::is_sorted_asc;
    use crate::sorting::random_vec;

    #[test]
    fn test_linear_search_empty() {
        assert_eq!(linear_search(&Vec::<i32>::new(), 1 as i32), None);
    }

    #[test]
    fn test_linear_search_single_found() {
        assert_eq!(linear_search(&vec![1], 1 as i32), Some(0));
    }

    #[test]
    fn test_linear_search_single_not_found() {
        assert_eq!(linear_search(&vec![1], 2 as i32), None);
    }

    #[test]
    fn test_linear_search_multiple_found() {
        assert_eq!(linear_search(&vec![1, 2, 3, 4, 5], 3 as i32), Some(2));
    }

    #[test]
    fn test_linear_search_multiple_not_found() {
        assert_eq!(linear_search(&vec![1, 2, 3, 4, 5], 9 as i32), None);
    }

    #[test]
    fn test_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &insertion_sort(&v)));
    }

    #[test]
    fn test_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &insertion_sort(&v)));
    }

    #[test]
    fn test_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &insertion_sort(&values);
        assert!(is_sorted_asc(actual));
    }
}
