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
        // TODO: j as i32 is a mess (possible underflow), refactor
        let mut j: i32 = (i - 1).try_into().unwrap();
        while j >= 0 && ordered[j as usize] > key {
            ordered[(j + 1) as usize] = ordered[j as usize];
            j -= 1;
        }
        ordered[(j + 1) as usize] = key;
        // TODO: end of messy part
    }
    return ordered;
}

#[cfg(test)]
mod tests {
    use crate::ch02::insertion_sort;
    use crate::sorting::equal;
    use crate::sorting::is_sorted_asc;
    use crate::sorting::random_vec;

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
