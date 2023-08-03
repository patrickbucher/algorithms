use rand::Rng;

pub fn equal<T: Eq>(xs: &Vec<T>, ys: &Vec<T>) -> bool {
    if xs.len() != ys.len() {
        return false;
    }
    for i in 0..xs.len() {
        if xs[i] != ys[i] {
            return false;
        }
    }
    return true;
}

pub fn random_vec(n: i32, min: i32, max: i32) -> Vec<i32> {
    if min >= max || n < 1 {
        return Vec::new();
    }
    let mut v: Vec<i32> = Vec::with_capacity(n as usize);
    for i in 0..n {
        v.push(rand::thread_rng().gen_range(min..max));
    }
    return v;
}

pub fn is_sorted<T: PartialOrd>(values: &Vec<T>, cmp: fn(&T, &T) -> bool) -> bool {
    let n = values.len();
    if n == 0 {
        return true;
    }
    let mut prev = &values[0];
    for i in 1..n {
        let v = &values[i];
        if !cmp(prev, v) {
            return false;
        }
        prev = v;
    }
    return true;
}

pub fn is_sorted_asc<T: PartialOrd>(values: &Vec<T>) -> bool {
    is_sorted(values, |l, r| l <= r)
}

pub fn is_sorted_desc<T: PartialOrd>(values: &Vec<T>) -> bool {
    is_sorted(values, |l, r| l >= r)
}

#[cfg(test)]
mod tests {
    use crate::sorting::equal;
    use crate::sorting::is_sorted;
    use crate::sorting::is_sorted_asc;
    use crate::sorting::is_sorted_desc;
    use crate::sorting::random_vec;

    #[test]
    fn test_is_sorted_empty_vec() {
        assert!(is_sorted(&vec![0; 0], |l, r| l < r));
        assert!(is_sorted_asc(&vec![0; 0]));
        assert!(is_sorted_desc(&vec![0; 0]));
    }

    #[test]
    fn test_is_sorted_single_entry() {
        assert!(is_sorted(&vec![1], |l, r| l < r));
        assert!(is_sorted_asc(&vec![1]));
        assert!(is_sorted_desc(&vec![1]));
    }

    #[test]
    fn test_is_sorted_asc() {
        assert!(is_sorted(&vec![1, 1, 1, 2, 2, 3, 4, 5], |l, r| l <= r));
        assert!(is_sorted_asc(&vec![1, 1, 1, 2, 2, 3, 4, 5]));
    }

    #[test]
    fn test_is_sorted_desc() {
        assert!(is_sorted(&vec![3, 3, 2, 2, 1, 1, 1, 0], |l, r| l >= r));
        assert!(is_sorted_desc(&vec![3, 3, 2, 2, 1, 1, 1, 0]));
    }

    #[test]
    fn test_not_is_sorted_asc() {
        assert_eq!(is_sorted(&vec![1, 2, 4, 3], |l, r| l <= r), false);
    }

    #[test]
    fn test_not_is_sorted_desc() {
        assert_eq!(is_sorted(&vec![4, 3, 1, 2], |l, r| l >= r), false);
    }

    #[test]
    fn test_random_vec() {
        let numbers = random_vec(1000, 1, 2);
        assert_eq!(numbers.len(), 1000);
        for i in 0..numbers.len() {
            assert!(numbers[i] >= 1 && numbers[i] < 2);
        }
    }

    #[test]
    fn test_equal_empty() {
        assert_eq!(equal(&Vec::<i32>::new(), &Vec::<i32>::new()), true);
    }

    #[test]
    fn test_equal_single_element() {
        assert_eq!(equal(&vec![5], &vec![5]), true);
    }

    #[test]
    fn test_equal_items() {
        assert_eq!(equal(&vec![1, 2, 3], &vec![1, 2, 3]), true);
    }

    #[test]
    fn test_not_equal_different_length() {
        assert_eq!(equal(&vec![1, 2, 3], &vec![1, 2]), false);
    }

    #[test]
    fn test_not_equal_different_values() {
        assert_eq!(equal(&vec![1, 2, 3], &vec![1, 2, 4]), false);
    }
}
