pub fn equal<T: Eq>(xs: Vec<T>, ys: Vec<T>) -> bool {
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

#[cfg(test)]
mod tests {
    use crate::sorting::equal;

    #[test]
    fn test_equal_empty() {
        assert_eq!(equal(Vec::<i32>::new(), Vec::<i32>::new()), true);
    }

    #[test]
    fn test_equal_single_element() {
        assert_eq!(equal(vec![5], vec![5]), true);
    }

    #[test]
    fn test_equal_items() {
        assert_eq!(equal(vec![1, 2, 3], vec![1, 2, 3]), true);
    }

    #[test]
    fn test_not_equal_different_length() {
        assert_eq!(equal(vec![1, 2, 3], vec![1, 2]), false);
    }

    #[test]
    fn test_not_equal_different_values() {
        assert_eq!(equal(vec![1, 2, 3], vec![1, 2, 4]), false);
    }
}
