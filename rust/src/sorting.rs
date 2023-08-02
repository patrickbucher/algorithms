pub fn equal() -> bool {
    let a = vec![1, 2, 3];
    let b = vec![1, 2, 3];
    return a == b;
}

#[cfg(test)]
mod tests {
    use crate::sorting::equal;
    #[test]
    fn test_equal() {
        assert_eq!(equal(), false);
    }
}
