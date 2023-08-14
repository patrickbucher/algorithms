use std::marker::{Send, Sync};
use std::sync::mpsc::{sync_channel, Receiver, SyncSender};
use std::thread;

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

pub fn add_binary(xs: &Vec<u8>, ys: &Vec<u8>) -> Vec<u8> {
    let m = xs.len();
    let n = ys.len();
    if m == 0 {
        return ys.clone();
    }
    if n == 0 {
        return xs.clone();
    }
    if m != n {
        return Vec::new();
    }
    let mut zs: Vec<u8> = Vec::with_capacity(m + 1);
    for i in 0..=m {
        zs.push(0);
    }
    for i in 0..m {
        let e = xs[i] + ys[i] + zs[i];
        if e == 0 || e == 1 {
            zs[i] = e;
        } else if e == 2 {
            zs[i] = 0;
            zs[i + 1] = 1;
        } else if e == 3 {
            zs[i] = 1;
            zs[i + 1] = 1;
        }
    }
    return zs;
}

pub fn binary_to_decimal(bin: &Vec<u8>) -> u32 {
    let mut acc: u32 = 0;
    for i in 0..bin.len() {
        acc += bin[i] as u32 * 2_u32.pow(i as u32);
    }
    return acc;
}

pub fn decimal_to_binary(mut dec: u32) -> Vec<u8> {
    if dec == 0 {
        return vec![0];
    }
    let mut acc: Vec<u8> = Vec::new();
    while dec > 0 {
        acc.push((dec % 2) as u8);
        dec /= 2;
    }
    return acc;
}

pub fn selection_sort<T: PartialOrd + Copy>(values: &Vec<T>) -> Vec<T> {
    let n = values.len();
    let mut ordered: Vec<T> = Vec::with_capacity(n);
    for i in 0..n {
        ordered.push(values[i]);
    }
    if n <= 1 {
        return ordered;
    }
    for i in 0..(n - 1) {
        let mut l = ordered[i];
        let mut li = i;
        for j in (i + 1)..n {
            if ordered[j] < l {
                l = ordered[j];
                li = j;
            }
        }
        ordered[li] = ordered[i];
        ordered[i] = l;
    }
    return ordered;
}

pub fn merge_sort<T: PartialOrd + Copy>(values: &Vec<T>) -> Vec<T> {
    let n = values.len();
    let mut copied = Vec::with_capacity(n);
    for i in 0..n {
        copied.push(values[i]);
    }
    if n == 0 || n == 1 {
        return copied;
    }
    merge_sort_in_place(&mut copied, 0, n - 1);
    return copied;
}

fn merge_sort_in_place<T: PartialOrd + Copy>(values: &mut Vec<T>, p: usize, r: usize) {
    if p >= r {
        return;
    }
    let q = (p + r) / 2;
    merge_sort_in_place(values, p, q);
    merge_sort_in_place(values, q + 1, r);
    merge(values, p, q, r);
}

fn merge<T: PartialOrd + Copy>(values: &mut Vec<T>, p: usize, q: usize, r: usize) {
    let nl = q - p + 1;
    let nr = r - q;
    let mut left: Vec<T> = Vec::with_capacity(nl);
    let mut right: Vec<T> = Vec::with_capacity(nr);
    for i in p..=q {
        left.push(values[i]);
    }
    for i in (q + 1)..=r {
        right.push(values[i]);
    }
    let mut i = 0;
    let mut j = 0;
    let mut k = p;
    while i < nl && j < nr {
        if left[i] < right[j] {
            values[k] = left[i];
            i += 1;
        } else {
            values[k] = right[j];
            j += 1;
        }
        k += 1;
    }
    while i < nl {
        values[k] = left[i];
        i += 1;
        k += 1;
    }
    while j < nr {
        values[k] = right[j];
        j += 1;
        k += 1;
    }
}

pub fn parallel_merge_sort<T: PartialOrd + Copy + Send + Sync + 'static>(
    values: &Vec<T>,
) -> Vec<T> {
    let n = values.len();
    let mut copied = Vec::with_capacity(n);
    for i in 0..n {
        copied.push(values[i]);
    }
    let (tx, rx) = sync_channel::<T>(0);
    thread::spawn(move || {
        split_and_merge(&copied, tx);
    });
    let mut result = Vec::with_capacity(n);
    for v in rx {
        result.push(v);
    }
    return result;
}

fn split_and_merge<T: PartialOrd + Copy + Send + Sync + 'static>(
    values: &Vec<T>,
    tx: SyncSender<T>,
) {
    let n = values.len();
    if n < 1 {
        return;
    }
    if n == 1 {
        tx.send(values[0]).unwrap();
        return;
    }
    let mid = n / 2;
    let (nl, nr) = (mid, n - mid);
    let mut left = Vec::with_capacity(nl);
    let mut right = Vec::with_capacity(nr);
    for i in 0..mid {
        left.push(values[i]);
    }
    for i in mid..n {
        right.push(values[i]);
    }
    let (ltx, lrx) = sync_channel::<T>(0);
    let (rtx, rrx) = sync_channel::<T>(0);
    thread::spawn(move || {
        split_and_merge(&left, ltx);
    });
    thread::spawn(move || {
        split_and_merge(&right, rtx);
    });
    let (mut i, mut j) = (0, 0);
    let mut bl: Option<T> = None;
    let mut br: Option<T> = None;
    while i < nl && j < nr {
        if let None = bl {
            let l = lrx.recv().unwrap();
            bl = Some(l);
            i += 1;
        }
        if let None = br {
            let r = rrx.recv().unwrap();
            br = Some(r);
            j += 1;
        }
        if let Some(l) = bl {
            if let Some(r) = br {
                if l < r {
                    tx.send(l).unwrap();
                    bl = None;
                } else {
                    tx.send(r).unwrap();
                    br = None;
                }
            }
        }
    }
    let mut leftover: Option<T> = None;
    if let Some(v) = bl {
        leftover = Some(v);
    }
    if let Some(v) = br {
        leftover = Some(v);
    }
    let drain = |rx: Receiver<T>, mut buf, i: usize, n: usize| -> Option<T> {
        for _ in i..n {
            let x = rx.recv().unwrap();
            if let Some(y) = buf {
                if y < x {
                    tx.send(y).unwrap();
                    buf = None;
                }
            }
            tx.send(x).unwrap();
        }
        return buf;
    };
    leftover = drain(lrx, leftover, i, nl);
    leftover = drain(rrx, leftover, j, nr);
    if let Some(v) = leftover {
        tx.send(v).unwrap();
    }
}

pub fn binary_search<T: PartialOrd>(haystack: &Vec<T>, needle: T) -> Option<usize> {
    let n = haystack.len();
    if n == 0 {
        return None;
    }
    if n == 1 && haystack[0] == needle {
        return Some(0);
    }
    return recursive_binary_search(haystack, needle, 0, n);
}

fn recursive_binary_search<T: PartialOrd>(
    haystack: &Vec<T>,
    needle: T,
    p: usize,
    r: usize,
) -> Option<usize> {
    let n = r - p;
    if n == 0 {
        return None;
    }
    let q = p + n / 2;
    if needle == haystack[q] {
        return Some(q);
    } else if needle < haystack[q] {
        return recursive_binary_search(haystack, needle, p, q);
    } else {
        return recursive_binary_search(haystack, needle, q + 1, r);
    }
}

fn recursive_insertion_sort<T: PartialOrd + Copy>(values: &Vec<T>) -> Vec<T> {
    let n = values.len();
    let mut ordered = Vec::with_capacity(n);
    for i in 0..n {
        ordered.push(values[i]);
    }
    if n <= 1 {
        return ordered;
    }
    insert_next(&mut ordered, n);
    return ordered;
}

fn insert_next<T: PartialOrd + Copy>(values: &mut Vec<T>, n: usize) {
    if n <= 1 {
        return;
    }
    insert_next(values, n - 1);
    let mut insert_at = n - 1;
    let e = values[insert_at];
    for i in (0..=(n - 2)).rev() {
        if values[i] > e {
            values[i + 1] = values[i];
            insert_at = i;
        } else {
            break;
        }
    }
    values[insert_at] = e;
}

pub fn bubble_sort<T: PartialOrd + Copy>(values: &Vec<T>) -> Vec<T> {
    let n = values.len();
    let mut ordered: Vec<T> = Vec::with_capacity(n);
    for i in 0..n {
        ordered.push(values[i]);
    }
    if n <= 1 {
        return ordered;
    }
    for i in 0..n {
        for j in ((i + 1)..n).rev() {
            if ordered[j] < ordered[j - 1] {
                let tmp = ordered[j];
                ordered[j] = ordered[j - 1];
                ordered[j - 1] = tmp;
            }
        }
    }
    return ordered;
}

#[cfg(test)]
mod tests {
    use crate::ch02::add_binary;
    use crate::ch02::binary_search;
    use crate::ch02::binary_to_decimal;
    use crate::ch02::bubble_sort;
    use crate::ch02::decimal_to_binary;
    use crate::ch02::insertion_sort;
    use crate::ch02::linear_search;
    use crate::ch02::merge_sort;
    use crate::ch02::parallel_merge_sort;
    use crate::ch02::recursive_insertion_sort;
    use crate::ch02::selection_sort;
    use crate::sorting::equal;
    use crate::sorting::is_sorted_asc;
    use crate::sorting::random_vec;
    use rand::Rng;

    #[test]
    fn test_bubble_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &bubble_sort(&v)));
    }

    #[test]
    fn test_bubble_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &bubble_sort(&v)));
    }

    #[test]
    fn test_bubble_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &bubble_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_bubble_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &bubble_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_bubble_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &bubble_sort(&values);
        assert!(is_sorted_asc(actual));
    }

    #[test]
    fn test_recursive_insertion_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &recursive_insertion_sort(&v)));
    }

    #[test]
    fn test_recursive_insertion_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &recursive_insertion_sort(&v)));
    }

    #[test]
    fn test_recursive_insertion_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &recursive_insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_recursive_insertion_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &recursive_insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_recursive_insertion_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &recursive_insertion_sort(&values);
        assert!(is_sorted_asc(actual));
    }

    #[test]
    fn test_binary_search_empty_vec() {
        let v: Vec<i32> = vec![];
        let actual = binary_search(&v, 0);
        assert!(actual == None);
    }

    #[test]
    fn test_binary_search_find_in_single_element_vec() {
        let v: Vec<i32> = vec![0];
        let actual = binary_search(&v, 0);
        assert!(actual == Some(0));
    }

    #[test]
    fn test_binary_search_miss_in_single_element_vec() {
        let v: Vec<i32> = vec![0];
        let actual = binary_search(&v, 1);
        assert!(actual == None);
    }

    #[test]
    fn test_binary_search_find_in_multi_element_vec() {
        let v: Vec<i32> = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let actual = binary_search(&v, 3);
        assert!(actual == Some(3));
    }

    #[test]
    fn test_binary_search_find_in_big_vec() {
        let n = 1000;
        let mut v = random_vec(n, 0, n);
        let value = v[rand::thread_rng().gen_range(0..n) as usize];
        let mut occurrence = 0;
        for i in 0..(n as usize) {
            // make value searched for unique
            if v[i] == value {
                if occurrence > 0 {
                    v[i] = 0;
                }
                occurrence += 1;
            }
        }
        v = insertion_sort(&v);
        let mut expected: Option<usize> = None;
        for i in 0..(n as usize) {
            if v[i] == value {
                expected = Some(i);
                break;
            }
        }
        let actual = binary_search(&v, value);
        assert!(actual == expected);
    }

    #[test]
    fn test_parallel_merge_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &parallel_merge_sort(&v)));
    }

    #[test]
    fn test_parallel_merge_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &parallel_merge_sort(&v)));
    }

    #[test]
    fn test_parallel_merge_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &parallel_merge_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_parallel_merge_sort_more_multiple_elements() {
        let values = vec![6, 1, 0, 2, 5, 4, 3, 7];
        let expected = vec![0, 1, 2, 3, 4, 5, 6, 7];
        let actual = &parallel_merge_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_parallel_merge_sort_long_vector() {
        let values = random_vec(5000, 1, 1e6 as i32);
        let actual = &parallel_merge_sort(&values);
        assert!(is_sorted_asc(actual));
    }

    #[test]
    fn test_merge_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &merge_sort(&v)));
    }

    #[test]
    fn test_merge_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &merge_sort(&v)));
    }

    #[test]
    fn test_merge_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &merge_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_merge_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &merge_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_merge_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &merge_sort(&values);
        assert!(is_sorted_asc(actual));
    }

    #[test]
    fn test_selection_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &selection_sort(&v)));
    }

    #[test]
    fn test_selection_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &selection_sort(&v)));
    }

    #[test]
    fn test_selection_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &selection_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_selection_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &selection_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_selection_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &selection_sort(&values);
        assert!(is_sorted_asc(actual));
    }

    #[test]
    fn test_decimal_to_binary() {
        assert_eq!(decimal_to_binary(0), vec![0]);
        assert_eq!(decimal_to_binary(1), vec![1]);
        assert_eq!(decimal_to_binary(2), vec![0, 1]);
        assert_eq!(decimal_to_binary(3), vec![1, 1]);
        assert_eq!(decimal_to_binary(4), vec![0, 0, 1]);
        assert_eq!(decimal_to_binary(5), vec![1, 0, 1]);
        assert_eq!(decimal_to_binary(6), vec![0, 1, 1]);
        assert_eq!(decimal_to_binary(7), vec![1, 1, 1]);
        assert_eq!(decimal_to_binary(8), vec![0, 0, 0, 1]);
    }

    #[test]
    fn test_binary_to_decimal() {
        assert_eq!(binary_to_decimal(&Vec::new()), 0);
        assert_eq!(binary_to_decimal(&vec![0]), 0);
        assert_eq!(binary_to_decimal(&vec![1]), 1);
        assert_eq!(binary_to_decimal(&vec![0, 1]), 2);
        assert_eq!(binary_to_decimal(&vec![1, 1]), 3);
        assert_eq!(binary_to_decimal(&vec![0, 0, 1]), 4);
        assert_eq!(binary_to_decimal(&vec![1, 0, 1]), 5);
        assert_eq!(binary_to_decimal(&vec![0, 1, 1]), 6);
        assert_eq!(binary_to_decimal(&vec![1, 1, 1]), 7);
        assert_eq!(binary_to_decimal(&vec![0, 0, 0, 1]), 8);
    }

    #[test]
    fn test_add_empty_binaries() {
        assert!(equal(&add_binary(&Vec::new(), &Vec::new()), &Vec::new()));
    }

    #[test]
    fn test_add_to_empty_binary() {
        assert!(equal(&add_binary(&Vec::new(), &vec![0]), &vec![0]));
        assert!(equal(&add_binary(&vec![0], &Vec::new()), &vec![0]));
    }

    #[test]
    fn test_add_of_different_size() {
        assert!(equal(&add_binary(&vec![0, 1], &vec![1]), &Vec::new()));
    }

    #[test]
    fn test_one_plus_one_is_two() {
        assert!(equal(&add_binary(&vec![1], &vec![1]), &vec![0, 1]));
    }

    #[test]
    fn test_one_plus_two_is_three() {
        assert!(equal(&add_binary(&vec![1, 0], &vec![0, 1]), &vec![1, 1, 0]));
    }

    #[test]
    fn test_two_plus_two_is_four() {
        assert!(equal(&add_binary(&vec![0, 1], &vec![0, 1]), &vec![0, 0, 1]));
    }

    #[test]
    fn test_ten_plus_five_is_fifteen() {
        assert!(equal(
            &add_binary(&vec![0, 1, 0, 1], &vec![1, 0, 1, 0]),
            &vec![1, 1, 1, 1, 0]
        ));
    }

    #[test]
    fn test_eleven_plus_five_is_sixteen() {
        assert!(equal(
            &add_binary(&vec![1, 1, 0, 1], &vec![1, 0, 1, 0]),
            &vec![0, 0, 0, 0, 1]
        ));
    }

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
    fn test_insertion_sort_empty_vec() {
        let v: Vec<i32> = vec![];
        assert!(equal(&v, &insertion_sort(&v)));
    }

    #[test]
    fn test_insertion_sort_single_element() {
        let v: Vec<i32> = vec![1];
        assert!(equal(&v, &insertion_sort(&v)));
    }

    #[test]
    fn test_insertion_sort_multiple_elements() {
        let values = vec![3, 1, 2];
        let expected = vec![1, 2, 3];
        let actual = &insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_insertion_sort_more_multiple_elements() {
        let values = vec![6, 1, 2, 5, 4, 3];
        let expected = vec![1, 2, 3, 4, 5, 6];
        let actual = &insertion_sort(&values);
        assert!(equal(&expected, &actual));
    }

    #[test]
    fn test_insertion_sort_long_vector() {
        let values = random_vec(1000, 1, 1000);
        let actual = &insertion_sort(&values);
        assert!(is_sorted_asc(actual));
    }
}
