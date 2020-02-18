/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut x = 0;

    if n < 0 {

        x = -1;
    }
    else {
        for y in 1..(n + 1) {

            x = y + x;
        }
    }

    x
}

/**
    Returns the number of elements in the list that
    are in the range [s,e]
**/
pub fn in_range(lst: &[i32], s: i32, e: i32) -> i32 {
    let mut count = 0;
    for i in lst  {

        if i <= &e && i>= &s {

            count = count + 1 ;
        }

    }
    count
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut b = true;
    
    for i in target{
        let mut temp = false;
        for j in set {
            if &j == &i{
                temp = true;
            }

        }
        b = b && temp;
    }

    b
}

/**
    Returns the mean of elements in lst. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(lst: &[f64]) -> Option<f64> {
    let mut count = 0.0;
    let mut n = 0.0;
    let mut avg = None;

    if lst == [] {

        avg
    }else{
        for i in lst.iter(){
            count = i + count;
            n = n + 1.0;
        }
        count = count / n;
        Some(count)
    }

}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array

    Ex: to_decimal of [1,0,1,0] returns 10
**/

pub fn to_decimal(lst: &[i32]) -> i32 {
    let mut total = 0;
    let mut base: i32 = 2;
    let mut n = lst.len() as i32 - 1;
    for i in lst.iter() {
        total += i * base.pow(n as u32);
        n = n - 1;
    }
    total
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/

fn fact_help(n: u32) -> u32 {
    let mut p = 0;
    for i in 2 .. n{
        if n % i == 0{
            p = i;
            break;
        }
    }
    p
}

pub fn factorize(n: u32) -> Vec<u32> {
    let mut vec = Vec::new();
    let mut num = n ;
    loop {
        let x = fact_help(num);
        if x == 0 {
            vec.push(num);
            break;
        }else{

            num = num / x;
            vec.push(x);
        }
    }
    if vec.len() as u32 == 0 {
        vec.push(n);
    }
    vec
}

/**
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them,
    so the first becomes the last, the second becomes first, and so on.

    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut vec = Vec::new();
    for i in lst.iter(){
        vec.push(*i);
    }
    if lst.len() > 0 {
        let mut x = vec.remove(0);
        vec.push(x);
    }
    vec
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation

    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let mut index = 0;
    let mut lst = target.as_bytes();
    let mut ret = false;
    let mut targ_len = target.len();
    let mut s_len = s.len();

    if s_len < targ_len {
        return false;
    }

    for i in 0..((s_len - targ_len) + 1){
       let substr = &s[i as usize .. (i + targ_len) as usize];
       if target == substr {
        return true;
       }
    }
    return false;
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    let mut longest_count = 0;
    let mut longest_start = 0;
    let mut ret = None;
    let mut start = 0;
    let mut curr = ' ';
    let mut count = 0;
    for (i, c) in s.chars().enumerate(){
        if c == curr {
            count += 1;
            if count > longest_count {
                longest_start = start; 
                longest_count = count;
            }
        }else{
            count = 0;
            start = i;
            curr = c;
        }
    }
    if longest_count != 0 {
        ret = Some(&s[longest_start..(longest_start + longest_count + 1)])
    }

    ret
}
