use std::ops::{Add, Mul, Neg, Shl, Shr, Sub};

#[derive(Debug, Clone, PartialEq, Eq, Ord)]
pub struct BigInt {
    sign: bool,
    digits: Vec<u32>,
}

impl From<i32> for BigInt {
    fn from(i: i32) -> Self {
        let digits = i.checked_abs();
        if let Some(d) = digits {
            Self { sign: i < 0, digits: vec![d as u32] }
        } else {
            Self { sign: true, digits: vec![1] }
        }
    }
}

impl From<u32> for BigInt {
    fn from(u: u32) -> Self {
        Self { sign: false, digits: vec![u] }
    }
}

impl From<i64> for BigInt {
    fn from(i: i64) -> Self {
        let digits = i.checked_abs();
        if let Some(d) = digits {
            Self { sign: i < 0, digits: vec![d as u32, (d >> 32) as u32] }
        } else {
            Self { sign: true, digits: vec![1] }
        }
    }
}

impl From<u64> for BigInt {
    fn from(u: u64) -> Self {
        Self { sign: false, digits: vec![u as u32, (u >> 32) as u32]}
    }
}

impl Add for BigInt {
    type Output = Self;

    fn add(mut self, mut rhs: Self) -> Self {
        if self.sign != rhs.sign {
            return if self.sign {
                self.sign = false;
                rhs - self
            } else {
                rhs.sign = false;
                self - rhs
            }
        };

        let (mut lhs, rhs) = if self.digits.len() > rhs.digits.len() {
            (self, rhs)
        } else {
            (rhs, self)
        };

        let mut carry = 0;
        for (i, l) in lhs.digits.iter_mut().enumerate() {
            let rhs = rhs.digits
                .get(i)
                .cloned()
                .unwrap_or(0)
                .wrapping_add(carry);

            let (new, overflow) =
                l.overflowing_add(rhs);

            *l = new;
            carry = overflow as u32;
        }

        if carry == 1 {
            lhs.digits.push(1);
        }

        lhs.compact();
        lhs
    }
}

impl Sub for BigInt {
    type Output = Self;

    fn sub(mut self, mut rhs: Self) -> Self {
        if self.sign != rhs.sign {
            return if self.sign {
                self.sign = false;
                let mut result = self + rhs;
                result.sign = true;
                result
            } else {
                self + rhs
            }
        };

        if self.sign && rhs.sign {
            return rhs - self
        };

        let sign = self < rhs;

        let mut borrow = 0;
        if sign {
            for (i, rhs_val) in rhs.digits.iter_mut().enumerate() {
                let self_val = *self.digits.get(i).unwrap_or(&0);
                let (new_val, overflow) = rhs_val.overflowing_sub(self_val.wrapping_add(borrow));
                *rhs_val = new_val;
                borrow = overflow as u32;
            }
        } else {
            for (i, self_val) in self.digits.iter_mut().enumerate() {
                let rhs_val = *rhs.digits.get(i).unwrap_or(&0);
                let (new_val, overflow) = self_val.overflowing_sub(rhs_val.wrapping_add(borrow));
                *self_val = new_val;
                borrow = overflow as u32;
            }
        }

        assert!(borrow == 0);

        Self {
            sign,
            digits: if sign { rhs.digits } else { self.digits },
        }
    }
}

impl Mul for BigInt {
    type Output = Self;

    fn mul(self, rhs: Self) -> Self {
        let mut result = BigInt::new();
        
        for (i, &x) in self.digits.iter().enumerate() {
            for (j, &y) in rhs.digits.iter().enumerate() {
                // the place of a (base-2^32) number where the digit will end up in
                let place = (i + j) as u32;
                let digit = 
                    BigInt::from((x as u64) * (y as u64)) << (place * 32);
                result = result + digit;
            }
        }

        result.sign = self.sign ^ rhs.sign;

        result.compact();
        result
    }
}

impl Shl<u32> for BigInt {
    type Output = Self;

    fn shl(self, rhs: u32) -> Self {
        let mut s = self;
        let mut temp = 0;
        
        let vec_append = (rhs / 32) as usize;
        let rhs = rhs % 32;

        for s in s.digits.iter_mut() {
            let s_old = *s;
            if rhs > 0 {
                *s = (*s << rhs) | temp;
                temp = s_old >> (32 - rhs);
            }
        }

        let mut new_inner = vec![0; vec_append];
        new_inner.append(&mut s.digits);
        
        Self {
            sign: s.sign,
            digits: new_inner
        }
    }
}

impl Shr<u32> for BigInt {
    type Output = Self;

    fn shr(self, rhs: u32) -> Self {
        let mut s = self;
        let mut temp = 0;
        
        let vec_remove = (rhs / 32) as usize;
        let rhs = rhs % 32;

        for s in s.digits.iter_mut().rev() {
            let s_old = *s;
            if rhs > 0 {
                *s = (temp << (32 - rhs)) | (*s >> rhs);
                temp = s_old & ((1 << rhs) - 1);
            }
        }

        for _ in 0..vec_remove {
            s.digits.remove(0);
        }

        if s.digits.is_empty() {
            s.digits.push(0)
        }

        s
    }
}

impl PartialOrd for BigInt {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        if self.sign != other.sign {
            return if self.sign {
                Some(std::cmp::Ordering::Less)
            } else {
                Some(std::cmp::Ordering::Greater)
            }
        }

        if self.digits.len() != other.digits.len() {
            return if self.digits.len() < other.digits.len() {
                Some(std::cmp::Ordering::Less)
            } else {
                Some(std::cmp::Ordering::Greater)
            }
        }

        for (x, y) in self.digits.iter().rev().zip(other.digits.iter().rev()) {
            if *x < *y {
                return Some(std::cmp::Ordering::Less)
            } else if *x > *y {
                return Some(std::cmp::Ordering::Greater)
            }
        }

        return Some(std::cmp::Ordering::Equal)
    }
}

impl Neg for BigInt {
    type Output = Self;

    fn neg(self) -> Self {
        Self {
            sign: !self.sign,
            digits: self.digits,
        }
    }
}

impl BigInt {
    pub fn new() -> Self {
        Self { sign: false, digits: vec![0] }
    }

    fn compact(&mut self) {
        while let Some(&0) = self.digits.last() {
            self.digits.pop().unwrap();
        }

        if self.digits.is_empty() {
            self.digits.push(0);
        }
    }

    pub fn from_binary(s: &str) -> Self {
        let mut chars = s.chars().peekable();
        
        let is_neg = chars.peek() == Some(&&'-');
        if is_neg { chars.next().unwrap(); }

        let mut result = BigInt::new();

        for x in chars {
            let digit = x as u8 - b'0';
            assert!(digit < 2);
            result = (result << 1) + BigInt::from(digit as u32);
        }

        result * BigInt::from(if is_neg { -1 } else { 1 })
    }

    pub fn from_decimal(s: &str) -> Self {
        let mut chars = s.chars().peekable();
        
        let is_neg = chars.peek() == Some(&&'-');
        if is_neg { chars.next().unwrap(); }

        let mut result = BigInt::new();

        for x in chars {
            let digit = x as u8 - b'0';
            assert!(digit < 10);
            result = result * BigInt::from(10) + BigInt::from(digit as u32);
        }

        result * BigInt::from(if is_neg { -1 } else { 1 })
    }

    pub fn from_hexadecimal(s: &str) -> Self {
        let mut chars = s.chars().peekable();
        
        let is_neg = chars.peek() == Some(&&'-');
        if is_neg { chars.next().unwrap(); }

        let mut result = BigInt::new();

        for x in chars {
            let digit = match x {
                x @ '0' ..= '9' => { x as u8 - b'0' },
                x @ 'a' ..= 'f' => { x as u8 - b'a' + 10 },
                x @ 'A' ..= 'F' => { x as u8 - b'A' + 10 },
                _ => panic!("invalid hex digit")
            };
            result = (result * BigInt::from(16)) + BigInt::from(digit as u32);
            //dbg!(&result);
        }

        result * BigInt::from(if is_neg { -1 } else { 1 })
    }

    pub fn from_octal(s: &str) -> Self {
        let mut chars = s.chars().peekable();
        
        let is_neg = chars.peek() == Some(&&'-');
        if is_neg { chars.next().unwrap(); }

        let mut result = BigInt::new();

        for x in chars {
            let digit = x as u8 - b'0';
            assert!(digit < 8);
            result = (result << 3) + BigInt::from(digit as u32);
        }

        result * BigInt::from(if is_neg { -1 } else { 1 })
    }
}
