pub fn eq(c: char) -> impl Fn(char) -> bool {
    move |x| x == c
}

pub fn neq(c: char) -> impl Fn(char) -> bool {
    move |x| x != c
}

pub fn eq_any(cs: &'static [char]) -> impl Fn(char) -> bool {
    move |x| cs.iter().any(|&c| x == c)
}
