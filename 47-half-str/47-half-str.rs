fn get_middle(s: &str) -> &str {
  let len = s.len();
  let from = (len - 1) / 2;
  let to = (len / 2) + 1;
  &s[from .. to]
}
