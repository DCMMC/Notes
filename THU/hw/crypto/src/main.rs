mod aes;
use crate::aes::test_aes;
mod rsa;
use crate::rsa::test_rsa;

fn main() {
    test_aes();
    test_rsa();
}
