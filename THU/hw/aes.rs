use std::io;
let block_size_byte: u8 = 16;

fn main() {
    println!("AES Lab");
    let mut plaintext = String::new();
    println!("Please enter the plaintext:");

    io::stdin()
        .read_line(&mut plaintext)
        .expect("Failed to read plaintext");

    let mut plain_bytes: Vec<u8> = plaintext.into_bytes();
    let mut padding_size = plain_bytes.len() % block_size_byte;
    if padding_size != 0
        padding_size = block_size_byte - padding_size;
    plain_bytes.expand();

    println!("Plaintext = {:?}", plaintext);
}
