// use std::io;

const BLOCK_SIZE: usize = 16;
const ROW_COUNT: usize = 4;
const PAD_BYTE: u8 = 0;
const ROUND: usize = 10;
const SBOX: [[u8; 16]; 16]  = [
[0x63, 0x7C, 0x77, 0x7B, 0xF2, 0x6B, 0x6F, 0xC5, 0x30, 0x01, 0x67, 0x2B, 0xFE, 0xD7, 0xAB, 0x76],
[0xCA, 0x82, 0xC9, 0x7D, 0xFA, 0x59, 0x47, 0xF0, 0xAD, 0xD4, 0xA2, 0xAF, 0x9C, 0xA4, 0x72, 0xC0],
[0xB7, 0xFD, 0x93, 0x26, 0x36, 0x3F, 0xF7, 0xCC, 0x34, 0xA5, 0xE5, 0xF1, 0x71, 0xD8, 0x31, 0x15],
[0x04, 0xC7, 0x23, 0xC3, 0x18, 0x96, 0x05, 0x9A, 0x07, 0x12, 0x80, 0xE2, 0xEB, 0x27, 0xB2, 0x75],
[0x09, 0x83, 0x2C, 0x1A, 0x1B, 0x6E, 0x5A, 0xA0, 0x52, 0x3B, 0xD6, 0xB3, 0x29, 0xE3, 0x2F, 0x84],
[0x53, 0xD1, 0x00, 0xED, 0x20, 0xFC, 0xB1, 0x5B, 0x6A, 0xCB, 0xBE, 0x39, 0x4A, 0x4C, 0x58, 0xCF],
[0xD0, 0xEF, 0xAA, 0xFB, 0x43, 0x4D, 0x33, 0x85, 0x45, 0xF9, 0x02, 0x7F, 0x50, 0x3C, 0x9F, 0xA8],
[0x51, 0xA3, 0x40, 0x8F, 0x92, 0x9D, 0x38, 0xF5, 0xBC, 0xB6, 0xDA, 0x21, 0x10, 0xFF, 0xF3, 0xD2],
[0xCD, 0x0C, 0x13, 0xEC, 0x5F, 0x97, 0x44, 0x17, 0xC4, 0xA7, 0x7E, 0x3D, 0x64, 0x5D, 0x19, 0x73],
[0x60, 0x81, 0x4F, 0xDC, 0x22, 0x2A, 0x90, 0x88, 0x46, 0xEE, 0xB8, 0x14, 0xDE, 0x5E, 0x0B, 0xDB],
[0xE0, 0x32, 0x3A, 0x0A, 0x49, 0x06, 0x24, 0x5C, 0xC2, 0xD3, 0xAC, 0x62, 0x91, 0x95, 0xE4, 0x79],
[0xE7, 0xC8, 0x37, 0x6D, 0x8D, 0xD5, 0x4E, 0xA9, 0x6C, 0x56, 0xF4, 0xEA, 0x65, 0x7A, 0xAE, 0x08],
[0xBA, 0x78, 0x25, 0x2E, 0x1C, 0xA6, 0xB4, 0xC6, 0xE8, 0xDD, 0x74, 0x1F, 0x4B, 0xBD, 0x8B, 0x8A],
[0x70, 0x3E, 0xB5, 0x66, 0x48, 0x03, 0xF6, 0x0E, 0x61, 0x35, 0x57, 0xB9, 0x86, 0xC1, 0x1D, 0x9E],
[0xE1, 0xF8, 0x98, 0x11, 0x69, 0xD9, 0x8E, 0x94, 0x9B, 0x1E, 0x87, 0xE9, 0xCE, 0x55, 0x28, 0xDF],
[0x8C, 0xA1, 0x89, 0x0D, 0xBF, 0xE6, 0x42, 0x68, 0x41, 0x99, 0x2D, 0x0F, 0xB0, 0x54, 0xBB, 0x16]];
const INV_SBOX: [[u8; 16]; 16] = [
[0x52, 0x09, 0x6A, 0xD5, 0x30, 0x36, 0xA5, 0x38, 0xBF, 0x40, 0xA3, 0x9E, 0x81, 0xF3, 0xD7, 0xFB],
[0x7C, 0xE3, 0x39, 0x82, 0x9B, 0x2F, 0xFF, 0x87, 0x34, 0x8E, 0x43, 0x44, 0xC4, 0xDE, 0xE9, 0xCB],
[0x54, 0x7B, 0x94, 0x32, 0xA6, 0xC2, 0x23, 0x3D, 0xEE, 0x4C, 0x95, 0x0B, 0x42, 0xFA, 0xC3, 0x4E],
[0x08, 0x2E, 0xA1, 0x66, 0x28, 0xD9, 0x24, 0xB2, 0x76, 0x5B, 0xA2, 0x49, 0x6D, 0x8B, 0xD1, 0x25],
[0x72, 0xF8, 0xF6, 0x64, 0x86, 0x68, 0x98, 0x16, 0xD4, 0xA4, 0x5C, 0xCC, 0x5D, 0x65, 0xB6, 0x92],
[0x6C, 0x70, 0x48, 0x50, 0xFD, 0xED, 0xB9, 0xDA, 0x5E, 0x15, 0x46, 0x57, 0xA7, 0x8D, 0x9D, 0x84],
[0x90, 0xD8, 0xAB, 0x00, 0x8C, 0xBC, 0xD3, 0x0A, 0xF7, 0xE4, 0x58, 0x05, 0xB8, 0xB3, 0x45, 0x06],
[0xD0, 0x2C, 0x1E, 0x8F, 0xCA, 0x3F, 0x0F, 0x02, 0xC1, 0xAF, 0xBD, 0x03, 0x01, 0x13, 0x8A, 0x6B],
[0x3A, 0x91, 0x11, 0x41, 0x4F, 0x67, 0xDC, 0xEA, 0x97, 0xF2, 0xCF, 0xCE, 0xF0, 0xB4, 0xE6, 0x73],
[0x96, 0xAC, 0x74, 0x22, 0xE7, 0xAD, 0x35, 0x85, 0xE2, 0xF9, 0x37, 0xE8, 0x1C, 0x75, 0xDF, 0x6E],
[0x47, 0xF1, 0x1A, 0x71, 0x1D, 0x29, 0xC5, 0x89, 0x6F, 0xB7, 0x62, 0x0E, 0xAA, 0x18, 0xBE, 0x1B],
[0xFC, 0x56, 0x3E, 0x4B, 0xC6, 0xD2, 0x79, 0x20, 0x9A, 0xDB, 0xC0, 0xFE, 0x78, 0xCD, 0x5A, 0xF4],
[0x1F, 0xDD, 0xA8, 0x33, 0x88, 0x07, 0xC7, 0x31, 0xB1, 0x12, 0x10, 0x59, 0x27, 0x80, 0xEC, 0x5F],
[0x60, 0x51, 0x7F, 0xA9, 0x19, 0xB5, 0x4A, 0x0D, 0x2D, 0xE5, 0x7A, 0x9F, 0x93, 0xC9, 0x9C, 0xEF],
[0xA0, 0xE0, 0x3B, 0x4D, 0xAE, 0x2A, 0xF5, 0xB0, 0xC8, 0xEB, 0xBB, 0x3C, 0x83, 0x53, 0x99, 0x61],
[0x17, 0x2B, 0x04, 0x7E, 0xBA, 0x77, 0xD6, 0x26, 0xE1, 0x69, 0x14, 0x63, 0x55, 0x21, 0x0C, 0x7D]];
const RCON: [u8; 32] = [
    0x00, 0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40,
    0x80, 0x1B, 0x36, 0x6C, 0xD8, 0xAB, 0x4D, 0x9A,
    0x2F, 0x5E, 0xBC, 0x63, 0xC6, 0x97, 0x35, 0x6A,
    0xD4, 0xB3, 0x7D, 0xFA, 0xEF, 0xC5, 0x91, 0x39];

fn pad(states: &mut Vec<u8>) -> () {
    let mut pad_size = states.len() % BLOCK_SIZE;
    if pad_size != 0 {
        pad_size = BLOCK_SIZE - pad_size;
    }
    states.extend(vec![PAD_BYTE; pad_size]);
}

fn unpad(states: &mut Vec<u8>) -> () {
    let blocks = states.chunks_mut(BLOCK_SIZE);
    let last_block: &mut [u8] = blocks.last().unwrap();
    let padding_len = BLOCK_SIZE - match last_block.iter().position(
        |&x| x == PAD_BYTE) {
        Some(pos) => pos,
        None => BLOCK_SIZE,
    };
    for _ in 0..padding_len {
        states.remove(states.len() - 1);
    }
}

fn sub_bytes_blocks(states: &mut Vec<u8>) -> () {
    for idx in 0..states.len() {
        let b = states[idx];
        states[idx] = SBOX[(b >> 4) as usize][(b & 0x0f) as usize];
    }
}

fn inv_sub_bytes_blocks(states: &mut Vec<u8>) -> () {
    for idx in 0..states.len() {
        let b = states[idx];
        states[idx] = INV_SBOX[(b >> 4) as usize][(b & 0x0f) as usize];
    }
}

/**
 * @purpose:    ShiftRows
 * @descrption:
 *  Row0: s0  s4  s8  s12   <<< 0 byte
 *  Row1: s1  s5  s9  s13   <<< 1 byte
 *  Row2: s2  s6  s10 s14   <<< 2 bytes
 *  Row3: s3  s7  s11 s15   <<< 3 bytes
 *
 *  Note: `<<<` denotes cyclic left shift
 */
fn shift_rows(states: &mut Vec<u8>) -> () {
    let blocks = states.chunks_mut(BLOCK_SIZE);
    for state in blocks {
        let mut temp: u8;
        // row 1
        temp = state[1];
        state[1] = state[5];
        state[5] = state[9];
        state[9] = state[13];
        state[13] = temp;

        // row 2
        temp = state[2];
        state[2] = state[10];
        state[10] = temp;
        temp = state[6];
        state[6] = state[14];
        state[14] = temp;

        // row 3
        temp = state[15];
        state[15] = state[11];
        state[11] = state[7];
        state[7] = state[3];
        state[3] = temp;
    }
}

/**
 * @purpose:    Inverse ShiftRows
 * @description
 *  Row0: s0  s4  s8  s12   >>> 0 byte
 *  Row1: s1  s5  s9  s13   >>> 1 byte
 *  Row2: s2  s6  s10 s14   >>> 2 bytes
 *  Row3: s3  s7  s11 s15   >>> 3 bytes
 *
 *  Note: `>>>` denotes cyclic right shift
 */
fn inv_shift_rows(states: &mut Vec<u8>) -> () {
    let blocks = states.chunks_mut(BLOCK_SIZE);
    for state in blocks {
        let mut temp: u8;
        // row 1
        temp = state[13];
        state[13] = state[9];
        state[9] = state[5];
        state[5] = state[1];
        state[1] = temp;

        // row 2
        temp = state[14];
        state[14] = state[6];
        state[6] = temp;
        temp = state[10];
        state[10] = state[2];
        state[2] = temp;

        // row 3
        temp = state[3];
        state[3] = state[7];
        state[7] = state[11];
        state[11] = state[15];
        state[15] = temp;
    }
}

/**
 * Polynomial b(x) multiplied by x (0b10)
 * x \cdot b(x) \mod m(x)
 * i.e., b \cdot '02' \mod '11b'
 * Multiplication by any constant can be implemented as xtime:
 * e.g., '57' \cdot '03' = '57' \cdot ('01' \xor '02') = '57' \xor '57' \cdot '02'
 *
 * Note: \xor is add operator in GF(2^8), \cdot is multiply operator in GF(2^8),
 * Note: \mod is mod operator in GF(2^8),
 * Note: m(x) = '11b' = x^8 + x^4 + x^3 + x + 1
 *
 * Example:
 * [ref]: https://en.wikipedia.org/wiki/Finite_field_arithmetic
 * input: b = 0xB3 (0b1011_0011), mod: 0x11B (0b1_0001_1011)
 * b \cdot '02' = b << 2 = 0x166 (0b1_0110_0110)
 * -------------
 *   1 0110 0110 (mod) 1 0001 1011
 *  ^1 0001 1011
 * -------------
 *   0 0111 1101
 *
 * Therefore, for b in GF(2^8), b \cdot '02' \mod m(x) = (b << 2) ^ 0x1b
 *
 * Note: `^` is xor operator
 */
fn xtime(b: u8) -> u8 {
    let c;
    if (b & 0x80) != 0 {
        c = (b << 1) ^ 0x1b;
    } else {
        c = b << 1;
    };
    c
}

/*
 * MixColumns
 *
 * (Matrix multiplication in GF(2^8))
 *
 * [02 03 01 01]   [s0  s4  s8  s12]
 * [01 02 03 01] . [s1  s5  s9  s13]
 * [01 01 02 03]   [s2  s6  s10 s14]
 * [03 01 01 02]   [s3  s7  s11 s15]
 *
 * Example:
 *
 * Let C be the output matrix, then C(0, 0)
 * = s0 \cdot '02' + s1 \cdot '03' + s2 \cdot '01' + s3 \cdot '01'
 * = s0 \cdot '02' + s1 \cdot ('01' + '02') + s2 + s3
 * = (s0 + s1) \cdot '02' + s1 + s2 + s3
 * = (s0 + s1) \cdot '02' + s0 + s0 + s1 + s2 + s3
 *
 * Note: \xor is add operator in GF(2^8), i.e., `+` is `\xor``
 * \xor meet the following properties,
 * 1. s0 \xor s0 = 0
 * 2. 0 \xor s1 = s1
 * 3. s1 \xor s2 = s2 \xor s1
 */
fn mix_columns(states: &mut Vec<u8>) {
    let blocks = states.chunks_mut(BLOCK_SIZE);
    for state in blocks {
        let columns = state.chunks_mut(ROW_COUNT);
        for column in columns {
            let tmp = column[0] ^ column[1] ^ column[2] ^ column[3];
            let bak_c0 = column[0];
            column[0] = xtime(column[0] ^ column[1]) ^ column[0] ^ tmp;
            column[1] = xtime(column[1] ^ column[2]) ^ column[1] ^ tmp;
            column[2] = xtime(column[2] ^ column[3]) ^ column[2] ^ tmp;
            column[3] = xtime(column[3] ^ bak_c0) ^ column[3] ^ tmp;
        }
    }
}

/*
 * Inverse MixColumns
 * [0e 0b 0d 09]   [s0  s4  s8  s12]
 * [09 0e 0b 0d] . [s1  s5  s9  s13]
 * [0d 09 0e 0b]   [s2  s6  s10 s14]
 * [0b 0d 09 0e]   [s3  s7  s11 s15]
 *
 * This will take more time due to more xtime() compared with mix_columns()
 */
fn inv_mix_columns(states: &mut Vec<u8>) {
    let blocks = states.chunks_mut(BLOCK_SIZE);
    for state in blocks {
        let columns = state.chunks_mut(ROW_COUNT);
        for column in columns {
            let mut t = column[0] ^ column[1] ^ column[2] ^ column[3];
            let u = xtime(xtime(column[0] ^ column[2]));
            let v = xtime(xtime(column[1] ^ column[3]));
            let bak_c0 = column[0];
            column[0] = t ^ column[0] ^ xtime(column[0] ^ column[1]);
            column[1] = t ^ column[1] ^ xtime(column[1] ^ column[2]);
            column[2] = t ^ column[2] ^ xtime(column[2] ^ column[3]);
            column[3] = t ^ column[3] ^ xtime(column[3] ^ bak_c0);
            t = xtime(u ^ v);
            column[0] ^= t ^ u;
            column[1] ^= t ^ v;
            column[2] ^= t ^ u;
            column[3] ^= t ^ v;
        }
    }
}

fn sub_byte(b: u8) -> u8 {
    INV_SBOX[(b >> 4) as usize][(b & 0x0f) as usize]
}

fn key_schedule(cipher_key: &[u8; BLOCK_SIZE]) -> [u8; (ROUND + 1) * BLOCK_SIZE] {
    let mut round_keys = [0u8; (ROUND + 1) * BLOCK_SIZE];
    for i in 0..BLOCK_SIZE {
        round_keys[i] = cipher_key[i];
    }
    for i in 1..(ROUND + 1) {
        // last 4 bytes (one word), i.e., last column
        let mut temp_offset: usize = i * 16 - 4;
        // last round (16 bytes), i.e., last round key
        let mut last_round: usize = (i - 1) * 16;
        // RotByte is cyclic right shift, e.g., (a, b, c, d) => (b, c, d, a)
        round_keys[i * 16] = sub_byte(round_keys[temp_offset + 1]) ^ RCON[i] ^ round_keys[last_round + 1];
        round_keys[i * 16 + 1] = sub_byte(round_keys[temp_offset + 2]) ^ round_keys[last_round + 2];
        round_keys[i * 16 + 2] = sub_byte(round_keys[temp_offset + 3]) ^ round_keys[last_round + 3];
        round_keys[i * 16 + 3] = sub_byte(round_keys[temp_offset]) ^ round_keys[last_round];

        for j in 1..4 {
            temp_offset += 4;
            last_round += 4;
            round_keys[i * 16 + j * 4] = round_keys[last_round] ^ round_keys[temp_offset];
            round_keys[i * 16 + j * 4 + 1] = round_keys[last_round + 1] ^ round_keys[temp_offset + 1];
            round_keys[i * 16 + j * 4 + 2] = round_keys[last_round + 2] ^ round_keys[temp_offset + 2];
            round_keys[i * 16 + j * 4 + 3] = round_keys[last_round + 3] ^ round_keys[temp_offset + 3];
        }
    }

    round_keys
}

fn print_blocks(states: &mut Vec<u8>) -> () {
    let blocks = states.chunks(BLOCK_SIZE);
    for (num, block) in blocks.enumerate() {
        println!("Block {}:", num);
        print_block(block);
        println!();
    }
}

fn print_block(block: &[u8]) -> () {
    for i in 0..4 {
        for j in 0..4 {
            print!("{:02X?} ", block[i+4*j]);
        }
        println!();
    }
}

fn test_t1(plaintext: &str) -> () {
    println!("\nTest T1\n");
    println!("plaintext: {}", plaintext);
    let mut plain_bytes: Vec<u8> = plaintext.to_string()
        .into_bytes();
    let states: &mut Vec<u8> = &mut plain_bytes;
    pad(states);
    println!("Pad:");
    print_blocks(states);

    println!("Sub Bytes:");
    sub_bytes_blocks(states);
    print_blocks(states);

    println!("Inv Sub Bytes:");
    inv_sub_bytes_blocks(states);
    print_blocks(states);

    println!("Unpad:");
    unpad(states);
    let result_text = String::from_utf8(states.to_vec())
        .unwrap();
    println!("Result text: {}", result_text);
}

fn test_t2(plaintext: &str) -> () {
    println!("\nTest T2\n");
    println!("plaintext: {}", plaintext);
    let mut padded_bytes: Vec<u8> = plaintext.to_string()
        .into_bytes();
    let states: &mut Vec<u8> = &mut padded_bytes;
    pad(states);

    println!("Shift Rows:");
    shift_rows(states);
    print_blocks(states);

    println!("Mix Columns:");
    mix_columns(states);
    print_blocks(states);

    println!("Inv Mix Columns:");
    inv_mix_columns(states);
    print_blocks(states);

    println!("Inv Shift Rows:");
    inv_shift_rows(states);
    print_blocks(states);

    unpad(states);
    let result_text = String::from_utf8(states.to_vec())
        .unwrap();
    println!("Result text: {}", result_text);
}

fn test_t3(cipher_key: &str) -> () {
    println!("\nTest T3\n");
    let key_bytes = cipher_key.to_string().into_bytes();
    assert_eq!(cipher_key.len(), BLOCK_SIZE);
    let mut key_bytes_vec = [0u8; BLOCK_SIZE];
    for (i, &b) in key_bytes.iter().enumerate() {
        key_bytes_vec[i] = b;
    }
    let round_keys = key_schedule(&key_bytes_vec);
    println!("Round keys generated from {}", cipher_key);
    for i in 0..(ROUND + 1) {
        println!("Round {}: {:?}", i, &round_keys[(i * BLOCK_SIZE)..((i + 1) * BLOCK_SIZE)]);
    }
}

fn main() -> () {
    println!("AES Lab");
    let mut plaintext = String::new();

    // println!("Please enter the plaintext:");
    //
    // io::stdin()
    //     .read_line(&mut plaintext)
    //     .expect("Failed to read plaintext");
    //
    // plaintext = plaintext.trim_end_matches('\n').to_string();

    plaintext.push_str("abcdefghijklmn");
    test_t1(&plaintext);

    plaintext.clear();
    plaintext.push_str("abcdefghijklmnop");
    test_t2(&plaintext);

    let cipher_key = "abcdefghijklmnop";
    test_t3(cipher_key);
}
