// [ref] https://github.com/openssl/openssl/blob/master/crypto/bn/bn_prime.c
// [ref] https://www.openssl.org/docs/man1.0.2/man3/BN_generate_prime.html
// [ref] https://docs.rs/rug/1.12.0/rug/struct.Integer.html
// [ref] https://carol-nichols.com/2017/04/20/rust-profiling-with-dtrace-on-osx/

// 3rd part dependencies:
// 1. random
// 2. hash
// 3. rug (GNU GMP) for high precision arithmetic operations
//
// Note that we can naively implement mul and div (and pow_mod) manually
// using long multiplication and long devide.
// However, it's VERY SLOW!!
// And the speed up tricks are difficult to implement and trivial.
// More details plz refer to the GMP project.
use rand::random;
use std::hash::Hash;
use std::hash::Hasher;
use std::collections::hash_map::DefaultHasher;
use rug::rand::{RandGen, RandState};
use rug::Integer;
use rug::integer::IsPrime;
use rug::Assign;
use rug::ops::PowAssign;

mod primes;
use crate::rsa::primes::PRIMES;
use crate::aes::title;

// In my implementation, the length of RSA keys are fixed to 2048
// which is the most popular and secure kind of RSA.
// One can easily adapt this to RSA-4096 (more time-consuming) or RSA-1024 (insecure).
const BIT_SIZE: u32 = 2048;
// [deprecated] Zero padding
// PAD BYTE: 0x00
// const PAD_BYTE: u8 = 0;

struct SimpleGenerator {
    seed: u64,
}

impl RandGen for SimpleGenerator {
    fn gen(&mut self) -> u32 {
        random::<u32>()
    }
    fn seed(&mut self, seed: &Integer) {
        self.seed = seed.to_u64_wrapping();
    }
}

fn generate_number(bit_size: u32) -> Integer {
    let mut gen = SimpleGenerator { seed: 2014 };
    // Because use RandState::new() to generate random numbers directly will
    // get the same number every time.
    // I guess this is a bug of rug.
    // So I decided to use custom randstate to workaround this.
    let mut rng = RandState::new_custom(&mut gen);
    let mut big_number = Integer::from(Integer::random_bits(bit_size, &mut rng));
    // odd number
    &mut big_number.set_bit(0, true);
    &mut big_number.set_bit(bit_size - 1, true);

    big_number
}

/**
 * Main idea in openSSL (RSA-2048):
 * 1. generate a big odd number `rnd` (BN_priv_rand_ex)
 * 2. (trial_division) calculate `rnd` moded by the first 384 primes starting from 2.
 * 3. if any mod result equal to 0 (i.e., gcd(rnd, prime) == 1),
 * then this `rnd` failed to test, `rnd` += delta (i.e., 2) and goto step 2
 * to test the new `rnd`. Otherwise continue to 4.
 * 4. (ossl_bn_miller_rabin_is_prime) repeat 64 Miller-Rabin probabilistic primality checks,
 * this yields a false positive rate of at most 2^{-64} for random input.
 */
fn generate_prime(method: &str) -> Integer {
    'generate: loop {
        // p, q are both 1024 bit
        let mut rnd = generate_number(BIT_SIZE / 2);
        let mut delta = 0;
        // for 2048 bits, more details refer to bn_prime.c#L74:12
        let trial_divisions = 384;
        // According to Prime number theorem
        // probability of primes in natural numbers is about 1/log(2^{2048}) \approx 1/1418
        let maxdelta = 1418 * 3;
        let mut mods = vec![0u32; trial_divisions];
        // refer to bn_mr_min_checks in bn_prime.c#L94:12
        let checks = 64;
        for i in 1..trial_divisions {
            mods[i] = Integer::from(&rnd % PRIMES[i]).to_u32().unwrap();
        }

        'trial: loop {
            for i in 1..trial_divisions {
                if (mods[i] + delta) % PRIMES[i] == 0 {
                    // try to test new rnd with rnd += 2
                    delta += 2;
                    if delta > maxdelta {
                        continue 'generate;
                    }
                    // println!("trial_divisions w/ {} failed, plus 2 and retry",
                    //          PRIMES[i]);
                    continue 'trial;
                }
            }
            break 'trial;
        }
        rnd += delta;
        if rnd.significant_bits() != BIT_SIZE / 2 {
            continue 'generate;
        } else if method == "miller_rabin" && !miller_rabin_test(&rnd, checks, false) {
            // println!("Miller-Rabin test failed, regenerate");
            continue 'generate;
        } else if method == "baillie_psw" && !baillie_psw(&rnd, true) {
            continue 'generate;
        } else if method == "build_in" && (&rnd).is_probably_prime(checks as u32) == IsPrime::No {
            continue 'generate;
        } else {
            return rnd;
        }
    }
}

/**
 * [ref] https://en.wikipedia.org/wiki/Jacobi_symbol
 * [ref] https://primes.utm.edu/glossary/page.php?sort=JacobiSymbol
 * a: integer
 * n: positive odd integer
 */
fn jacobi(a_ro: &Integer, n_ro: &Integer) -> i8 {
    let mut j: i8 = 1;
    let mut a = Integer::from(a_ro);
    let mut n = Integer::from(n_ro);
    a %= &n;
    while a != 0 {
        while a.is_even() {
            a /= 2;
            let k = (&n).mod_u(8);
            if k == 3u32 || k == 5u32 {
                j = -j;
            }
        }
        // interchange(a,n)
        a += &n;
        n *= -1;
        n += &a;
        a -= &n;
        if a.mod_u(4) == 3 && n.mod_u(4) == 3 {
            j = -j;
        }
        a %= &n;
    }
    if &n == &1 {
        return j;
    } else {
        return 0;
    }
}

/**
 * Baillie–PSW primality test
 * [ref] https://en.wikipedia.org/wiki/Baillie%E2%80%93PSW_primality_test
 * Let n be the odd positive integer that we wish to test for primality.
 * 1. Optionally, perform trial division to check if n is divisible by a small prime number
 * less than some convenient limit.
 * 2. Perform a base 2 strong probable prime test. If n is not a strong probable prime
 * base 2, then n is composite; quit.
 * 3. Find the first D in the sequence 5, −7, 9, −11, 13, −15, ... for which the Jacobi
 * symbol (D/n) is −1. Set P = 1 and Q = (1 − D) / 4.
 * 4. Perform a strong Lucas probable prime test on n using parameters D, P, and Q. If
 * n is not a strong Lucas probable prime, then n is composite. Otherwise, n is almost certainly prime.
 */
fn baillie_psw(n: &Integer, with_mr: bool) -> bool {
    // (step 2)
    if with_mr && !miller_rabin_test(n, 1, true) {
        return false;
    }
    // (step 3)
    // If n is a perfect square, then step 3 will never yield a D with (D/n) = −1; this is
    // not a problem because perfect squares are easy to detect using Newton's method for
    // square roots. If step 3 fails to produce a D quickly, one should check whether n is a
    // perfect square.
    if n.is_perfect_square() {
        return false;
    }
    let mut d = Integer::from(5);
    while jacobi(&d, n) != -1 {
        d += if d > 0 { 2 } else { -2 };
        d *= -1;
    }
    d %= n;
    // (step 4)
    // [ref] https://en.wikipedia.org/wiki/Lucas_pseudoprime#Implementing_a_Lucas_probable_prime_test
    // let p = 1;
    // let q = Integer::from((1 - d) / 4);
    // V_1 = P = 1
    let mut v = Integer::from(1);
    // U_1 = 1
    let mut u = Integer::from(1);
    // the first (highest) bit must be 1
    // Calculate U_{n+1}
    for bit in Integer::from(n + 1u8).to_string_radix(2).get(1..).unwrap().chars() {
        // double the subscript from k to 2k, aka., left shift, e.g., 11 => 110
        let mut t = Integer::from(&u);
        t.pow_assign(2);
        // (!!) we must implement this very carefully. Keep the modulo properties in mind!
        // (!!) 1. (a + b) mod n = [(a mod n) + (b mod n)] mod n
        // (!!) 2. ab mod n = (a mod n)(b mod n) mod n
        u *= &v;
        u %= n;
        t *= &d;
        v.pow_assign(2);
        v += &t;
        if v.is_odd() {
            v += n;
        }
        v /= 2;
        v %= n;
        if bit == '1' {
            // increase the subscript by 1, e.g., 110 => 111
            t.assign(&u + &v);
            if t.is_odd() {
                t += n;
            }
            t /= 2;
            u *= &d;
            v += &u;
            if v.is_odd() {
                v += n;
            }
            v /= 2;
            v %= n;
            u.assign(&t % n);
        }
    }
    if &u == &0 {
        // U_{n+1} = 0 (mod n)
        return true
    } else {
        return false
    }
}

/**
 * The Miller-Rabin test extends the ideas from the Fermat test.
 * Fermat test will 100% fail for Carmichael numbers.
 * The Miller-Rabin test checks if number is *strong probable* prime.
 */
fn miller_rabin_test(rnd: &Integer, iteration: u8, base_2: bool) -> bool {
    let n = Integer::from(rnd);
    // (step 1) write n as 2^r*d + 1 with d odd (by factoring out
    // powers of 2 from n − 1)
    let mut d: Integer = Integer::from(&n - 1);
    let mut r = 0;
    while !(&d.get_bit(0)) {
        d >>= 1;
        r += 1;
    }

    let mut rng = RandState::new();
    // println!("r={}", r);
    'witness_loop: for _ in 0..iteration {
        let mut n_sub = Integer::from(&n - 2u8);
        let a = if base_2 {
            Integer::from(2)
        } else {
            Integer::from(n_sub.random_below_ref(&mut rng)) + 2u8
        };
        // (step 2) x = a^d mod n
        let mut x = a.pow_mod(&d, &n).unwrap();
        // println!("step 2 finish");
        // (step 3)
        n_sub += 1u8;
        if &x == &1 || &x == &n_sub {
            continue 'witness_loop;
        }
        // (step 4)
        for _ in 1..r {
            x = x.pow_mod(&Integer::from(2), &n).unwrap();
            if &x == &n_sub {
                continue 'witness_loop;
            }
        }
        // println!("step 4 finish");
        // composite
        return false;
    }
    // probably prime
    true
}

/**
 * [ref] https://cp-algorithms.com/algebra/module-inverse.html
 * Complexity:
 * 1. extend_gcd: O(2 \log_{10}(\phi(n))) divide operators on average
 * 2. binary exponentiation: O(\log(\phi(n))) mul operators if \phi(n) is prime.
 *    However, most of \phi(n) is not prime.
 */
fn modular_inverse(e: &Integer, phi_n: &Integer, method: &str) -> Integer {
    if method == "extend_gcd" {
        // [ref] https://en.wikipedia.org/wiki/Extended_Euclidean_algorithm#Modular_integers
        let mut t = Integer::from(0);
        let mut newt = Integer::from(1);
        let mut r = Integer::from(phi_n);
        let mut newr = Integer::from(e);
        let mut quotient = Integer::new();
        let mut tmp = Integer::new();
        while newr.significant_bits() != 0 {
            quotient.assign(&r / &newr);
            // (t, newt) := (newt, t − quotient × newt)
            tmp.assign(&quotient * &newt);
            tmp *= -1;
            tmp += &t;
            t.assign(&newt);
            newt.assign(&tmp);
            // (r, newr) := (newr, r − quotient × newr)
            tmp.assign(&quotient * &newr);
            tmp *= -1;
            tmp += &r;
            r.assign(&newr);
            newr.assign(&tmp);
        }
        if r > 1 {
            panic!("e is not invertible!");
        }
        if t < 0 {
            t += phi_n;
        }
        return t;
    } else if method == "binary_exp" {
        // if phi_n is prime, according to Fermat's little theorem,
        // d can be easily calculated using binary exp,
        // i.e. e^{-1} = a^{\phi(n) - 2} (mod \phi(n))
        if baillie_psw(phi_n, true) {
            println!("phi_n is prime, use binary_exp");
            return Integer::from((&e).pow_mod_ref(
                    &Integer::from(phi_n - 2), &phi_n).unwrap())
        } else {
            return Integer::from((&e).invert_ref(&phi_n).unwrap());
        }
    } else if method == "build_in" {
        return Integer::from((&e).invert_ref(&phi_n).unwrap());
    } else {
        panic!("wrong method!");
    }
}

/**
 * Must be private method, because \phi_n is the secret.
 */
fn rsa_key_phase1() -> (Integer, Integer, Integer) {
    let p = generate_prime("miller_rabin");
    let q = generate_prime("miller_rabin");
    let n = Integer::from(&p * &q);
    // \phi(n) = (p-1)(q-1), \phi is Euler's totient function
    let phi_n = (p - 1) * (q - 1);
    // choose e s.t. 1 \le e \le \phi(n) and gcd(e, \phi(n)) = 1
    // i.e., e and \phi(n) are coprime;
    // e having a short bit-length and small Hamming weight results in more
    // efficient encryption - most commonly 0x10001 = 65537. However, small values of
    // e (such as e=3) have been shown to be less secure in some settings.
    let e = Integer::from(65537);

    (n, phi_n, e)
}

/**
 * Must be private method, because d is the secret.
 */
fn rsa_key_pair(method: &str) -> (Integer, Integer, Integer) {
    let (n, phi_n, e) = rsa_key_phase1();
    // de = 1 (mod \phi(n))
    let d = modular_inverse(&e, &phi_n, method);

    // public key: (n, e)
    // private key: (n, d)
    (n, e, d)
}

/**
 * Naive algo to calculate `m^e mod n`
 * O(2 log (e)) mul operators in worst case
 */
fn quick_pow_mod(mut m: Integer, e: &Integer, n: &Integer) -> Integer {
    // e >= 1
    m %= n;
    let mut ans = Integer::from(1);
    let mut e_curr = Integer::from(e);
    let two = Integer::from(2);
    while e_curr.significant_bits() != 0 {
        if e_curr.get_bit(0) {
            ans *= &m;
            ans %= n;
        }
        m.pow_mod_mut(&two, n).unwrap();
        e_curr >>= 1;
    }

    ans
}

fn rsa_encrypt(key: (&Integer, &Integer), plaintext: &str) -> Vec<u8> {
    let (n, e)  = key;
    // log_2 n
    let block_size = (n.significant_bits() as f64 / 8.0).ceil() as usize - 1;
    println!("block_size: {}", block_size);
    // PKCS#7
    // TODO(DCMMC) only supprt RSA <= 2048. For RSA 4096, refer to OEAP.
    // [ref] https://en.wikipedia.org/wiki/Padding_(cryptography)#PKCS#5_and_PKCS#7
    let mut padded_bytes: Vec<u8> = plaintext.to_string()
            .into_bytes();
    let mut cipher: Vec<u8> = Vec::new();
    let mut pad_size = padded_bytes.len() % block_size;
    pad_size = block_size - pad_size;
    assert!(pad_size <= 256);
    // right padding, e.g., 0x2021 => 0x2021[1][1] (padding_size=2)
    padded_bytes.extend(vec![(pad_size - 1) as u8; pad_size]);
    println!("padding_size={}", pad_size);
    for block in padded_bytes.chunks_mut(block_size) {
        println!("block={:?}", block);
        let mut num_str = block.iter()
            .map(|x| format!("{:02X}", x))
            .fold(String::from(""),
                  |res: String, curr: String| res + &curr
            );
        num_str = Integer::from_str_radix(&num_str, 16).unwrap().to_string_radix(16);
        let mut m = Integer::from(Integer::parse_radix(
            num_str, 16).unwrap());
        // NOTE: m is 255-byte block, however c is 256-byte block
        m = quick_pow_mod(m, e, n);
        // m.pow_mod_mut(e, n).unwrap();
        // len(c) may less than len(m)
        let mut digits = m.to_string_radix(16);
        for _ in 0..((block_size + 1) * 2 - digits.len()) {
            digits.insert(0, '0');
        }
        for idx in 0..(block_size + 1) {
            cipher.push(u8::from_str_radix(&digits[(idx*2)..(idx*2+2)], 16).unwrap());
        }
    }

    cipher
}

fn rsa_decrypt(key: (&Integer, &Integer), cipher: &Vec<u8>) -> String {
    title("decryption");
    let (n, d) = key;
    let block_size = (n.significant_bits() as f64 / 8.0).ceil() as usize - 1;
    let chunks = cipher.chunks(block_size + 1);
    let num_chunks = chunks.len();
    let mut plain = String::new();
    for (idx, block) in chunks.enumerate() {
        let num_str = block.iter()
            .map(|x| format!("{:02X}", x))
            .fold(String::from(""),
                  |res: String, curr: String| res + &curr
            );
        let mut c = Integer::from(Integer::parse_radix(
            num_str, 16).unwrap());
        c = quick_pow_mod(c, d, n);
        // c.pow_mod_mut(d, n).unwrap();

        let mut digits = c.to_string_radix(16);
        for _ in 0..(block_size * 2 - digits.len()) {
            digits.insert(0, '0');
        }
        let mut c_block = vec![0u8; block_size];
        for idx in 0..block_size {
            c_block[idx] = u8::from_str_radix(&digits[(idx*2)..(idx*2+2)], 16).unwrap();
        }

        if idx == (num_chunks - 1) {
            let pad_byte: u8 = c_block[c_block.len() - 1];
            let mut pad_cnt: i16 = pad_byte as i16 + 1i16;
            while c_block.len() > 0 && c_block[c_block.len() - 1] == pad_byte && pad_cnt > 0 {
                c_block.pop();
                pad_cnt -= 1;
            }
            println!("unpad:");
            let blocks = c_block.chunks(block_size);
            for (idx, b) in blocks.enumerate() {
                println!("Block {}:", idx + 1);
                for c in b {
                    print!("{:02X} ", c);
                }
                println!("\n");
            }
        }
        plain.push_str(&String::from_utf8(c_block).unwrap());
    }

    plain
}

/**
 * Return:
 * (n, e, cipher, sign)
 */
fn rsa_sign(plaintext: &str) -> (Integer, Integer, Vec<u8>, String) {
    let (n, e, d) = rsa_key_pair("extend_gcd");
    // In practice, sender should use the public key of the receiver to
    // encrypt the message.
    let cipher = rsa_encrypt((&n, &d), plaintext);
    let mut hasher = DefaultHasher::new();
    plaintext.hash(&mut hasher);
    let hash: u64 = hasher.finish();
    // use private key to encrypt the hash of plain message
    let sign = quick_pow_mod(Integer::from(hash), &d, &n).to_string_radix(16);

    (n, e, cipher, sign)
}

fn rsa_check_sign(cipher: &Vec<u8>, n: &Integer, e: &Integer,
                  sign: &str, plain: &str) -> bool {
    let decrypted = rsa_decrypt((n, e), cipher);
    assert!(decrypted == plain);
    let mut hasher = DefaultHasher::new();
    decrypted.hash(&mut hasher);
    let hash: u64 = hasher.finish();
    // use public key of the sender to decrypt the hash of the decrypted message
    let hash_in_sign: u64 = quick_pow_mod(
        Integer::from(Integer::parse_radix(sign, 16).unwrap()),
        e, n
    ).to_u64().unwrap();
    println!("hash: {},\nsign_in_sign: {}", hash, hash_in_sign);

    hash == hash_in_sign
}

pub fn test_all_internal() -> () {
    title("Test Internals");
    print!("test jacbi..");
    assert!(jacobi(&Integer::from(1001), &Integer::from(9907)) == -1);
    assert!(jacobi(&Integer::from(19), &Integer::from(45)) == 1);
    assert!(jacobi(&Integer::from(8), &Integer::from(21)) == -1);
    assert!(jacobi(&Integer::from(5), &Integer::from(21)) == 1);
    println!(" passed");
    // 1159 = 19 * 61 is a lucas pseudoprime
    // [ref] https://stackoverflow.com/a/38352706
    print!("test baillie_psw..");
    assert!(baillie_psw(&Integer::from(113), true));
    assert!(baillie_psw(&Integer::from(19), true));
    assert!(baillie_psw(&Integer::from(1159), false));
    assert!(baillie_psw(&Integer::from(17389), true));
    println!(" passed");

    // recommend: run 4096 tests
    let mut checks = 256;
    println!("test PSW and MR with {} random numbers..", checks);
    for i in 1..=checks {
        let n = generate_number(BIT_SIZE);
        let r = n.is_probably_prime(64) != IsPrime::No;
        assert!(r == baillie_psw(&n, true));
        assert!(r == miller_rabin_test(&n, 64, false));
        if i % 256 == 0 {
            println!("test {:?} passed.", i);
        }
    }

    checks = 8;
    println!("test modular_inverse with {} random pairs..", checks);
    assert!(modular_inverse(&Integer::from(5), &Integer::from(9),
        "extend_gcd") == 2);

    for i in 1..=checks {
        let (_, phi_n, e) = rsa_key_phase1();
        // de = 1 (mod \phi(n))
        let mut d = modular_inverse(&e, &phi_n, "extend_gcd");
        let mut de = Integer::from(&e * &d);
        de %= &phi_n;
        assert!(de == 1);
        d = modular_inverse(&e, &phi_n, "binary_exp");
        de.assign(&e * &d);
        de %= &phi_n;
        assert!(de == 1);
        if i % (checks / 4) == 0 {
            println!("test {:?} passed.", i);
        }
    }

    print!("test quick_pow_mod...");
    let mut m = Integer::from(3);
    m = quick_pow_mod(m, &Integer::from(3), &Integer::from(15));
    assert!(m == 12);
    m = Integer::from(2);
    m = quick_pow_mod(m, &Integer::from(4), &Integer::from(15));
    assert!(m == 1);
    println!(" passed");

    // test padding
    // test_t2_t3(&String::from_utf8(vec![0; 255]).unwrap());
    // test_t2_t3(&String::from_utf8(vec![0; 256]).unwrap());
    // test_t2_t3(&String::from_utf8(vec![1; 256]).unwrap());

    println!("All passed\n\n");
}

fn test_t2_t3(plaintext: &str) -> () {
    title("Test T2 & T3");
    let (n, e, d) = rsa_key_pair("extend_gcd");
    let cipher = rsa_encrypt((&n, &e), plaintext);
    println!("RSA encryption of {}", plaintext);
    let block_size = (n.significant_bits() as f64 / 8.0).ceil() as usize - 1;
    let blocks = cipher.chunks(block_size + 1);
    for (idx, b) in blocks.enumerate() {
        println!("Block {}:", idx + 1);
        for c in b {
            print!("{:02X} ", c);
        }
        println!("\n");
    }
    let decrypted = rsa_decrypt((&n, &d), &cipher);
    println!("RSA decryption: {}", decrypted);
    assert!(plaintext == decrypted);
}

fn test_t4(plain: &str) -> () {
    title("Test T4");
    let (n, e, cipher, sign) = rsa_sign(plain);
    println!("cipher of {}:\n", plain);

    let block_size = (n.significant_bits() as f64 / 8.0).ceil() as usize - 1;
    let blocks = cipher.chunks(block_size + 1);
    for (idx, b) in blocks.enumerate() {
        println!("Block {}:", idx + 1);
        for c in b {
            print!("{:02X} ", c);
        }
        println!("\n");
    }
    println!("sign:\n{}\n", &sign);

    let check_res = rsa_check_sign(&cipher, &n, &e, &sign, plain);
    println!("Check result by the receiver: {}", check_res);
}

pub fn test_rsa() -> () {
    test_all_internal();
    // test T1
    let (n, e, d) = rsa_key_pair("extend_gcd");
    println!("Generate RSA key pair:\nn={},\ne={},\nd={}\n\n", &n, &e, &d);
    test_t2_t3("Cryptography and Network Security; 2020214245; 肖文韬 (Wentao Xiao) 🎉🚀");
    test_t4("Cryptography and Network Security; 2020214245; 肖文韬 (Wentao Xiao) 🎉🚀");
}
//~
