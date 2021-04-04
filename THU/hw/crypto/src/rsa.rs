// [ref] https://github.com/openssl/openssl/blob/master/crypto/bn/bn_prime.c
// [ref] https://www.openssl.org/docs/man1.0.2/man3/BN_generate_prime.html
// [ref] https://docs.rs/rug/1.12.0/rug/struct.Integer.html
// [ref] https://carol-nichols.com/2017/04/20/rust-profiling-with-dtrace-on-osx/
use rand::random;
use rug::rand::{RandGen, RandState};
use rug::Integer;
use rug::integer::IsPrime;
use rug::Assign;
use rug::ops::PowAssign;
mod primes;
use crate::rsa::primes::PRIMES;

const BIT_SIZE: u32 = 2048;

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
    // 因为如果直接用 RandState::new() 生成的随机数每次都是一摸一样的，估计是软件 bug
    // 所以改用自定义随机数生成器
    let mut rng = RandState::new_custom(&mut gen);
    let mut big_number = Integer::from(Integer::random_bits(bit_size, &mut rng));
    // odd number
    &mut big_number.set_bit(0, true);
    &mut big_number.set_bit(bit_size - 1, true);

    // println!("big_number={:?}", big_number);

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
        let mut rnd = generate_number(BIT_SIZE);
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
        if rnd.significant_bits() != BIT_SIZE {
            continue 'generate;
        } else if method == "miller_rabin" && !miller_rabin_test(&rnd, checks, false) {
            // println!("Miller-Rabin test failed, regenerate");
            continue 'generate;
        } else if method == "baillie_psw" && !baillie_psw(&rnd) {
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
fn baillie_psw(n: &Integer) -> bool {
    // (step 2)
    if !miller_rabin_test(n, 1, true) {
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

pub fn test_all(checks: u32) -> () {
    assert!(jacobi(&Integer::from(1001), &Integer::from(9907)) == -1);
    assert!(jacobi(&Integer::from(19), &Integer::from(45)) == 1);
    assert!(jacobi(&Integer::from(8), &Integer::from(21)) == -1);
    assert!(jacobi(&Integer::from(5), &Integer::from(21)) == 1);
    //  1159 = 19 * 61 is a lucas pseudoprime
    // [ref] https://stackoverflow.com/a/38352706
    assert!(baillie_psw(&Integer::from(113)));
    assert!(baillie_psw(&Integer::from(19)));
    assert!(baillie_psw(&Integer::from(479)));
    assert!(baillie_psw(&Integer::from(17389)));

    for i in 1..=checks {
        let n = generate_number(BIT_SIZE);
        let r = n.is_probably_prime(64) != IsPrime::No;
        assert!(r == baillie_psw(&n));
        assert!(r == miller_rabin_test(&n, 64, false));
        if i % 256 == 0 {
            println!("test {:?} passed.", i);
        }
    }
}

pub fn test_rsa() -> () {
    test_all(4096);
    println!("\n\nRSA 2048-bits key:\n{}", generate_prime("baillie_psw"));
}
