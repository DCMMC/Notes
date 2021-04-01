// [ref] https://github.com/openssl/openssl/blob/master/crypto/bn/bn_prime.c
// [ref] https://www.openssl.org/docs/man1.0.2/man3/BN_generate_prime.html
// [ref] https://docs.rs/rug/1.12.0/rug/struct.Integer.html
// [ref] https://carol-nichols.com/2017/04/20/rust-profiling-with-dtrace-on-osx/
use rand::random;
use rug::rand::{RandGen, RandState};
use rug::Integer;
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
fn generate_prime() -> Integer {
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
        if rnd.significant_bits() != BIT_SIZE || !miller_rabin_test(&rnd, checks) {
            // println!("Miller-Rabin test failed, regenerate");
            continue 'generate;
        } else {
            return rnd;
        }
    }
}

/**
 * The Miller-Rabin test extends the ideas from the Fermat test.
 * Fermat test will 100% fail for Carmichael numbers.
 * The Miller-Rabin test checks if number is *strong probable* prime.
 */
fn miller_rabin_test(rnd: &Integer, iteration: u8) -> bool {
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
        let a = Integer::from(n_sub.random_below_ref(&mut rng)) + 2u8;
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

pub fn test_rsa() -> () {
    println!("\n\nRSA 2048-bits key:\n{}", generate_prime());
}
