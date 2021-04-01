// [ref] https://github.com/openssl/openssl/blob/master/crypto/bn/bn_prime.c
// [ref] https://www.openssl.org/docs/man1.0.2/man3/BN_generate_prime.html
use num_bigint::{BigUint, RandBigInt, ToBigUint};
use std::convert::TryFrom;
mod primes;
use crate::rsa::primes::primes;

const BIT_SIZE: u64 = 2048;

fn generate_number(bit_size: u64) -> BigUint {
    let mut rng = rand::thread_rng();
    let mut big_number = rng.gen_biguint(bit_size);
    // odd number
    big_number |= 1.to_biguint().unwrap();
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
fn generate_prime() -> BigUint {
    'generate: loop {
        let mut rnd = generate_number(BIT_SIZE);
        let mut delta = 0;
        // for 2048 bits, more details refer to bn_prime.c#L74:12
        let trial_divisions = 384;
        // According to Prime number theorem
        // probability of primes in natural numbers is about 1/log(2^{2048}) \approx 1/1418
        let maxdelta = 1418 * 3;
        let mut mods = vec![0u64; trial_divisions];
        // refer to bn_mr_min_checks in bn_prime.c#L94:12
        let checks = 64;
        for i in 1..trial_divisions {
            mods[i] = u64::try_from(&(&rnd % primes[i])).unwrap();
        }

        'trial: loop {
            for i in 1..trial_divisions {
                if (mods[i] + delta) % primes[i] == 0 {
                    // try to test new rnd with rnd += 2
                    delta += 2;
                    if delta > maxdelta {
                        continue 'generate;
                    }
                    // println!("trial_divisions w/ {} failed, plus 2 and retry",
                    //          primes[i]);
                    continue 'trial;
                }
            }
            break 'trial;
        }
        rnd += delta;
        if rnd.bits() != BIT_SIZE || !miller_rabin_test(&rnd, checks) {
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
fn miller_rabin_test(n: &BigUint, iteration: u8) -> bool {
    // (step 1) write n as 2^r*d + 1 with d odd (by factoring out
    // powers of 2 from n − 1)
    let one = 1.to_biguint().unwrap();
    let mut d = n - &one;
    let mut r = 0;
    while (&(&d & &one)).bits() == 0 {
        d >>= 1;
        r += 1;
    }

    let mut rng = rand::thread_rng();
    let two = 2.to_biguint().unwrap();
    // println!("r={}", r);
    'witness_loop: for _ in 0..iteration {
        let a = rng.gen_biguint_range(&two, &(n - &two));
        // (step 2) x = a^d mod n
        let mut x = a.modpow(&d, n);
        // println!("step 2 finish");
        // (step 3)
        if &x == &one || &x == &(n - &one) {
            continue 'witness_loop;
        }
        // (step 4)
        for _ in 1..r {
            x = x.modpow(&two, n);
            if &x == &(n - &one) {
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
    println!("RSA 2048-bits key:\n{}", generate_prime());
}
