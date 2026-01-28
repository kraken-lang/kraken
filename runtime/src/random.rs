use std::cell::RefCell;
use std::time::{SystemTime, UNIX_EPOCH};

pub trait RandomNumberGenerator {
    fn next_u32(&mut self) -> u32;
    fn next_u64(&mut self) -> u64;
    fn next_f64(&mut self) -> f64;
}

pub trait SeedableRng: RandomNumberGenerator {
    fn from_seed(seed: u64) -> Self;
    fn seed(&mut self, seed: u64);
}

#[derive(Debug, Clone)]
pub struct PcgRng {
    state: u64,
    inc: u64,
}

impl PcgRng {
    pub fn new(seed: u64) -> Self {
        let mut rng = PcgRng {
            state: 0,
            inc: (seed << 1) | 1,
        };
        rng.state = rng.state.wrapping_add(seed);
        rng.next_u32();
        rng
    }

    pub fn from_entropy() -> Self {
        let seed = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;
        Self::new(seed)
    }
}

impl RandomNumberGenerator for PcgRng {
    fn next_u32(&mut self) -> u32 {
        let oldstate = self.state;
        self.state = oldstate
            .wrapping_mul(6364136223846793005)
            .wrapping_add(self.inc);
        let xorshifted = (((oldstate >> 18) ^ oldstate) >> 27) as u32;
        let rot = (oldstate >> 59) as u32;
        xorshifted.rotate_right(rot)
    }

    fn next_u64(&mut self) -> u64 {
        let high = self.next_u32() as u64;
        let low = self.next_u32() as u64;
        (high << 32) | low
    }

    fn next_f64(&mut self) -> f64 {
        let value = self.next_u64();
        (value >> 11) as f64 / (1u64 << 53) as f64
    }
}

impl SeedableRng for PcgRng {
    fn from_seed(seed: u64) -> Self {
        Self::new(seed)
    }

    fn seed(&mut self, seed: u64) {
        self.state = 0;
        self.inc = (seed << 1) | 1;
        self.state = self.state.wrapping_add(seed);
        self.next_u32();
    }
}

#[derive(Debug, Clone)]
pub struct XorshiftRng {
    state: [u64; 2],
}

impl XorshiftRng {
    pub fn new(seed: u64) -> Self {
        let mut rng = XorshiftRng {
            state: [seed, seed.wrapping_add(1)],
        };
        for _ in 0..10 {
            rng.next_u64();
        }
        rng
    }

    pub fn from_entropy() -> Self {
        let seed = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos() as u64;
        Self::new(seed)
    }
}

impl RandomNumberGenerator for XorshiftRng {
    fn next_u32(&mut self) -> u32 {
        self.next_u64() as u32
    }

    fn next_u64(&mut self) -> u64 {
        let mut s1 = self.state[0];
        let s0 = self.state[1];
        self.state[0] = s0;
        s1 ^= s1 << 23;
        s1 ^= s1 >> 17;
        s1 ^= s0;
        s1 ^= s0 >> 26;
        self.state[1] = s1;
        s1.wrapping_add(s0)
    }

    fn next_f64(&mut self) -> f64 {
        let value = self.next_u64();
        (value >> 11) as f64 / (1u64 << 53) as f64
    }
}

impl SeedableRng for XorshiftRng {
    fn from_seed(seed: u64) -> Self {
        Self::new(seed)
    }

    fn seed(&mut self, seed: u64) {
        self.state = [seed, seed.wrapping_add(1)];
        for _ in 0..10 {
            self.next_u64();
        }
    }
}

#[derive(Debug)]
pub struct ChaCha20Rng {
    state: [u32; 16],
    buffer: [u32; 16],
    index: usize,
}

impl ChaCha20Rng {
    const CONSTANTS: [u32; 4] = [0x61707865, 0x3320646e, 0x79622d32, 0x6b206574];

    pub fn new(seed: [u8; 32]) -> Self {
        let mut state = [0u32; 16];
        state[0..4].copy_from_slice(&Self::CONSTANTS);

        for i in 0..8 {
            state[4 + i] = u32::from_le_bytes([
                seed[i * 4],
                seed[i * 4 + 1],
                seed[i * 4 + 2],
                seed[i * 4 + 3],
            ]);
        }

        state[12] = 0;
        state[13] = 0;
        state[14] = 0;
        state[15] = 0;

        let mut rng = ChaCha20Rng {
            state,
            buffer: [0; 16],
            index: 16,
        };
        rng.generate_block();
        rng
    }

    pub fn from_entropy() -> Self {
        let seed = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .unwrap()
            .as_nanos();
        let mut seed_bytes = [0u8; 32];
        for i in 0..4 {
            let bytes = ((seed >> (i * 32)) as u64).to_le_bytes();
            seed_bytes[i * 8..(i + 1) * 8].copy_from_slice(&bytes);
        }
        Self::new(seed_bytes)
    }

    fn quarter_round(state: &mut [u32; 16], a: usize, b: usize, c: usize, d: usize) {
        state[a] = state[a].wrapping_add(state[b]);
        state[d] ^= state[a];
        state[d] = state[d].rotate_left(16);
        state[c] = state[c].wrapping_add(state[d]);
        state[b] ^= state[c];
        state[b] = state[b].rotate_left(12);
        state[a] = state[a].wrapping_add(state[b]);
        state[d] ^= state[a];
        state[d] = state[d].rotate_left(8);
        state[c] = state[c].wrapping_add(state[d]);
        state[b] ^= state[c];
        state[b] = state[b].rotate_left(7);
    }

    fn generate_block(&mut self) {
        let mut working_state = self.state;

        for _ in 0..10 {
            Self::quarter_round(&mut working_state, 0, 4, 8, 12);
            Self::quarter_round(&mut working_state, 1, 5, 9, 13);
            Self::quarter_round(&mut working_state, 2, 6, 10, 14);
            Self::quarter_round(&mut working_state, 3, 7, 11, 15);

            Self::quarter_round(&mut working_state, 0, 5, 10, 15);
            Self::quarter_round(&mut working_state, 1, 6, 11, 12);
            Self::quarter_round(&mut working_state, 2, 7, 8, 13);
            Self::quarter_round(&mut working_state, 3, 4, 9, 14);
        }

        for (i, &val) in working_state.iter().enumerate() {
            self.buffer[i] = val.wrapping_add(self.state[i]);
        }

        self.state[12] = self.state[12].wrapping_add(1);
        if self.state[12] == 0 {
            self.state[13] = self.state[13].wrapping_add(1);
        }

        self.index = 0;
    }
}

impl RandomNumberGenerator for ChaCha20Rng {
    fn next_u32(&mut self) -> u32 {
        if self.index >= 16 {
            self.generate_block();
        }
        let value = self.buffer[self.index];
        self.index += 1;
        value
    }

    fn next_u64(&mut self) -> u64 {
        let high = self.next_u32() as u64;
        let low = self.next_u32() as u64;
        (high << 32) | low
    }

    fn next_f64(&mut self) -> f64 {
        let value = self.next_u64();
        (value >> 11) as f64 / (1u64 << 53) as f64
    }
}

pub struct Distributions;

impl Distributions {
    pub fn uniform<R: RandomNumberGenerator>(rng: &mut R, min: f64, max: f64) -> f64 {
        min + rng.next_f64() * (max - min)
    }

    pub fn uniform_int<R: RandomNumberGenerator>(rng: &mut R, min: i64, max: i64) -> i64 {
        let range = (max - min) as u64;
        let value = rng.next_u64() % range;
        min + value as i64
    }

    pub fn normal<R: RandomNumberGenerator>(rng: &mut R, mean: f64, std_dev: f64) -> f64 {
        let u1 = rng.next_f64();
        let u2 = rng.next_f64();
        let z0 = (-2.0 * u1.ln()).sqrt() * (2.0 * std::f64::consts::PI * u2).cos();
        mean + z0 * std_dev
    }

    pub fn exponential<R: RandomNumberGenerator>(rng: &mut R, lambda: f64) -> f64 {
        let u = rng.next_f64();
        -u.ln() / lambda
    }

    pub fn bernoulli<R: RandomNumberGenerator>(rng: &mut R, p: f64) -> bool {
        rng.next_f64() < p
    }

    pub fn poisson<R: RandomNumberGenerator>(rng: &mut R, lambda: f64) -> u32 {
        let l = (-lambda).exp();
        let mut k = 0;
        let mut p = 1.0;
        loop {
            k += 1;
            p *= rng.next_f64();
            if p <= l {
                break;
            }
        }
        k - 1
    }
}

thread_local! {
    static THREAD_RNG: RefCell<PcgRng> = RefCell::new(PcgRng::from_entropy());
}

pub fn thread_rng() -> impl RandomNumberGenerator {
    ThreadRng
}

pub struct ThreadRng;

impl RandomNumberGenerator for ThreadRng {
    fn next_u32(&mut self) -> u32 {
        THREAD_RNG.with(|rng| rng.borrow_mut().next_u32())
    }

    fn next_u64(&mut self) -> u64 {
        THREAD_RNG.with(|rng| rng.borrow_mut().next_u64())
    }

    fn next_f64(&mut self) -> f64 {
        THREAD_RNG.with(|rng| rng.borrow_mut().next_f64())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_pcg_rng_deterministic() {
        let mut rng1 = PcgRng::new(12345);
        let mut rng2 = PcgRng::new(12345);
        assert_eq!(rng1.next_u32(), rng2.next_u32());
        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_pcg_rng_different_seeds() {
        let mut rng1 = PcgRng::new(12345);
        let mut rng2 = PcgRng::new(54321);
        assert_ne!(rng1.next_u32(), rng2.next_u32());
    }

    #[test]
    fn test_pcg_rng_f64_range() {
        let mut rng = PcgRng::new(12345);
        for _ in 0..100 {
            let value = rng.next_f64();
            assert!((0.0..1.0).contains(&value));
        }
    }

    #[test]
    fn test_pcg_rng_seedable() {
        let mut rng = PcgRng::from_seed(12345);
        let first = rng.next_u32();
        rng.seed(12345);
        let second = rng.next_u32();
        assert_eq!(first, second);
    }

    #[test]
    fn test_xorshift_rng_deterministic() {
        let mut rng1 = XorshiftRng::new(12345);
        let mut rng2 = XorshiftRng::new(12345);
        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_xorshift_rng_different_seeds() {
        let mut rng1 = XorshiftRng::new(12345);
        let mut rng2 = XorshiftRng::new(54321);
        assert_ne!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_chacha20_rng() {
        let seed = [42u8; 32];
        let mut rng = ChaCha20Rng::new(seed);
        let value = rng.next_u32();
        assert!(value > 0);
    }

    #[test]
    fn test_chacha20_rng_deterministic() {
        let seed = [42u8; 32];
        let mut rng1 = ChaCha20Rng::new(seed);
        let mut rng2 = ChaCha20Rng::new(seed);
        assert_eq!(rng1.next_u32(), rng2.next_u32());
        assert_eq!(rng1.next_u64(), rng2.next_u64());
    }

    #[test]
    fn test_distributions_uniform() {
        let mut rng = PcgRng::new(12345);
        for _ in 0..100 {
            let value = Distributions::uniform(&mut rng, 10.0, 20.0);
            assert!((10.0..20.0).contains(&value));
        }
    }

    #[test]
    fn test_distributions_uniform_int() {
        let mut rng = PcgRng::new(12345);
        for _ in 0..100 {
            let value = Distributions::uniform_int(&mut rng, 10, 20);
            assert!((10..20).contains(&value));
        }
    }

    #[test]
    fn test_distributions_normal() {
        let mut rng = PcgRng::new(12345);
        let mut sum = 0.0;
        let n = 1000;
        for _ in 0..n {
            sum += Distributions::normal(&mut rng, 0.0, 1.0);
        }
        let mean = sum / n as f64;
        assert!(mean.abs() < 0.2);
    }

    #[test]
    fn test_distributions_exponential() {
        let mut rng = PcgRng::new(12345);
        for _ in 0..100 {
            let value = Distributions::exponential(&mut rng, 1.0);
            assert!(value >= 0.0);
        }
    }

    #[test]
    fn test_distributions_bernoulli() {
        let mut rng = PcgRng::new(12345);
        let mut count = 0;
        let n = 1000;
        for _ in 0..n {
            if Distributions::bernoulli(&mut rng, 0.5) {
                count += 1;
            }
        }
        let ratio = count as f64 / n as f64;
        assert!(ratio > 0.4 && ratio < 0.6);
    }

    #[test]
    fn test_distributions_poisson() {
        let mut rng = PcgRng::new(12345);
        for _ in 0..100 {
            let value = Distributions::poisson(&mut rng, 5.0);
            assert!(value < 50);
        }
    }

    #[test]
    fn test_thread_rng() {
        let mut rng = thread_rng();
        let value = rng.next_u32();
        assert!(value > 0);
    }
}
