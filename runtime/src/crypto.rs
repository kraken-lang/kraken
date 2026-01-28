pub struct Sha256 {
    state: [u32; 8],
    buffer: Vec<u8>,
    length: u64,
}

impl Sha256 {
    const K: [u32; 64] = [
        0x428a2f98, 0x71374491, 0xb5c0fbcf, 0xe9b5dba5, 0x3956c25b, 0x59f111f1, 0x923f82a4,
        0xab1c5ed5, 0xd807aa98, 0x12835b01, 0x243185be, 0x550c7dc3, 0x72be5d74, 0x80deb1fe,
        0x9bdc06a7, 0xc19bf174, 0xe49b69c1, 0xefbe4786, 0x0fc19dc6, 0x240ca1cc, 0x2de92c6f,
        0x4a7484aa, 0x5cb0a9dc, 0x76f988da, 0x983e5152, 0xa831c66d, 0xb00327c8, 0xbf597fc7,
        0xc6e00bf3, 0xd5a79147, 0x06ca6351, 0x14292967, 0x27b70a85, 0x2e1b2138, 0x4d2c6dfc,
        0x53380d13, 0x650a7354, 0x766a0abb, 0x81c2c92e, 0x92722c85, 0xa2bfe8a1, 0xa81a664b,
        0xc24b8b70, 0xc76c51a3, 0xd192e819, 0xd6990624, 0xf40e3585, 0x106aa070, 0x19a4c116,
        0x1e376c08, 0x2748774c, 0x34b0bcb5, 0x391c0cb3, 0x4ed8aa4a, 0x5b9cca4f, 0x682e6ff3,
        0x748f82ee, 0x78a5636f, 0x84c87814, 0x8cc70208, 0x90befffa, 0xa4506ceb, 0xbef9a3f7,
        0xc67178f2,
    ];

    pub fn new() -> Self {
        Sha256 {
            state: [
                0x6a09e667, 0xbb67ae85, 0x3c6ef372, 0xa54ff53a, 0x510e527f, 0x9b05688c, 0x1f83d9ab,
                0x5be0cd19,
            ],
            buffer: Vec::new(),
            length: 0,
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        self.length += data.len() as u64;
        self.buffer.extend_from_slice(data);

        while self.buffer.len() >= 64 {
            let block: [u8; 64] = self.buffer[..64].try_into().unwrap();
            self.process_block(&block);
            self.buffer.drain(..64);
        }
    }

    pub fn finalize(mut self) -> [u8; 32] {
        let bit_len = self.length * 8;
        self.buffer.push(0x80);

        while (self.buffer.len() % 64) != 56 {
            self.buffer.push(0x00);
        }

        self.buffer.extend_from_slice(&bit_len.to_be_bytes());

        while !self.buffer.is_empty() {
            let block: [u8; 64] = self.buffer[..64].try_into().unwrap();
            self.process_block(&block);
            self.buffer.drain(..64);
        }

        let mut result = [0u8; 32];
        for (i, &val) in self.state.iter().enumerate() {
            result[i * 4..(i + 1) * 4].copy_from_slice(&val.to_be_bytes());
        }
        result
    }

    fn process_block(&mut self, block: &[u8; 64]) {
        let mut w = [0u32; 64];

        for (i, chunk) in block.chunks_exact(4).enumerate().take(16) {
            w[i] = u32::from_be_bytes([chunk[0], chunk[1], chunk[2], chunk[3]]);
        }

        for i in 16..64 {
            let s0 = w[i - 15].rotate_right(7) ^ w[i - 15].rotate_right(18) ^ (w[i - 15] >> 3);
            let s1 = w[i - 2].rotate_right(17) ^ w[i - 2].rotate_right(19) ^ (w[i - 2] >> 10);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }

        let mut a = self.state[0];
        let mut b = self.state[1];
        let mut c = self.state[2];
        let mut d = self.state[3];
        let mut e = self.state[4];
        let mut f = self.state[5];
        let mut g = self.state[6];
        let mut h = self.state[7];

        #[allow(clippy::needless_range_loop)]
        for i in 0..64 {
            let s1 = e.rotate_right(6) ^ e.rotate_right(11) ^ e.rotate_right(25);
            let ch = (e & f) ^ ((!e) & g);
            let temp1 = h
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(Self::K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(2) ^ a.rotate_right(13) ^ a.rotate_right(22);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);

            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }

        self.state[0] = self.state[0].wrapping_add(a);
        self.state[1] = self.state[1].wrapping_add(b);
        self.state[2] = self.state[2].wrapping_add(c);
        self.state[3] = self.state[3].wrapping_add(d);
        self.state[4] = self.state[4].wrapping_add(e);
        self.state[5] = self.state[5].wrapping_add(f);
        self.state[6] = self.state[6].wrapping_add(g);
        self.state[7] = self.state[7].wrapping_add(h);
    }

    pub fn hash(data: &[u8]) -> [u8; 32] {
        let mut hasher = Self::new();
        hasher.update(data);
        hasher.finalize()
    }
}

impl Default for Sha256 {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Sha512 {
    state: [u64; 8],
    buffer: Vec<u8>,
    length: u128,
}

impl Sha512 {
    const K: [u64; 80] = [
        0x428a2f98d728ae22,
        0x7137449123ef65cd,
        0xb5c0fbcfec4d3b2f,
        0xe9b5dba58189dbbc,
        0x3956c25bf348b538,
        0x59f111f1b605d019,
        0x923f82a4af194f9b,
        0xab1c5ed5da6d8118,
        0xd807aa98a3030242,
        0x12835b0145706fbe,
        0x243185be4ee4b28c,
        0x550c7dc3d5ffb4e2,
        0x72be5d74f27b896f,
        0x80deb1fe3b1696b1,
        0x9bdc06a725c71235,
        0xc19bf174cf692694,
        0xe49b69c19ef14ad2,
        0xefbe4786384f25e3,
        0x0fc19dc68b8cd5b5,
        0x240ca1cc77ac9c65,
        0x2de92c6f592b0275,
        0x4a7484aa6ea6e483,
        0x5cb0a9dcbd41fbd4,
        0x76f988da831153b5,
        0x983e5152ee66dfab,
        0xa831c66d2db43210,
        0xb00327c898fb213f,
        0xbf597fc7beef0ee4,
        0xc6e00bf33da88fc2,
        0xd5a79147930aa725,
        0x06ca6351e003826f,
        0x142929670a0e6e70,
        0x27b70a8546d22ffc,
        0x2e1b21385c26c926,
        0x4d2c6dfc5ac42aed,
        0x53380d139d95b3df,
        0x650a73548baf63de,
        0x766a0abb3c77b2a8,
        0x81c2c92e47edaee6,
        0x92722c851482353b,
        0xa2bfe8a14cf10364,
        0xa81a664bbc423001,
        0xc24b8b70d0f89791,
        0xc76c51a30654be30,
        0xd192e819d6ef5218,
        0xd69906245565a910,
        0xf40e35855771202a,
        0x106aa07032bbd1b8,
        0x19a4c116b8d2d0c8,
        0x1e376c085141ab53,
        0x2748774cdf8eeb99,
        0x34b0bcb5e19b48a8,
        0x391c0cb3c5c95a63,
        0x4ed8aa4ae3418acb,
        0x5b9cca4f7763e373,
        0x682e6ff3d6b2b8a3,
        0x748f82ee5defb2fc,
        0x78a5636f43172f60,
        0x84c87814a1f0ab72,
        0x8cc702081a6439ec,
        0x90befffa23631e28,
        0xa4506cebde82bde9,
        0xbef9a3f7b2c67915,
        0xc67178f2e372532b,
        0xca273eceea26619c,
        0xd186b8c721c0c207,
        0xeada7dd6cde0eb1e,
        0xf57d4f7fee6ed178,
        0x06f067aa72176fba,
        0x0a637dc5a2c898a6,
        0x113f9804bef90dae,
        0x1b710b35131c471b,
        0x28db77f523047d84,
        0x32caab7b40c72493,
        0x3c9ebe0a15c9bebc,
        0x431d67c49c100d4c,
        0x4cc5d4becb3e42b6,
        0x597f299cfc657e2a,
        0x5fcb6fab3ad6faec,
        0x6c44198c4a475817,
    ];

    pub fn new() -> Self {
        Sha512 {
            state: [
                0x6a09e667f3bcc908,
                0xbb67ae8584caa73b,
                0x3c6ef372fe94f82b,
                0xa54ff53a5f1d36f1,
                0x510e527fade682d1,
                0x9b05688c2b3e6c1f,
                0x1f83d9abfb41bd6b,
                0x5be0cd19137e2179,
            ],
            buffer: Vec::new(),
            length: 0,
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        self.length += data.len() as u128;
        self.buffer.extend_from_slice(data);

        while self.buffer.len() >= 128 {
            let block: [u8; 128] = self.buffer[..128].try_into().unwrap();
            self.process_block(&block);
            self.buffer.drain(..128);
        }
    }

    pub fn finalize(mut self) -> [u8; 64] {
        let bit_len = self.length * 8;
        self.buffer.push(0x80);

        while (self.buffer.len() % 128) != 112 {
            self.buffer.push(0x00);
        }

        self.buffer.extend_from_slice(&bit_len.to_be_bytes());

        while !self.buffer.is_empty() {
            let block: [u8; 128] = self.buffer[..128].try_into().unwrap();
            self.process_block(&block);
            self.buffer.drain(..128);
        }

        let mut result = [0u8; 64];
        for (i, &val) in self.state.iter().enumerate() {
            result[i * 8..(i + 1) * 8].copy_from_slice(&val.to_be_bytes());
        }
        result
    }

    fn process_block(&mut self, block: &[u8; 128]) {
        let mut w = [0u64; 80];

        for (i, chunk) in block.chunks_exact(8).enumerate().take(16) {
            w[i] = u64::from_be_bytes([
                chunk[0], chunk[1], chunk[2], chunk[3], chunk[4], chunk[5], chunk[6], chunk[7],
            ]);
        }

        for i in 16..80 {
            let s0 = w[i - 15].rotate_right(1) ^ w[i - 15].rotate_right(8) ^ (w[i - 15] >> 7);
            let s1 = w[i - 2].rotate_right(19) ^ w[i - 2].rotate_right(61) ^ (w[i - 2] >> 6);
            w[i] = w[i - 16]
                .wrapping_add(s0)
                .wrapping_add(w[i - 7])
                .wrapping_add(s1);
        }

        let mut a = self.state[0];
        let mut b = self.state[1];
        let mut c = self.state[2];
        let mut d = self.state[3];
        let mut e = self.state[4];
        let mut f = self.state[5];
        let mut g = self.state[6];
        let mut h = self.state[7];

        #[allow(clippy::needless_range_loop)]
        for i in 0..80 {
            let s1 = e.rotate_right(14) ^ e.rotate_right(18) ^ e.rotate_right(41);
            let ch = (e & f) ^ ((!e) & g);
            let temp1 = h
                .wrapping_add(s1)
                .wrapping_add(ch)
                .wrapping_add(Self::K[i])
                .wrapping_add(w[i]);
            let s0 = a.rotate_right(28) ^ a.rotate_right(34) ^ a.rotate_right(39);
            let maj = (a & b) ^ (a & c) ^ (b & c);
            let temp2 = s0.wrapping_add(maj);

            h = g;
            g = f;
            f = e;
            e = d.wrapping_add(temp1);
            d = c;
            c = b;
            b = a;
            a = temp1.wrapping_add(temp2);
        }

        self.state[0] = self.state[0].wrapping_add(a);
        self.state[1] = self.state[1].wrapping_add(b);
        self.state[2] = self.state[2].wrapping_add(c);
        self.state[3] = self.state[3].wrapping_add(d);
        self.state[4] = self.state[4].wrapping_add(e);
        self.state[5] = self.state[5].wrapping_add(f);
        self.state[6] = self.state[6].wrapping_add(g);
        self.state[7] = self.state[7].wrapping_add(h);
    }

    pub fn hash(data: &[u8]) -> [u8; 64] {
        let mut hasher = Self::new();
        hasher.update(data);
        hasher.finalize()
    }
}

impl Default for Sha512 {
    fn default() -> Self {
        Self::new()
    }
}

pub struct Hmac<H> {
    hasher: H,
    key: Vec<u8>,
}

impl Hmac<Sha256> {
    pub fn new(key: &[u8]) -> Self {
        let mut processed_key = if key.len() > 64 {
            Sha256::hash(key).to_vec()
        } else {
            key.to_vec()
        };

        while processed_key.len() < 64 {
            processed_key.push(0);
        }

        Hmac {
            hasher: Sha256::new(),
            key: processed_key,
        }
    }

    pub fn update(&mut self, data: &[u8]) {
        self.hasher.update(data);
    }

    pub fn finalize(self) -> [u8; 32] {
        let mut ipad = [0x36u8; 64];
        let mut opad = [0x5cu8; 64];

        for i in 0..64 {
            ipad[i] ^= self.key[i];
            opad[i] ^= self.key[i];
        }

        let mut inner = Sha256::new();
        inner.update(&ipad);
        inner.update(&self.hasher.buffer);
        let inner_hash = inner.finalize();

        let mut outer = Sha256::new();
        outer.update(&opad);
        outer.update(&inner_hash);
        outer.finalize()
    }

    pub fn compute(key: &[u8], data: &[u8]) -> [u8; 32] {
        let mut hmac = Self::new(key);
        hmac.update(data);
        hmac.finalize()
    }
}

pub struct Pbkdf2;

impl Pbkdf2 {
    pub fn derive_key(password: &[u8], salt: &[u8], iterations: u32, key_len: usize) -> Vec<u8> {
        let hlen = 32;
        let blocks_needed = key_len.div_ceil(hlen);
        let mut result = Vec::with_capacity(blocks_needed * hlen);

        for block in 1..=blocks_needed {
            let u = Self::compute_block(password, salt, iterations, block as u32);
            result.extend_from_slice(&u);
        }

        result.truncate(key_len);
        result
    }

    fn compute_block(password: &[u8], salt: &[u8], iterations: u32, block: u32) -> [u8; 32] {
        let mut salt_block = salt.to_vec();
        salt_block.extend_from_slice(&block.to_be_bytes());

        let mut u = Hmac::<Sha256>::compute(password, &salt_block);
        let mut result = u;

        for _ in 1..iterations {
            u = Hmac::<Sha256>::compute(password, &u);
            for i in 0..32 {
                result[i] ^= u[i];
            }
        }

        result
    }
}

pub fn constant_time_compare(a: &[u8], b: &[u8]) -> bool {
    if a.len() != b.len() {
        return false;
    }

    let mut result = 0u8;
    for i in 0..a.len() {
        result |= a[i] ^ b[i];
    }
    result == 0
}

pub struct Aes256 {
    round_keys: [[u8; 16]; 15],
}

impl Aes256 {
    const SBOX: [u8; 256] = [
        0x63, 0x7c, 0x77, 0x7b, 0xf2, 0x6b, 0x6f, 0xc5, 0x30, 0x01, 0x67, 0x2b, 0xfe, 0xd7, 0xab,
        0x76, 0xca, 0x82, 0xc9, 0x7d, 0xfa, 0x59, 0x47, 0xf0, 0xad, 0xd4, 0xa2, 0xaf, 0x9c, 0xa4,
        0x72, 0xc0, 0xb7, 0xfd, 0x93, 0x26, 0x36, 0x3f, 0xf7, 0xcc, 0x34, 0xa5, 0xe5, 0xf1, 0x71,
        0xd8, 0x31, 0x15, 0x04, 0xc7, 0x23, 0xc3, 0x18, 0x96, 0x05, 0x9a, 0x07, 0x12, 0x80, 0xe2,
        0xeb, 0x27, 0xb2, 0x75, 0x09, 0x83, 0x2c, 0x1a, 0x1b, 0x6e, 0x5a, 0xa0, 0x52, 0x3b, 0xd6,
        0xb3, 0x29, 0xe3, 0x2f, 0x84, 0x53, 0xd1, 0x00, 0xed, 0x20, 0xfc, 0xb1, 0x5b, 0x6a, 0xcb,
        0xbe, 0x39, 0x4a, 0x4c, 0x58, 0xcf, 0xd0, 0xef, 0xaa, 0xfb, 0x43, 0x4d, 0x33, 0x85, 0x45,
        0xf9, 0x02, 0x7f, 0x50, 0x3c, 0x9f, 0xa8, 0x51, 0xa3, 0x40, 0x8f, 0x92, 0x9d, 0x38, 0xf5,
        0xbc, 0xb6, 0xda, 0x21, 0x10, 0xff, 0xf3, 0xd2, 0xcd, 0x0c, 0x13, 0xec, 0x5f, 0x97, 0x44,
        0x17, 0xc4, 0xa7, 0x7e, 0x3d, 0x64, 0x5d, 0x19, 0x73, 0x60, 0x81, 0x4f, 0xdc, 0x22, 0x2a,
        0x90, 0x88, 0x46, 0xee, 0xb8, 0x14, 0xde, 0x5e, 0x0b, 0xdb, 0xe0, 0x32, 0x3a, 0x0a, 0x49,
        0x06, 0x24, 0x5c, 0xc2, 0xd3, 0xac, 0x62, 0x91, 0x95, 0xe4, 0x79, 0xe7, 0xc8, 0x37, 0x6d,
        0x8d, 0xd5, 0x4e, 0xa9, 0x6c, 0x56, 0xf4, 0xea, 0x65, 0x7a, 0xae, 0x08, 0xba, 0x78, 0x25,
        0x2e, 0x1c, 0xa6, 0xb4, 0xc6, 0xe8, 0xdd, 0x74, 0x1f, 0x4b, 0xbd, 0x8b, 0x8a, 0x70, 0x3e,
        0xb5, 0x66, 0x48, 0x03, 0xf6, 0x0e, 0x61, 0x35, 0x57, 0xb9, 0x86, 0xc1, 0x1d, 0x9e, 0xe1,
        0xf8, 0x98, 0x11, 0x69, 0xd9, 0x8e, 0x94, 0x9b, 0x1e, 0x87, 0xe9, 0xce, 0x55, 0x28, 0xdf,
        0x8c, 0xa1, 0x89, 0x0d, 0xbf, 0xe6, 0x42, 0x68, 0x41, 0x99, 0x2d, 0x0f, 0xb0, 0x54, 0xbb,
        0x16,
    ];

    const RCON: [u8; 10] = [0x01, 0x02, 0x04, 0x08, 0x10, 0x20, 0x40, 0x80, 0x1b, 0x36];

    pub fn new(key: &[u8; 32]) -> Self {
        let mut round_keys = [[0u8; 16]; 15];
        Self::key_expansion(key, &mut round_keys);
        Aes256 { round_keys }
    }

    fn key_expansion(key: &[u8; 32], round_keys: &mut [[u8; 16]; 15]) {
        round_keys[0].copy_from_slice(&key[0..16]);
        round_keys[1].copy_from_slice(&key[16..32]);

        for i in 2..15 {
            let prev = round_keys[i - 1];
            let prev2 = round_keys[i - 2];

            for j in 0..16 {
                if i % 2 == 0 {
                    if j < 4 {
                        let temp = if j == 0 {
                            Self::SBOX[prev[13] as usize] ^ Self::RCON[(i / 2) - 1]
                        } else {
                            Self::SBOX[prev[12 + j] as usize]
                        };
                        round_keys[i][j] = prev2[j] ^ temp;
                    } else {
                        round_keys[i][j] = prev2[j] ^ round_keys[i][j - 4];
                    }
                } else if j < 4 {
                    round_keys[i][j] = prev2[j] ^ Self::SBOX[prev[j] as usize];
                } else {
                    round_keys[i][j] = prev2[j] ^ round_keys[i][j - 4];
                }
            }
        }
    }

    pub fn encrypt_block(&self, block: &[u8; 16]) -> [u8; 16] {
        let mut state = *block;

        Self::add_round_key(&mut state, &self.round_keys[0]);

        for round in 1..14 {
            Self::sub_bytes(&mut state);
            Self::shift_rows(&mut state);
            Self::mix_columns(&mut state);
            Self::add_round_key(&mut state, &self.round_keys[round]);
        }

        Self::sub_bytes(&mut state);
        Self::shift_rows(&mut state);
        Self::add_round_key(&mut state, &self.round_keys[14]);

        state
    }

    pub fn encrypt_cbc(&self, plaintext: &[u8], iv: &[u8; 16]) -> Vec<u8> {
        let mut result = Vec::new();
        let mut prev_block = *iv;

        for chunk in plaintext.chunks(16) {
            let mut block = [0u8; 16];
            block[..chunk.len()].copy_from_slice(chunk);

            if chunk.len() < 16 {
                let padding = 16 - chunk.len();
                for item in block.iter_mut().skip(chunk.len()) {
                    *item = padding as u8;
                }
            }

            for i in 0..16 {
                block[i] ^= prev_block[i];
            }

            let encrypted = self.encrypt_block(&block);
            result.extend_from_slice(&encrypted);
            prev_block = encrypted;
        }

        result
    }

    fn sub_bytes(state: &mut [u8; 16]) {
        for byte in state.iter_mut() {
            *byte = Self::SBOX[*byte as usize];
        }
    }

    fn shift_rows(state: &mut [u8; 16]) {
        let temp = *state;
        state[1] = temp[5];
        state[5] = temp[9];
        state[9] = temp[13];
        state[13] = temp[1];

        state[2] = temp[10];
        state[6] = temp[14];
        state[10] = temp[2];
        state[14] = temp[6];

        state[3] = temp[15];
        state[7] = temp[3];
        state[11] = temp[7];
        state[15] = temp[11];
    }

    fn mix_columns(state: &mut [u8; 16]) {
        for i in 0..4 {
            let s0 = state[i * 4];
            let s1 = state[i * 4 + 1];
            let s2 = state[i * 4 + 2];
            let s3 = state[i * 4 + 3];

            state[i * 4] = Self::gf_mul(s0, 2) ^ Self::gf_mul(s1, 3) ^ s2 ^ s3;
            state[i * 4 + 1] = s0 ^ Self::gf_mul(s1, 2) ^ Self::gf_mul(s2, 3) ^ s3;
            state[i * 4 + 2] = s0 ^ s1 ^ Self::gf_mul(s2, 2) ^ Self::gf_mul(s3, 3);
            state[i * 4 + 3] = Self::gf_mul(s0, 3) ^ s1 ^ s2 ^ Self::gf_mul(s3, 2);
        }
    }

    fn gf_mul(a: u8, b: u8) -> u8 {
        let mut p = 0u8;
        let mut a = a;
        let mut b = b;

        for _ in 0..8 {
            if b & 1 != 0 {
                p ^= a;
            }
            let hi_bit_set = a & 0x80 != 0;
            a <<= 1;
            if hi_bit_set {
                a ^= 0x1b;
            }
            b >>= 1;
        }
        p
    }

    fn add_round_key(state: &mut [u8; 16], round_key: &[u8; 16]) {
        for i in 0..16 {
            state[i] ^= round_key[i];
        }
    }
}

pub struct ChaCha20Poly1305 {
    key: [u8; 32],
}

impl ChaCha20Poly1305 {
    pub fn new(key: &[u8; 32]) -> Self {
        ChaCha20Poly1305 { key: *key }
    }

    pub fn encrypt(&self, nonce: &[u8; 12], plaintext: &[u8], aad: &[u8]) -> Vec<u8> {
        let mut ciphertext = Vec::with_capacity(plaintext.len() + 16);

        let mut counter = [0u8; 16];
        counter[0..4].copy_from_slice(&[0, 0, 0, 0]);
        counter[4..16].copy_from_slice(nonce);

        let poly_key = self.chacha20_block(&counter, 0);

        for (i, chunk) in plaintext.chunks(64).enumerate() {
            let keystream = self.chacha20_block(&counter, (i + 1) as u32);
            for (j, &byte) in chunk.iter().enumerate() {
                ciphertext.push(byte ^ keystream[j]);
            }
        }

        let tag = self.poly1305(&poly_key[..32].try_into().unwrap(), aad, &ciphertext);
        ciphertext.extend_from_slice(&tag);

        ciphertext
    }

    pub fn decrypt(
        &self,
        nonce: &[u8; 12],
        ciphertext: &[u8],
        aad: &[u8],
    ) -> Result<Vec<u8>, String> {
        if ciphertext.len() < 16 {
            return Err("Ciphertext too short".to_string());
        }

        let (ct, tag) = ciphertext.split_at(ciphertext.len() - 16);

        let mut counter = [0u8; 16];
        counter[0..4].copy_from_slice(&[0, 0, 0, 0]);
        counter[4..16].copy_from_slice(nonce);

        let poly_key = self.chacha20_block(&counter, 0);
        let computed_tag = self.poly1305(&poly_key[..32].try_into().unwrap(), aad, ct);

        if !constant_time_compare(tag, &computed_tag) {
            return Err("Authentication failed".to_string());
        }

        let mut plaintext = Vec::with_capacity(ct.len());
        for (i, chunk) in ct.chunks(64).enumerate() {
            let keystream = self.chacha20_block(&counter, (i + 1) as u32);
            for (j, &byte) in chunk.iter().enumerate() {
                plaintext.push(byte ^ keystream[j]);
            }
        }

        Ok(plaintext)
    }

    fn chacha20_block(&self, nonce: &[u8; 16], counter: u32) -> [u8; 64] {
        let mut state = [0u32; 16];

        state[0] = 0x61707865;
        state[1] = 0x3320646e;
        state[2] = 0x79622d32;
        state[3] = 0x6b206574;

        for i in 0..8 {
            state[4 + i] = u32::from_le_bytes([
                self.key[i * 4],
                self.key[i * 4 + 1],
                self.key[i * 4 + 2],
                self.key[i * 4 + 3],
            ]);
        }

        state[12] = counter;
        for i in 0..3 {
            state[13 + i] = u32::from_le_bytes([
                nonce[i * 4],
                nonce[i * 4 + 1],
                nonce[i * 4 + 2],
                nonce[i * 4 + 3],
            ]);
        }

        let mut working = state;
        for _ in 0..10 {
            Self::quarter_round_idx(&mut working, 0, 4, 8, 12);
            Self::quarter_round_idx(&mut working, 1, 5, 9, 13);
            Self::quarter_round_idx(&mut working, 2, 6, 10, 14);
            Self::quarter_round_idx(&mut working, 3, 7, 11, 15);

            Self::quarter_round_idx(&mut working, 0, 5, 10, 15);
            Self::quarter_round_idx(&mut working, 1, 6, 11, 12);
            Self::quarter_round_idx(&mut working, 2, 7, 8, 13);
            Self::quarter_round_idx(&mut working, 3, 4, 9, 14);
        }

        for i in 0..16 {
            working[i] = working[i].wrapping_add(state[i]);
        }

        let mut output = [0u8; 64];
        for (i, &val) in working.iter().enumerate() {
            output[i * 4..(i + 1) * 4].copy_from_slice(&val.to_le_bytes());
        }
        output
    }

    fn quarter_round_idx(state: &mut [u32; 16], a: usize, b: usize, c: usize, d: usize) {
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

    fn poly1305(&self, key: &[u8; 32], aad: &[u8], ciphertext: &[u8]) -> [u8; 16] {
        let mut acc = [0u8; 17];
        let r = &key[..16];
        let s = &key[16..32];

        let mut msg = Vec::new();
        msg.extend_from_slice(aad);
        while msg.len() % 16 != 0 {
            msg.push(0);
        }
        msg.extend_from_slice(ciphertext);
        while msg.len() % 16 != 0 {
            msg.push(0);
        }
        msg.extend_from_slice(&(aad.len() as u64).to_le_bytes());
        msg.extend_from_slice(&(ciphertext.len() as u64).to_le_bytes());

        for chunk in msg.chunks(16) {
            let mut block = [0u8; 17];
            block[..chunk.len()].copy_from_slice(chunk);
            block[chunk.len()] = 1;

            for i in 0..17 {
                acc[i] = acc[i].wrapping_add(block[i]);
            }

            let mut temp = [0u8; 17];
            for (i, &r_val) in r.iter().enumerate().take(16) {
                for (j, &acc_val) in acc.iter().enumerate() {
                    let prod = (acc_val as u16) * (r_val as u16);
                    let idx = (i + j) % 17;
                    temp[idx] = temp[idx].wrapping_add((prod & 0xff) as u8);
                }
            }
            acc = temp;
        }

        let mut tag = [0u8; 16];
        for i in 0..16 {
            tag[i] = acc[i].wrapping_add(s[i]);
        }
        tag
    }
}

pub struct Ed25519;

impl Ed25519 {
    pub fn generate_keypair(seed: &[u8; 32]) -> ([u8; 32], [u8; 64]) {
        use ed25519_dalek::{SigningKey, VerifyingKey};

        let signing_key = SigningKey::from_bytes(seed);
        let verifying_key: VerifyingKey = (&signing_key).into();

        let public_key = verifying_key.to_bytes();

        let mut secret_key = [0u8; 64];
        secret_key[..32].copy_from_slice(seed);
        secret_key[32..].copy_from_slice(&public_key);

        (public_key, secret_key)
    }

    pub fn sign(message: &[u8], secret_key: &[u8; 64]) -> [u8; 64] {
        use ed25519_dalek::{Signature, Signer, SigningKey};

        let mut seed = [0u8; 32];
        seed.copy_from_slice(&secret_key[..32]);

        let signing_key = SigningKey::from_bytes(&seed);
        let signature: Signature = signing_key.sign(message);

        signature.to_bytes()
    }

    pub fn verify(message: &[u8], signature: &[u8; 64], public_key: &[u8; 32]) -> bool {
        use ed25519_dalek::{Signature, Verifier, VerifyingKey};

        let verifying_key = match VerifyingKey::from_bytes(public_key) {
            Ok(key) => key,
            Err(_) => return false,
        };

        let sig = match Signature::try_from(signature.as_slice()) {
            Ok(s) => s,
            Err(_) => return false,
        };

        verifying_key.verify(message, &sig).is_ok()
    }
}

pub struct Base64;

impl Base64 {
    const CHARSET: &'static [u8] =
        b"ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/";

    pub fn encode(data: &[u8]) -> String {
        let mut result = String::new();
        let mut i = 0;

        while i + 2 < data.len() {
            let b1 = data[i];
            let b2 = data[i + 1];
            let b3 = data[i + 2];

            result.push(Self::CHARSET[(b1 >> 2) as usize] as char);
            result.push(Self::CHARSET[(((b1 & 0x03) << 4) | (b2 >> 4)) as usize] as char);
            result.push(Self::CHARSET[(((b2 & 0x0f) << 2) | (b3 >> 6)) as usize] as char);
            result.push(Self::CHARSET[(b3 & 0x3f) as usize] as char);

            i += 3;
        }

        if i < data.len() {
            let b1 = data[i];
            result.push(Self::CHARSET[(b1 >> 2) as usize] as char);

            if i + 1 < data.len() {
                let b2 = data[i + 1];
                result.push(Self::CHARSET[(((b1 & 0x03) << 4) | (b2 >> 4)) as usize] as char);
                result.push(Self::CHARSET[((b2 & 0x0f) << 2) as usize] as char);
                result.push('=');
            } else {
                result.push(Self::CHARSET[((b1 & 0x03) << 4) as usize] as char);
                result.push('=');
                result.push('=');
            }
        }

        result
    }

    pub fn decode(data: &str) -> Result<Vec<u8>, String> {
        let data = data.trim_end_matches('=');
        let mut result = Vec::new();
        let bytes = data.as_bytes();
        let mut i = 0;

        while i + 3 < bytes.len() {
            let b1 = Self::decode_char(bytes[i])?;
            let b2 = Self::decode_char(bytes[i + 1])?;
            let b3 = Self::decode_char(bytes[i + 2])?;
            let b4 = Self::decode_char(bytes[i + 3])?;

            result.push((b1 << 2) | (b2 >> 4));
            result.push((b2 << 4) | (b3 >> 2));
            result.push((b3 << 6) | b4);

            i += 4;
        }

        if i < bytes.len() {
            let b1 = Self::decode_char(bytes[i])?;
            if i + 1 < bytes.len() {
                let b2 = Self::decode_char(bytes[i + 1])?;
                result.push((b1 << 2) | (b2 >> 4));

                if i + 2 < bytes.len() {
                    let b3 = Self::decode_char(bytes[i + 2])?;
                    result.push((b2 << 4) | (b3 >> 2));
                }
            }
        }

        Ok(result)
    }

    fn decode_char(c: u8) -> Result<u8, String> {
        match c {
            b'A'..=b'Z' => Ok(c - b'A'),
            b'a'..=b'z' => Ok(c - b'a' + 26),
            b'0'..=b'9' => Ok(c - b'0' + 52),
            b'+' => Ok(62),
            b'/' => Ok(63),
            _ => Err(format!("Invalid base64 character: {}", c as char)),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sha256_empty() {
        let hash = Sha256::hash(b"");
        assert_eq!(
            hash,
            [
                0xe3, 0xb0, 0xc4, 0x42, 0x98, 0xfc, 0x1c, 0x14, 0x9a, 0xfb, 0xf4, 0xc8, 0x99, 0x6f,
                0xb9, 0x24, 0x27, 0xae, 0x41, 0xe4, 0x64, 0x9b, 0x93, 0x4c, 0xa4, 0x95, 0x99, 0x1b,
                0x78, 0x52, 0xb8, 0x55
            ]
        );
    }

    #[test]
    fn test_sha256_hello() {
        let hash = Sha256::hash(b"hello");
        assert_eq!(hash.len(), 32);
    }

    #[test]
    fn test_sha256_incremental() {
        let mut hasher = Sha256::new();
        hasher.update(b"hel");
        hasher.update(b"lo");
        let hash1 = hasher.finalize();

        let hash2 = Sha256::hash(b"hello");
        assert_eq!(hash1, hash2);
    }

    #[test]
    fn test_sha512_empty() {
        let hash = Sha512::hash(b"");
        assert_eq!(hash.len(), 64);
    }

    #[test]
    fn test_sha512_hello() {
        let hash = Sha512::hash(b"hello");
        assert_eq!(hash.len(), 64);
    }

    #[test]
    fn test_hmac_sha256() {
        let key = b"secret";
        let data = b"message";
        let mac = Hmac::<Sha256>::compute(key, data);
        assert_eq!(mac.len(), 32);
    }

    #[test]
    fn test_hmac_sha256_verify() {
        let key = b"secret";
        let data = b"message";
        let mac1 = Hmac::<Sha256>::compute(key, data);
        let mac2 = Hmac::<Sha256>::compute(key, data);
        assert_eq!(mac1, mac2);
    }

    #[test]
    fn test_pbkdf2() {
        let password = b"password";
        let salt = b"salt";
        let key = Pbkdf2::derive_key(password, salt, 1000, 32);
        assert_eq!(key.len(), 32);
    }

    #[test]
    fn test_pbkdf2_deterministic() {
        let password = b"password";
        let salt = b"salt";
        let key1 = Pbkdf2::derive_key(password, salt, 1000, 32);
        let key2 = Pbkdf2::derive_key(password, salt, 1000, 32);
        assert_eq!(key1, key2);
    }

    #[test]
    fn test_constant_time_compare() {
        let a = b"hello";
        let b = b"hello";
        assert!(constant_time_compare(a, b));

        let c = b"world";
        assert!(!constant_time_compare(a, c));

        let d = b"hell";
        assert!(!constant_time_compare(a, d));
    }

    #[test]
    fn test_base64_encode() {
        assert_eq!(Base64::encode(b"hello"), "aGVsbG8=");
        assert_eq!(Base64::encode(b"hello world"), "aGVsbG8gd29ybGQ=");
        assert_eq!(Base64::encode(b""), "");
    }

    #[test]
    fn test_base64_decode() {
        assert_eq!(Base64::decode("aGVsbG8=").unwrap(), b"hello");
        assert_eq!(Base64::decode("aGVsbG8gd29ybGQ=").unwrap(), b"hello world");
        assert_eq!(Base64::decode("").unwrap(), b"");
    }

    #[test]
    fn test_base64_roundtrip() {
        let data = b"The quick brown fox jumps over the lazy dog";
        let encoded = Base64::encode(data);
        let decoded = Base64::decode(&encoded).unwrap();
        assert_eq!(data.as_slice(), decoded.as_slice());
    }

    #[test]
    fn test_aes256_encrypt_block() {
        let key = [0u8; 32];
        let aes = Aes256::new(&key);
        let plaintext = [0u8; 16];
        let ciphertext = aes.encrypt_block(&plaintext);
        assert_ne!(plaintext, ciphertext);
    }

    #[test]
    fn test_aes256_cbc() {
        let key = [1u8; 32];
        let iv = [2u8; 16];
        let aes = Aes256::new(&key);
        let plaintext = b"Hello, World!";
        let ciphertext = aes.encrypt_cbc(plaintext, &iv);
        assert!(!ciphertext.is_empty());
        assert!(ciphertext.len() >= plaintext.len());
    }

    #[test]
    fn test_aes256_deterministic() {
        let key = [42u8; 32];
        let iv = [7u8; 16];
        let aes = Aes256::new(&key);
        let plaintext = b"Test message";
        let ct1 = aes.encrypt_cbc(plaintext, &iv);
        let ct2 = aes.encrypt_cbc(plaintext, &iv);
        assert_eq!(ct1, ct2);
    }

    #[test]
    fn test_chacha20poly1305_encrypt_decrypt() {
        let key = [1u8; 32];
        let nonce = [2u8; 12];
        let cipher = ChaCha20Poly1305::new(&key);
        let plaintext = b"Hello, World!";
        let aad = b"additional data";

        let ciphertext = cipher.encrypt(&nonce, plaintext, aad);
        let decrypted = cipher.decrypt(&nonce, &ciphertext, aad).unwrap();

        assert_eq!(plaintext.as_slice(), decrypted.as_slice());
    }

    #[test]
    fn test_chacha20poly1305_authentication() {
        let key = [1u8; 32];
        let nonce = [2u8; 12];
        let cipher = ChaCha20Poly1305::new(&key);
        let plaintext = b"Hello, World!";
        let aad = b"additional data";

        let mut ciphertext = cipher.encrypt(&nonce, plaintext, aad);
        ciphertext[0] ^= 1;

        let result = cipher.decrypt(&nonce, &ciphertext, aad);
        assert!(result.is_err());
    }

    #[test]
    fn test_chacha20poly1305_empty() {
        let key = [0u8; 32];
        let nonce = [0u8; 12];
        let cipher = ChaCha20Poly1305::new(&key);
        let plaintext = b"";
        let aad = b"";

        let ciphertext = cipher.encrypt(&nonce, plaintext, aad);
        let decrypted = cipher.decrypt(&nonce, &ciphertext, aad).unwrap();

        assert_eq!(plaintext.as_slice(), decrypted.as_slice());
    }

    #[test]
    fn test_ed25519_keypair_generation() {
        let seed = [42u8; 32];
        let (public_key, secret_key) = Ed25519::generate_keypair(&seed);
        assert_eq!(public_key.len(), 32);
        assert_eq!(secret_key.len(), 64);
    }

    #[test]
    fn test_ed25519_sign_verify() {
        let seed = [1u8; 32];
        let (public_key, secret_key) = Ed25519::generate_keypair(&seed);
        let message = b"Hello, World!";

        let signature = Ed25519::sign(message, &secret_key);
        let valid = Ed25519::verify(message, &signature, &public_key);

        assert!(valid);
    }

    #[test]
    fn test_ed25519_verify_invalid() {
        let seed = [1u8; 32];
        let (public_key, secret_key) = Ed25519::generate_keypair(&seed);
        let message = b"Hello, World!";

        let mut signature = Ed25519::sign(message, &secret_key);
        signature[0] ^= 1;

        let valid = Ed25519::verify(message, &signature, &public_key);
        assert!(!valid);
    }

    #[test]
    fn test_ed25519_deterministic() {
        let seed = [99u8; 32];
        let (pk1, sk1) = Ed25519::generate_keypair(&seed);
        let (pk2, sk2) = Ed25519::generate_keypair(&seed);
        assert_eq!(pk1, pk2);
        assert_eq!(sk1, sk2);
    }
}
