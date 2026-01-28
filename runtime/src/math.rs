//! Math library providing vector, matrix, quaternion, and statistical operations.

#![allow(dead_code)]

use std::ops::{Add, Mul, Sub};

/// 2D Vector
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec2 {
    pub x: f64,
    pub y: f64,
}

impl Vec2 {
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    pub fn zero() -> Self {
        Self { x: 0.0, y: 0.0 }
    }

    pub fn dot(&self, other: &Vec2) -> f64 {
        self.x * other.x + self.y * other.y
    }

    pub fn length(&self) -> f64 {
        (self.x * self.x + self.y * self.y).sqrt()
    }

    pub fn normalize(&self) -> Vec2 {
        let len = self.length();
        if len > 0.0 {
            Vec2::new(self.x / len, self.y / len)
        } else {
            *self
        }
    }

    pub fn distance(&self, other: &Vec2) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }
}

impl Add for Vec2 {
    type Output = Vec2;
    fn add(self, other: Vec2) -> Vec2 {
        Vec2::new(self.x + other.x, self.y + other.y)
    }
}

impl Sub for Vec2 {
    type Output = Vec2;
    fn sub(self, other: Vec2) -> Vec2 {
        Vec2::new(self.x - other.x, self.y - other.y)
    }
}

impl Mul<f64> for Vec2 {
    type Output = Vec2;
    fn mul(self, scalar: f64) -> Vec2 {
        Vec2::new(self.x * scalar, self.y * scalar)
    }
}

/// 3D Vector
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec3 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Vec3 {
    pub fn new(x: f64, y: f64, z: f64) -> Self {
        Self { x, y, z }
    }

    pub fn zero() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            z: 0.0,
        }
    }

    pub fn dot(&self, other: &Vec3) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z
    }

    pub fn cross(&self, other: &Vec3) -> Vec3 {
        Vec3::new(
            self.y * other.z - self.z * other.y,
            self.z * other.x - self.x * other.z,
            self.x * other.y - self.y * other.x,
        )
    }

    pub fn length(&self) -> f64 {
        (self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    pub fn normalize(&self) -> Vec3 {
        let len = self.length();
        if len > 0.0 {
            Vec3::new(self.x / len, self.y / len, self.z / len)
        } else {
            *self
        }
    }

    pub fn distance(&self, other: &Vec3) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2) + (self.z - other.z).powi(2))
            .sqrt()
    }
}

impl Add for Vec3 {
    type Output = Vec3;
    fn add(self, other: Vec3) -> Vec3 {
        Vec3::new(self.x + other.x, self.y + other.y, self.z + other.z)
    }
}

impl Sub for Vec3 {
    type Output = Vec3;
    fn sub(self, other: Vec3) -> Vec3 {
        Vec3::new(self.x - other.x, self.y - other.y, self.z - other.z)
    }
}

impl Mul<f64> for Vec3 {
    type Output = Vec3;
    fn mul(self, scalar: f64) -> Vec3 {
        Vec3::new(self.x * scalar, self.y * scalar, self.z * scalar)
    }
}

/// 4D Vector
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Vec4 {
    pub x: f64,
    pub y: f64,
    pub z: f64,
    pub w: f64,
}

impl Vec4 {
    pub fn new(x: f64, y: f64, z: f64, w: f64) -> Self {
        Self { x, y, z, w }
    }

    pub fn zero() -> Self {
        Self {
            x: 0.0,
            y: 0.0,
            z: 0.0,
            w: 0.0,
        }
    }

    pub fn dot(&self, other: &Vec4) -> f64 {
        self.x * other.x + self.y * other.y + self.z * other.z + self.w * other.w
    }

    pub fn length(&self) -> f64 {
        (self.x * self.x + self.y * self.y + self.z * self.z + self.w * self.w).sqrt()
    }

    pub fn normalize(&self) -> Vec4 {
        let len = self.length();
        if len > 0.0 {
            Vec4::new(self.x / len, self.y / len, self.z / len, self.w / len)
        } else {
            *self
        }
    }
}

impl Add for Vec4 {
    type Output = Vec4;
    fn add(self, other: Vec4) -> Vec4 {
        Vec4::new(
            self.x + other.x,
            self.y + other.y,
            self.z + other.z,
            self.w + other.w,
        )
    }
}

impl Sub for Vec4 {
    type Output = Vec4;
    fn sub(self, other: Vec4) -> Vec4 {
        Vec4::new(
            self.x - other.x,
            self.y - other.y,
            self.z - other.z,
            self.w - other.w,
        )
    }
}

impl Mul<f64> for Vec4 {
    type Output = Vec4;
    fn mul(self, scalar: f64) -> Vec4 {
        Vec4::new(
            self.x * scalar,
            self.y * scalar,
            self.z * scalar,
            self.w * scalar,
        )
    }
}

/// 2x2 Matrix
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mat2 {
    pub m: [[f64; 2]; 2],
}

impl Mat2 {
    pub fn new(m: [[f64; 2]; 2]) -> Self {
        Self { m }
    }

    pub fn identity() -> Self {
        Self {
            m: [[1.0, 0.0], [0.0, 1.0]],
        }
    }

    pub fn zero() -> Self {
        Self {
            m: [[0.0, 0.0], [0.0, 0.0]],
        }
    }

    pub fn determinant(&self) -> f64 {
        self.m[0][0] * self.m[1][1] - self.m[0][1] * self.m[1][0]
    }

    pub fn transpose(&self) -> Mat2 {
        Mat2::new([[self.m[0][0], self.m[1][0]], [self.m[0][1], self.m[1][1]]])
    }

    pub fn inverse(&self) -> Option<Mat2> {
        let det = self.determinant();
        if det.abs() < 1e-10 {
            return None;
        }
        let inv_det = 1.0 / det;
        Some(Mat2::new([
            [self.m[1][1] * inv_det, -self.m[0][1] * inv_det],
            [-self.m[1][0] * inv_det, self.m[0][0] * inv_det],
        ]))
    }
}

impl Mul for Mat2 {
    type Output = Mat2;
    fn mul(self, other: Mat2) -> Mat2 {
        let mut result = Mat2::zero();
        for i in 0..2 {
            for j in 0..2 {
                for k in 0..2 {
                    result.m[i][j] += self.m[i][k] * other.m[k][j];
                }
            }
        }
        result
    }
}

/// 3x3 Matrix
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mat3 {
    pub m: [[f64; 3]; 3],
}

impl Mat3 {
    pub fn new(m: [[f64; 3]; 3]) -> Self {
        Self { m }
    }

    pub fn identity() -> Self {
        Self {
            m: [[1.0, 0.0, 0.0], [0.0, 1.0, 0.0], [0.0, 0.0, 1.0]],
        }
    }

    pub fn zero() -> Self {
        Self {
            m: [[0.0, 0.0, 0.0], [0.0, 0.0, 0.0], [0.0, 0.0, 0.0]],
        }
    }

    pub fn determinant(&self) -> f64 {
        self.m[0][0] * (self.m[1][1] * self.m[2][2] - self.m[1][2] * self.m[2][1])
            - self.m[0][1] * (self.m[1][0] * self.m[2][2] - self.m[1][2] * self.m[2][0])
            + self.m[0][2] * (self.m[1][0] * self.m[2][1] - self.m[1][1] * self.m[2][0])
    }

    pub fn transpose(&self) -> Mat3 {
        Mat3::new([
            [self.m[0][0], self.m[1][0], self.m[2][0]],
            [self.m[0][1], self.m[1][1], self.m[2][1]],
            [self.m[0][2], self.m[1][2], self.m[2][2]],
        ])
    }
}

impl Mul for Mat3 {
    type Output = Mat3;
    fn mul(self, other: Mat3) -> Mat3 {
        let mut result = Mat3::zero();
        for i in 0..3 {
            for j in 0..3 {
                for k in 0..3 {
                    result.m[i][j] += self.m[i][k] * other.m[k][j];
                }
            }
        }
        result
    }
}

/// 4x4 Matrix
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Mat4 {
    pub m: [[f64; 4]; 4],
}

impl Mat4 {
    pub fn new(m: [[f64; 4]; 4]) -> Self {
        Self { m }
    }

    pub fn identity() -> Self {
        Self {
            m: [
                [1.0, 0.0, 0.0, 0.0],
                [0.0, 1.0, 0.0, 0.0],
                [0.0, 0.0, 1.0, 0.0],
                [0.0, 0.0, 0.0, 1.0],
            ],
        }
    }

    pub fn zero() -> Self {
        Self {
            m: [
                [0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0],
                [0.0, 0.0, 0.0, 0.0],
            ],
        }
    }

    pub fn transpose(&self) -> Mat4 {
        let mut result = Mat4::zero();
        for i in 0..4 {
            for j in 0..4 {
                result.m[i][j] = self.m[j][i];
            }
        }
        result
    }
}

impl Mul for Mat4 {
    type Output = Mat4;
    fn mul(self, other: Mat4) -> Mat4 {
        let mut result = Mat4::zero();
        for i in 0..4 {
            for j in 0..4 {
                for k in 0..4 {
                    result.m[i][j] += self.m[i][k] * other.m[k][j];
                }
            }
        }
        result
    }
}

/// Quaternion for 3D rotations
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct Quaternion {
    pub w: f64,
    pub x: f64,
    pub y: f64,
    pub z: f64,
}

impl Quaternion {
    pub fn new(w: f64, x: f64, y: f64, z: f64) -> Self {
        Self { w, x, y, z }
    }

    pub fn identity() -> Self {
        Self {
            w: 1.0,
            x: 0.0,
            y: 0.0,
            z: 0.0,
        }
    }

    pub fn from_axis_angle(axis: Vec3, angle: f64) -> Self {
        let half_angle = angle / 2.0;
        let s = half_angle.sin();
        let normalized = axis.normalize();
        Self {
            w: half_angle.cos(),
            x: normalized.x * s,
            y: normalized.y * s,
            z: normalized.z * s,
        }
    }

    pub fn length(&self) -> f64 {
        (self.w * self.w + self.x * self.x + self.y * self.y + self.z * self.z).sqrt()
    }

    pub fn normalize(&self) -> Quaternion {
        let len = self.length();
        if len > 0.0 {
            Quaternion::new(self.w / len, self.x / len, self.y / len, self.z / len)
        } else {
            *self
        }
    }

    pub fn conjugate(&self) -> Quaternion {
        Quaternion::new(self.w, -self.x, -self.y, -self.z)
    }

    pub fn inverse(&self) -> Quaternion {
        let len_sq = self.w * self.w + self.x * self.x + self.y * self.y + self.z * self.z;
        if len_sq > 0.0 {
            let conj = self.conjugate();
            Quaternion::new(
                conj.w / len_sq,
                conj.x / len_sq,
                conj.y / len_sq,
                conj.z / len_sq,
            )
        } else {
            *self
        }
    }
}

impl Mul for Quaternion {
    type Output = Quaternion;
    fn mul(self, other: Quaternion) -> Quaternion {
        Quaternion::new(
            self.w * other.w - self.x * other.x - self.y * other.y - self.z * other.z,
            self.w * other.x + self.x * other.w + self.y * other.z - self.z * other.y,
            self.w * other.y - self.x * other.z + self.y * other.w + self.z * other.x,
            self.w * other.z + self.x * other.y - self.y * other.x + self.z * other.w,
        )
    }
}

/// Statistical functions
pub struct Statistics;

impl Statistics {
    pub fn mean(data: &[f64]) -> f64 {
        if data.is_empty() {
            return 0.0;
        }
        data.iter().sum::<f64>() / data.len() as f64
    }

    pub fn median(data: &[f64]) -> f64 {
        if data.is_empty() {
            return 0.0;
        }
        let mut sorted = data.to_vec();
        sorted.sort_by(|a, b| a.partial_cmp(b).unwrap());
        let mid = sorted.len() / 2;
        if sorted.len() % 2 == 0 {
            (sorted[mid - 1] + sorted[mid]) / 2.0
        } else {
            sorted[mid]
        }
    }

    pub fn variance(data: &[f64]) -> f64 {
        if data.is_empty() {
            return 0.0;
        }
        let mean = Self::mean(data);
        data.iter().map(|x| (x - mean).powi(2)).sum::<f64>() / data.len() as f64
    }

    pub fn std_dev(data: &[f64]) -> f64 {
        Self::variance(data).sqrt()
    }

    pub fn min(data: &[f64]) -> f64 {
        data.iter().copied().fold(f64::INFINITY, |a, b| a.min(b))
    }

    pub fn max(data: &[f64]) -> f64 {
        data.iter()
            .copied()
            .fold(f64::NEG_INFINITY, |a, b| a.max(b))
    }

    pub fn sum(data: &[f64]) -> f64 {
        data.iter().sum()
    }

    pub fn product(data: &[f64]) -> f64 {
        data.iter().product()
    }
}

/// Advanced trigonometry
pub struct Trigonometry;

impl Trigonometry {
    pub fn sin(x: f64) -> f64 {
        x.sin()
    }

    pub fn cos(x: f64) -> f64 {
        x.cos()
    }

    pub fn tan(x: f64) -> f64 {
        x.tan()
    }

    pub fn asin(x: f64) -> f64 {
        x.asin()
    }

    pub fn acos(x: f64) -> f64 {
        x.acos()
    }

    pub fn atan(x: f64) -> f64 {
        x.atan()
    }

    pub fn atan2(y: f64, x: f64) -> f64 {
        y.atan2(x)
    }

    pub fn sinh(x: f64) -> f64 {
        x.sinh()
    }

    pub fn cosh(x: f64) -> f64 {
        x.cosh()
    }

    pub fn tanh(x: f64) -> f64 {
        x.tanh()
    }

    pub fn asinh(x: f64) -> f64 {
        x.asinh()
    }

    pub fn acosh(x: f64) -> f64 {
        x.acosh()
    }

    pub fn atanh(x: f64) -> f64 {
        x.atanh()
    }

    pub fn deg_to_rad(deg: f64) -> f64 {
        deg * std::f64::consts::PI / 180.0
    }

    pub fn rad_to_deg(rad: f64) -> f64 {
        rad * 180.0 / std::f64::consts::PI
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_vec2_operations() {
        let v1 = Vec2::new(3.0, 4.0);
        let v2 = Vec2::new(1.0, 2.0);

        assert_eq!(v1 + v2, Vec2::new(4.0, 6.0));
        assert_eq!(v1 - v2, Vec2::new(2.0, 2.0));
        assert_eq!(v1 * 2.0, Vec2::new(6.0, 8.0));
        assert_eq!(v1.dot(&v2), 11.0);
        assert_eq!(v1.length(), 5.0);
    }

    #[test]
    fn test_vec2_normalize() {
        let v = Vec2::new(3.0, 4.0);
        let normalized = v.normalize();
        assert!((normalized.length() - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_vec3_operations() {
        let v1 = Vec3::new(1.0, 2.0, 3.0);
        let v2 = Vec3::new(4.0, 5.0, 6.0);

        assert_eq!(v1 + v2, Vec3::new(5.0, 7.0, 9.0));
        assert_eq!(v1.dot(&v2), 32.0);
    }

    #[test]
    fn test_vec3_cross() {
        let v1 = Vec3::new(1.0, 0.0, 0.0);
        let v2 = Vec3::new(0.0, 1.0, 0.0);
        let cross = v1.cross(&v2);
        assert_eq!(cross, Vec3::new(0.0, 0.0, 1.0));
    }

    #[test]
    fn test_vec4_operations() {
        let v1 = Vec4::new(1.0, 2.0, 3.0, 4.0);
        let v2 = Vec4::new(5.0, 6.0, 7.0, 8.0);

        assert_eq!(v1 + v2, Vec4::new(6.0, 8.0, 10.0, 12.0));
        assert_eq!(v1.dot(&v2), 70.0);
    }

    #[test]
    fn test_mat2_identity() {
        let m = Mat2::identity();
        let v = Mat2::new([[2.0, 3.0], [4.0, 5.0]]);
        assert_eq!(m * v, v);
    }

    #[test]
    fn test_mat2_determinant() {
        let m = Mat2::new([[1.0, 2.0], [3.0, 4.0]]);
        assert_eq!(m.determinant(), -2.0);
    }

    #[test]
    fn test_mat2_inverse() {
        let m = Mat2::new([[4.0, 7.0], [2.0, 6.0]]);
        let inv = m.inverse().unwrap();
        let product = m * inv;
        let identity = Mat2::identity();

        for i in 0..2 {
            for j in 0..2 {
                assert!((product.m[i][j] - identity.m[i][j]).abs() < 1e-10);
            }
        }
    }

    #[test]
    fn test_mat3_operations() {
        let m1 = Mat3::identity();
        let m2 = Mat3::new([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0], [7.0, 8.0, 9.0]]);
        assert_eq!(m1 * m2, m2);
    }

    #[test]
    fn test_mat3_determinant() {
        let m = Mat3::new([[1.0, 2.0, 3.0], [0.0, 1.0, 4.0], [5.0, 6.0, 0.0]]);
        assert_eq!(m.determinant(), 1.0);
    }

    #[test]
    fn test_mat4_operations() {
        let m = Mat4::identity();
        let transposed = m.transpose();
        assert_eq!(m, transposed);
    }

    #[test]
    fn test_quaternion_identity() {
        let q = Quaternion::identity();
        assert_eq!(q.length(), 1.0);
    }

    #[test]
    fn test_quaternion_multiply() {
        let q1 = Quaternion::identity();
        let q2 = Quaternion::new(0.5, 0.5, 0.5, 0.5);
        let result = q1 * q2;
        assert_eq!(result, q2);
    }

    #[test]
    fn test_quaternion_conjugate() {
        let q = Quaternion::new(1.0, 2.0, 3.0, 4.0);
        let conj = q.conjugate();
        assert_eq!(conj, Quaternion::new(1.0, -2.0, -3.0, -4.0));
    }

    #[test]
    fn test_statistics_mean() {
        let data = vec![1.0, 2.0, 3.0, 4.0, 5.0];
        assert_eq!(Statistics::mean(&data), 3.0);
    }

    #[test]
    fn test_statistics_median() {
        let data = vec![1.0, 3.0, 2.0, 5.0, 4.0];
        assert_eq!(Statistics::median(&data), 3.0);
    }

    #[test]
    fn test_statistics_variance() {
        let data = vec![2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        let variance = Statistics::variance(&data);
        assert!((variance - 4.0).abs() < 0.1);
    }

    #[test]
    fn test_statistics_std_dev() {
        let data = vec![2.0, 4.0, 4.0, 4.0, 5.0, 5.0, 7.0, 9.0];
        let std_dev = Statistics::std_dev(&data);
        assert!((std_dev - 2.0).abs() < 0.1);
    }

    #[test]
    fn test_statistics_min_max() {
        let data = vec![3.0, 1.0, 4.0, 1.0, 5.0, 9.0, 2.0, 6.0];
        assert_eq!(Statistics::min(&data), 1.0);
        assert_eq!(Statistics::max(&data), 9.0);
    }

    #[test]
    fn test_trigonometry_basic() {
        let angle = std::f64::consts::PI / 4.0;
        let expected = std::f64::consts::FRAC_1_SQRT_2;
        assert!((Trigonometry::sin(angle) - expected).abs() < 1e-10);
        assert!((Trigonometry::cos(angle) - expected).abs() < 1e-10);
    }

    #[test]
    fn test_trigonometry_conversion() {
        let deg = 180.0;
        let rad = Trigonometry::deg_to_rad(deg);
        assert!((rad - std::f64::consts::PI).abs() < 1e-10);
        assert!((Trigonometry::rad_to_deg(rad) - deg).abs() < 1e-10);
    }
}
