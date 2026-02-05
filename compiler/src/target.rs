//! Cross-platform target specification and compilation.

use serde::{Deserialize, Serialize};
use std::fmt;

/// Target triple specification
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct Target {
    pub arch: Architecture,
    pub vendor: Vendor,
    pub os: OperatingSystem,
    pub env: Environment,
}

/// CPU Architecture
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Architecture {
    X86_64,
    Aarch64,
    Arm,
    Riscv64,
    Wasm32,
}

/// Vendor
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Vendor {
    Unknown,
    Apple,
    Pc,
    Nvidia,
}

/// Operating System
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum OperatingSystem {
    Linux,
    Windows,
    MacOS,
    FreeBSD,
    Android,
    IOS,
    Wasi,
}

/// Environment/ABI
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Environment {
    Gnu,
    Musl,
    Msvc,
    Darwin,
    None,
}

impl Target {
    /// Get the host target (current platform)
    pub fn host() -> Self {
        #[cfg(all(target_arch = "x86_64", target_os = "linux", target_env = "gnu"))]
        return Self {
            arch: Architecture::X86_64,
            vendor: Vendor::Unknown,
            os: OperatingSystem::Linux,
            env: Environment::Gnu,
        };

        #[cfg(all(target_arch = "x86_64", target_os = "macos"))]
        return Self {
            arch: Architecture::X86_64,
            vendor: Vendor::Apple,
            os: OperatingSystem::MacOS,
            env: Environment::Darwin,
        };

        #[cfg(all(target_arch = "x86_64", target_os = "windows", target_env = "msvc"))]
        return Self {
            arch: Architecture::X86_64,
            vendor: Vendor::Pc,
            os: OperatingSystem::Windows,
            env: Environment::Msvc,
        };

        #[cfg(all(target_arch = "aarch64", target_os = "macos"))]
        return Self {
            arch: Architecture::Aarch64,
            vendor: Vendor::Apple,
            os: OperatingSystem::MacOS,
            env: Environment::Darwin,
        };

        #[cfg(all(target_arch = "aarch64", target_os = "linux"))]
        return Self {
            arch: Architecture::Aarch64,
            vendor: Vendor::Unknown,
            os: OperatingSystem::Linux,
            env: Environment::Gnu,
        };

        // Fallback
        #[allow(unreachable_code)]
        Self {
            arch: Architecture::X86_64,
            vendor: Vendor::Unknown,
            os: OperatingSystem::Linux,
            env: Environment::Gnu,
        }
    }

    /// Parse target from string (e.g., "x86_64-pc-windows-msvc")
    pub fn from_str(s: &str) -> Result<Self, String> {
        let parts: Vec<&str> = s.split('-').collect();
        if parts.len() < 3 {
            return Err(format!("Invalid target triple: {}", s));
        }

        let arch = match parts[0] {
            "x86_64" | "amd64" => Architecture::X86_64,
            "aarch64" | "arm64" => Architecture::Aarch64,
            "arm" => Architecture::Arm,
            "riscv64" => Architecture::Riscv64,
            "wasm32" => Architecture::Wasm32,
            _ => return Err(format!("Unknown architecture: {}", parts[0])),
        };

        let vendor = match parts[1] {
            "unknown" => Vendor::Unknown,
            "apple" => Vendor::Apple,
            "pc" => Vendor::Pc,
            "nvidia" => Vendor::Nvidia,
            _ => Vendor::Unknown,
        };

        let os = match parts[2] {
            "linux" => OperatingSystem::Linux,
            "windows" => OperatingSystem::Windows,
            "darwin" | "macos" => OperatingSystem::MacOS,
            "freebsd" => OperatingSystem::FreeBSD,
            "android" => OperatingSystem::Android,
            "ios" => OperatingSystem::IOS,
            "wasi" => OperatingSystem::Wasi,
            _ => return Err(format!("Unknown OS: {}", parts[2])),
        };

        let env = if parts.len() > 3 {
            match parts[3] {
                "gnu" => Environment::Gnu,
                "musl" => Environment::Musl,
                "msvc" => Environment::Msvc,
                "darwin" => Environment::Darwin,
                _ => Environment::None,
            }
        } else {
            Environment::None
        };

        Ok(Self {
            arch,
            vendor,
            os,
            env,
        })
    }

    /// Get LLVM target triple string
    pub fn to_llvm_triple(&self) -> String {
        format!("{}-{}-{}-{}", self.arch, self.vendor, self.os, self.env)
    }
}

impl fmt::Display for Architecture {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Architecture::X86_64 => write!(f, "x86_64"),
            Architecture::Aarch64 => write!(f, "aarch64"),
            Architecture::Arm => write!(f, "arm"),
            Architecture::Riscv64 => write!(f, "riscv64"),
            Architecture::Wasm32 => write!(f, "wasm32"),
        }
    }
}

impl fmt::Display for Vendor {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Vendor::Unknown => write!(f, "unknown"),
            Vendor::Apple => write!(f, "apple"),
            Vendor::Pc => write!(f, "pc"),
            Vendor::Nvidia => write!(f, "nvidia"),
        }
    }
}

impl fmt::Display for OperatingSystem {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            OperatingSystem::Linux => write!(f, "linux"),
            OperatingSystem::Windows => write!(f, "windows"),
            OperatingSystem::MacOS => write!(f, "darwin"),
            OperatingSystem::FreeBSD => write!(f, "freebsd"),
            OperatingSystem::Android => write!(f, "android"),
            OperatingSystem::IOS => write!(f, "ios"),
            OperatingSystem::Wasi => write!(f, "wasi"),
        }
    }
}

impl fmt::Display for Environment {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Environment::Gnu => write!(f, "gnu"),
            Environment::Musl => write!(f, "musl"),
            Environment::Msvc => write!(f, "msvc"),
            Environment::Darwin => write!(f, "darwin"),
            Environment::None => write!(f, ""),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_host_target() {
        let target = Target::host();
        assert!(!target.to_llvm_triple().is_empty());
    }

    #[test]
    fn test_parse_target() {
        let target = Target::from_str("x86_64-pc-windows-msvc").unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
        assert_eq!(target.os, OperatingSystem::Windows);
        assert_eq!(target.env, Environment::Msvc);
    }

    #[test]
    fn test_llvm_triple() {
        let target = Target {
            arch: Architecture::X86_64,
            vendor: Vendor::Apple,
            os: OperatingSystem::MacOS,
            env: Environment::Darwin,
        };
        assert_eq!(target.to_llvm_triple(), "x86_64-apple-darwin-darwin");
    }
}
