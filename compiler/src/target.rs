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

impl std::str::FromStr for Target {
    type Err = String;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = s.split('-').collect();
        if parts.len() < 3 {
            return Err(format!("Invalid target triple: {s}"));
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
    fn test_parse_target_windows() {
        let target: Target = "x86_64-pc-windows-msvc".parse().unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
        assert_eq!(target.vendor, Vendor::Pc);
        assert_eq!(target.os, OperatingSystem::Windows);
        assert_eq!(target.env, Environment::Msvc);
    }

    #[test]
    fn test_parse_target_linux_gnu() {
        let target: Target = "x86_64-unknown-linux-gnu".parse().unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
        assert_eq!(target.vendor, Vendor::Unknown);
        assert_eq!(target.os, OperatingSystem::Linux);
        assert_eq!(target.env, Environment::Gnu);
    }

    #[test]
    fn test_parse_target_linux_musl() {
        let target: Target = "x86_64-unknown-linux-musl".parse().unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
        assert_eq!(target.os, OperatingSystem::Linux);
        assert_eq!(target.env, Environment::Musl);
    }

    #[test]
    fn test_parse_target_macos_x86() {
        let target: Target = "x86_64-apple-darwin".parse().unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
        assert_eq!(target.vendor, Vendor::Apple);
        assert_eq!(target.os, OperatingSystem::MacOS);
    }

    #[test]
    fn test_parse_target_macos_aarch64() {
        let target: Target = "aarch64-apple-darwin".parse().unwrap();
        assert_eq!(target.arch, Architecture::Aarch64);
        assert_eq!(target.vendor, Vendor::Apple);
        assert_eq!(target.os, OperatingSystem::MacOS);
    }

    #[test]
    fn test_parse_target_aarch64_linux() {
        let target: Target = "aarch64-unknown-linux-gnu".parse().unwrap();
        assert_eq!(target.arch, Architecture::Aarch64);
        assert_eq!(target.os, OperatingSystem::Linux);
        assert_eq!(target.env, Environment::Gnu);
    }

    #[test]
    fn test_parse_target_arm() {
        let target: Target = "arm-unknown-linux-gnu".parse().unwrap();
        assert_eq!(target.arch, Architecture::Arm);
        assert_eq!(target.os, OperatingSystem::Linux);
    }

    #[test]
    fn test_parse_target_riscv64() {
        let target: Target = "riscv64-unknown-linux-gnu".parse().unwrap();
        assert_eq!(target.arch, Architecture::Riscv64);
        assert_eq!(target.os, OperatingSystem::Linux);
    }

    #[test]
    fn test_parse_target_wasm32() {
        let target: Target = "wasm32-unknown-wasi".parse().unwrap();
        assert_eq!(target.arch, Architecture::Wasm32);
        assert_eq!(target.os, OperatingSystem::Wasi);
    }

    #[test]
    fn test_parse_target_freebsd() {
        let target: Target = "x86_64-unknown-freebsd".parse().unwrap();
        assert_eq!(target.os, OperatingSystem::FreeBSD);
    }

    #[test]
    fn test_parse_target_android() {
        let target: Target = "aarch64-unknown-android".parse().unwrap();
        assert_eq!(target.arch, Architecture::Aarch64);
        assert_eq!(target.os, OperatingSystem::Android);
    }

    #[test]
    fn test_parse_target_ios() {
        let target: Target = "aarch64-apple-ios".parse().unwrap();
        assert_eq!(target.arch, Architecture::Aarch64);
        assert_eq!(target.vendor, Vendor::Apple);
        assert_eq!(target.os, OperatingSystem::IOS);
    }

    #[test]
    fn test_parse_invalid_target_too_short() {
        let result: Result<Target, String> = "x86_64-unknown".parse();
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_arch() {
        let result: Result<Target, String> = "mips-unknown-linux".parse();
        assert!(result.is_err());
    }

    #[test]
    fn test_parse_invalid_os() {
        let result: Result<Target, String> = "x86_64-unknown-solaris".parse();
        assert!(result.is_err());
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

    #[test]
    fn test_llvm_triple_linux() {
        let target = Target {
            arch: Architecture::X86_64,
            vendor: Vendor::Unknown,
            os: OperatingSystem::Linux,
            env: Environment::Gnu,
        };
        assert_eq!(target.to_llvm_triple(), "x86_64-unknown-linux-gnu");
    }

    #[test]
    fn test_architecture_display() {
        assert_eq!(format!("{}", Architecture::X86_64), "x86_64");
        assert_eq!(format!("{}", Architecture::Aarch64), "aarch64");
        assert_eq!(format!("{}", Architecture::Arm), "arm");
        assert_eq!(format!("{}", Architecture::Riscv64), "riscv64");
        assert_eq!(format!("{}", Architecture::Wasm32), "wasm32");
    }

    #[test]
    fn test_os_display() {
        assert_eq!(format!("{}", OperatingSystem::Linux), "linux");
        assert_eq!(format!("{}", OperatingSystem::Windows), "windows");
        assert_eq!(format!("{}", OperatingSystem::MacOS), "darwin");
        assert_eq!(format!("{}", OperatingSystem::FreeBSD), "freebsd");
        assert_eq!(format!("{}", OperatingSystem::Android), "android");
        assert_eq!(format!("{}", OperatingSystem::IOS), "ios");
        assert_eq!(format!("{}", OperatingSystem::Wasi), "wasi");
    }

    #[test]
    fn test_vendor_display() {
        assert_eq!(format!("{}", Vendor::Unknown), "unknown");
        assert_eq!(format!("{}", Vendor::Apple), "apple");
        assert_eq!(format!("{}", Vendor::Pc), "pc");
        assert_eq!(format!("{}", Vendor::Nvidia), "nvidia");
    }

    #[test]
    fn test_environment_display() {
        assert_eq!(format!("{}", Environment::Gnu), "gnu");
        assert_eq!(format!("{}", Environment::Musl), "musl");
        assert_eq!(format!("{}", Environment::Msvc), "msvc");
        assert_eq!(format!("{}", Environment::Darwin), "darwin");
        assert_eq!(format!("{}", Environment::None), "");
    }

    #[test]
    fn test_amd64_alias() {
        let target: Target = "amd64-unknown-linux-gnu".parse().unwrap();
        assert_eq!(target.arch, Architecture::X86_64);
    }

    #[test]
    fn test_arm64_alias() {
        let target: Target = "arm64-apple-darwin".parse().unwrap();
        assert_eq!(target.arch, Architecture::Aarch64);
    }

    #[test]
    fn test_macos_alias() {
        let target: Target = "x86_64-apple-macos".parse().unwrap();
        assert_eq!(target.os, OperatingSystem::MacOS);
    }
}
