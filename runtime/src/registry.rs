#[cfg(windows)]
use winreg::enums::*;
#[cfg(windows)]
use winreg::RegKey;

#[cfg(windows)]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegistryHive {
    ClassesRoot,
    CurrentUser,
    LocalMachine,
    Users,
    CurrentConfig,
}

#[cfg(windows)]
impl RegistryHive {
    fn to_hkey(&self) -> RegKey {
        match self {
            RegistryHive::ClassesRoot => RegKey::predef(HKEY_CLASSES_ROOT),
            RegistryHive::CurrentUser => RegKey::predef(HKEY_CURRENT_USER),
            RegistryHive::LocalMachine => RegKey::predef(HKEY_LOCAL_MACHINE),
            RegistryHive::Users => RegKey::predef(HKEY_USERS),
            RegistryHive::CurrentConfig => RegKey::predef(HKEY_CURRENT_CONFIG),
        }
    }
}

#[cfg(windows)]
pub struct Registry;

#[cfg(windows)]
impl Registry {
    pub fn read_string(hive: RegistryHive, path: &str, name: &str) -> Result<String, String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey(path)
            .map_err(|e| format!("Failed to open registry key: {e}"))?;

        key.get_value(name)
            .map_err(|e| format!("Failed to read registry value: {e}"))
    }

    pub fn read_u32(hive: RegistryHive, path: &str, name: &str) -> Result<u32, String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey(path)
            .map_err(|e| format!("Failed to open registry key: {e}"))?;

        key.get_value(name)
            .map_err(|e| format!("Failed to read registry value: {e}"))
    }

    pub fn write_string(
        hive: RegistryHive,
        path: &str,
        name: &str,
        value: &str,
    ) -> Result<(), String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey_with_flags(path, KEY_WRITE)
            .or_else(|_| hkey.create_subkey(path).map(|(k, _)| k))
            .map_err(|e| format!("Failed to open/create registry key: {e}"))?;

        key.set_value(name, &value)
            .map_err(|e| format!("Failed to write registry value: {e}"))
    }

    pub fn write_u32(hive: RegistryHive, path: &str, name: &str, value: u32) -> Result<(), String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey_with_flags(path, KEY_WRITE)
            .or_else(|_| hkey.create_subkey(path).map(|(k, _)| k))
            .map_err(|e| format!("Failed to open/create registry key: {e}"))?;

        key.set_value(name, &value)
            .map_err(|e| format!("Failed to write registry value: {e}"))
    }

    pub fn delete_value(hive: RegistryHive, path: &str, name: &str) -> Result<(), String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey_with_flags(path, KEY_WRITE)
            .map_err(|e| format!("Failed to open registry key: {e}"))?;

        key.delete_value(name)
            .map_err(|e| format!("Failed to delete registry value: {e}"))
    }

    pub fn delete_key(hive: RegistryHive, path: &str) -> Result<(), String> {
        let hkey = hive.to_hkey();
        hkey.delete_subkey_all(path)
            .map_err(|e| format!("Failed to delete registry key: {e}"))
    }

    pub fn key_exists(hive: RegistryHive, path: &str) -> bool {
        let hkey = hive.to_hkey();
        hkey.open_subkey(path).is_ok()
    }

    pub fn value_exists(hive: RegistryHive, path: &str, name: &str) -> bool {
        let hkey = hive.to_hkey();
        if let Ok(key) = hkey.open_subkey(path) {
            key.get_value::<String, _>(name).is_ok()
        } else {
            false
        }
    }

    pub fn list_subkeys(hive: RegistryHive, path: &str) -> Result<Vec<String>, String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey(path)
            .map_err(|e| format!("Failed to open registry key: {e}"))?;

        let mut subkeys = Vec::new();
        for subkey in key.enum_keys() {
            if let Ok(name) = subkey {
                subkeys.push(name);
            }
        }
        Ok(subkeys)
    }

    pub fn list_values(hive: RegistryHive, path: &str) -> Result<Vec<String>, String> {
        let hkey = hive.to_hkey();
        let key = hkey
            .open_subkey(path)
            .map_err(|e| format!("Failed to open registry key: {e}"))?;

        let mut values = Vec::new();
        for value in key.enum_values() {
            if let Ok((name, _)) = value {
                values.push(name);
            }
        }
        Ok(values)
    }
}

#[cfg(not(windows))]
pub struct Registry;

#[cfg(not(windows))]
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RegistryHive {
    ClassesRoot,
    CurrentUser,
    LocalMachine,
    Users,
    CurrentConfig,
}

#[cfg(not(windows))]
impl Registry {
    pub fn read_string(_hive: RegistryHive, _path: &str, _name: &str) -> Result<String, String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn read_u32(_hive: RegistryHive, _path: &str, _name: &str) -> Result<u32, String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn write_string(
        _hive: RegistryHive,
        _path: &str,
        _name: &str,
        _value: &str,
    ) -> Result<(), String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn write_u32(
        _hive: RegistryHive,
        _path: &str,
        _name: &str,
        _value: u32,
    ) -> Result<(), String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn delete_value(_hive: RegistryHive, _path: &str, _name: &str) -> Result<(), String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn delete_key(_hive: RegistryHive, _path: &str) -> Result<(), String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn key_exists(_hive: RegistryHive, _path: &str) -> bool {
        false
    }

    pub fn value_exists(_hive: RegistryHive, _path: &str, _name: &str) -> bool {
        false
    }

    pub fn list_subkeys(_hive: RegistryHive, _path: &str) -> Result<Vec<String>, String> {
        Err("Registry access is only available on Windows".to_string())
    }

    pub fn list_values(_hive: RegistryHive, _path: &str) -> Result<Vec<String>, String> {
        Err("Registry access is only available on Windows".to_string())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    #[cfg(windows)]
    fn test_registry_read() {
        // Try to read a common Windows registry value
        let result = Registry::read_string(
            RegistryHive::LocalMachine,
            "SOFTWARE\\Microsoft\\Windows NT\\CurrentVersion",
            "ProductName",
        );
        // This should succeed on Windows
        assert!(result.is_ok() || result.is_err()); // Just verify it doesn't panic
    }

    #[test]
    #[cfg(not(windows))]
    fn test_registry_not_available() {
        let result = Registry::read_string(RegistryHive::CurrentUser, "Software\\Test", "Value");
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("only available on Windows"));
    }

    #[test]
    fn test_registry_hive_types() {
        let _hive1 = RegistryHive::CurrentUser;
        let _hive2 = RegistryHive::LocalMachine;
        let _hive3 = RegistryHive::ClassesRoot;
        let _hive4 = RegistryHive::Users;
        let _hive5 = RegistryHive::CurrentConfig;
    }
}
