use std::env;
use std::io::{self, Read, Write};
use std::path::{Path, PathBuf};
use std::process::{Child, Command, ExitStatus, Stdio};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Platform {
    Windows,
    Linux,
    MacOS,
    FreeBSD,
    Unknown,
}

impl Platform {
    pub fn current() -> Self {
        if cfg!(target_os = "windows") {
            Platform::Windows
        } else if cfg!(target_os = "linux") {
            Platform::Linux
        } else if cfg!(target_os = "macos") {
            Platform::MacOS
        } else if cfg!(target_os = "freebsd") {
            Platform::FreeBSD
        } else {
            Platform::Unknown
        }
    }

    pub fn is_windows(&self) -> bool {
        matches!(self, Platform::Windows)
    }

    pub fn is_unix(&self) -> bool {
        matches!(self, Platform::Linux | Platform::MacOS | Platform::FreeBSD)
    }

    pub fn name(&self) -> &'static str {
        match self {
            Platform::Windows => "windows",
            Platform::Linux => "linux",
            Platform::MacOS => "macos",
            Platform::FreeBSD => "freebsd",
            Platform::Unknown => "unknown",
        }
    }

    pub fn path_separator(&self) -> char {
        if self.is_windows() {
            '\\'
        } else {
            '/'
        }
    }

    pub fn path_list_separator(&self) -> char {
        if self.is_windows() {
            ';'
        } else {
            ':'
        }
    }
}

pub struct PathUtils;

impl PathUtils {
    pub fn normalize(path: &str) -> String {
        let path = Path::new(path);
        let mut components = Vec::new();

        for component in path.components() {
            match component {
                std::path::Component::ParentDir => {
                    components.pop();
                }
                std::path::Component::CurDir => {}
                _ => components.push(component.as_os_str().to_string_lossy().to_string()),
            }
        }

        components.join(&Platform::current().path_separator().to_string())
    }

    pub fn join(base: &str, path: &str) -> String {
        let base_path = Path::new(base);
        let joined = base_path.join(path);
        joined.to_string_lossy().to_string()
    }

    pub fn absolute(path: &str) -> io::Result<String> {
        let path = Path::new(path);
        let abs = if path.is_absolute() {
            path.to_path_buf()
        } else {
            env::current_dir()?.join(path)
        };
        Ok(abs.to_string_lossy().to_string())
    }

    pub fn parent(path: &str) -> Option<String> {
        Path::new(path)
            .parent()
            .map(|p| p.to_string_lossy().to_string())
    }

    pub fn filename(path: &str) -> Option<String> {
        Path::new(path)
            .file_name()
            .map(|f| f.to_string_lossy().to_string())
    }

    pub fn extension(path: &str) -> Option<String> {
        Path::new(path)
            .extension()
            .map(|e| e.to_string_lossy().to_string())
    }

    pub fn stem(path: &str) -> Option<String> {
        Path::new(path)
            .file_stem()
            .map(|s| s.to_string_lossy().to_string())
    }

    pub fn exists(path: &str) -> bool {
        Path::new(path).exists()
    }

    pub fn is_file(path: &str) -> bool {
        Path::new(path).is_file()
    }

    pub fn is_dir(path: &str) -> bool {
        Path::new(path).is_dir()
    }

    pub fn is_absolute(path: &str) -> bool {
        Path::new(path).is_absolute()
    }

    pub fn is_relative(path: &str) -> bool {
        Path::new(path).is_relative()
    }
}

pub struct Process {
    child: Child,
}

impl Process {
    pub fn spawn(program: &str, args: &[&str]) -> io::Result<Self> {
        let child = Command::new(program).args(args).spawn()?;
        Ok(Process { child })
    }

    pub fn spawn_with_env(
        program: &str,
        args: &[&str],
        env_vars: &[(&str, &str)],
    ) -> io::Result<Self> {
        let mut cmd = Command::new(program);
        cmd.args(args);
        for (key, value) in env_vars {
            cmd.env(key, value);
        }
        let child = cmd.spawn()?;
        Ok(Process { child })
    }

    pub fn spawn_piped(program: &str, args: &[&str]) -> io::Result<Self> {
        let child = Command::new(program)
            .args(args)
            .stdin(Stdio::piped())
            .stdout(Stdio::piped())
            .stderr(Stdio::piped())
            .spawn()?;
        Ok(Process { child })
    }

    pub fn wait(&mut self) -> io::Result<ExitStatus> {
        self.child.wait()
    }

    pub fn try_wait(&mut self) -> io::Result<Option<ExitStatus>> {
        self.child.try_wait()
    }

    pub fn kill(&mut self) -> io::Result<()> {
        self.child.kill()
    }

    pub fn id(&self) -> u32 {
        self.child.id()
    }

    pub fn stdin(&mut self) -> Option<&mut std::process::ChildStdin> {
        self.child.stdin.as_mut()
    }

    pub fn stdout(&mut self) -> Option<&mut std::process::ChildStdout> {
        self.child.stdout.as_mut()
    }

    pub fn stderr(&mut self) -> Option<&mut std::process::ChildStderr> {
        self.child.stderr.as_mut()
    }

    pub fn write_stdin(&mut self, data: &[u8]) -> io::Result<()> {
        if let Some(stdin) = self.stdin() {
            stdin.write_all(data)?;
            stdin.flush()?;
        }
        Ok(())
    }

    pub fn read_stdout(&mut self) -> io::Result<Vec<u8>> {
        let mut output = Vec::new();
        if let Some(stdout) = self.stdout() {
            stdout.read_to_end(&mut output)?;
        }
        Ok(output)
    }

    pub fn read_stderr(&mut self) -> io::Result<Vec<u8>> {
        let mut output = Vec::new();
        if let Some(stderr) = self.stderr() {
            stderr.read_to_end(&mut output)?;
        }
        Ok(output)
    }
}

pub struct ProcessBuilder {
    program: String,
    args: Vec<String>,
    env_vars: Vec<(String, String)>,
    cwd: Option<PathBuf>,
    stdin: Stdio,
    stdout: Stdio,
    stderr: Stdio,
}

impl ProcessBuilder {
    pub fn new(program: &str) -> Self {
        ProcessBuilder {
            program: program.to_string(),
            args: Vec::new(),
            env_vars: Vec::new(),
            cwd: None,
            stdin: Stdio::inherit(),
            stdout: Stdio::inherit(),
            stderr: Stdio::inherit(),
        }
    }

    pub fn arg(mut self, arg: &str) -> Self {
        self.args.push(arg.to_string());
        self
    }

    pub fn args(mut self, args: &[&str]) -> Self {
        for arg in args {
            self.args.push(arg.to_string());
        }
        self
    }

    pub fn env(mut self, key: &str, value: &str) -> Self {
        self.env_vars.push((key.to_string(), value.to_string()));
        self
    }

    pub fn current_dir(mut self, dir: &str) -> Self {
        self.cwd = Some(PathBuf::from(dir));
        self
    }

    pub fn stdin(mut self, cfg: Stdio) -> Self {
        self.stdin = cfg;
        self
    }

    pub fn stdout(mut self, cfg: Stdio) -> Self {
        self.stdout = cfg;
        self
    }

    pub fn stderr(mut self, cfg: Stdio) -> Self {
        self.stderr = cfg;
        self
    }

    pub fn spawn(self) -> io::Result<Process> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);

        for (key, value) in &self.env_vars {
            cmd.env(key, value);
        }

        if let Some(cwd) = self.cwd {
            cmd.current_dir(cwd);
        }

        cmd.stdin(self.stdin);
        cmd.stdout(self.stdout);
        cmd.stderr(self.stderr);

        let child = cmd.spawn()?;
        Ok(Process { child })
    }

    pub fn output(self) -> io::Result<std::process::Output> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);

        for (key, value) in &self.env_vars {
            cmd.env(key, value);
        }

        if let Some(cwd) = self.cwd {
            cmd.current_dir(cwd);
        }

        cmd.output()
    }

    pub fn status(self) -> io::Result<ExitStatus> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);

        for (key, value) in &self.env_vars {
            cmd.env(key, value);
        }

        if let Some(cwd) = self.cwd {
            cmd.current_dir(cwd);
        }

        cmd.status()
    }
}

pub fn execute(program: &str, args: &[&str]) -> io::Result<(i32, String, String)> {
    let output = Command::new(program).args(args).output()?;

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    let code = output.status.code().unwrap_or(-1);

    Ok((code, stdout, stderr))
}

pub fn execute_shell(command: &str) -> io::Result<(i32, String, String)> {
    let (shell, flag) = if Platform::current().is_windows() {
        ("cmd", "/C")
    } else {
        ("sh", "-c")
    };

    execute(shell, &[flag, command])
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_platform_current() {
        let platform = Platform::current();
        assert_ne!(platform, Platform::Unknown);
    }

    #[test]
    fn test_platform_separators() {
        let platform = Platform::current();
        let sep = platform.path_separator();
        assert!(sep == '/' || sep == '\\');
    }

    #[test]
    fn test_path_normalize() {
        let normalized = PathUtils::normalize("foo/./bar/../baz");
        assert!(normalized.contains("foo"));
        assert!(normalized.contains("baz"));
    }

    #[test]
    fn test_path_join() {
        let joined = PathUtils::join("/home/user", "documents");
        assert!(joined.contains("home"));
        assert!(joined.contains("documents"));
    }

    #[test]
    fn test_path_filename() {
        let filename = PathUtils::filename("/path/to/file.txt");
        assert_eq!(filename, Some("file.txt".to_string()));
    }

    #[test]
    fn test_path_extension() {
        let ext = PathUtils::extension("/path/to/file.txt");
        assert_eq!(ext, Some("txt".to_string()));
    }

    #[test]
    fn test_path_stem() {
        let stem = PathUtils::stem("/path/to/file.txt");
        assert_eq!(stem, Some("file".to_string()));
    }

    #[test]
    fn test_path_parent() {
        let parent = PathUtils::parent("/path/to/file.txt");
        assert!(parent.is_some());
        let p = parent.unwrap();
        assert!(p.contains("path"));
        assert!(p.contains("to"));
    }

    #[test]
    fn test_path_is_absolute() {
        assert!(PathUtils::is_absolute("/absolute/path"));
        assert!(!PathUtils::is_relative("/absolute/path"));
    }

    #[test]
    fn test_execute_echo() {
        let (code, stdout, _) = if Platform::current().is_windows() {
            execute("cmd", &["/C", "echo", "test"])
        } else {
            execute("echo", &["test"])
        }
        .unwrap();

        assert_eq!(code, 0);
        assert!(stdout.contains("test"));
    }

    #[test]
    fn test_process_builder() {
        let result = if Platform::current().is_windows() {
            ProcessBuilder::new("cmd")
                .args(&["/C", "echo", "hello"])
                .output()
        } else {
            ProcessBuilder::new("echo").arg("hello").output()
        };

        assert!(result.is_ok());
        let output = result.unwrap();
        assert_eq!(output.status.code(), Some(0));
    }

    #[test]
    fn test_process_spawn_and_wait() {
        let result = if Platform::current().is_windows() {
            Process::spawn("cmd", &["/C", "echo", "test"])
        } else {
            Process::spawn("echo", &["test"])
        };

        assert!(result.is_ok());
        let mut process = result.unwrap();
        let status = process.wait().unwrap();
        assert!(status.success());
    }
}
