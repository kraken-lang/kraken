//! Comprehensive process management with pipes, exit codes, and advanced spawning.

#![allow(dead_code)]

use std::collections::HashMap;
use std::io::{self, Read, Write};
use std::process::{Child, Command, ExitStatus, Output, Stdio};

/// Process builder with fluent API for configuration
pub struct ProcessBuilder {
    program: String,
    args: Vec<String>,
    env: HashMap<String, String>,
    current_dir: Option<String>,
    stdin_config: Stdio,
    stdout_config: Stdio,
    stderr_config: Stdio,
}

impl ProcessBuilder {
    /// Create a new process builder for the given program
    pub fn new<S: AsRef<str>>(program: S) -> Self {
        Self {
            program: program.as_ref().to_string(),
            args: Vec::new(),
            env: HashMap::new(),
            current_dir: None,
            stdin_config: Stdio::inherit(),
            stdout_config: Stdio::inherit(),
            stderr_config: Stdio::inherit(),
        }
    }

    /// Add a single argument
    pub fn arg<S: AsRef<str>>(mut self, arg: S) -> Self {
        self.args.push(arg.as_ref().to_string());
        self
    }

    /// Add multiple arguments
    pub fn args<I, S>(mut self, args: I) -> Self
    where
        I: IntoIterator<Item = S>,
        S: AsRef<str>,
    {
        for arg in args {
            self.args.push(arg.as_ref().to_string());
        }
        self
    }

    /// Set an environment variable
    pub fn env<K, V>(mut self, key: K, value: V) -> Self
    where
        K: AsRef<str>,
        V: AsRef<str>,
    {
        self.env
            .insert(key.as_ref().to_string(), value.as_ref().to_string());
        self
    }

    /// Set multiple environment variables
    pub fn envs<I, K, V>(mut self, vars: I) -> Self
    where
        I: IntoIterator<Item = (K, V)>,
        K: AsRef<str>,
        V: AsRef<str>,
    {
        for (key, value) in vars {
            self.env
                .insert(key.as_ref().to_string(), value.as_ref().to_string());
        }
        self
    }

    /// Clear all environment variables
    pub fn env_clear(mut self) -> Self {
        self.env.clear();
        self
    }

    /// Set the working directory
    pub fn current_dir<S: AsRef<str>>(mut self, dir: S) -> Self {
        self.current_dir = Some(dir.as_ref().to_string());
        self
    }

    /// Configure stdin as piped
    pub fn stdin_piped(mut self) -> Self {
        self.stdin_config = Stdio::piped();
        self
    }

    /// Configure stdin as null
    pub fn stdin_null(mut self) -> Self {
        self.stdin_config = Stdio::null();
        self
    }

    /// Configure stdout as piped
    pub fn stdout_piped(mut self) -> Self {
        self.stdout_config = Stdio::piped();
        self
    }

    /// Configure stdout as null
    pub fn stdout_null(mut self) -> Self {
        self.stdout_config = Stdio::null();
        self
    }

    /// Configure stderr as piped
    pub fn stderr_piped(mut self) -> Self {
        self.stderr_config = Stdio::piped();
        self
    }

    /// Configure stderr as null
    pub fn stderr_null(mut self) -> Self {
        self.stderr_config = Stdio::null();
        self
    }

    /// Spawn the process
    pub fn spawn(self) -> io::Result<ProcessHandle> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd.envs(&self.env);

        if let Some(dir) = &self.current_dir {
            cmd.current_dir(dir);
        }

        cmd.stdin(self.stdin_config);
        cmd.stdout(self.stdout_config);
        cmd.stderr(self.stderr_config);

        let child = cmd.spawn()?;
        Ok(ProcessHandle { child })
    }

    /// Spawn and wait for the process to complete, returning output
    pub fn output(self) -> io::Result<ProcessOutput> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd.envs(&self.env);

        if let Some(dir) = &self.current_dir {
            cmd.current_dir(dir);
        }

        let output = cmd.output()?;
        Ok(ProcessOutput::from_std(output))
    }

    /// Spawn and wait for the process to complete, returning exit status
    pub fn status(self) -> io::Result<ProcessExitStatus> {
        let mut cmd = Command::new(&self.program);
        cmd.args(&self.args);
        cmd.envs(&self.env);

        if let Some(dir) = &self.current_dir {
            cmd.current_dir(dir);
        }

        let status = cmd.status()?;
        Ok(ProcessExitStatus::from_std(status))
    }
}

/// Handle to a running process
pub struct ProcessHandle {
    child: Child,
}

impl ProcessHandle {
    /// Get the process ID
    pub fn id(&self) -> u32 {
        self.child.id()
    }

    /// Wait for the process to exit
    pub fn wait(mut self) -> io::Result<ProcessExitStatus> {
        let status = self.child.wait()?;
        Ok(ProcessExitStatus::from_std(status))
    }

    /// Check if the process has exited without blocking
    pub fn try_wait(&mut self) -> io::Result<Option<ProcessExitStatus>> {
        match self.child.try_wait()? {
            Some(status) => Ok(Some(ProcessExitStatus::from_std(status))),
            None => Ok(None),
        }
    }

    /// Force kill the process
    pub fn kill(mut self) -> io::Result<()> {
        self.child.kill()
    }

    /// Get mutable access to stdin (if piped)
    pub fn stdin(&mut self) -> Option<&mut std::process::ChildStdin> {
        self.child.stdin.as_mut()
    }

    /// Get mutable access to stdout (if piped)
    pub fn stdout(&mut self) -> Option<&mut std::process::ChildStdout> {
        self.child.stdout.as_mut()
    }

    /// Get mutable access to stderr (if piped)
    pub fn stderr(&mut self) -> Option<&mut std::process::ChildStderr> {
        self.child.stderr.as_mut()
    }

    /// Write data to stdin
    pub fn write_stdin(&mut self, data: &[u8]) -> io::Result<()> {
        if let Some(stdin) = self.stdin() {
            stdin.write_all(data)?;
            stdin.flush()?;
        }
        Ok(())
    }

    /// Read all data from stdout
    pub fn read_stdout(&mut self) -> io::Result<Vec<u8>> {
        let mut output = Vec::new();
        if let Some(stdout) = self.stdout() {
            stdout.read_to_end(&mut output)?;
        }
        Ok(output)
    }

    /// Read all data from stderr
    pub fn read_stderr(&mut self) -> io::Result<Vec<u8>> {
        let mut output = Vec::new();
        if let Some(stderr) = self.stderr() {
            stderr.read_to_end(&mut output)?;
        }
        Ok(output)
    }

    /// Wait for the process and capture all output
    pub fn wait_with_output(mut self) -> io::Result<ProcessOutput> {
        let stdout = self.read_stdout()?;
        let stderr = self.read_stderr()?;
        let status = self.wait()?;

        Ok(ProcessOutput {
            status,
            stdout,
            stderr,
        })
    }
}

/// Process exit status
#[derive(Debug, Clone, Copy)]
pub struct ProcessExitStatus {
    code: Option<i32>,
    success: bool,
}

impl ProcessExitStatus {
    fn from_std(status: ExitStatus) -> Self {
        Self {
            code: status.code(),
            success: status.success(),
        }
    }

    /// Returns true if the process exited successfully
    pub fn success(&self) -> bool {
        self.success
    }

    /// Returns the exit code if available
    pub fn code(&self) -> Option<i32> {
        self.code
    }

    /// Returns the exit code or a default value
    pub fn code_or(&self, default: i32) -> i32 {
        self.code.unwrap_or(default)
    }
}

/// Process output including stdout, stderr, and exit status
#[derive(Debug, Clone)]
pub struct ProcessOutput {
    pub status: ProcessExitStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

impl ProcessOutput {
    fn from_std(output: Output) -> Self {
        Self {
            status: ProcessExitStatus::from_std(output.status),
            stdout: output.stdout,
            stderr: output.stderr,
        }
    }

    /// Get stdout as a UTF-8 string
    pub fn stdout_str(&self) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(self.stdout.clone())
    }

    /// Get stderr as a UTF-8 string
    pub fn stderr_str(&self) -> Result<String, std::string::FromUtf8Error> {
        String::from_utf8(self.stderr.clone())
    }

    /// Get stdout as a lossy UTF-8 string
    pub fn stdout_lossy(&self) -> String {
        String::from_utf8_lossy(&self.stdout).to_string()
    }

    /// Get stderr as a lossy UTF-8 string
    pub fn stderr_lossy(&self) -> String {
        String::from_utf8_lossy(&self.stderr).to_string()
    }
}

/// Pipe two processes together
pub struct ProcessPipe {
    processes: Vec<ProcessHandle>,
}

impl ProcessPipe {
    /// Create a new process pipe
    pub fn new() -> Self {
        Self {
            processes: Vec::new(),
        }
    }

    /// Add a process to the pipe
    pub fn add_process(mut self, builder: ProcessBuilder) -> io::Result<Self> {
        let handle = builder.stdin_piped().stdout_piped().spawn()?;
        self.processes.push(handle);
        Ok(self)
    }

    /// Execute the pipe and return the final output
    pub fn execute(mut self) -> io::Result<ProcessOutput> {
        if self.processes.is_empty() {
            return Err(io::Error::new(
                io::ErrorKind::InvalidInput,
                "No processes in pipe",
            ));
        }

        // Connect pipes between processes
        for i in 0..self.processes.len() - 1 {
            let output = self.processes[i].read_stdout()?;
            self.processes[i + 1].write_stdin(&output)?;
        }

        // Get final output from last process
        let last_idx = self.processes.len() - 1;
        self.processes.remove(last_idx).wait_with_output()
    }
}

impl Default for ProcessPipe {
    fn default() -> Self {
        Self::new()
    }
}

/// Utility functions for process management
pub struct ProcessUtils;

impl ProcessUtils {
    /// Get the current process ID
    pub fn current_pid() -> u32 {
        std::process::id()
    }

    /// Execute a command and return its output
    pub fn execute<S: AsRef<str>>(program: S, args: &[&str]) -> io::Result<ProcessOutput> {
        ProcessBuilder::new(program).args(args.iter()).output()
    }

    /// Execute a command and return its exit status
    pub fn execute_status<S: AsRef<str>>(
        program: S,
        args: &[&str],
    ) -> io::Result<ProcessExitStatus> {
        ProcessBuilder::new(program).args(args.iter()).status()
    }

    /// Check if a command exists in PATH
    pub fn command_exists<S: AsRef<str>>(command: S) -> bool {
        #[cfg(unix)]
        {
            ProcessBuilder::new("which")
                .arg(command.as_ref())
                .stdout_null()
                .stderr_null()
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        }

        #[cfg(windows)]
        {
            ProcessBuilder::new("where")
                .arg(command.as_ref())
                .stdout_null()
                .stderr_null()
                .status()
                .map(|s| s.success())
                .unwrap_or(false)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_process_builder_basic() {
        let result = ProcessBuilder::new("echo").arg("hello").output();

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.status.success());
    }

    #[test]
    fn test_process_builder_args() {
        let result = ProcessBuilder::new("echo")
            .args(vec!["hello", "world"])
            .output();

        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.status.success());
        assert!(output.stdout_lossy().contains("hello"));
    }

    #[test]
    fn test_process_exit_code() {
        #[cfg(unix)]
        {
            let result = ProcessBuilder::new("sh").arg("-c").arg("exit 42").status();

            assert!(result.is_ok());
            let status = result.unwrap();
            assert!(!status.success());
            assert_eq!(status.code(), Some(42));
        }

        #[cfg(windows)]
        {
            let result = ProcessBuilder::new("cmd").arg("/C").arg("exit 42").status();

            assert!(result.is_ok());
            let status = result.unwrap();
            assert!(!status.success());
            assert_eq!(status.code(), Some(42));
        }
    }

    #[test]
    fn test_process_env() {
        #[cfg(unix)]
        {
            let result = ProcessBuilder::new("sh")
                .arg("-c")
                .arg("echo $TEST_VAR")
                .env("TEST_VAR", "test_value")
                .output();

            assert!(result.is_ok());
            let output = result.unwrap();
            assert!(output.stdout_lossy().contains("test_value"));
        }

        #[cfg(windows)]
        {
            let result = ProcessBuilder::new("cmd")
                .arg("/C")
                .arg("echo %TEST_VAR%")
                .env("TEST_VAR", "test_value")
                .output();

            assert!(result.is_ok());
            let output = result.unwrap();
            assert!(output.stdout_lossy().contains("test_value"));
        }
    }

    #[test]
    fn test_process_piped_io() {
        let mut handle = ProcessBuilder::new("echo")
            .arg("test")
            .stdout_piped()
            .spawn()
            .unwrap();

        let stdout = handle.read_stdout().unwrap();
        assert!(!stdout.is_empty());
    }

    #[test]
    fn test_process_current_pid() {
        let pid = ProcessUtils::current_pid();
        assert!(pid > 0);
    }

    #[test]
    fn test_process_execute() {
        let result = ProcessUtils::execute("echo", &["test"]);
        assert!(result.is_ok());
        let output = result.unwrap();
        assert!(output.status.success());
    }

    #[test]
    fn test_command_exists() {
        // Test with a command that should exist on all systems
        #[cfg(unix)]
        assert!(ProcessUtils::command_exists("sh"));

        #[cfg(windows)]
        assert!(ProcessUtils::command_exists("cmd"));

        // Test with a command that shouldn't exist
        assert!(!ProcessUtils::command_exists(
            "this_command_definitely_does_not_exist_12345"
        ));
    }

    #[test]
    fn test_process_output_strings() {
        let output = ProcessBuilder::new("echo").arg("hello").output().unwrap();

        assert!(output.stdout_str().is_ok());
        assert!(output.stdout_lossy().contains("hello"));
    }

    #[test]
    fn test_process_try_wait() {
        let mut handle = ProcessBuilder::new("echo").arg("test").spawn().unwrap();

        // Process should complete quickly
        std::thread::sleep(std::time::Duration::from_millis(100));

        let status = handle.try_wait().unwrap();
        assert!(status.is_some());
    }
}
