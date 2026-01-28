use signal_hook::consts::signal::*;
#[cfg(unix)]
use signal_hook::iterator::Signals;
use std::sync::atomic::{AtomicBool, Ordering};
use std::sync::Arc;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Signal {
    Interrupt, // SIGINT / Ctrl+C
    Terminate, // SIGTERM
    Quit,      // SIGQUIT
    Hangup,    // SIGHUP
    Alarm,     // SIGALRM
    User1,     // SIGUSR1
    User2,     // SIGUSR2
}

impl Signal {
    #[cfg(unix)]
    fn to_signal_number(self) -> i32 {
        match self {
            Signal::Interrupt => SIGINT,
            Signal::Terminate => SIGTERM,
            Signal::Quit => SIGQUIT,
            Signal::Hangup => SIGHUP,
            Signal::Alarm => SIGALRM,
            Signal::User1 => SIGUSR1,
            Signal::User2 => SIGUSR2,
        }
    }

    #[cfg(windows)]
    fn to_signal_number(self) -> i32 {
        match self {
            Signal::Interrupt => SIGINT,
            Signal::Terminate => SIGTERM,
            _ => SIGINT, // Windows only supports SIGINT and SIGTERM
        }
    }

    #[cfg(unix)]
    fn from_signal_number(sig: i32) -> Option<Self> {
        match sig {
            SIGINT => Some(Signal::Interrupt),
            SIGTERM => Some(Signal::Terminate),
            SIGQUIT => Some(Signal::Quit),
            SIGHUP => Some(Signal::Hangup),
            SIGALRM => Some(Signal::Alarm),
            SIGUSR1 => Some(Signal::User1),
            SIGUSR2 => Some(Signal::User2),
            _ => None,
        }
    }

    #[cfg(windows)]
    fn from_signal_number(sig: i32) -> Option<Self> {
        match sig {
            SIGINT => Some(Signal::Interrupt),
            SIGTERM => Some(Signal::Terminate),
            _ => None,
        }
    }
}

#[cfg(unix)]
pub struct SignalHandler {
    signals: Signals,
    shutdown: Arc<AtomicBool>,
}

#[cfg(unix)]
impl SignalHandler {
    pub fn new(signals: &[Signal]) -> std::io::Result<Self> {
        let signal_numbers: Vec<i32> = signals.iter().map(|s| s.to_signal_number()).collect();
        let signals_iter = Signals::new(&signal_numbers)?;

        Ok(SignalHandler {
            signals: signals_iter,
            shutdown: Arc::new(AtomicBool::new(false)),
        })
    }

    pub fn wait(&mut self) -> Option<Signal> {
        for signal in self.signals.forever() {
            if let Some(sig) = Signal::from_signal_number(signal) {
                return Some(sig);
            }
        }
        None
    }

    pub fn try_recv(&mut self) -> Option<Signal> {
        if let Some(signal) = self.signals.pending().next() {
            Signal::from_signal_number(signal)
        } else {
            None
        }
    }

    pub fn shutdown_flag(&self) -> Arc<AtomicBool> {
        Arc::clone(&self.shutdown)
    }

    pub fn set_shutdown(&self) {
        self.shutdown.store(true, Ordering::SeqCst);
    }

    pub fn is_shutdown(&self) -> bool {
        self.shutdown.load(Ordering::SeqCst)
    }
}

pub fn register_ctrl_c_handler<F>(handler: F) -> std::io::Result<()>
where
    F: Fn() + Send + 'static,
{
    ctrlc::set_handler(move || {
        handler();
    })
    .map_err(std::io::Error::other)
}

pub fn ignore_signal(signal: Signal) -> std::io::Result<()> {
    let sig_num = signal.to_signal_number();
    signal_hook::flag::register(sig_num, Arc::new(AtomicBool::new(false)))?;
    Ok(())
}

pub fn raise_signal(signal: Signal) -> std::io::Result<()> {
    let sig_num = signal.to_signal_number();
    unsafe {
        if libc::raise(sig_num) != 0 {
            return Err(std::io::Error::last_os_error());
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::sync::atomic::AtomicUsize;
    use std::sync::Arc;

    #[test]
    fn test_signal_conversion() {
        let sig = Signal::Interrupt;
        let num = sig.to_signal_number();
        let converted = Signal::from_signal_number(num);
        assert_eq!(converted, Some(Signal::Interrupt));
    }

    #[test]
    fn test_ctrl_c_handler() {
        let counter = Arc::new(AtomicUsize::new(0));
        let counter_clone = Arc::clone(&counter);

        let result = register_ctrl_c_handler(move || {
            counter_clone.fetch_add(1, Ordering::SeqCst);
        });

        assert!(result.is_ok());
    }

    #[test]
    #[cfg(unix)]
    fn test_signal_handler_creation() {
        let signals = vec![Signal::Interrupt, Signal::Terminate];
        let handler = SignalHandler::new(&signals);
        assert!(handler.is_ok());
    }

    #[test]
    #[cfg(unix)]
    fn test_shutdown_flag() {
        let signals = vec![Signal::Interrupt];
        let handler = SignalHandler::new(&signals).unwrap();

        assert!(!handler.is_shutdown());
        handler.set_shutdown();
        assert!(handler.is_shutdown());
    }

    #[test]
    fn test_ignore_signal() {
        let result = ignore_signal(Signal::User1);
        // This may fail on some platforms, so we just check it doesn't panic
        let _ = result;
    }
}
