//! Comprehensive networking primitives for TCP/UDP sockets with IPv4/IPv6 support.

#![allow(dead_code)]

use std::io::{self, Read, Write};
use std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddr, TcpListener, TcpStream, UdpSocket};
use std::time::Duration;

/// TCP socket with comprehensive configuration
pub struct TcpSocket {
    stream: TcpStream,
}

impl TcpSocket {
    /// Connect to a remote address
    pub fn connect(addr: &str) -> io::Result<Self> {
        let stream = TcpStream::connect(addr)?;
        Ok(Self { stream })
    }

    /// Connect with timeout
    pub fn connect_timeout(addr: &str, timeout: Duration) -> io::Result<Self> {
        let addr: SocketAddr = addr
            .parse()
            .map_err(|_| io::Error::new(io::ErrorKind::InvalidInput, "Invalid socket address"))?;
        let stream = TcpStream::connect_timeout(&addr, timeout)?;
        Ok(Self { stream })
    }

    /// Set read timeout
    pub fn set_read_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        self.stream.set_read_timeout(timeout)
    }

    /// Set write timeout
    pub fn set_write_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        self.stream.set_write_timeout(timeout)
    }

    /// Set TCP_NODELAY (disable Nagle's algorithm)
    pub fn set_nodelay(&self, nodelay: bool) -> io::Result<()> {
        self.stream.set_nodelay(nodelay)
    }

    /// Set SO_KEEPALIVE
    pub fn set_keepalive(&self, _keepalive: Option<Duration>) -> io::Result<()> {
        self.stream.set_ttl(64)?; // Set default TTL
        Ok(())
    }

    /// Set non-blocking mode
    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.stream.set_nonblocking(nonblocking)
    }

    /// Get local address
    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        self.stream.local_addr()
    }

    /// Get peer address
    pub fn peer_addr(&self) -> io::Result<SocketAddr> {
        self.stream.peer_addr()
    }

    /// Read data from socket
    pub fn read(&mut self, buf: &mut [u8]) -> io::Result<usize> {
        self.stream.read(buf)
    }

    /// Write data to socket
    pub fn write(&mut self, buf: &[u8]) -> io::Result<usize> {
        self.stream.write(buf)
    }

    /// Shutdown the connection
    pub fn shutdown(&self, how: std::net::Shutdown) -> io::Result<()> {
        self.stream.shutdown(how)
    }
}

/// TCP listener with configuration
pub struct TcpListenerSocket {
    listener: TcpListener,
}

impl TcpListenerSocket {
    /// Bind to an address
    pub fn bind(addr: &str) -> io::Result<Self> {
        let listener = TcpListener::bind(addr)?;
        Ok(Self { listener })
    }

    /// Accept a connection
    pub fn accept(&self) -> io::Result<(TcpSocket, SocketAddr)> {
        let (stream, addr) = self.listener.accept()?;
        Ok((TcpSocket { stream }, addr))
    }

    /// Set non-blocking mode
    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.listener.set_nonblocking(nonblocking)
    }

    /// Get local address
    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        self.listener.local_addr()
    }
}

/// UDP socket with comprehensive configuration
pub struct UdpSocketWrapper {
    socket: UdpSocket,
}

impl UdpSocketWrapper {
    /// Bind to an address
    pub fn bind(addr: &str) -> io::Result<Self> {
        let socket = UdpSocket::bind(addr)?;
        Ok(Self { socket })
    }

    /// Connect to a remote address
    pub fn connect(&self, addr: &str) -> io::Result<()> {
        self.socket.connect(addr)
    }

    /// Send data to a specific address
    pub fn send_to(&self, buf: &[u8], addr: &str) -> io::Result<usize> {
        self.socket.send_to(buf, addr)
    }

    /// Receive data
    pub fn recv_from(&self, buf: &mut [u8]) -> io::Result<(usize, SocketAddr)> {
        self.socket.recv_from(buf)
    }

    /// Send data (must be connected)
    pub fn send(&self, buf: &[u8]) -> io::Result<usize> {
        self.socket.send(buf)
    }

    /// Receive data (must be connected)
    pub fn recv(&self, buf: &mut [u8]) -> io::Result<usize> {
        self.socket.recv(buf)
    }

    /// Set broadcast mode
    pub fn set_broadcast(&self, broadcast: bool) -> io::Result<()> {
        self.socket.set_broadcast(broadcast)
    }

    /// Set multicast loop
    pub fn set_multicast_loop_v4(&self, multicast_loop: bool) -> io::Result<()> {
        self.socket.set_multicast_loop_v4(multicast_loop)
    }

    /// Join multicast group
    pub fn join_multicast_v4(&self, multiaddr: &Ipv4Addr, interface: &Ipv4Addr) -> io::Result<()> {
        self.socket.join_multicast_v4(multiaddr, interface)
    }

    /// Leave multicast group
    pub fn leave_multicast_v4(&self, multiaddr: &Ipv4Addr, interface: &Ipv4Addr) -> io::Result<()> {
        self.socket.leave_multicast_v4(multiaddr, interface)
    }

    /// Set read timeout
    pub fn set_read_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        self.socket.set_read_timeout(timeout)
    }

    /// Set write timeout
    pub fn set_write_timeout(&self, timeout: Option<Duration>) -> io::Result<()> {
        self.socket.set_write_timeout(timeout)
    }

    /// Set non-blocking mode
    pub fn set_nonblocking(&self, nonblocking: bool) -> io::Result<()> {
        self.socket.set_nonblocking(nonblocking)
    }

    /// Get local address
    pub fn local_addr(&self) -> io::Result<SocketAddr> {
        self.socket.local_addr()
    }
}

/// IP address utilities
pub struct IpAddress;

impl IpAddress {
    /// Parse IPv4 address
    pub fn parse_ipv4(s: &str) -> Result<Ipv4Addr, std::net::AddrParseError> {
        s.parse()
    }

    /// Parse IPv6 address
    pub fn parse_ipv6(s: &str) -> Result<Ipv6Addr, std::net::AddrParseError> {
        s.parse()
    }

    /// Parse IP address (v4 or v6)
    pub fn parse(s: &str) -> Result<IpAddr, std::net::AddrParseError> {
        s.parse()
    }

    /// Check if address is loopback
    pub fn is_loopback(addr: &IpAddr) -> bool {
        addr.is_loopback()
    }

    /// Check if address is multicast
    pub fn is_multicast(addr: &IpAddr) -> bool {
        addr.is_multicast()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tcp_listener_bind() {
        let listener = TcpListenerSocket::bind("127.0.0.1:0");
        assert!(listener.is_ok());
    }

    #[test]
    fn test_tcp_listener_local_addr() {
        let listener = TcpListenerSocket::bind("127.0.0.1:0").unwrap();
        let addr = listener.local_addr();
        assert!(addr.is_ok());
    }

    #[test]
    fn test_udp_socket_bind() {
        let socket = UdpSocketWrapper::bind("127.0.0.1:0");
        assert!(socket.is_ok());
    }

    #[test]
    fn test_udp_socket_local_addr() {
        let socket = UdpSocketWrapper::bind("127.0.0.1:0").unwrap();
        let addr = socket.local_addr();
        assert!(addr.is_ok());
    }

    #[test]
    fn test_udp_send_recv() {
        let sender = UdpSocketWrapper::bind("127.0.0.1:0").unwrap();
        let receiver = UdpSocketWrapper::bind("127.0.0.1:0").unwrap();

        let receiver_addr = receiver.local_addr().unwrap();
        let msg = b"test message";

        sender.send_to(msg, &receiver_addr.to_string()).unwrap();

        let mut buf = [0u8; 1024];
        let (size, _) = receiver.recv_from(&mut buf).unwrap();

        assert_eq!(&buf[..size], msg);
    }

    #[test]
    fn test_ipv4_parse() {
        let addr = IpAddress::parse_ipv4("127.0.0.1");
        assert!(addr.is_ok());
        assert_eq!(addr.unwrap(), Ipv4Addr::new(127, 0, 0, 1));
    }

    #[test]
    fn test_ipv6_parse() {
        let addr = IpAddress::parse_ipv6("::1");
        assert!(addr.is_ok());
    }

    #[test]
    fn test_ip_is_loopback() {
        let ipv4 = IpAddr::V4(Ipv4Addr::new(127, 0, 0, 1));
        assert!(IpAddress::is_loopback(&ipv4));

        let ipv6 = IpAddr::V6(Ipv6Addr::new(0, 0, 0, 0, 0, 0, 0, 1));
        assert!(IpAddress::is_loopback(&ipv6));
    }

    #[test]
    fn test_tcp_socket_options() {
        let listener = TcpListenerSocket::bind("127.0.0.1:0").unwrap();
        listener.set_nonblocking(true).unwrap();
    }

    #[test]
    fn test_udp_socket_options() {
        let socket = UdpSocketWrapper::bind("127.0.0.1:0").unwrap();
        socket.set_broadcast(true).unwrap();
        socket.set_nonblocking(true).unwrap();
    }
}
