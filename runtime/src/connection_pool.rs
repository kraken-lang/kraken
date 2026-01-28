//! HTTP connection pooling and keep-alive support.

#![allow(dead_code)]

use std::collections::HashMap;
use std::io;
use std::net::TcpStream;
use std::sync::{Arc, Mutex};
use std::time::{Duration, Instant};

/// Connection pool entry
struct PooledConnection {
    stream: TcpStream,
    last_used: Instant,
}

/// HTTP connection pool
pub struct ConnectionPool {
    connections: Arc<Mutex<HashMap<String, Vec<PooledConnection>>>>,
    max_idle_per_host: usize,
    idle_timeout: Duration,
}

impl ConnectionPool {
    /// Create a new connection pool
    pub fn new(max_idle_per_host: usize, idle_timeout: Duration) -> Self {
        Self {
            connections: Arc::new(Mutex::new(HashMap::new())),
            max_idle_per_host,
            idle_timeout,
        }
    }

    /// Get a connection from the pool or create a new one
    pub fn get_connection(&self, host: &str) -> io::Result<TcpStream> {
        let mut connections = self.connections.lock().unwrap();

        if let Some(pool) = connections.get_mut(host) {
            while let Some(conn) = pool.pop() {
                if conn.last_used.elapsed() < self.idle_timeout {
                    return Ok(conn.stream);
                }
            }
        }

        TcpStream::connect(host)
    }

    /// Return a connection to the pool
    pub fn return_connection(&self, host: String, stream: TcpStream) {
        let mut connections = self.connections.lock().unwrap();

        let pool = connections.entry(host).or_default();

        if pool.len() < self.max_idle_per_host {
            pool.push(PooledConnection {
                stream,
                last_used: Instant::now(),
            });
        }
    }

    /// Clear expired connections
    pub fn clear_expired(&self) {
        let mut connections = self.connections.lock().unwrap();

        for pool in connections.values_mut() {
            pool.retain(|conn| conn.last_used.elapsed() < self.idle_timeout);
        }
    }

    /// Get pool statistics
    pub fn stats(&self) -> PoolStats {
        let connections = self.connections.lock().unwrap();

        let mut total_connections = 0;
        let mut hosts = 0;

        for pool in connections.values() {
            total_connections += pool.len();
            hosts += 1;
        }

        PoolStats {
            total_connections,
            hosts,
        }
    }
}

impl Clone for ConnectionPool {
    fn clone(&self) -> Self {
        Self {
            connections: self.connections.clone(),
            max_idle_per_host: self.max_idle_per_host,
            idle_timeout: self.idle_timeout,
        }
    }
}

/// Pool statistics
#[derive(Debug, Clone)]
pub struct PoolStats {
    pub total_connections: usize,
    pub hosts: usize,
}

/// HTTP client with connection pooling
pub struct PooledHttpClient {
    pool: ConnectionPool,
}

impl PooledHttpClient {
    /// Create a new pooled HTTP client
    pub fn new() -> Self {
        Self {
            pool: ConnectionPool::new(10, Duration::from_secs(30)),
        }
    }

    /// Create with custom pool settings
    pub fn with_pool_settings(max_idle_per_host: usize, idle_timeout: Duration) -> Self {
        Self {
            pool: ConnectionPool::new(max_idle_per_host, idle_timeout),
        }
    }

    /// Get a connection from the pool
    pub fn get_connection(&self, host: &str) -> io::Result<TcpStream> {
        self.pool.get_connection(host)
    }

    /// Return a connection to the pool
    pub fn return_connection(&self, host: String, stream: TcpStream) {
        self.pool.return_connection(host, stream);
    }

    /// Get pool statistics
    pub fn pool_stats(&self) -> PoolStats {
        self.pool.stats()
    }

    /// Clear expired connections
    pub fn clear_expired(&self) {
        self.pool.clear_expired();
    }
}

impl Default for PooledHttpClient {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_connection_pool_creation() {
        let pool = ConnectionPool::new(10, Duration::from_secs(30));
        let stats = pool.stats();
        assert_eq!(stats.total_connections, 0);
        assert_eq!(stats.hosts, 0);
    }

    #[test]
    fn test_pooled_http_client_creation() {
        let client = PooledHttpClient::new();
        let stats = client.pool_stats();
        assert_eq!(stats.total_connections, 0);
    }

    #[test]
    fn test_pooled_http_client_with_settings() {
        let client = PooledHttpClient::with_pool_settings(5, Duration::from_secs(60));
        let stats = client.pool_stats();
        assert_eq!(stats.total_connections, 0);
    }

    #[test]
    fn test_pool_stats() {
        let pool = ConnectionPool::new(10, Duration::from_secs(30));
        let stats = pool.stats();
        assert_eq!(stats.total_connections, 0);
        assert_eq!(stats.hosts, 0);
    }

    #[test]
    fn test_clear_expired() {
        let pool = ConnectionPool::new(10, Duration::from_secs(30));
        pool.clear_expired();
        let stats = pool.stats();
        assert_eq!(stats.total_connections, 0);
    }
}
