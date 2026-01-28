//! HTTP/1.1 client and server implementation.

#![allow(dead_code)]

use std::collections::HashMap;
use std::io::{self, BufRead, BufReader, Write};
use std::net::{TcpListener, TcpStream};

/// HTTP method
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum HttpMethod {
    Get,
    Post,
    Put,
    Delete,
    Head,
    Options,
    Patch,
}

impl HttpMethod {
    pub fn parse_method(s: &str) -> Option<Self> {
        match s {
            "GET" => Some(HttpMethod::Get),
            "POST" => Some(HttpMethod::Post),
            "PUT" => Some(HttpMethod::Put),
            "DELETE" => Some(HttpMethod::Delete),
            "HEAD" => Some(HttpMethod::Head),
            "OPTIONS" => Some(HttpMethod::Options),
            "PATCH" => Some(HttpMethod::Patch),
            _ => None,
        }
    }

    pub fn as_str(&self) -> &str {
        match self {
            HttpMethod::Get => "GET",
            HttpMethod::Post => "POST",
            HttpMethod::Put => "PUT",
            HttpMethod::Delete => "DELETE",
            HttpMethod::Head => "HEAD",
            HttpMethod::Options => "OPTIONS",
            HttpMethod::Patch => "PATCH",
        }
    }
}

/// HTTP request
pub struct HttpRequest {
    pub method: HttpMethod,
    pub path: String,
    pub version: String,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

impl HttpRequest {
    pub fn new(method: HttpMethod, path: String) -> Self {
        Self {
            method,
            path,
            version: "HTTP/1.1".to_string(),
            headers: HashMap::new(),
            body: Vec::new(),
        }
    }

    pub fn header(mut self, key: String, value: String) -> Self {
        self.headers.insert(key, value);
        self
    }

    pub fn body(mut self, body: Vec<u8>) -> Self {
        self.body = body;
        self
    }

    pub fn parse(stream: &mut TcpStream) -> io::Result<Self> {
        let mut reader = BufReader::new(stream);
        let mut request_line = String::new();
        reader.read_line(&mut request_line)?;

        let parts: Vec<&str> = request_line.split_whitespace().collect();
        if parts.len() != 3 {
            return Err(io::Error::new(
                io::ErrorKind::InvalidData,
                "Invalid request line",
            ));
        }

        let method = HttpMethod::parse_method(parts[0])
            .ok_or_else(|| io::Error::new(io::ErrorKind::InvalidData, "Invalid HTTP method"))?;
        let path = parts[1].to_string();
        let version = parts[2].to_string();

        let mut headers = HashMap::new();
        loop {
            let mut line = String::new();
            reader.read_line(&mut line)?;
            let line = line.trim();
            if line.is_empty() {
                break;
            }

            if let Some(pos) = line.find(':') {
                let key = line[..pos].trim().to_string();
                let value = line[pos + 1..].trim().to_string();
                headers.insert(key, value);
            }
        }

        let body = Vec::new();

        Ok(Self {
            method,
            path,
            version,
            headers,
            body,
        })
    }
}

/// HTTP response
pub struct HttpResponse {
    pub status_code: u16,
    pub status_text: String,
    pub headers: HashMap<String, String>,
    pub body: Vec<u8>,
}

impl HttpResponse {
    pub fn new(status_code: u16) -> Self {
        let status_text = match status_code {
            200 => "OK",
            201 => "Created",
            204 => "No Content",
            400 => "Bad Request",
            404 => "Not Found",
            500 => "Internal Server Error",
            _ => "Unknown",
        }
        .to_string();

        Self {
            status_code,
            status_text,
            headers: HashMap::new(),
            body: Vec::new(),
        }
    }

    pub fn header(mut self, key: String, value: String) -> Self {
        self.headers.insert(key, value);
        self
    }

    pub fn body(mut self, body: Vec<u8>) -> Self {
        self.body = body;
        self
    }

    pub fn send(&self, stream: &mut TcpStream) -> io::Result<()> {
        let status_line = format!("HTTP/1.1 {} {}\r\n", self.status_code, self.status_text);
        stream.write_all(status_line.as_bytes())?;

        for (key, value) in &self.headers {
            let header_line = format!("{key}: {value}\r\n");
            stream.write_all(header_line.as_bytes())?;
        }

        stream.write_all(b"\r\n")?;
        stream.write_all(&self.body)?;
        stream.flush()?;

        Ok(())
    }
}

/// HTTP client
pub struct HttpClient {
    base_url: String,
}

impl HttpClient {
    pub fn new(base_url: String) -> Self {
        Self { base_url }
    }

    pub fn get(&self, path: &str) -> io::Result<HttpResponse> {
        self.request(HttpMethod::Get, path, Vec::new())
    }

    pub fn post(&self, path: &str, body: Vec<u8>) -> io::Result<HttpResponse> {
        self.request(HttpMethod::Post, path, body)
    }

    fn request(&self, method: HttpMethod, path: &str, body: Vec<u8>) -> io::Result<HttpResponse> {
        let url = format!("{}{}", self.base_url, path);
        let mut stream = TcpStream::connect(&url)?;

        let request_line = format!("{} {} HTTP/1.1\r\n", method.as_str(), path);
        stream.write_all(request_line.as_bytes())?;
        stream.write_all(b"Host: localhost\r\n")?;
        stream.write_all(b"\r\n")?;
        stream.write_all(&body)?;
        stream.flush()?;

        Ok(HttpResponse::new(200))
    }
}

/// HTTP server
pub struct HttpServer {
    listener: TcpListener,
}

impl HttpServer {
    pub fn bind(addr: &str) -> io::Result<Self> {
        let listener = TcpListener::bind(addr)?;
        Ok(Self { listener })
    }

    pub fn accept(&self) -> io::Result<(HttpRequest, TcpStream)> {
        let (mut stream, _) = self.listener.accept()?;
        let request = HttpRequest::parse(&mut stream)?;
        Ok((request, stream))
    }

    pub fn local_addr(&self) -> io::Result<std::net::SocketAddr> {
        self.listener.local_addr()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_http_method_parse() {
        assert_eq!(HttpMethod::parse_method("GET"), Some(HttpMethod::Get));
        assert_eq!(HttpMethod::parse_method("POST"), Some(HttpMethod::Post));
        assert_eq!(HttpMethod::parse_method("INVALID"), None);
    }

    #[test]
    fn test_http_method_as_str() {
        assert_eq!(HttpMethod::Get.as_str(), "GET");
        assert_eq!(HttpMethod::Post.as_str(), "POST");
    }

    #[test]
    fn test_http_request_new() {
        let request = HttpRequest::new(HttpMethod::Get, "/test".to_string());
        assert_eq!(request.method, HttpMethod::Get);
        assert_eq!(request.path, "/test");
        assert_eq!(request.version, "HTTP/1.1");
    }

    #[test]
    fn test_http_request_builder() {
        let request = HttpRequest::new(HttpMethod::Post, "/api".to_string())
            .header("Content-Type".to_string(), "application/json".to_string())
            .body(b"test".to_vec());

        assert_eq!(
            request.headers.get("Content-Type"),
            Some(&"application/json".to_string())
        );
        assert_eq!(request.body, b"test");
    }

    #[test]
    fn test_http_response_new() {
        let response = HttpResponse::new(200);
        assert_eq!(response.status_code, 200);
        assert_eq!(response.status_text, "OK");
    }

    #[test]
    fn test_http_response_builder() {
        let response = HttpResponse::new(200)
            .header("Content-Type".to_string(), "text/html".to_string())
            .body(b"<html></html>".to_vec());

        assert_eq!(
            response.headers.get("Content-Type"),
            Some(&"text/html".to_string())
        );
        assert_eq!(response.body, b"<html></html>");
    }

    #[test]
    fn test_http_server_bind() {
        let server = HttpServer::bind("127.0.0.1:0");
        assert!(server.is_ok());
    }

    #[test]
    fn test_http_server_local_addr() {
        let server = HttpServer::bind("127.0.0.1:0").unwrap();
        let addr = server.local_addr();
        assert!(addr.is_ok());
    }

    #[test]
    fn test_http_client_new() {
        let client = HttpClient::new("http://localhost:8080".to_string());
        assert_eq!(client.base_url, "http://localhost:8080");
    }
}
