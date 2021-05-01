// Big thanks to https://github.com/WANG-lp/socks5-rs
use async_std::io;
use async_std::io::{BufReader, BufRead, Read, Write};
use async_std::net::{IpAddr, Ipv4Addr, Ipv6Addr, SocketAddrV4, SocketAddrV6};
use async_std::net::{TcpStream, UdpSocket};
use async_std::prelude::*;
use async_std::sync::{Arc, Mutex};
use async_std::task;
use async_std::task::{Poll, Context};
use std::pin::Pin;
use std::io::{IoSlice, IoSliceMut};
use bytes::{Buf, BufMut};
use std::collections::HashSet;
use std::net::Shutdown;
use std::time::Duration;
use std::convert::TryInto;
use pin_project_lite::pin_project;
use crate::rsa::{generate_number, rsa_encrypt};
use rug::Integer;
use sha3::{Shake128, digest::{Update, ExtendableOutput, XofReader}};
use hex;
use crate::aes::{aes128_decrypt, aes128_encrypt};
use openssl::x509::{X509VerifyResult, X509};

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>; // 4

const PROXY_ADDR: &str = "0.0.0.0:8080";

lazy_static! {
    static ref HASHSET: Mutex<HashSet<String>> = Mutex::new(HashSet::new());
    // secret used for symmetric encryption
    static ref SECRET: Mutex<Vec<String>> = Mutex::new(vec![]);
}

pub async fn copy<R, W>(reader: &mut R, writer: &mut W,
                        proxy_role: ProxyRole, proxy_direct: ProxyDirect,
                        secret: String) -> io::Result<u64>
where
R: Read + Unpin + ?Sized,
W: Write + Unpin + ?Sized,
{
pin_project! {
    struct CopyFuture<R, W> {
        #[pin]
        reader: R,
        #[pin]
        writer: W,
        amt: u64,
        proxy_role: ProxyRole,
        proxy_direct: ProxyDirect,
        secret: String,
    }
}

impl<R, W> Future for CopyFuture<R, W>
where
    R: BufRead,
    W: Write + Unpin,
{
    type Output = io::Result<u64>;

    fn poll(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<Self::Output> {
        let mut this = self.project();
        loop {
            let polled_buf = futures_core::ready!(this.reader.as_mut().poll_fill_buf(cx))?;
            if polled_buf.is_empty() {
                futures_core::ready!(this.writer.as_mut().poll_flush(cx))?;
                return Poll::Ready(Ok(*this.amt));
            }
            let cnt_read = polled_buf.len();
            let mut buffer = vec![0u8; cnt_read];
            buffer.copy_from_slice(polled_buf);
            if matches!(this.proxy_direct, ProxyDirect::FromProxy) {
                println!("received encrypted len={}", cnt_read);
                // if polled_buf.len() % 16 > 0 {
                //     // TODO(DCMMC) BUG!! there are some duplicated packets whose
                //     // length is not multiplication of 16 (BLOCK_SIZE of aes128)!
                //     futures_core::ready!(this.writer.as_mut().poll_flush(cx))?;
                //     return Poll::Ready(Ok(*this.amt));
                // }
                match aes128_decrypt(&buffer, &this.secret) {
                    Ok(res) => {
                        println!("decrypted len={}", res.len());
                        match hex::decode(res) {
                            Err(e) => {
                                eprintln!("err when decode: {}", e);
                            },
                            Ok(b) => {
                                buffer = b;
                            }
                        };
                    },
                    Err(e) => {
                        eprintln!("err when aes128_decrypt: {}", e);
                        futures_core::ready!(this.writer.as_mut().poll_flush(cx))?;
                        return Poll::Ready(Ok(*this.amt));
                    }
                }
            }

            if matches!(this.proxy_role, ProxyRole::Client) && matches!(
                this.proxy_direct, ProxyDirect::ToProxy) {
                // add header
                if buffer.len() > 0xffff_ffff_ffff {
                    return task::Poll::Ready(Err(std::io::Error::new(
                                io::ErrorKind::Other,
                                "length of addr must <= 0xffff_ffff_ffff!")));
                }
                let len = buffer.len().to_le_bytes();
                let mut buf = vec![0b0010u8, len[0], len[1], len[2], 0, 0, 0];
                buf.extend(buffer.iter());
                buffer = buf;
                println!("Client => Server, pkt={}", buffer.len());
            } else if matches!(this.proxy_role, ProxyRole::Client) && matches!(
                this.proxy_direct, ProxyDirect::FromProxy) {
                // remove header
                let flag = buffer[0];
                if flag & 0b0010 != 0b0010 {
                    // currently only support record message type
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                let payload_size = usize::from_le_bytes(
                    [&buffer[1..4], &[0u8; 5]].concat().try_into().unwrap());
                let padding_size = usize::from_le_bytes(
                    [&buffer[4..7], &[0u8; 5]].concat().try_into().unwrap());
                if payload_size < padding_size {
                    // payload_size must large than padding_size!
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                if payload_size == 0 {
                    // EOF
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                println!("Server => Client, payload={}, pad={}, pkt={}",
                         payload_size, padding_size, buffer.len());
                // remove the header
                buffer = buffer.drain(7..).collect();
            } else if matches!(this.proxy_role, ProxyRole::Server) && matches!(
                this.proxy_direct, ProxyDirect::FromProxy) {
                // remove header
                let flag = buffer[0];
                if flag & 0b0010 != 0b0010 {
                    // currently only support record message type
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                let payload_size = usize::from_le_bytes(
                    [&buffer[1..4], &[0u8; 5]].concat().try_into().unwrap());
                let padding_size = usize::from_le_bytes(
                    [&buffer[4..7], &[0u8; 5]].concat().try_into().unwrap());
                if payload_size < padding_size {
                    // payload_size must large than padding_size!
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                if payload_size == 0 {
                    // EOF
                    // drop this packet
                    *this.amt += cnt_read as u64;
                    this.reader.as_mut().consume(cnt_read);
                    continue;
                }
                println!("Server => Target, payload={}, pad={}, pkt={}",
                         payload_size, padding_size, buffer.len());
                // remove the header
                buffer = buffer.drain(7..).collect();
            } else if matches!(this.proxy_role, ProxyRole::Server) && matches!(
                this.proxy_direct, ProxyDirect::ToProxy) {
                // add header
                if buffer.len() > 0xffff_ffff_ffff {
                    return task::Poll::Ready(Err(std::io::Error::new(
                                io::ErrorKind::Other,
                                "length of addr must <= 0xffff_ffff_ffff!")));
                }
                let len = buffer.len().to_le_bytes();
                let mut buf = vec![0b0010u8, len[0], len[1], len[2], 0, 0, 0];
                buf.extend(buffer.iter());
                buffer = buf;
                println!("Target => Server, pkt={}", buffer.len());
            }

            if matches!(this.proxy_direct, ProxyDirect::ToProxy) {
                // TODO(DCMMC) encode will lead to the content increase 100%!
                // e.g. 1B ff => 2B 'ff' (i.e., 0x0f, 0x0f)
                match aes128_encrypt(&hex::encode(&buffer), &this.secret) {
                    Ok(b) => {buffer = b},
                    Err(e) => {
                        eprintln!("err when aes128_encrypt: {}", e);
                    },
                }
                println!("encrypt to len={}", buffer.len());
                // println!("polled_write={}, content={:?}", buffer.len(), buffer);
            }
            let i = futures_core::ready!(this.writer.as_mut().poll_write(cx, &buffer))?;
            futures_core::ready!(this.writer.as_mut().poll_flush(cx))?;
            println!("poll_write: {}", i);
            if i == 0 {
                return Poll::Ready(Err(io::ErrorKind::WriteZero.into()));
            }
            *this.amt += cnt_read as u64;
            this.reader.as_mut().consume(cnt_read);
        }
    }
}

    let future = CopyFuture {
        // TODO(DCMMC) must set large capacity... while default is 8K (8192!)
        // now I set to 640K while maximum size of TCP is 64K (65535)
        reader: BufReader::with_capacity(655350, reader),
        writer,
        amt: 0,
        proxy_role: proxy_role,
        proxy_direct: proxy_direct,
        secret: secret,
    };
    future.await
}

#[derive(Debug, Clone)]
pub struct ProxyStream {
    pub(super) tcp_stream: Arc<TcpStream>,
    pub(super) proxy_role: ProxyRole,
}

#[derive(Debug, Clone)]
pub enum ProxyRole {
    Client,
    Server,
    Channel,
}

#[derive(Debug, Clone)]
pub enum ProxyDirect {
    ToProxy,
    FromProxy,
}

impl ProxyStream {
    pub async fn new(mut tcp_stream: TcpStream, proxy_role: ProxyRole, target_addr: &str) -> Result<ProxyStream> {
        tcp_stream.set_nodelay(true)?;
        match proxy_role {
            ProxyRole::Client => {
                // handshake
                let addr = String::from(target_addr).into_bytes();
                if addr.len() > 0xffff_ffff_ffff {
                    return Err(Box::new(std::io::Error::new(
                                io::ErrorKind::Other,
                                "length of addr must <= 0xffff_ffff_ffff!")));
                }
                let addr_len = addr.len().to_le_bytes();
                // padding_size == 0
                let mut pkt: Vec<u8> = vec![0b0001u8, addr_len[0], addr_len[1], addr_len[2], 0, 0, 0];
                pkt.extend(addr);
                tcp_stream.write_all(&pkt[..]).await?;
            },
            ProxyRole::Server => {},
            ProxyRole::Channel => {},
        }
        return Ok(ProxyStream {
            tcp_stream: Arc::new(tcp_stream),
            proxy_role: proxy_role,
        });
    }

    pub fn shutdown(&self, how: std::net::Shutdown) -> std::io::Result<()> {
        self.tcp_stream.shutdown(how)
    }
}

impl io::Read for ProxyStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self).poll_read(cx, buf)
    }

    fn poll_read_vectored(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bufs: &mut [IoSliceMut<'_>],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self).poll_read_vectored(cx, bufs)
    }
}

impl io::Read for &ProxyStream {
    fn poll_read(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &mut [u8],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self.tcp_stream).poll_read(cx, buf)
    }
}

impl io::Write for ProxyStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self).poll_write(cx, buf)
    }

    fn poll_write_vectored(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        bufs: &[IoSlice<'_>],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self).poll_write_vectored(cx, bufs)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Pin::new(&mut &*self).poll_flush(cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Pin::new(&mut &*self).poll_close(cx)
    }
}

impl io::Write for &ProxyStream {
    fn poll_write(
        self: Pin<&mut Self>,
        cx: &mut Context<'_>,
        buf: &[u8],
    ) -> Poll<io::Result<usize>> {
        Pin::new(&mut &*self.tcp_stream).poll_write(cx, buf)
    }

    fn poll_flush(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Pin::new(&mut &*self.tcp_stream).poll_flush(cx)
    }

    fn poll_close(self: Pin<&mut Self>, cx: &mut Context<'_>) -> Poll<io::Result<()>> {
        Pin::new(&mut &*self.tcp_stream).poll_close(cx)
    }
}

// start from ATYPE, then ADDRESS and PORT
fn socket_addr_to_vec(socket_addr: std::net::SocketAddr) -> Vec<u8> {
    let mut res = Vec::new();
    let ip_bytes = match socket_addr.ip() {
        IpAddr::V4(ip) => {
            res.push(0x01);
            ip.octets().to_vec()
        }
        IpAddr::V6(ip) => {
            res.push(0x04);
            ip.octets().to_vec()
        }
    };
    for val in ip_bytes.iter() {
        res.push(*val);
    }
    res.put_u16(socket_addr.port());
    res
}

pub async fn establish_session() -> io::Result<()> {
    let mut remote_stream = TcpStream::connect(String::from(PROXY_ADDR)).await?;
    let client_random = generate_number(48).to_string_radix(16).into_bytes();
    remote_stream.write_all(&([&[0b0100,][..], &client_random[..]].concat())[..]).await?;
    let mut buf = vec![0u8; 20480];
    let res_size = remote_stream.read(&mut buf).await?;
    println!("debug: res_size={}", res_size);
    // println!("debug: buf={:?}", buf);

    let len_sr: usize = ((buf[1] as usize) << 8) + buf[0] as usize;
    let len_cert: usize = ((buf[3] as usize) << 8) + buf[2] as usize;
    println!("debug: #server_random={}, #cert={}", len_sr, len_cert);
    let server_random = Integer::from_str_radix(
        std::str::from_utf8(
            &buf[4..(4 + len_sr)]
            .to_vec()).unwrap(), 16).unwrap();
    let cert = X509::from_pem(&buf[(4 + len_sr)..(4 + len_sr + len_cert)])?;
    // DCMMC: CA chain verify
    let ca_cert = X509::from_pem(include_bytes!("root-ca.pem"))?;
    match ca_cert.issued(&cert) {
        X509VerifyResult::OK => {
            println!("cert verify success!");
        },
        err => {
            println!("error when verifying cert: {}", err.error_string());
            return Err(std::io::Error::new(
                std::io::ErrorKind::ConnectionAborted,
                "error when verifying cert: ".to_owned() + err.error_string(),
            ));
        }
    }
    // DCMMC: n is the pubkey in the server_cert
    let n = Integer::from_str_radix(
        &cert.public_key()?.rsa()?.n().to_hex_str()?, 16).unwrap();
    let pre_master_secret = generate_number(48);
    let pre_master_cipher = rsa_encrypt((&n, &Integer::from(65537)),
        &pre_master_secret.to_string_radix(16));
    println!("debug: pre_master={}", pre_master_secret);
    println!("debug: pre_master_cipher={:?}", pre_master_cipher);
    remote_stream.write_all(
        &([&[
          pre_master_cipher.len() as u8,
          (pre_master_cipher.len() >> 8) as u8][..],
        &pre_master_cipher].concat())[..]).await?;
    let mut hasher = Shake128::default();
    // println!("debug: client_random: {:?}", client_random);
    // println!("debug: server_random: {:?}", server_random.to_string_radix(16).into_bytes());
    // println!("debug: pre_master: {:?}", pre_master_secret.to_string_radix(16).into_bytes());
    hasher.update(client_random);
    hasher.update(server_random.to_string_radix(16).into_bytes());
    hasher.update(pre_master_secret.to_string_radix(16).into_bytes());
    let mut reader_sha = hasher.finalize_xof();
    let mut secret_buf = [0u8; 32];
    reader_sha.read(&mut secret_buf);
    let num_str = secret_buf.iter()
        .map(|x| format!("{:02X}", x))
        .fold(String::from(""),
              |res: String, curr: String| res + &curr
        );
    let secret = &(Integer::from(Integer::parse_radix(
        num_str, 16).unwrap()).to_string_radix(16))[0..16];
    println!("debug: secret={:?}", secret);
    SECRET.lock().await.push(String::from(secret));
    let _ = remote_stream.shutdown(Shutdown::Both);
    Ok(())
}

pub async fn process(stream: TcpStream, addr: String) -> io::Result<()> {
    let peer_addr = stream.peer_addr()?;
    println!("Accepted from: {}", peer_addr);

    let mut reader = stream.clone();
    let mut writer = stream;

    // read socks5 header
    let mut buffer = vec![0u8; 512];
    reader.read_exact(&mut buffer[0..2]).await?;
    if buffer[0] != 0x05 {
        return Err(std::io::Error::new(
            std::io::ErrorKind::ConnectionAborted,
            "only socks5 protocol is supported!",
        )); // stream will be closed automaticly
    }
    let methods = buffer[1] as usize;
    reader.read_exact(&mut buffer[0..methods]).await?;
    let mut has_no_auth = false;
    for i in 0..methods {
        if buffer[i] == 0x00 {
            has_no_auth = true;
        }
    }
    if !has_no_auth {
        return Err(std::io::Error::new(
            std::io::ErrorKind::ConnectionAborted,
            "only no-auth is supported!",
        )); // stream will be closed automaticly
    }

    // server send to client accepted auth method (0x00 no-auth only yet)
    writer.write(&[0x05u8, 0x00]).await?;
    writer.flush().await?;

    // read socks5 cmd
    reader.read_exact(&mut buffer[0..4]).await?;
    let cmd = buffer[1]; // support 0x01(CONNECT) and 0x03(UDP Associate)
    let atype = buffer[3];

    let mut addr_port = String::from("");
    let mut flag_addr_ok = true;

    // parse addr and port first
    match atype {
        0x01 => {
            // ipv4: 4bytes + port
            reader.read_exact(&mut buffer[0..6]).await?;
            let mut tmp_array: [u8; 4] = Default::default();
            tmp_array.copy_from_slice(&buffer[0..4]);
            let v4addr = Ipv4Addr::from(tmp_array);
            let port: u16 = buffer[4..6].as_ref().get_u16();
            let socket = SocketAddrV4::new(v4addr, port);
            addr_port = format!("{}", socket);
            // println!("ipv4: {}", addr_port);
        }
        0x03 => {
            reader.read_exact(&mut buffer[0..1]).await?;
            let len = buffer[0] as usize;
            reader.read_exact(&mut buffer[0..len + 2]).await?;
            let port: u16 = buffer[len..len + 2].as_ref().get_u16();
            if let Ok(addr) = std::str::from_utf8(&buffer[0..len]) {
                addr_port = format!("{}:{}", addr, port);
            } else {
                flag_addr_ok = false;
            }
            // println!("domain: {}", addr_port);
        }
        0x04 => {
            // ipv6: 16bytes + port
            reader.read_exact(&mut buffer[0..18]).await?;
            let mut tmp_array: [u8; 16] = Default::default();
            tmp_array.copy_from_slice(&buffer[0..16]);
            let v6addr = Ipv6Addr::from(tmp_array);
            let port: u16 = buffer[16..18].as_ref().get_u16();
            let socket = SocketAddrV6::new(v6addr, port, 0, 0);
            addr_port = format!("{}", socket);
            // println!("ipv6: {}", addr_port);
        }
        _ => {
            flag_addr_ok = false;
        }
    }
    if !flag_addr_ok {
        writer
            .write(&[
                0x05u8, 0x08, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
            ])
            .await?;
        return Err(std::io::Error::new(
            std::io::ErrorKind::AddrNotAvailable,
            "address is not valid!".to_string(),
        ));
    }

    // parse cmd: support CONNECT(0x01) and UDP (0x03) currently
    match cmd {
        0x01 => {
            //create connection to remote server
            // (DCMMC) connect to a ip according to the socks5 client's packet
            if let Ok(remote_stream) = TcpStream::connect(String::from(PROXY_ADDR)).await {
                println!("connect to {} ok", addr_port);
                writer
                    .write(&[
                        0x05u8, 0x00, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                    ])
                    .await?;
                let proxy_stream: ProxyStream = ProxyStream::new(
                    remote_stream, ProxyRole::Client, &addr_port).await.unwrap();
                let mut proxy_read = proxy_stream.clone();
                let mut proxy_write = proxy_stream;
                // let mut remote_read = remote_stream.clone();
                // let mut remote_write = remote_stream;
                task::spawn(async move {
                    // (DCMMC) local socks5 server => target website's server
                    // TODO(DCMMC) is this line replace with secret in the copy call,
                    // the whole process will stuck! This is a bug (maybe)!
                    let secret = String::from(SECRET.lock().await.get(0).unwrap());
                    match copy(&mut reader, &mut proxy_write, ProxyRole::Client,
                               ProxyDirect::ToProxy,
                               secret
                               // String::from("")
                               ).await {
                        Ok(_) => {}
                        Err(e) => {
                            eprintln!("broken pipe: {}", e);
                        }
                    }
                    task::sleep(Duration::from_secs(30)).await;
                    let _ = reader.shutdown(Shutdown::Both);
                    let _ = proxy_write.shutdown(Shutdown::Both);
                });
                // (DCMMC) target website's server => local socks5 server
                let secret = String::from(SECRET.lock().await.get(0).unwrap());
                copy(&mut proxy_read, &mut writer, ProxyRole::Client,
                     ProxyDirect::FromProxy,
                     secret,
                     ).await?;
                task::sleep(Duration::from_secs(30)).await;
                proxy_read.shutdown(Shutdown::Both)?;
                writer.shutdown(Shutdown::Both)?
            } else {
                writer
                    .write(&[
                        0x05u8, 0x05, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                    ])
                    .await?;
                return Err(std::io::Error::new(
                    std::io::ErrorKind::ConnectionRefused,
                    format!("cannot make connection to {}!", addr_port),
                )); // stream will be closed automaticly
            };
        }
        0x03 => {
            // UDP Associate
            println!("start udp associate for {}", peer_addr);
            let raw_socket = UdpSocket::bind(format!("{}:0", addr)).await?;
            let socket = Arc::new(raw_socket);
            let socket_addr = socket.local_addr();

            let mut addr_port = String::from("");

            match socket_addr {
                Ok(addr) => {
                    writer.write(&[0x05u8, 0x00, 0x00]).await?;

                    let content = socket_addr_to_vec(addr);
                    writer.write(&content).await?;

                    HASHSET.lock().await.insert(peer_addr.to_string());

                    task::spawn(async move {
                        let mut buf = vec![0u8; 1];

                        // close connection if we read more bytes
                        match reader.read_exact(&mut buf[0..1]).await {
                            Ok(_) => {
                                // eprintln!("read something {:x?}", buf);
                            }
                            Err(_) => {
                                // eprintln!("error while reading");
                            }
                        }
                        HASHSET.lock().await.remove(&peer_addr.to_string());
                        println!("udp-tcp disconnect from {}", peer_addr);
                    });

                    //start to transfer data
                    //recv first packet
                    let mut buf = vec![0u8; 20480];
                    let (mut n, local_peer) = socket.recv_from(&mut buf).await?;

                    let socket_remote_raw = UdpSocket::bind("0.0.0.0:0").await?;
                    let socket_remote_reader = Arc::new(socket_remote_raw);
                    let socket_remote_writer = socket_remote_reader.clone();
                    let local_socket_writer = socket.clone();
                    task::spawn(async move {
                        let mut buf = vec![0u8; 2048];

                        loop {
                            if HASHSET.lock().await.contains(&peer_addr.to_string()) {
                                let res = io::timeout(Duration::from_secs(5), async {
                                    socket_remote_reader.recv_from(&mut buf).await
                                })
                                .await;
                                match res {
                                    Ok((n, remote_addr)) => {
                                        let mut write_packet = vec![0x0u8, 0, 0];

                                        let content = socket_addr_to_vec(remote_addr);
                                        for val in content.iter() {
                                            write_packet.push(*val);
                                        }
                                        for val in buf[0..n].iter() {
                                            write_packet.push(*val);
                                        }
                                        // write the udp packet at once
                                        let _ = local_socket_writer
                                            .send_to(&write_packet, local_peer)
                                            .await;
                                    }
                                    Err(e) if e.kind() == io::ErrorKind::TimedOut => {
                                        eprintln!("timeout {:?}", e.kind());
                                    }
                                    Err(e) => {
                                        HASHSET.lock().await.remove(&peer_addr.to_string());
                                        eprintln!("error read udp from remote: {}", e);
                                    }
                                }
                            } else {
                                break;
                            }
                        }
                    });
                    loop {
                        if HASHSET.lock().await.contains(&peer_addr.to_string()) {
                            if n > 4 {
                                let mut addr_is_ok = true;
                                //processing receved packet from client
                                if buf[0] == 0x00 && buf[1] == 0x00 && buf[2] == 0x00 {
                                    let mut idx = 0usize;
                                    match buf[3] {
                                        0x01 => {
                                            if n < 4 + 4 + 2 {
                                                addr_is_ok = false;
                                            } else {
                                                let mut tmp_array: [u8; 4] = Default::default();
                                                tmp_array.copy_from_slice(&buf[4..8]);
                                                let v4addr = Ipv4Addr::from(tmp_array);
                                                let port: u16 = buf[8..10].as_ref().get_u16();
                                                let socket = SocketAddrV4::new(v4addr, port);
                                                addr_port = format!("{}", socket);
                                                idx = 10;
                                                // println!("ipv4: {}", addr_port);
                                            }
                                        }
                                        0x03 => {
                                            let len = buf[4] as usize;
                                            if n < 4 + len + 2 {
                                                addr_is_ok = false;
                                            } else {
                                                let port: u16 =
                                                    buf[5 + len..5 + 2 + len].as_ref().get_u16();
                                                if let Ok(addr) =
                                                    std::str::from_utf8(&buf[5..5 + len])
                                                {
                                                    addr_port = format!("{}:{}", addr, port);
                                                    idx = 5 + 2 + len;
                                                } else {
                                                    addr_is_ok = false;
                                                }

                                                // println!("domain: {}", addr_port);
                                            }
                                        }
                                        0x04 => {
                                            if n < 4 + 16 + 2 {
                                                addr_is_ok = false;
                                            } else {
                                                // ipv6: 16bytes + port
                                                let mut tmp_array: [u8; 16] = Default::default();
                                                tmp_array.copy_from_slice(&buf[4..20]);
                                                let v6addr = Ipv6Addr::from(tmp_array);
                                                let port: u16 = buf[20..22].as_ref().get_u16();
                                                let socket = SocketAddrV6::new(v6addr, port, 0, 0);
                                                addr_port = format!("{}", socket);
                                                idx = 22;
                                                // println!("ipv6: {}", addr_port);
                                            }
                                        }
                                        _ => {}
                                    }
                                    if addr_is_ok {
                                        println!("send UDP to {} for {}", addr_port, peer_addr);
                                        let _ = socket_remote_writer
                                            .send_to(&buf[idx..n], &addr_port)
                                            .await;
                                    } else {
                                        HASHSET.lock().await.remove(&peer_addr.to_string());
                                    }
                                }
                            }
                            let read_res = io::timeout(Duration::from_secs(5), async {
                                socket.recv_from(&mut buf).await
                            })
                            .await;
                            match read_res {
                                Ok((nn, _)) => {
                                    n = nn;
                                }
                                Err(e) if e.kind() == io::ErrorKind::TimedOut => {
                                    n = 0;
                                    eprintln!("timeout {:?}", e.kind());
                                }
                                Err(_) => {
                                    HASHSET.lock().await.remove(&peer_addr.to_string());
                                }
                            }
                        } else {
                            break;
                        }
                    }
                }
                Err(_) => {
                    writer
                        .write(&[
                            0x05u8, 0x01, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                        ])
                        .await?;
                    return Err(std::io::Error::new(
                        std::io::ErrorKind::ConnectionRefused,
                        format!("udp listen port failed {}!", addr_port),
                    )); // stream will be closed automaticly
                }
            }
        }
        _ => {
            writer
                .write(&[
                    0x05u8, 0x07, 0x00, 0x01, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
                ])
                .await?;
            return Err(std::io::Error::new(
                std::io::ErrorKind::ConnectionAborted,
                "command is not supported!",
            ));
        }
    }

    println!("disconnect from {}", peer_addr);
    Ok(())
}
