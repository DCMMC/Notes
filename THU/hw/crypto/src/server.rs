// [ref] https://book.async.rs/tutorial/accept_loop.html
use async_std::{
    prelude::*,
    task,
    net::{TcpListener, TcpStream, ToSocketAddrs},
};
use async_std::io;
use std::time::Duration;
use async_std::future::timeout;
use std::convert::TryInto;
use std::error::Error;
use std::str;
use std::net::Shutdown;
use async_std::sync::Mutex;
use crate::socks5::{ProxyStream, ProxyRole, ProxyDirect, copy};
use crate::rsa::{rsa_key_pair, rsa_decrypt, generate_number};
use rug::Integer;
use sha3::{Shake128, digest::{Update, ExtendableOutput, XofReader}};
use openssl::rsa::Rsa;
use openssl::bn::{MsbOption, BigNum};
use openssl::pkey::PKey;
use openssl::x509::{X509NameBuilder, X509};
use openssl::asn1::Asn1Time;
use openssl::hash::MessageDigest;

lazy_static! {
    // secret used for symmetric encryption
    static ref SECRET: Mutex<Vec<String>> = Mutex::new(vec![]);
}

type Result<T> = std::result::Result<T, Box<dyn Error + Send + Sync>>; // 4

async fn accept_loop(addr: impl ToSocketAddrs) -> Result<()> { // 1
    let listener = TcpListener::bind(addr).await?; // 2
    let mut incoming = listener.incoming();
    while let Some(stream) = incoming.next().await { // 3
        let stream = stream?;
        stream.set_nodelay(true)?;
        println!("\nAccepting from: {:?}", stream.peer_addr()?);
        let _handle = spawn_and_log_error(connection_loop(stream));
    }
    Ok(())
}

fn spawn_and_log_error<F>(fut: F) -> task::JoinHandle<()>
where
    F: Future<Output = Result<()>> + Send + 'static,
{
    task::spawn(async move {
        if let Err(e) = fut.await {
            eprintln!("Error: {}", e)
        }
    })
}

async fn connection_loop(stream: TcpStream) -> Result<()> {
    let mut writer = stream.clone();
    let mut reader = stream;
    let mut connection: Option<TcpStream> = None;
    // (DCMMC) 1B flag, 3B payload length (in bytes), 3B padding length (in bytes)
    // TODO(DCMMC) buffer size
    let mut buf = vec![0u8; 2048];
    match timeout(Duration::from_secs(60), reader.read(&mut buf)).await {
        Err(e) => {
            eprintln!("Timeout: {:?}", e);
            return Err(Box::new(e));
        },
    }
    let flag = buf[0];
    if SECRET.lock().await.len() == 0 {
        if flag & 0b0100 == 0b0100 {
            // handshake for global secret
            // like TLS 1.2
            // [ref] https://halfrost.com/https-key-cipher/
            let client_random = &buf[1..13];
            let (n, e, d) = rsa_key_pair("extend_gcd");
            let server_random = generate_number(48).to_string_radix(16).into_bytes();
            // X509 cert
            // [ref] https://zhuanlan.zhihu.com/p/69995175
            // [ref] https://github.com/sfackler/rust-openssl/pull/1339/files
            // [ref] https://zhaohuabing.com/post/2020-03-19-pki/
            let ca_cert = X509::from_pem(include_bytes!("root-ca.pem"))?;
            let ca_key = PKey::private_key_from_pem(include_bytes!("root-ca.key"))?;
            let pubkey = PKey::from_rsa(Rsa::from_public_components(
                BigNum::from_hex_str(&n.to_string_radix(16)).unwrap(),
                BigNum::from_hex_str(&e.to_string_radix(16)).unwrap(),
            ).unwrap()).unwrap();
            let mut x509_name = X509NameBuilder::new().unwrap();
            x509_name.append_entry_by_text("C", "CN").unwrap();
            x509_name.append_entry_by_text("ST", "SZ").unwrap();
            x509_name.append_entry_by_text("O", "DCMMC").unwrap();
            x509_name.append_entry_by_text("CN", "www.example.com").unwrap();
            let x509_name = x509_name.build();
            let mut builder = X509::builder().unwrap();
            builder.set_pubkey(&pubkey)?;
            builder.set_subject_name(&x509_name)?;
            let mut serial = BigNum::new().unwrap();
            serial.rand(128, MsbOption::MAYBE_ZERO, false).unwrap();
            builder
                .set_serial_number(&serial.to_asn1_integer().unwrap())
                .unwrap();
            builder
                .set_not_before(&Asn1Time::days_from_now(0).unwrap())
                .unwrap();
            builder
                .set_not_after(&Asn1Time::days_from_now(365).unwrap())
                .unwrap();
            builder.set_issuer_name(&ca_cert.subject_name())?;
            builder.sign(&ca_key, MessageDigest::sha256())?;
            let server_cert: Vec<u8> = builder.build().to_pem()?;

            // let n_str = n.to_string_radix(16).into_bytes();
            println!("debug: #server_random={}, #cert={}",
                     server_random.len(), server_cert.len());
            writer.write_all(
                &([&[
                  server_random.len() as u8, (server_random.len() >> 8) as u8,
                  server_cert.len() as u8, (server_cert.len() >> 8) as u8][..],
                &server_random[..], &server_cert[..]].concat())[..]).await?;
            let mut buf_new = vec![0u8; 2048];
            let _ = reader.read(&mut buf_new).await?;
            let len_master = buf_new[0] as usize + (buf_new[1] as usize) << 8;
            let mut pre_master_cipher = vec![0u8; len_master];
            pre_master_cipher.copy_from_slice(&buf_new[2..(2 + len_master)]);
            let pre_master_secret = Integer::from_str_radix(&rsa_decrypt(
                    (&n, &d), &pre_master_cipher), 16).unwrap();
            let mut hasher = Shake128::default();
            // println!("debug: client_random={:?}", client_random);
            // println!("debug: server_random={:?}", server_random);
            // println!("debug: pre_master={:?}", pre_master_secret.to_string_radix(16).into_bytes());
            hasher.update(client_random);
            hasher.update(server_random);
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
            let _ = writer.shutdown(Shutdown::Both);
            let _ = reader.shutdown(Shutdown::Both);
            return Ok(());
        } else {
            return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other, "handshake error")));
        }
    }
    let payload_size = usize::from_le_bytes(
        [&buf[1..4], &[0u8; 5]].concat().try_into().unwrap());
    let padding_size = usize::from_le_bytes(
        [&buf[4..7], &[0u8; 5]].concat().try_into().unwrap());
    if payload_size < padding_size {
        return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other, "payload_size must large than padding_size!")));
    }
    if payload_size == 0 {
        // EOF
        return Ok(());
    }
    if flag & 0b0001 == 0b0001 {
        buf = (&buf[7..]).to_vec();
        // set target addr
        let target = str::from_utf8(
                &buf[0..(payload_size - padding_size)])?;
        println!("debug: server get target={}", target);
        connection = Some(TcpStream::connect(target).await?);
    } else {
        // the flag of the first packet must be 0b0001
        return Err(Box::new(io::Error::new(
                    io::ErrorKind::Other,
                    "The first pkt of the session must have flag 0b0001")));
    }

    let remote_stream: ProxyStream = ProxyStream::new(
        connection.unwrap(), ProxyRole::Server, "").await.unwrap();
    let mut remote_read = remote_stream.clone();
    let mut remote_write = remote_stream;
    // let mut remote_read = remote_stream.clone();
    // let mut remote_write = remote_stream;
    task::spawn(async move {
        // (DCMMC) local socks5 server => target website's server
        let secret = String::from(SECRET.lock().await.get(0).unwrap());
        match copy(&mut reader, &mut remote_write, ProxyRole::Server,
                   ProxyDirect::FromProxy,
                   secret
                   ).await {
            Ok(_) => {}
            Err(e) => {
                eprintln!("broken pipe: {}", e);
            }
        }
        task::sleep(Duration::from_secs(30)).await;
        let _ = reader.shutdown(Shutdown::Both);
        let _ = remote_write.shutdown(Shutdown::Both);
    });
    // (DCMMC) target website's server => local socks5 server
    let secret = String::from(SECRET.lock().await.get(0).unwrap());
    copy(&mut remote_read, &mut writer, ProxyRole::Server,
         ProxyDirect::ToProxy,
         secret
         ).await?;
    task::sleep(Duration::from_secs(30)).await;
    remote_read.shutdown(Shutdown::Both)?;
    writer.shutdown(Shutdown::Both)?;
    println!("EOF");
    Ok(())
}

pub async fn run_server() -> Result<()> {
    println!("\n\nStart running server on 0.0.0.0:8080");
    let _fut = accept_loop("0.0.0.0:8080");
    match _fut.await {
        Err(e) => eprintln!("Error: {}", e),
        Ok(_) => ()
    };
    // task::block_on(fut)
    Ok(())
}
