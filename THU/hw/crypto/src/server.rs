// [ref] https://book.async.rs/tutorial/accept_loop.html
use async_std::{
    prelude::*,
    task,
    io::BufReader,
    net::{TcpListener, TcpStream, ToSocketAddrs},
};
use async_std::io;
use std::time::Duration;
use async_std::future::timeout;
use std::convert::TryInto;
use std::error::Error;
use std::str;

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
    let writer = stream.clone();
    let mut reader = stream;
    let mut connection: Option<TcpStream> = None;
    // TODO(DCMMC) buffer size
    loop {
        // (DCMMC) 1B flag, 3B payload length (in bytes), 3B padding length (in bytes)
        let mut buf = vec![0u8; 7];
        match timeout(Duration::from_secs(60), reader.read_exact(&mut buf)).await {
            Err(e) => {
                eprintln!("Timeout: {:?}", e);
                break;
            },
            Ok(Err(e)) => return Err(Box::new(e)),
            Ok(Ok(())) => println!("debug: header={:?}", buf),
        }
        let flag = buf[0];
        let payload_size = usize::from_le_bytes(buf[1..4].try_into().unwrap());
        let padding_size = usize::from_le_bytes(buf[4..7].try_into().unwrap());
        if payload_size < padding_size {
            return Err(Box::new(io::Error::new(
                        io::ErrorKind::Other, "payload_size must large than padding_size!")));
        }
        if payload_size == 0 {
            // EOF
            break;
        }
        buf = vec![0u8; payload_size];
        match timeout(Duration::from_secs(60), reader.read_exact(&mut buf)).await {
            Err(e) => {
                eprintln!("Timeout: {:?}", e);
                break;
            },
            Ok(Err(e)) => return Err(Box::new(e)),
            Ok(Ok(())) => println!("debug: payload={:?}", buf),
        }
        if flag & 0b0001 > 0 {
            // set target addr
            let target = str::from_utf8(
                    &buf[0..(payload_size - padding_size)])?;
            println!("debug: server get target={}", target);
            connection = Some(TcpStream::connect(target).await?);
        } else if flag & 0b0010 > 0 {
            // record message
        }
    }
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
