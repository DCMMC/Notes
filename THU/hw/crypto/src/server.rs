// [ref] https://book.async.rs/tutorial/accept_loop.html
use async_std::{
    prelude::*,
    task,
    io::BufReader,
    net::{TcpListener, TcpStream, ToSocketAddrs},
};
use std::time::Duration;
use async_std::future::timeout;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>; // 4

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
    let reader = BufReader::new(&stream);
    // TODO(DCMMC) buffer size
    let mut stream_writer = stream.clone();
    loop {
        // (DCMMC) 1B flag, 3B payload length (in bytes), 3B padding length (in bytes)
        let mut buf = vec![0u8; 7];
        match timeout(Duration::from_secs(60), reader.read_exact(&mut buf)).await {
            Err(e) => return Err(e),
            Ok(Err(e)) => return Err(e),
            Ok(Ok(())) => println!("debug: header={:?}", buf),
        }
        let flag = buf[0];
        let payload_size = usize::from_be_bytes(buf[1..4]);
        let padding_size = usize::from_be_bytes(buf[4..7]);
        if payload_size < padding_size {
            return Err("payload_size must large than padding_size!");
        }
        buf = vec![0u8; payload_size];
        match timeout(Duration::from_secs(60), reader.read_exact(&mut buf)).await {
            Err(e) => return Err(e),
            Ok(Err(e)) => return Err(e),
            Ok(Ok(())) => println!("debug: payload={:?}", buf),
        }
        
    }
    let mut lines = reader.lines();

    while let Some(line) = lines.next().await {
        let line = line?;
        println!("line: {:?}\n", &line);
        let msg = "Response to client: ".to_owned() + &line + "\n";
        stream_writer.write_all(&msg.as_bytes()).await?;
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
