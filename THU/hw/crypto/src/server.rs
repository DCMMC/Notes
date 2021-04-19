// [ref] https://book.async.rs/tutorial/accept_loop.html
use async_std::{
    prelude::*,
    task,
    io::BufReader,
    net::{TcpListener, TcpStream, ToSocketAddrs},
};
type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>; // 4

async fn accept_loop(addr: impl ToSocketAddrs) -> Result<()> { // 1
    let listener = TcpListener::bind(addr).await?; // 2
    let mut incoming = listener.incoming();
    while let Some(stream) = incoming.next().await { // 3
        let stream = stream?;
        stream.set_nodelay(true)?;
        println!("Accepting from: {:?}", stream.peer_addr()?);
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
    let mut stream_writer = stream.clone();
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

pub fn run_server() -> Result<()> {
    println!("\n\nStart running server on 0.0.0.0:8080");
    let fut = accept_loop("0.0.0.0:8080");
    task::block_on(fut)
}
