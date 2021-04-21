use async_std::{
    net::TcpStream,
    net::TcpListener,
    prelude::*,
    task,
    io::BufReader,
};
mod socks5;
use crate::client::socks5::process;
type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

async fn test_t1(msg: &str) -> Result<()> {
    let stream = TcpStream::connect("127.0.0.1:8080").await?;
    stream.set_nodelay(true)?;
    let (reader, mut writer) = (&stream, &stream);
    let mut lines_from_server = BufReader::new(reader).lines();
    writer.write_all(msg.as_bytes()).await?;
    writer.write_all(b"\n").await?;
    while let Some(Ok(line)) = lines_from_server.next().await {
        println!("Received from server: {:?}", line);
        break;
    }
    Ok(())
}

pub async fn spawn_socks_server() -> Result<()> {
    let bind_str = "0.0.0.0:1080";
    let bind_addr = "0.0.0.0".to_string();
    let listener = TcpListener::bind(bind_str).await?;
    println!("Socks5 server Listening on {}", listener.local_addr()?);
    let mut incoming = listener.incoming();
    while let Some(stream) = incoming.next().await {
        let stream = stream?;
        let addr = bind_addr.clone();
        task::spawn(async {
            match process(stream, addr).await {
                Ok(()) => {}
                Err(e) => {
                    eprintln!("broken pipe: {}", e);
                }
            }
        });
    }
    Ok(())
}

pub fn run_client() -> Result<()> {
    task::block_on(test_t1("Cryptography and Network Security; 2020214245; 肖文韬 (Wentao Xiao) 🎉🚀"))
}
