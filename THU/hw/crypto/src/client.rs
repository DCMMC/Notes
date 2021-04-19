use async_std::{
    net::TcpStream,
    prelude::*,
    task,
    io::BufReader,
};
use fast_socks5::{
    server::{Config, Socks5Server, Socks5Socket},
};
use futures::{AsyncRead, AsyncWrite};
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

fn spawn_and_log_error<F>(fut: F) -> task::JoinHandle<()>
where
    F: Future<Output = Result<()>> + Send + 'static,
{
    task::spawn(async move {
        if let Err(e) = fut.await {
            println!("Error in socks5 server: {:#}", &e);
        }
    })
}

async fn connection_loop<T>(mut socket: Socks5Socket<T>) -> Result<()>
where T: AsyncRead + AsyncWrite + Unpin
{
    let mut content = String::new();
    while let Ok(content_size) = socket.read_to_string(&mut content).await {
        println!("line (socks5, {}B): {:?}\n", content_size, &content);
        let msg = "Response to client: ".to_owned() + &content + "\n";
        socket.write_all(&msg.as_bytes()).await?;
    }
    println!("EOF");

    Ok(())
}

pub async fn spawn_socks_server() -> Result<()> {
    let mut config = Config::default();
    config.set_skip_auth(false);
    // socks5h
    config.set_dns_resolve(true);
    let listen_addr = "127.0.0.1:1080";
    let mut listener = Socks5Server::bind(listen_addr).await?;
    listener.set_config(config);
    let mut incoming = listener.incoming();
    println!("Listen for socks5 connections @ {}", listen_addr);

    while let Some(socket_res) = incoming.next().await {
        match socket_res {
            Ok(socket) => {
                spawn_and_log_error(connection_loop(
                    socket.upgrade_to_socks5().await?));
            }
            Err(e) => {
                println!("Error: {}", e);
                return Err(Box::new(e));
            }
        }
    }

    Ok(())
}

pub fn run_client() -> Result<()> {
    task::block_on(test_t1("Cryptography and Network Security; 2020214245; 肖文韬 (Wentao Xiao) 🎉🚀"))
}
