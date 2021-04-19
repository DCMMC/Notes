use async_std::{
    net::TcpStream,
    prelude::*,
    task,
    io::BufReader,
};
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
    }
    Ok(())
}

pub fn run_client() -> Result<()> {
    task::block_on(test_t1("Cryptography and Network Security; 2020214245; 肖文韬 (Wentao Xiao) 🎉🚀"))
}
