use async_std::{
    net::TcpListener,
    prelude::*,
    task,
};
use crate::socks5::{process, establish_session};
type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

pub async fn spawn_socks_server() -> Result<()> {
    match establish_session().await {
        Ok(()) => {
            println!("establish session success.");
        },
        Err(e) => {
            eprintln!("establish session failed!");
            return Err(Box::new(e));
        },
    }
    let bind_str = "0.0.0.0:1088";
    let bind_addr = "0.0.0.0".to_string();
    let listener = TcpListener::bind(bind_str).await?;
    println!("\n\nSocks5 server Listening on {}", listener.local_addr()?);
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
