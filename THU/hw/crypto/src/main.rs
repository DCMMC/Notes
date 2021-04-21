mod aes;
use crate::aes::test_aes;
mod rsa;
use crate::rsa::test_rsa;
mod server;
use crate::server::run_server;
mod client;
#[macro_use]
extern crate lazy_static;
use crate::client::run_client;
use crate::client::spawn_socks_server;
use async_std::task;
use std::time::Duration;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

fn main() -> Result<()> {
    test_aes();
    test_rsa();
    task::spawn(async {
        task::sleep(Duration::from_secs(3u64)).await; // 1
        if let Err(e) = run_client() {
            eprintln!("Error: {}", e);
        };
        println!("Client: send twice.");
        task::sleep(Duration::from_secs(3u64)).await; // 1
        if let Err(e) = run_client() {
            eprintln!("Error: {}", e);
        };
    });
    task::block_on(async {
        let _socks5_server = task::spawn(async {
            loop {
                match spawn_socks_server().await {
                    Ok(_) => (),
                    Err(e) => {
                        eprintln!("Error: {}, recovering server.", e);
                        continue;
                    },
                };
            }
        });
        let _server = task::spawn(run_server());
        match _server.await {
            Ok(_) => (),
            Err(e) => eprintln!("Error: {}", e)
        };
    });

    Ok(())
}
