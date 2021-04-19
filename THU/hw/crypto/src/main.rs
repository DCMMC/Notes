mod aes;
use crate::aes::test_aes;
mod rsa;
use crate::rsa::test_rsa;
mod server;
use crate::server::run_server;
mod client;
use crate::client::run_client;
use async_std::task;
use std::time::Duration;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

fn main() -> Result<()> {
    test_aes();
    test_rsa();
    task::spawn(async {
        task::sleep(Duration::from_secs(3u64)).await; // 1
        if let Err(e) =  run_client() {
            eprintln!("Error: {}", e);
        };
    });
    run_server()
}
