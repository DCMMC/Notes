mod aes;
use crate::aes::test_aes;
mod rsa;
use crate::rsa::test_rsa;
mod server;
use crate::server::run_server;
mod socks5;
mod client;
#[macro_use]
extern crate lazy_static;
use crate::client::spawn_socks_server;
use async_std::task;
use clap::App;
use std::time::Duration;

type Result<T> = std::result::Result<T, Box<dyn std::error::Error + Send + Sync>>;

fn main() -> Result<()> {
    let matches = App::new("Cryptop lab")
        .version("1.0")
        .author("Wentao Xiao; Duan Guanglin")
        .about("powered by rust")
        .arg("-c, --client 'start client'")
        .arg("-s, --server 'start server'")
        .get_matches();
    test_aes();
    test_rsa();
    task::block_on(async {
        let mut server = None;
        if matches.is_present("client") {
            server = Some(task::spawn(async {
            loop {
                match spawn_socks_server().await {
                    Ok(_) => (),
                    Err(e) => {
                        task::sleep(Duration::from_secs(2)).await;
                        eprintln!("Error: {}, recovering server.", e);
                        continue;
                    },
                };
            }
            }));
        }
        if matches.is_present("server") {
            server = Some(task::spawn(run_server()));
        }
        match server.unwrap().await {
            Ok(_) => (),
            Err(e) => eprintln!("Error: {}", e)
        };
    });

    Ok(())
}
