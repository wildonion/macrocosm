

use macrocosm::{go, gokio, passport, SaveMe, defer};
use tokio::net::unix::SocketAddr;
mod mac;


#[derive(Debug, SaveMe)] // SaveMe save the struct info into a file at compile time
struct Data{
    pub Numbers: Vec<i32>,
    pub name: String, 
    pub socket: SocketAddr
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>>{

    #[defer]
    pub async fn deferMe(){
        println!("defer function");
    }

    Ok(())

}