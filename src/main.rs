

use macrocosm::{SaveMe, defer};
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

    async fn test(){
        println!("test");
    }

    async fn execute(){
        println!("execute");
    }

    // it uses drop to execute a function when the type is about to 
    // be dropped out of the ram and its lifetime comes to die
    #[defer]
    async fn run(){
        
        println!("defer function");
    }


    test().await;
    execute().await;

    Ok(())

}