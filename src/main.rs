

use macrocosm::{go, gokio};
mod mac;


#[derive(Debug)]
struct Data{
    pub Numbers: Vec<i32>
}

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error + Send + Sync + 'static>>{

    // sleep asyncly
    tokio::time::sleep(tokio::time::Duration::from_secs(2)).await; 

    let d = Data{ 
        Numbers: 
            (0..100)
                .into_iter()
                .map(|i| i*100)
                .collect::<Vec<i32>>()
    };

    Ok(())
}