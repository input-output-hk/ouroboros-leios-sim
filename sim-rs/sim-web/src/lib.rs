mod utils;

use std::{future::Future, time::Instant};

use anyhow::Result;
use sim_core::{clock::{Clock, Timestamp}, config::{RawConfig, SimConfiguration}, events::{Event, EventTracker}, model::TransactionId, sim::Simulation};
use tokio::sync::mpsc;
use utils::set_panic_hook;
use wasm_bindgen::prelude::*;

#[wasm_bindgen(start)]
pub fn start() {
    set_panic_hook();
}

pub enum WebEvent {
    NewTransaction(TransactionId)
}

#[wasm_bindgen]
pub struct WebSimulation {
    running: Box<dyn Future<Output = Result<()>>>,
    receiver: mpsc::UnboundedReceiver<(Event, Timestamp)>,
}

impl WebSimulation {
    pub async fn next_event(&mut self) -> Option<TransactionId> {
        None
    }
}

#[wasm_bindgen]
pub async fn begin_sim(config_str: &str) -> Result<WebSimulation, JsError> {
    let raw_config: RawConfig = toml::from_str(config_str)?;
    let config: SimConfiguration = raw_config.into();
    config.validate().map_err(|e| JsError::new(&e.to_string()))?;

    let clock = Clock::new(Instant::now(), config.timescale);
    let (sender, receiver) = mpsc::unbounded_channel();
    let tracker = EventTracker::new(sender, clock.clone());
    let mut sim = Simulation::new(config, tracker, clock).map_err(|e| JsError::new(&e.to_string()))?;

    let running = async move {
        sim.run().await
    };
    Ok(WebSimulation {
        running: Box::new(running),
        receiver
    })
}
