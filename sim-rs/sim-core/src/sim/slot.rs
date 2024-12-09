use std::time::Duration;

use crate::{
    clock::{Clock, Timestamp},
    events::EventTracker,
};

pub struct SlotWitness {
    clock: Clock,
    tracker: EventTracker,
}

impl SlotWitness {
    pub fn new(clock: Clock, tracker: EventTracker) -> Self {
        Self { clock, tracker }
    }

    pub async fn run(&mut self) {
        let mut slot = 0;
        let mut next_slot_at = Timestamp::zero();
        loop {
            self.tracker.track_slot(slot);
            slot += 1;
            next_slot_at = next_slot_at + Duration::from_secs(1);
            self.clock.wait_until(next_slot_at).await;
        }
    }
}
