use std::{cmp::Reverse, collections::BinaryHeap, time::Duration};

use crate::{
    clock::{Clock, Timestamp},
    network::{NetworkSource, PendingMessage},
};

use super::{SimulationEvent, SimulationMessage};

pub struct EventQueue {
    clock: Clock,
    scheduled: BinaryHeap<FutureEvent>,
    msg_source: NetworkSource<SimulationMessage>,
}

impl EventQueue {
    pub fn new(clock: Clock, msg_source: NetworkSource<SimulationMessage>) -> Self {
        Self {
            clock,
            scheduled: BinaryHeap::new(),
            msg_source,
        }
    }

    pub fn queue_event(&mut self, event: SimulationEvent, after: Duration) {
        self.scheduled
            .push(FutureEvent(self.clock.now() + after, event));
    }

    pub async fn next_event(&mut self) -> Option<SimulationEvent> {
        let scheduled_at = self.scheduled.peek().map(|e| e.0);
        let receiving_at = self.msg_source.peek_next_message_at();

        if scheduled_at.is_none() && receiving_at.is_none() {
            return None;
        }

        let next_event_is_scheduled =
            scheduled_at.is_some_and(|s| !receiving_at.is_some_and(|r| r < s));
        let (event, timestamp) = if next_event_is_scheduled {
            let FutureEvent(timestamp, event) = self.scheduled.pop().unwrap();
            (event, timestamp)
        } else {
            let PendingMessage {
                from,
                to,
                msg,
                arriving,
            } = self.msg_source.pop_next_message().unwrap();
            let event = SimulationEvent::NetworkMessage { from, to, msg };
            (event, arriving)
        };

        self.clock.advance(timestamp);
        Some(event)
    }
}

// wrapper struct which holds a SimulationEvent,
// but is ordered by a timestamp (in reverse)
#[derive(Clone, Debug)]
struct FutureEvent(Timestamp, SimulationEvent);
impl FutureEvent {
    fn key(&self) -> Reverse<Timestamp> {
        Reverse(self.0)
    }
}

impl PartialEq for FutureEvent {
    fn eq(&self, other: &Self) -> bool {
        self.key() == other.key()
    }
}
impl Eq for FutureEvent {}
impl PartialOrd for FutureEvent {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}
impl Ord for FutureEvent {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        self.key().cmp(&other.key())
    }
}
