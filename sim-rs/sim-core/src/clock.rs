use std::{
    ops::{Add, Sub},
    sync::{Arc, RwLock},
    time::{Duration, Instant},
};

use serde::Serialize;
use tokio::time::{self, Sleep};

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
pub struct Timestamp(Duration);
impl Add<Duration> for Timestamp {
    type Output = Timestamp;

    fn add(self, rhs: Duration) -> Self::Output {
        Timestamp(self.0 + rhs)
    }
}
impl Sub<Timestamp> for Timestamp {
    type Output = Duration;

    fn sub(self, rhs: Timestamp) -> Self::Output {
        self.0 - rhs.0
    }
}

impl Serialize for Timestamp {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_u128(self.0.as_nanos())
    }
}

#[derive(Clone)]
pub struct Clock {
    start: Instant,
    scale: u32,
    now: Arc<RwLock<Duration>>,
}

impl Clock {
    pub fn new(start: Instant, scale: u32) -> Self {
        Self {
            start,
            scale,
            now: Arc::new(RwLock::new(Duration::ZERO)),
        }
    }

    pub fn now(&self) -> Timestamp {
        let now = self.now.read().unwrap();
        Timestamp(*now)
    }

    pub fn wait_until(&self, timestamp: Timestamp) -> Sleep {
        let instant = self.start + (timestamp.0 / self.scale);
        time::sleep_until(instant.into())
    }

    pub fn advance(&self, now: Timestamp) {
        *self.now.write().unwrap() = now.0;
    }
}
