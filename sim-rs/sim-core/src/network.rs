use std::{
    collections::{BinaryHeap, HashMap},
    fmt::Debug,
    sync::{Arc, RwLock},
    time::Duration,
};

use anyhow::Result;

use crate::{
    clock::{Clock, Timestamp},
    config::NodeId,
};

type MessageQueue<T> = BinaryHeap<PendingMessage<T>>;

struct NetworkCore<T> {
    pending: MessageQueue<T>,
    latencies: HashMap<(NodeId, NodeId), Duration>,
    clock: Clock,
}
impl<T> NetworkCore<T> {
    fn send(&mut self, from: NodeId, to: NodeId, msg: T) {
        let latency = self.latencies.get(&(from, to)).unwrap();
        let arriving = self.clock.now() + *latency;
        let pending = PendingMessage {
            from,
            to,
            msg,
            arriving,
        };
        self.pending.push(pending);
    }
    fn connect(&mut self, from: NodeId, to: NodeId, latency: Duration) {
        self.latencies.insert((from, to), latency);
        self.latencies.insert((to, from), latency);
    }
}

pub struct Network<T> {
    core: Arc<RwLock<NetworkCore<T>>>,
}

impl<T> Network<T> {
    pub fn new(clock: Clock) -> Self {
        let core = Arc::new(RwLock::new(NetworkCore {
            pending: BinaryHeap::new(),
            latencies: HashMap::new(),
            clock,
        }));
        Self { core }
    }

    pub fn open(&mut self, node_id: NodeId) -> Result<NetworkSink<T>> {
        Ok(NetworkSink {
            from: node_id,
            core: self.core.clone(),
        })
    }

    pub fn msg_source(&self) -> NetworkSource<T> {
        NetworkSource {
            core: self.core.clone(),
        }
    }

    pub fn connect(&mut self, from: NodeId, to: NodeId, latency: Duration) {
        let mut core = self.core.write().unwrap();
        core.connect(from, to, latency);
    }
}

pub struct NetworkSource<T> {
    core: Arc<RwLock<NetworkCore<T>>>,
}
impl<T> NetworkSource<T> {
    pub fn peek_next_message_at(&self) -> Option<Timestamp> {
        let core = self.core.read().unwrap();
        core.pending.peek().map(|m| m.arriving)
    }
    pub fn pop_next_message(&self) -> Option<PendingMessage<T>> {
        let mut core = self.core.write().unwrap();
        core.pending.pop()
    }
}

pub struct NetworkSink<T> {
    from: NodeId,
    core: Arc<RwLock<NetworkCore<T>>>,
}
impl<T> NetworkSink<T> {
    pub fn send_to(&self, to: NodeId, msg: T) -> Result<()> {
        let mut lock = self.core.write().unwrap();
        lock.send(self.from, to, msg);
        Ok(())
    }
}

// ordered by timestamp descending, so that BinaryHeap returns the earliest first
#[derive(Debug)]
pub struct PendingMessage<T> {
    pub from: NodeId,
    pub to: NodeId,
    pub msg: T,
    pub arriving: Timestamp,
}

impl<T> PartialEq for PendingMessage<T> {
    fn eq(&self, other: &Self) -> bool {
        self.arriving.eq(&other.arriving)
    }
}

impl<T> Eq for PendingMessage<T> {}

impl<T> PartialOrd for PendingMessage<T> {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        Some(self.cmp(other))
    }
}

impl<T> Ord for PendingMessage<T> {
    fn cmp(&self, other: &Self) -> std::cmp::Ordering {
        other.arriving.cmp(&self.arriving)
    }
}
