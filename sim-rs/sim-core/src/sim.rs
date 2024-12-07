use std::{sync::Arc, time::Duration};

use anyhow::{Context, Result};
use netsim_async::{Bandwidth, EdgePolicy, HasBytesSize, Latency};
use node::Node;
use rand::RngCore;
use rand_chacha::{rand_core::SeedableRng, ChaChaRng};
use tokio::{select, task::JoinSet};
use tokio_util::sync::CancellationToken;

use crate::{
    clock::Clock,
    config::SimConfiguration,
    events::EventTracker,
    model::{
        Block, EndorserBlock, EndorserBlockId, InputBlock, InputBlockHeader, InputBlockId,
        Transaction, TransactionId, VoteBundle, VoteBundleId,
    },
    network::Network,
};

mod node;

pub struct Simulation {
    network: Network<SimulationMessage>,
    nodes: Vec<Node>,
}

impl Simulation {
    pub fn new(config: SimConfiguration, tracker: EventTracker, clock: Clock) -> Result<Self> {
        let config = Arc::new(config);
        let total_stake = config.nodes.iter().map(|p| p.stake).sum();

        let mut network = Network::new(clock.clone());

        let mut rng = ChaChaRng::seed_from_u64(config.seed);
        let mut nodes = vec![];
        for link_config in config.links.iter() {
            network.set_edge_policy(
                link_config.nodes.0,
                link_config.nodes.1,
                EdgePolicy {
                    latency: Latency::new(link_config.latency),
                    bandwidth_down: Bandwidth::bits_per(u64::MAX, Duration::from_millis(1)),
                    bandwidth_up: Bandwidth::bits_per(u64::MAX, Duration::from_millis(1)),
                    ..EdgePolicy::default()
                },
            )?;
        }
        for node_config in &config.nodes {
            let id = node_config.id;
            let (msg_sink, msg_source) = network.open(id).context("could not open socket")?;
            let node = Node::new(
                node_config,
                config.clone(),
                total_stake,
                msg_source,
                msg_sink,
                tracker.clone(),
                ChaChaRng::seed_from_u64(rng.next_u64()),
                clock.clone(),
            );
            nodes.push(node);
        }

        Ok(Self { network, nodes })
    }

    // Run the simulation indefinitely.
    pub async fn run(&mut self, token: CancellationToken) -> Result<()> {
        let mut set = JoinSet::new();

        for node in self.nodes.drain(..) {
            set.spawn(node.run());
        }

        select! {
            biased;
            _ = token.cancelled() => {}
            result = set.join_next() => {
                result.unwrap()??;
            }
        };

        Ok(())
    }

    pub fn shutdown(self) -> Result<()> {
        self.network.shutdown()
    }
}

#[derive(Clone, Debug)]
enum SimulationMessage {
    // tx "propagation"
    AnnounceTx(TransactionId),
    RequestTx(TransactionId),
    Tx(Arc<Transaction>),
    // praos block propagation
    RollForward(u64),
    RequestBlock(u64),
    Block(Arc<Block>),
    // IB header propagation
    AnnounceIBHeader(InputBlockId),
    RequestIBHeader(InputBlockId),
    IBHeader(InputBlockHeader, bool /* has_body */),
    // IB transmission
    AnnounceIB(InputBlockId),
    RequestIB(InputBlockId),
    IB(Arc<InputBlock>),
    // EB propagation
    AnnounceEB(EndorserBlockId),
    RequestEB(EndorserBlockId),
    EB(Arc<EndorserBlock>),
    // Get out the vote
    AnnounceVotes(VoteBundleId),
    RequestVotes(VoteBundleId),
    Votes(Arc<VoteBundle>),
}

impl HasBytesSize for SimulationMessage {
    fn bytes_size(&self) -> u64 {
        match self {
            Self::AnnounceTx(_) => 8,
            Self::RequestTx(_) => 8,
            Self::Tx(tx) => tx.bytes,

            Self::RollForward(_) => 8,
            Self::RequestBlock(_) => 8,
            Self::Block(block) => block.transactions.iter().map(|t| t.bytes).sum(),

            Self::AnnounceIBHeader(_) => 8,
            Self::RequestIBHeader(_) => 8,
            Self::IBHeader(_, _) => 32,

            Self::AnnounceIB(_) => 8,
            Self::RequestIB(_) => 8,
            Self::IB(ib) => ib.bytes(),

            Self::AnnounceEB(_) => 8,
            Self::RequestEB(_) => 8,
            Self::EB(_) => 32,

            Self::AnnounceVotes(_) => 8,
            Self::RequestVotes(_) => 8,
            Self::Votes(v) => 8 * v.ebs.len() as u64,
        }
    }
}
