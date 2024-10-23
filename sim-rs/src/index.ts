import init, { begin_sim } from '../sim-web/pkg'

const TEST_DATA = `
seed = 0x3c3373756e646165
nodes = [
    # producers
    { location = [-80,  40], stake = 1000000 },
    { location = [  0,  10], stake = 1000000 },
    { location = [ 70,  40], stake = 1000000 },
    { location = [ 80, 100], stake = 1000000 },
    { location = [-30, 170], stake = 10000000 },
    # relays
    { location = [-60,  60] },
    { location = [  0,  30] },
    { location = [ 40,  60] },
    { location = [ 50,  70] },
    { location = [ 55,  90] },
    { location = [ 30, 140] },
    { location = [-30, 140] },
    { location = [-70, 100] },
]
links = [
    { nodes = [0, 5] },
    { nodes = [1, 6] },
    { nodes = [2, 7] },
    { nodes = [2, 8] },
    { nodes = [3, 9] },
    { nodes = [4, 11] },
    { nodes = [5, 6] },
    { nodes = [5, 11] },
    { nodes = [5, 12] },
    { nodes = [6, 8] },
    { nodes = [6, 9] },
    { nodes = [6, 10] },
    { nodes = [6, 12] },
    { nodes = [7, 9] },
    { nodes = [7, 11] },
    { nodes = [8, 9] },
    { nodes = [8, 12] },
    { nodes = [9, 10] },
    { nodes = [9, 11] },
    { nodes = [10, 11] },
    { nodes = [11, 12] },
]

# transaction parameters
transaction_frequency_ms = { distribution = "exp", lambda = 0.85, scale = 1000 }
transaction_size_bytes = { distribution = "log_normal", mu = 6.833, sigma = 1.127 }
max_tx_size = 16384

# praos block generation parameters
block_generation_probability = 0.05
max_block_size = 90112

# IB parameters
ib_generation_probability = 0.5 # corresponds to 𝑓I in the model
max_ib_size = 327680
max_ib_requests_per_peer = 1
`;

console.log("Hello world!");
await init();
const result = await begin_sim(TEST_DATA);
console.log({ result });