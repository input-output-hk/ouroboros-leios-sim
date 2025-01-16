// This file contains TypeScript types for the configuration file formats.

/** A configuration for a Leios simulation. */
export interface Config {
  // Leios Protocol Configuration
  "leios-stage-length-slots": bigint;
  "leios-stage-active-voting-slots": bigint;

  // Transaction Configuration
  "tx-generation-distribution": Distribution;
  "tx-size-bytes-distribution": Distribution;
  "tx-validation-cpu-time-ms": number;
  "tx-max-size-bytes": bigint;

  // Ranking Block Configuration
  "rb-generation-probability": number;
  "rb-generation-cpu-time-ms": number;
  "rb-head-validation-cpu-time-ms": number;
  "rb-head-size-bytes": bigint;
  "rb-body-max-size-bytes": bigint;
  "rb-body-legacy-praos-payload-validation-cpu-time-ms-constant": number;
  "rb-body-legacy-praos-payload-validation-cpu-time-ms-per-byte": number;
  "rb-body-legacy-praos-payload-avg-size-bytes": bigint;

  // Input Block Configuration
  "ib-generation-probability": number;
  "ib-generation-cpu-time-ms": number;
  "ib-head-size-bytes": bigint;
  "ib-head-validation-cpu-time-ms": number;
  "ib-body-validation-cpu-time-ms-constant": number;
  "ib-body-validation-cpu-time-ms-per-byte": number;
  "ib-body-max-size-bytes": bigint;
  "ib-body-avg-size-bytes": bigint;

  // Endorsement Block Configuration
  "eb-generation-probability": number;
  "eb-generation-cpu-time-ms": number;
  "eb-validation-cpu-time-ms": number;
  "eb-size-bytes-constant": bigint;
  "eb-size-bytes-per-ib": bigint;

  // Vote Configuration
  "vote-generation-probability": number;
  "vote-generation-cpu-time-ms-constant": number;
  "vote-generation-cpu-time-ms-per-ib": number;
  "vote-validation-cpu-time-ms": number;
  "vote-threshold": bigint;
  "vote-one-eb-per-vrf-win": boolean;
  "vote-size-bytes-constant": bigint;
  "vote-size-bytes-per-node": bigint;

  // Certificate Configuration
  "cert-generation-cpu-time-ms-constant": number;
  "cert-generation-cpu-time-ms-per-node": number;
  "cert-validation-cpu-time-ms-constant": number;
  "cert-validation-cpu-time-ms-per-node": number;
  "cert-size-bytes-constant": bigint;
  "cert-size-bytes-per-node": bigint;
}

export type Distribution =
  | NormalDistribution
  | ExpDistribution
  | LogNormalDistribution;

export interface NormalDistribution {
  distribution: "normal";
  mean: number;
  std_dev: number;
}

export interface ExpDistribution {
  distribution: "exp";
  lambda: number;
  scale?: number;
}

export interface LogNormalDistribution {
  distribution: "log-normal";
  mu: number;
  sigma: number;
}

/** A topology for a Leios simulation. */
export interface Topology<lk extends LocationKind> {
  nodes: Partial<Record<NodeName, Node<lk>>>;
}

/** A node. */
export interface Node<lk extends LocationKind> {
  stake?: bigint;
  location: Location<lk>;
  producers: Partial<Record<NodeName, LinkInfo>>;
}

/** Link information. */
export interface LinkInfo {
  "latency-ms": number;
  "bandwidth-bytes-per-second": bigint;
  "cpu-core-count": bigint;
}

/** Node location. */
export type Location<lk extends LocationKind> = lk extends "CLUSTER"
  ? { cluster: string }
  : lk extends "COORD2D"
  ? [number, number]
  : undefined;

export type LocationKind = "CLUSTER" | "COORD2D";

export type NodeName = string;