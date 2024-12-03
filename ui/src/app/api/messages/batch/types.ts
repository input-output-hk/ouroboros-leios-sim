import {
    IEventData,
    IServerMessage,
    ITransactionGenerated,
    ITransactionReceived,
    ITransactionSent,
} from "@/components/Graph/types";

export interface ITransactionsData {
  byId: Map<number, IEventData[]>;
  generated: Map<number, IServerMessage<ITransactionGenerated>>;
  sent: Map<number, IServerMessage<ITransactionSent>[]>;
  received: Map<number, IServerMessage<ITransactionReceived>[]>;
}
