import { IEventData } from "./types";
console.log("Inside worker:", self.constructor.name);

export interface IParseActiveTxsEventData {
  transactions: Map<number, IEventData[]>;
  elapsed: number;
}

const parseActiveTransactions = ({
  transactions,
  elapsed,
}: IParseActiveTxsEventData): Map<number, IEventData[]> => {
  const activeTransactions = new Map<number, IEventData[]>();

  transactions.forEach((txList, id) => {
    const lastItem = txList[txList.length - 1];
    const endTime = lastItem.sentTime + lastItem.duration;
    if (endTime < elapsed) {
      return;
    }

    activeTransactions.set(id, []);

    txList.forEach((transaction) => {
      const { duration, sentTime } = transaction;
      const transactionElapsedTime = elapsed - sentTime;

      // Animation hasn't started.
      if (transactionElapsedTime < 0) {
        return;
      }

      // Animation done.
      if (transactionElapsedTime > sentTime + duration + 200) {
        return;
      }

      // Animation active.
      activeTransactions.get(id)?.push(transaction);
    });
  });

  return activeTransactions;
};

addEventListener("message", (e: MessageEvent<IParseActiveTxsEventData>) => {
  postMessage(parseActiveTransactions(e.data));
});
