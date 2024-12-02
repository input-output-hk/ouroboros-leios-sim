import { useCallback, useEffect, useRef } from "react";
import { ITransactionMessage } from "../types";
import { IParseActiveTxsEventData } from "../worker";

export const useGraphWorker = () => {
  const workerRef = useRef<Worker>();

  useEffect(() => {
    workerRef.current = new Worker(new URL("../worker.ts", import.meta.url));
    return () => {
      workerRef.current?.terminate();
    };
  }, []);

  const getActiveTransactions = useCallback(
    async (
      data: IParseActiveTxsEventData,
    ): Promise<Map<number, ITransactionMessage[]>> => {
      workerRef.current?.postMessage(data);
      return new Promise((res, rej) => {
        if (!workerRef.current) {
          rej("No worker has been set yet.");
        } else {
          workerRef.current.onmessage = (event: MessageEvent<Map<number, ITransactionMessage[]>>) =>
            res(event.data);
        }
      });
    },
    [],
  );

  return {
    getActiveTransactions,
  };
};
