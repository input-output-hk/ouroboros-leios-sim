import { useCallback, useRef } from "react";

import { useGraphContext } from "@/contexts/GraphContext/context";
import {
  EMessageType,
  IInputBlockGenerated,
  IInputBlockSent,
  IServerMessage,
  ITransactionGenerated,
  IEventData,
  ITransactionReceived,
  ITransactionSent,
} from "../types";

export const useStreamMessagesHandler = () => {
  const {
    state: {
      eventsByIdRef: transactionsByIdRef,
      txGeneratedMessagesById,
      txReceivedMessagesById,
      txSentMessagesById,
      ibGeneratedMessagesById,
      ibReceivedMessagesById,
      ibSentMessagesById
    },
  } = useGraphContext();
  const eventSource = useRef<EventSource>();

  const startStream = useCallback((startTime: number, range: number) => {
    const url = new URL("/api/messages/batch", window.location.href);
    url.searchParams.set("startTime", startTime.toString());
    url.searchParams.set("speed", range.toString());

    eventSource.current = new EventSource(url);
    eventSource.current.onmessage = function (message) {
      const json: IServerMessage = JSON.parse(message.data);
      processMessage(json);
    };
  }, []);

  const stopStream = useCallback(() => {
    eventSource.current?.close();
  }, []);

  // Function to process each message and update transactions
  const processMessage = useCallback((json: IServerMessage) => {
    const { message } = json;

    switch (message.type) {
      /**
       * Transaction Generated.
       */
      case EMessageType.TransactionGenerated: {
        txGeneratedMessagesById.current.set(
          message.id,
          json as IServerMessage<ITransactionGenerated>,
        );
        break;
      }
      /**
       * Transaction Sent.
       */
      case EMessageType.TransactionSent: {
        let sentMessages = txSentMessagesById.current.get(message.id) || [];
        sentMessages.push(json as IServerMessage<ITransactionSent>);
        txSentMessagesById.current.set(message.id, sentMessages);

        // Generation will always come first.
        const generatedMsg = txGeneratedMessagesById.current.get(
          message.id,
        ) as IServerMessage<ITransactionGenerated>;
        const receivedMessages =
          txReceivedMessagesById.current.get(message.id) || [];

        for (const receivedMsg of receivedMessages) {
          if (
            receivedMsg.message.sender === message.sender &&
            receivedMsg.message.recipient === message.recipient
          ) {
            const transaction: IEventData = {
              id: message.id,
              duration:
                Math.floor(receivedMsg.time / 1_000_000) -
                Math.floor(json.time / 1_000_000),
              source: message.sender,
              target: message.recipient,
              sentTime: Math.floor(json.time / 1_000_000),
              generated: Math.floor(generatedMsg?.time || 0 / 1_000_000),
            };

            let transactionList =
              transactionsByIdRef.current.get(message.id) || [];
            transactionList.push(transaction);
            transactionsByIdRef.current.set(message.id, transactionList);
          }
        }
        break;
      }
      /**
       * Transaction Received.
       */
      case EMessageType.TransactionReceived: {
        let receivedMessages =
          txReceivedMessagesById.current.get(message.id) || [];
        receivedMessages.push(json as IServerMessage<ITransactionReceived>);
        txReceivedMessagesById.current.set(message.id, receivedMessages);

        const generatedMsg = txGeneratedMessagesById.current.get(
          message.id,
        ) as IServerMessage<ITransactionGenerated>;
        const sentMessages = txSentMessagesById.current.get(message.id) || [];

        for (const sentMsg of sentMessages) {
          if (
            sentMsg.message.sender === message.sender &&
            sentMsg.message.recipient === message.recipient
          ) {
            const transaction: IEventData = {
              id: message.id,
              duration:
                Math.floor(json.time / 1_000_000) -
                Math.floor(sentMsg.time / 1_000_000),
              source: message.sender,
              target: message.recipient,
              sentTime: Math.floor(sentMsg.time / 1_000_000),
              generated: Math.floor(generatedMsg?.time || 0 / 1_000_000),
            };

            let transactionList =
              transactionsByIdRef.current.get(message.id) || [];
            transactionList.push(transaction);
            transactionsByIdRef.current.set(message.id, transactionList);
          }
        }
        break;
      }
      /**
       * Input Block Generated.
       */
      case EMessageType.InputBlockGenerated: {
        ibGeneratedMessagesById.current.set(
          `${message.slot}-${message.producer}-${message.index}`,
          json as IServerMessage<IInputBlockGenerated>,
        );
        break;
      }
      /**
       * Input Block Sent.
       */
      case EMessageType.InputBlockSent: {
        let sentMessages = ibSentMessagesById.current.get(message.id) || [];
        sentMessages.push(json as IServerMessage<IInputBlockSent>);
        ibSentMessagesById.current.set(message.id, sentMessages);

        // Generation will always come first.
        const generatedMsg = ibGeneratedMessagesById.current.get(
          message.id,
        ) as IServerMessage<IInputBlockGenerated>;
        const receivedMessages =
          ibReceivedMessagesById.current.get(message.id) || [];

        for (const receivedMsg of receivedMessages) {
          if (
            receivedMsg.message.sender === message.sender &&
            receivedMsg.message.recipient === message.recipient
          ) {
            const transaction: IEventData = {
              id: message.id,
              duration:
                Math.floor(receivedMsg.time / 1_000_000) -
                Math.floor(json.time / 1_000_000),
              source: message.sender,
              target: message.recipient,
              sentTime: Math.floor(json.time / 1_000_000),
              generated: Math.floor(generatedMsg?.time || 0 / 1_000_000),
            };

            let transactionList =
              transactionsByIdRef.current.get(message.id) || [];
            transactionList.push(transaction);
            transactionsByIdRef.current.set(message.id, transactionList);
          }
        }
    }
  }, []);

  return {
    startStream,
    stopStream,
  };
};
