import { FC, PropsWithChildren, useEffect, useMemo, useRef, useState } from "react";

import { useSetSimulationTimeHandler, useSetTopographyHandler, useStreamMessagesHandler } from "@/components/Graph/hooks/queries";
import { IServerMessage, ITransactionMessage, ITransformedNodeMap } from "@/components/Graph/types";
import { defaultState, GraphContext } from "./context";
import { IGraphContextState } from "./types";

export const GraphContextProvider: FC<PropsWithChildren> = ({ children }) => {
  const canvasRef = useRef<HTMLCanvasElement>(defaultState.canvasRef.current)
  const [currentTime, setCurrentTime] = useState(defaultState.currentTime);
  const [generatedMessages, setGeneratedMessages] = useState<Set<number>>(defaultState.generatedMessages);
  const intervalId = useRef<Timer | null>(defaultState.intervalId.current);
  const [maxTime, setMaxTime] = useState(defaultState.maxTime);
  const [messages, setMessages] = useState<IServerMessage[]>(defaultState.messages);
  const [playing, setPlaying] = useState(defaultState.playing);
  const [sentTxs, setSentTxs] = useState<Set<string>>(defaultState.sentTxs);
  const simulationPauseTime = useRef<number>(defaultState.simulationPauseTime.current);
  const simulationStartTime = useRef<number>(defaultState.simulationStartTime.current);
  const [speed, setSpeed] = useState(defaultState.speed);
  const [topography, setTopography] = useState<ITransformedNodeMap>(defaultState.topography);
  const [topographyLoaded, setTopographyLoaded] = useState<boolean>(defaultState.topographyLoaded);
  const [transactions, setTransactions] = useState<Map<number, ITransactionMessage[]>>(defaultState.transactions);

  const state: IGraphContextState = useMemo(() => {
    return {
      canvasRef,
      currentTime,
      generatedMessages,
      intervalId,
      maxTime,
      messages,
      playing,
      sentTxs,
      simulationPauseTime,
      simulationStartTime,
      speed,
      topography,
      topographyLoaded,
      transactions,
      setCurrentTime,
      setGeneratedMessages,
      setPlaying,
      setSentTxs,
      setSpeed,
      setMaxTime,
      setMessages,
      setTopography,
      setTopographyLoaded,
      setTransactions
    }
  }, [topography, speed, sentTxs, generatedMessages, messages, maxTime, playing, transactions, currentTime])

  const topographyHandler = useSetTopographyHandler(state);
  const messagesHandler = useStreamMessagesHandler(state)
  const simulationTimeHandler = useSetSimulationTimeHandler(state);
  
  useEffect(() => {
    topographyHandler();
    simulationTimeHandler();
    messagesHandler();
  }, [])

  const lastRequestTimeRef = useRef<number>(0);

  // Update to update on currentTime change
  useEffect(() => {
    if (currentTime === 0) {
      messagesHandler();
      return;
    }

    if (!playing) {
      return;
    }

    if (currentTime - lastRequestTimeRef.current < 100) {
      return;
    }

    messagesHandler();
    lastRequestTimeRef.current = currentTime;
  }, [currentTime])

  return (
    <GraphContext.Provider value={state}>
      {children}
    </GraphContext.Provider>
  )
}
