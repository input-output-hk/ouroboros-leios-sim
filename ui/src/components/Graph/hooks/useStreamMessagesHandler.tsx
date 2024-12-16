import { useCallback, useMemo, useRef, useState } from "react";

import { useGraphContext } from "@/contexts/GraphContext/context";
import { ISimulationAggregatedDataState } from "@/contexts/GraphContext/types";

export const useStreamMessagesHandler = (
  callback: (json: ISimulationAggregatedDataState) => void,
) => {
  const { dispatch } = useGraphContext();
  const eventSource = useRef<EventSource>();
  const [streaming, setStreaming] = useState(false);

  // const currentTimestamp = 0, currentTimeScale = 0.5;
  // onAnimationFrame(() => {
  //   let elapsed = now() - currentTimestamp;
  //   let advance = elapsed * currentTimeScale;
  //   let newTimestamp = currentTimestamp + advance;
  //   let events = requestEvents(currentTimestamp, newTimestamp, filter);
  //   for(event in events) {
  //     for(stat in stats) {
  //       stat.update(event);
  //     }
  //     for(viz in visualizations) {
  //       viz.enqueue(event);
  //     }
  //   }
  //   requestAnimationFrame();
  // })

  const startStream = useCallback(() => {
    setStreaming(true);

    const url = new URL("/api/messages/batch", window.location.href);
    eventSource.current = new EventSource(url);
    eventSource.current.onerror = function (error) {
      stopStream();
    };

    let timestamp = Date.now();
    let msgCount = 0;
    eventSource.current.onmessage = function (message) {
      msgCount += 1;
      if (msgCount % 10000 == 0) {
        let now = Date.now();
        console.log(`10k messages took ${now - timestamp}ms`);
        timestamp = now;
      }

      const json: ISimulationAggregatedDataState = JSON.parse(message.data);
      callback(json);
      // const json: ISimulationAggregatedDataState = JSON.parse(
      //   message.data,
      //   (key: string, v: any) => {
      //     if (key === "nodes") {
      //       return new Map(v);
      //     }

      //     return v;
      //   },
      // );

      // dispatch({ type: "SET_AGGREGATED_DATA", payload: json });
    };
  }, []);

  const stopStream = useCallback(() => {
    eventSource.current?.close();
    setStreaming(false);
  }, []);

  return useMemo(
    () => ({
      startStream,
      stopStream,
      streaming,
    }),
    [startStream, stopStream, streaming],
  );
};
