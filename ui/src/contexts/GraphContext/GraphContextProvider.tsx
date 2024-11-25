"use client";
import { FC, PropsWithChildren, useMemo, useReducer, useRef } from "react";

import { IGraphWrapperProps } from "@/components/Graph/GraphWapper";
import { ITransformedNodeMap } from "@/components/Graph/types";
import { defaultState, GraphContext } from "./context";
import { reducer } from "./reducer";

export const GraphContextProvider: FC<
  PropsWithChildren<IGraphWrapperProps>
> = ({ children, topography, maxTime }) => {
  const defaultSyncedState = useMemo(() => {
    const transformedTopography: ITransformedNodeMap = {
      nodes: new Map(
        topography.nodes.map((n, i) => [
          i,
          {
            data: n,
            fx: n.location[0],
            fy: n.location[1],
            id: i,
          },
        ]),
      ),
      links: new Map(
        topography.links.map((l) => [
          `${l.nodes[0]}|${l.nodes[1]}`,
          {
            source: l.nodes[0],
            target: l.nodes[1],
          },
        ]),
      ),
    };

    return {
      ...defaultState,
      maxTime,
      topography: transformedTopography,
      topographyLoaded: true,
    };
  }, [])

  const [state, dispatch] = useReducer(reducer, defaultSyncedState);

  const canvasRef = useRef<HTMLCanvasElement>(defaultState.canvasRef.current);
  const intervalId = useRef<Timer | null>(defaultState.intervalId.current);
  const simulationPauseTime = useRef<number>(
    defaultState.simulationPauseTime.current,
  );
  const simulationStartTime = useRef<number>(
    defaultState.simulationStartTime.current,
  );

  const resolvedState = useMemo(
    () => ({
      ...state,
      canvasRef,
      intervalId,
      simulationPauseTime,
      simulationStartTime,
    }),
    [state],
  );

  return (
    <GraphContext.Provider value={{ state: resolvedState, dispatch }}>
      {children}
    </GraphContext.Provider>
  );
};