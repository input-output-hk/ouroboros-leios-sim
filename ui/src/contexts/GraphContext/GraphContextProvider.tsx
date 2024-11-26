"use client";
import * as PIXI from "pixi.js";
import { FC, PropsWithChildren, useEffect, useMemo, useReducer, useRef } from "react";

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

  const canvasRef = useRef<HTMLDivElement | null>(null);
  const pixiAppRef = useRef<PIXI.Application<PIXI.Renderer> | null>(null);
  const linksContainerRef = useRef<PIXI.Container | null>(null);
  const nodesContainerRef = useRef<PIXI.Container | null>(null);
  const transactionsContainerRef = useRef<PIXI.Container | null>(null);
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
      pixiAppRef,
      linksContainerRef,
      nodesContainerRef,
      transactionsContainerRef,
      intervalId,
      simulationPauseTime,
      simulationStartTime,
    }),
    [state],
  );

  useEffect(() => {
    let app: PIXI.Application;
    // Initialize PixiJS Application
    const initializeApp = async () => {
      if (!canvasRef.current) {
        return;
      }

      const width = canvasRef.current?.getBoundingClientRect().width || 1024;
      const height = canvasRef.current?.getBoundingClientRect().height || 800;

      app = new PIXI.Application();
      await app.init({
        width,
        height,
        backgroundAlpha: 0,
        antialias: true,
        autoStart: false,
        sharedTicker: false
      });
      pixiAppRef.current = app;
  
      // Create containers for links, nodes, and transactions
      const linksContainer = new PIXI.Container();
      const nodesContainer = new PIXI.Container();
      const transactionsContainer = new PIXI.Container();
  
      app.stage.addChild(linksContainer);
      app.stage.addChild(nodesContainer);
      app.stage.addChild(transactionsContainer);
  
      linksContainerRef.current = linksContainer;
      nodesContainerRef.current = nodesContainer;
      transactionsContainerRef.current = transactionsContainer;

      canvasRef.current.appendChild(app.canvas);

      dispatch({ type: "SET_APP_INITIALIZED", payload: true });
    }

    initializeApp();

    return () => {
      if (app) {
        app.destroy(true, true);
      }
    }
  }, [])

  return (
    <GraphContext.Provider value={{ state: resolvedState, dispatch }}>
      {children}
    </GraphContext.Provider>
  );
};
