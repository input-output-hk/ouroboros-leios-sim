"use client";
import * as PIXI from "pixi.js";

import { useGraphContext } from "@/contexts/GraphContext/context";
import { ESpeedOptions } from "@/contexts/GraphContext/types";
import { useCallback } from "react";
import { isWithinRange } from "../utils";
import { useStreamMessagesHandler } from "./queries";

const scale = 4;
let offsetX = 0,
  offsetY = 0;

export const useHandlers = () => {
  const {
    state: {
      canvasRef,
      pixiAppInitialized,
      pixiAppRef,
      linksContainerRef,
      nodesContainerRef,
      transactionsContainerRef,
      currentTime,
      intervalId,
      maxTime,
      playing,
      speed,
      simulationPauseTime,
      simulationStartTime,
      topography,
    },
    dispatch,
  } = useGraphContext();

  const {
    startStream,
    stopStream,
    transactionsRef,
    txReceivedMessagesRef,
    txGeneratedRef,
    txSentMessagesRef,
  } = useStreamMessagesHandler();

  const drawCanvas = useCallback(() => {
    if (!pixiAppInitialized) {
      return;
    }

    // Current time in simulation
    const now = performance.now();
    const elapsed =
      simulationStartTime.current !== 0
        ? (now - simulationStartTime.current) * speed
        : 0;

    if (elapsed >= maxTime) {
      togglePlayPause();
      return;
    } else {
      dispatch({ type: "SET_CURRENT_TIME", payload: elapsed });
    }

    if (elapsed >= maxTime) {
      intervalId.current && clearInterval(intervalId.current);
      dispatch({ type: "SET_PLAYING", payload: false });
      return;
    }

    const canvas = canvasRef.current as HTMLDivElement;
    const app = pixiAppRef.current as PIXI.Application;

    // Set canvas dimensions
    const width = app.canvas?.getBoundingClientRect().width || 1024;
    const height = app.canvas?.getBoundingClientRect().height || 800;
    // canvas.width = width;
    // canvas.height = height;

    // Clear the canvas
    app.renderer.resize(width, height);

    // Clear the containers
    const linksContainer = linksContainerRef.current;
    const nodesContainer = nodesContainerRef.current;
    const transactionsContainer = transactionsContainerRef.current;

    if (
      linksContainer === null ||
      nodesContainer === null ||
      transactionsContainer === null
    ) {
      return;
    }

    linksContainer.removeChildren();
    nodesContainer.removeChildren();
    transactionsContainer.removeChildren();

    // Calculate the bounds
    const coordinates: { xValues: number[]; yValues: number[] } = {
      xValues: [],
      yValues: [],
    };
    for (const [_, { fx, fy }] of topography.nodes) {
      coordinates.xValues.push(fx);
      coordinates.yValues.push(fy);
    }
    const minX = Math.min(...coordinates.xValues);
    const maxX = Math.max(...coordinates.xValues);
    const minY = Math.min(...coordinates.yValues);
    const maxY = Math.max(...coordinates.yValues);

    const pathWidth = maxX - minX;
    const pathHeight = maxY - minY;

    // Compute the canvas center
    const canvasCenterX = width / 2;
    const canvasCenterY = height / 2;

    // Calculate the offset to center the path
    offsetX = canvasCenterX - (minX + pathWidth / 2) * scale;
    offsetY = canvasCenterY - (minY + pathHeight / 2) * scale;

    // Apply translation and scaling
    app.stage.position.set(offsetX, offsetY);
    app.stage.scale.set(scale, scale);

    // Draw the links
    const linksGraphics = new PIXI.Graphics();
    topography.links.forEach((link) => {
      const nodeStart = topography.nodes.get(link.source);
      const nodeEnd = topography.nodes.get(link.target);
      if (!nodeStart || !nodeEnd) {
        return;
      }

      linksGraphics.lineStyle(2, 0xdddddd); // Correct method
      linksGraphics.moveTo(nodeStart.fx, nodeStart.fy);
      linksGraphics.lineTo(nodeEnd.fx, nodeEnd.fy);
    });

    linksContainer.addChild(linksGraphics);

    // Draw the nodes
    const nodesGraphics = new PIXI.Graphics();
    topography.nodes.forEach((node) => {
      const fillColor = node.data.stake ? 0x00ff00 : 0x0000ff; // Green or blue
      let nodeFillColor = fillColor;

      txGeneratedRef.current.forEach((m) => {
        const target = m.time / 1_000_000;
        if (
          m.message.publisher === node.id &&
          isWithinRange(elapsed, target, 50)
        ) {
          nodeFillColor = 0xff0000; // Red
        }

        if (m.message.publisher === node.id && elapsed > target) {
          dispatch({
            type: "ADD_GENERATED_MESSAGE",
            payload: m.time / 1_000_000,
          });
        }
      });

      nodesGraphics.lineStyle(1 / scale, 0x000000);
      nodesGraphics.beginFill(nodeFillColor);
      nodesGraphics.drawCircle(node.fx, node.fy, 1);
      nodesGraphics.endFill();
    });

    nodesContainer.addChild(nodesGraphics);

    // Draw the transactions
    const transactionsGraphics = new PIXI.Graphics();
    transactionsRef.current.forEach((txList, id) => {
      const lastMessage = txList[txList.length - 1];
      if (elapsed > lastMessage.sentTime + lastMessage.duration) {
        transactionsRef.current.delete(id);
        txList.forEach((tx) => {
          txSentMessagesRef.current.delete(tx.sentTime);
          txReceivedMessagesRef.current.delete(tx.sentTime + tx.duration);
        });
      }

      txList.forEach((transaction) => {
        const { duration, source, target, sentTime } = transaction;
        const sourceNode = topography.nodes.get(source);
        const targetNode = topography.nodes.get(target);

        if (!sourceNode || !targetNode) {
          console.log(
            "Could not find source and target nodes for this transaction.",
          );
          return;
        }

        const startX = sourceNode.fx;
        const startY = sourceNode.fy;
        const endX = targetNode.fx;
        const endY = targetNode.fy;
        const transactionElapsedTime = elapsed - sentTime;

        // Skip if the animation hasn't started.
        if (transactionElapsedTime < 0) {
          return;
        }

        // If we're past the animation, log it to our sent transaction store.
        else if (elapsed > sentTime + duration) {
          dispatch({ type: "ADD_SENT_TX", payload: sentTime });
        }

        // Draw the transaction event.
        else {
          // Calculate the interpolation factor
          const t = Math.min(transactionElapsedTime / duration, 1);
          const x = startX + t * (endX - startX);
          const y = startY + t * (endY - startY);

          // Draw the moving circle
          transactionsGraphics.beginFill(0xff0000); // Red color
          transactionsGraphics.drawCircle(x, y, 0.5);
          transactionsGraphics.endFill();
        }
      });
    });

    transactionsContainer.addChild(transactionsGraphics);

    app.render();
  }, [playing, speed, maxTime]);

  // Function to toggle play/pause
  const togglePlayPause = useCallback(() => {
    const now = performance.now();
    if (!playing) {
      startStream(currentTime, speed);
      simulationStartTime.current = now - simulationPauseTime.current;
      simulationPauseTime.current = now;
      intervalId.current = setInterval(drawCanvas, 1000 / 60); // 60 FPS
    } else {
      stopStream();
      simulationPauseTime.current = now - simulationStartTime.current;
      if (intervalId.current) {
        clearInterval(intervalId.current);
        intervalId.current = null;
      }
    }

    dispatch({ type: "TOGGLE_PLAYING" });
  }, [drawCanvas, currentTime, speed]);

  const handleResetSim = useCallback(() => {
    dispatch({
      type: "BATCH_UPDATE",
      payload: {
        currentTime: 0,
        playing: false,
        sentTxs: new Set(),
        speed: ESpeedOptions["1/300"],
        generatedMessages: new Set(),
      },
    });

    simulationStartTime.current = 0;
    simulationPauseTime.current = 0;
    transactionsRef.current = new Map();
    txReceivedMessagesRef.current = new Map();
    txGeneratedRef.current = new Map();
    txSentMessagesRef.current = new Map();

    if (intervalId.current) {
      clearInterval(intervalId.current);
      intervalId.current = null;
    }

    drawCanvas();
  }, []);

  return {
    handleResetSim,
    drawCanvas,
    togglePlayPause,
  };
};
