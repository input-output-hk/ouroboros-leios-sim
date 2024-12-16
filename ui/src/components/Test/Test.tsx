"use client";

import { FC, useCallback, useEffect, useRef, useState } from "react";

export interface ITestProps {}

export const TestComponent: FC<ITestProps> = ({}) => {
  const realTimestamp = useRef(Date.now());
  const virtualTimestamp = useRef(0);

  const animation = useCallback(() => {
    console.log(
      `Animation frame: ${realTimestamp.current}, ${virtualTimestamp.current}`,
    );
    const now = Date.now();
    const elapsed = now - realTimestamp.current;
    console.log(`Elapsed: ${elapsed}`);
    virtualTimestamp.current = virtualTimestamp.current + elapsed;
    console.log(`VT: ${virtualTimestamp.current}`);
    realTimestamp.current = now;

    fetch(`/api/messages/batch?timestamp=${virtualTimestamp.current}`, {
      method: "POST",
    }).then(async (res) => {
      try {
        const events = await res.json();
        console.log(events.length);
        requestAnimationFrame(animation);
      } catch (e) {
        console.log(e);
      }
    });
  }, []);

  useEffect(() => {
    requestAnimationFrame(animation);
  }, []);

  return <p>test</p>;
};
