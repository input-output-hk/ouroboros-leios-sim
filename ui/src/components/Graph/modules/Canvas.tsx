"use client";
import { FC, useEffect } from "react";

import { useGraphContext } from "@/contexts/GraphContext/context";
import { useHandlers } from "../hooks/useHandlers";

export const Canvas: FC = () => {
  const { state: { sentTxs, generatedMessages, transactionCanvasRef, topographyCanvasRef } } = useGraphContext();
  const { drawCanvas, drawTransactions } = useHandlers();

  useEffect(() => {
    drawCanvas();
    drawTransactions();
  }, [])

  return (
    <div className="h-[80vh] border-2 border-gray-200 rounded mb-8 w-2/3 relative">
      <div className="flex items-center justify-center gap-4 mt-4">
        <div>
          <h4>Transactions Generated: {generatedMessages.length}</h4>
        </div>
        <div>
          <h4>Propogations: {sentTxs.length}</h4>
        </div>
      </div>
      <canvas className="absolute top-0 left-0" ref={topographyCanvasRef} />
      <canvas className="absolute top-0 left-0" ref={transactionCanvasRef} />
    </div>
  )
}
