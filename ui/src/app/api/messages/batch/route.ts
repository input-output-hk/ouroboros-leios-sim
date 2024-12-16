import { createReadStream, createWriteStream, WriteStream } from "fs";
import { NextResponse } from "next/server";
import fs from "fs/promises";

import { IServerMessage } from "@/components/Graph/types";
import { ISimulationAggregatedDataState } from "@/contexts/GraphContext/types";
import { messagesPath } from "../../utils";
import { findStartPosition, processMessage } from "./utils";

// function getTimestampAtPosition(
//   filePath: string,
//   position: number,
// ): Promise<number> {
//   return new Promise((resolve, reject) => {
//     const stream = createReadStream(filePath, { start: position });
//     let foundNewLine = false;
//     let adjustedPosition = position;

//     // Read a few bytes to find the newline character
//     stream.on("data", (chunk) => {
//       const decoded = chunk.toString("utf8");
//       for (let i = 0; i < decoded.length; i++) {
//         if (decoded[i] === "\n") {
//           foundNewLine = true;
//           adjustedPosition += i + 1; // Move to the start of the next line
//           break;
//         }
//       }

//       stream.close(); // Stop reading once the newline is found
//     });

//     stream.on("close", () => {
//       if (foundNewLine) {
//         // Now use readline to get the timestamp from the new line
//         const lineStream = createReadStream(filePath, {
//           start: adjustedPosition,
//         });
//         const rl = readline.createInterface({
//           input: lineStream,
//           crlfDelay: Infinity,
//         });

//         rl.on("line", (line) => {
//           const message: IServerMessage = JSON.parse(line);
//           const timestamp = message.time / 1_000_000;
//           rl.close();
//           resolve(timestamp);
//         });

//         rl.on("error", (err) => {
//           reject(err);
//         });
//       } else {
//         reject(
//           new Error("Could not find a newline character in the provided range"),
//         );
//       }
//     });

//     stream.on("error", (err) => {
//       reject(err);
//     });
//   });
// }

const fileContents = await fs.readFile(messagesPath);

const events: any[] = [];
for await (const line of fileContents.toString().split("\n")) {
  if (!!line) {
    let evt = JSON.parse(line);
    events.push(evt);
  }
}

function findTimestampIdx(timestamp: number): number {
  let _findTimestampIdx = (timestamp: number, lo: number, hi: number) => {
    if (lo > hi) {
      return lo;
    }
    let mid = lo + (hi - lo) / 2;
    let evt = events[mid];
    if (Number(evt.time) / 1_000_000 > timestamp) {
      return _findTimestampIdx(timestamp, lo, mid);
    } else {
      return _findTimestampIdx(timestamp, mid, hi);
    }
  };

  return _findTimestampIdx(timestamp, 0, events.length);
}

export async function POST(req: Request, res: Response) {
  const url = new URL(req.url);
  let start = url.searchParams.get("start");
  let end = url.searchParams.get("end");
  let events: any[] = [];
  let startIdx = findTimestampIdx(Number(start));
  let endIdx = findTimestampIdx(Number(end));
  return NextResponse.json(events.slice(startIdx, endIdx));

  // const stream = new ReadableStream({
  //   cancel() {
  //     rl.close();
  //     fileStream.close();
  //   },

  //   start(controller) {
  //     if (leftover) {
  //       controller.enqueue(`data: ${leftover}\n\n`);
  //       leftover = "";
  //     }
  //   },
  // });

  // return NextResponse.json(events);
}

// export async function GET(req: Request, res: Response) {
//   try {
//     const fileStream = createReadStream(messagesPath, {
//       encoding: "utf8",
//     });
//     const rl = readline.createInterface({
//       input: fileStream,
//       crlfDelay: Infinity,
//     });

//     let interval: Timer | undefined;
//     const eventBuffer: string[] = [];
//     let simulationDone = false;
//     let isProcessing = false;

//     const stream = new ReadableStream({
//       cancel() {
//         clearInterval(interval);
//         rl.close();
//         fileStream.close();
//       },

//       start(controller) {
//         const aggregatedData: ISimulationAggregatedDataState = {
//           progress: 0,
//           nodes: new Map(),
//         };

//         // interval = setInterval(() => {
//         //   if (isProcessing) {
//         //     return;
//         //   }

//         //   isProcessing = true;
//         //   if (eventBuffer.length === 0 && simulationDone) {
//         //     clearInterval(interval);
//         //     rl.close();
//         //     fileStream.close();
//         //     controller.close();
//         //     return;
//         //   }

//         //   try {
//         //     // Process 10k events at a time.
//         //     const batch = eventBuffer.splice(0, 100000);
//         //     for (const line of batch) {
//         //       const data: IServerMessage = JSON.parse(line);
//         //       processMessage(data, aggregatedData);
//         //     }

//         //     const serializedData = {
//         //       ...aggregatedData,
//         //       progress: JSON.parse(batch[batch.length - 1]).time / 1_000_000,
//         //       nodes: Array.from(aggregatedData.nodes.entries()),
//         //     };

//         //     controller.enqueue(`data: ${JSON.stringify(serializedData)}\n\n`);
//         //     isProcessing = false;
//         //   } catch (e) {
//         //     controller.error(e);
//         //     isProcessing = false;
//         //   }

//         // }, 100);

//         rl.on("line", (line: string) => {
//           try {
//             controller.enqueue(`data: ${line}\n\n`);
//             // eventBuffer.push(line);
//           } catch (error) {
//             controller.error(error);
//           }
//         });

//         rl.on("close", () => {
//           simulationDone = true;
//           console.log("closed");
//         });

//         rl.on("error", (error) => {
//           controller.error(error);
//         });
//       },
//     });

//     return new NextResponse(stream, {
//       headers: {
//         "Content-Type": "text/event-stream",
//         "Cache-Control": "no-cache",
//         Connection: "keep-alive",
//       },
//     });
//   } catch (e) {
//     return new NextResponse(null, {
//       status: 500,
//       statusText: (e as Error)?.message,
//     });
//   }
// }
