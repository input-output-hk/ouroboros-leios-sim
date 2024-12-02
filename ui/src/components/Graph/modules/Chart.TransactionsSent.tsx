"use client";

import {
  CategoryScale,
  Chart as ChartJS,
  LinearScale,
  LineElement,
  PointElement,
  TimeScale,
} from "chart.js";
import { FC, useMemo } from "react";
import { Line } from "react-chartjs-2";

import "chartjs-adapter-moment";

import { useGraphContext } from "@/contexts/GraphContext/context";
import { useThrottle } from "@uidotdev/usehooks";

// const CustomTooltip = ({
//   active,
//   payload,
// }: TooltipProps<ValueType, NameType>) => {
//   if (active && payload && payload.length) {
//     return (
//       <div className="custom-tooltip">
//         <p className="label">{`Message: #${payload[0].payload.message}`}</p>
//         <p className="intro">{`Time Sent: ${payload[0].payload.time}ms`}</p>
//       </div>
//     );
//   }

//   return null;
// };

// Register Chart.js components
ChartJS.register(
  LineElement,
  CategoryScale,
  LinearScale,
  TimeScale,
  PointElement,
);

export const ChartTransactionsSent: FC = () => {
  const {
    state: { maxTime, sentTxs },
    dispatch,
  } = useGraphContext();

  const formattedData = useMemo(() => {
    let totalCount = 0;
    const data = [...sentTxs]
        .sort((a, b) => a.timestamp - b.timestamp)
        .map((v, index) => {
          const plot = {
            x: v.timestamp,
            y: totalCount + v.count,
          }
          totalCount += v.count;
          
          return plot;
        });

        return data;
  }, [sentTxs])

  const throttledData = useThrottle(formattedData, 100);

  return (
    <Line
      data={{
        datasets: [
          {
            label: "Real-Time Data",
            backgroundColor: "rgba(75,192,192,0.4)",
            borderColor: "rgba(75,192,192,1)",
            data: throttledData,
          },
        ],
      }}
      options={{
        // parsing: false, // Required for decimation
        // plugins: {
        //   decimation: {
        //     enabled: true,
        //     algorithm: 'lttb', // 'min-max' is another option
        //     samples: 500, // Adjust as needed
        //   },
        // },
        scales: {
          x: {
            type: "linear",
            ticks: {
              autoSkip: true,
            },
          },
          y: {
            beginAtZero: true,
          },
        },
      }}
    />
    // <ResponsiveContainer width="100%" height="100%">
    //   <LineChart data={data}>
    //     <CartesianGrid strokeDasharray={2} />
    //     <YAxis
    //       tick={false}
    //       label={{
    //         value: "Tx Propagations",
    //         angle: -90,
    //       }}
    //       domain={[0, Math.max(sentTxs.length, 1000)]}
    //       allowDataOverflow
    //       type="number"
    //       dataKey="message"
    //     />
    //     <XAxis
    //       tickFormatter={(v) => `${v.toFixed(0)}ms`}
    //       label="Time"
    //       type="number"
    //       tickCount={2}
    //       allowDataOverflow
    //       domain={[0, maxTime]}
    //       dataKey="time"
    //     />
    //     <Line
    //       type="natural"
    //       dataKey="message"
    //       stroke="#8884d8"
    //       strokeWidth={2}
    //       dot={false}
    //     />
    //     <Tooltip content={(props) => <CustomTooltip {...props} />} />
    //   </LineChart>
    // </ResponsiveContainer>
  );
};
