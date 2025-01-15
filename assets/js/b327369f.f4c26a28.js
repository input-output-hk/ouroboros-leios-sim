"use strict";(self.webpackChunksite=self.webpackChunksite||[]).push([[3537],{8906:(e,s,t)=>{t.r(s),t.d(s,{assets:()=>o,contentTitle:()=>r,default:()=>u,frontMatter:()=>a,metadata:()=>l,toc:()=>d});var n=t(4848),i=t(8453);const a={title:"Weekly progress summary \u2013 January 6, 2025",authors:["will"],tags:["progress","update","weekly"]},r=void 0,l={permalink:"/news/2025/01/06/weekly-progress-summary",source:"@site/news/2025-01-06-weekly-progress-summary.md",title:"Weekly progress summary \u2013 January 6, 2025",description:"Rust simulation",date:"2025-01-06T00:00:00.000Z",tags:[{inline:!0,label:"progress",permalink:"/news/tags/progress"},{inline:!0,label:"update",permalink:"/news/tags/update"},{inline:!0,label:"weekly",permalink:"/news/tags/weekly"}],readingTime:1.165,hasTruncateMarker:!1,authors:[{name:"William Wolff",title:"Architect",url:"https://github.com/will-break-it",imageURL:"https://avatars.githubusercontent.com/u/9065638?v=4",key:"will"}],frontMatter:{title:"Weekly progress summary \u2013 January 6, 2025",authors:["will"],tags:["progress","update","weekly"]},unlisted:!1,nextItem:{title:"Weekly progress summary \u2013 December 30, 2024",permalink:"/news/2024/12/30/weekly-progress-summary"}},o={authorsImageUrls:[void 0]},d=[{value:"Rust simulation",id:"rust-simulation",level:2},{value:"DeltaQ summary update",id:"deltaq-summary-update",level:2},{value:"Cost dashboard updates",id:"cost-dashboard-updates",level:2},{value:"Benchmarking BLS signatures",id:"benchmarking-bls-signatures",level:2},{value:"Votes and certificates",id:"votes-and-certificates",level:2},{value:"Sortition analysis",id:"sortition-analysis",level:2}];function c(e){const s={code:"code",h2:"h2",li:"li",ul:"ul",...(0,i.R)(),...e.components};return(0,n.jsxs)(n.Fragment,{children:[(0,n.jsx)(s.h2,{id:"rust-simulation",children:"Rust simulation"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsx)(s.li,{children:"Added a basic simulation of central processing unit (CPU) usage/latency"}),"\n",(0,n.jsx)(s.li,{children:"Implemented 'lottery won' events to identify the start of CPU processing"}),"\n",(0,n.jsx)(s.li,{children:"Configured each node with four simulated cores, adjustable per node"}),"\n",(0,n.jsx)(s.li,{children:"Transaction validation and ranking block/input block/endorser block generation/validation each take one CPU task"}),"\n",(0,n.jsx)(s.li,{children:"All virtual CPU costs were copied from the cost estimator."}),"\n"]}),"\n",(0,n.jsx)(s.h2,{id:"deltaq-summary-update",children:"DeltaQ summary update"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsx)(s.li,{children:"Added MIN/MAX combinators for best- and worst-case simulation results"}),"\n",(0,n.jsx)(s.li,{children:"The Rust simulation best case does not match the analytically best behavior"}),"\n",(0,n.jsx)(s.li,{children:"The Haskell simulation best case is too fast; the \u0394Q expression must assume more than 200 peers per node."}),"\n"]}),"\n",(0,n.jsx)(s.h2,{id:"cost-dashboard-updates",children:"Cost dashboard updates"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsx)(s.li,{children:"Improved input parameters and computations"}),"\n",(0,n.jsx)(s.li,{children:"Lengthened phases and reduced endorser block rate"}),"\n",(0,n.jsx)(s.li,{children:"Updated CPU costs for votes and certificates"}),"\n",(0,n.jsx)(s.li,{children:"Revised input/output operations per second (IOPS) values based on empirical data from Cardano nodes."}),"\n"]}),"\n",(0,n.jsx)(s.h2,{id:"benchmarking-bls-signatures",children:"Benchmarking BLS signatures"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsxs)(s.li,{children:["Benchmarked BLS votes using the Rust ",(0,n.jsx)(s.code,{children:"bls-signatures"})," package"]}),"\n",(0,n.jsx)(s.li,{children:"Aggregate verification significantly speeds up the process"}),"\n",(0,n.jsx)(s.li,{children:"Provided CPU time estimates for various operations."}),"\n"]}),"\n",(0,n.jsx)(s.h2,{id:"votes-and-certificates",children:"Votes and certificates"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsx)(s.li,{children:"Updated size estimates for votes"}),"\n",(0,n.jsx)(s.li,{children:"Added CPU time estimates for BLS votes and certificates"}),"\n",(0,n.jsx)(s.li,{children:"Drafted technical report sections on BLS and MUSEN certificates."}),"\n"]}),"\n",(0,n.jsx)(s.h2,{id:"sortition-analysis",children:"Sortition analysis"}),"\n",(0,n.jsxs)(s.ul,{children:["\n",(0,n.jsx)(s.li,{children:"Analyzed sortition for input and endorser blocks and votes"}),"\n",(0,n.jsx)(s.li,{children:"Added findings to the draft of the first technical report."}),"\n"]})]})}function u(e={}){const{wrapper:s}={...(0,i.R)(),...e.components};return s?(0,n.jsx)(s,{...e,children:(0,n.jsx)(c,{...e})}):c(e)}},8453:(e,s,t)=>{t.d(s,{R:()=>r,x:()=>l});var n=t(6540);const i={},a=n.createContext(i);function r(e){const s=n.useContext(a);return n.useMemo((function(){return"function"==typeof e?e(s):{...s,...e}}),[s,e])}function l(e){let s;return s=e.disableParentContext?"function"==typeof e.components?e.components(i):e.components||i:r(e.components),n.createElement(a.Provider,{value:s},e.children)}}}]);