"use strict";(self.webpackChunksite=self.webpackChunksite||[]).push([[711],{9331:(e,t,r)=>{r.r(t),r.d(t,{default:()=>m});r(6540);var s=r(8774),a=r(1312),i=r(1213),n=r(6266),c=r(6065),l=r(1107),h=r(4848);function d(e){let{year:t,posts:r}=e;const a=(0,n.i)({day:"numeric",month:"long",timeZone:"UTC"});return(0,h.jsxs)(h.Fragment,{children:[(0,h.jsx)(l.A,{as:"h3",id:t,children:t}),(0,h.jsx)("ul",{children:r.map((e=>{return(0,h.jsx)("li",{children:(0,h.jsxs)(s.A,{to:e.metadata.permalink,children:[(t=e.metadata.date,a.format(new Date(t)))," - ",e.metadata.title]})},e.metadata.date);var t}))})]})}function o(e){let{years:t}=e;return(0,h.jsx)("section",{className:"margin-vert--lg",children:(0,h.jsx)("div",{className:"container",children:(0,h.jsx)("div",{className:"row",children:t.map(((e,t)=>(0,h.jsx)("div",{className:"col col--4 margin-vert--lg",children:(0,h.jsx)(d,{...e})},t)))})})})}function m(e){let{archive:t}=e;const r=(0,a.T)({id:"theme.blog.archive.title",message:"Archive",description:"The page & hero title of the blog archive page"}),s=(0,a.T)({id:"theme.blog.archive.description",message:"Archive",description:"The page & hero description of the blog archive page"}),n=function(e){const t=e.reduce(((e,t)=>{const r=t.metadata.date.split("-")[0],s=e.get(r)??[];return e.set(r,[t,...s])}),new Map);return Array.from(t,(e=>{let[t,r]=e;return{year:t,posts:r}}))}(t.blogPosts);return(0,h.jsxs)(h.Fragment,{children:[(0,h.jsx)(i.be,{title:r,description:s}),(0,h.jsxs)(c.A,{children:[(0,h.jsx)("header",{className:"hero hero--primary",children:(0,h.jsxs)("div",{className:"container",children:[(0,h.jsx)(l.A,{as:"h1",className:"hero__title",children:r}),(0,h.jsx)("p",{className:"hero__subtitle",children:s})]})}),(0,h.jsx)("main",{children:n.length>0&&(0,h.jsx)(o,{years:n})})]})]})}}}]);