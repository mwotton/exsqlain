(function(e){function n(n){for(var r,c,s=n[0],a=n[1],i=n[2],f=0,p=[];f<s.length;f++)c=s[f],Object.prototype.hasOwnProperty.call(o,c)&&o[c]&&p.push(o[c][0]),o[c]=0;for(r in a)Object.prototype.hasOwnProperty.call(a,r)&&(e[r]=a[r]);l&&l(n);while(p.length)p.shift()();return u.push.apply(u,i||[]),t()}function t(){for(var e,n=0;n<u.length;n++){for(var t=u[n],r=!0,s=1;s<t.length;s++){var a=t[s];0!==o[a]&&(r=!1)}r&&(u.splice(n--,1),e=c(c.s=t[0]))}return e}var r={},o={app:0},u=[];function c(n){if(r[n])return r[n].exports;var t=r[n]={i:n,l:!1,exports:{}};return e[n].call(t.exports,t,t.exports,c),t.l=!0,t.exports}c.m=e,c.c=r,c.d=function(e,n,t){c.o(e,n)||Object.defineProperty(e,n,{enumerable:!0,get:t})},c.r=function(e){"undefined"!==typeof Symbol&&Symbol.toStringTag&&Object.defineProperty(e,Symbol.toStringTag,{value:"Module"}),Object.defineProperty(e,"__esModule",{value:!0})},c.t=function(e,n){if(1&n&&(e=c(e)),8&n)return e;if(4&n&&"object"===typeof e&&e&&e.__esModule)return e;var t=Object.create(null);if(c.r(t),Object.defineProperty(t,"default",{enumerable:!0,value:e}),2&n&&"string"!=typeof e)for(var r in e)c.d(t,r,function(n){return e[n]}.bind(null,r));return t},c.n=function(e){var n=e&&e.__esModule?function(){return e["default"]}:function(){return e};return c.d(n,"a",n),n},c.o=function(e,n){return Object.prototype.hasOwnProperty.call(e,n)},c.p="/";var s=window["webpackJsonp"]=window["webpackJsonp"]||[],a=s.push.bind(s);s.push=n,s=s.slice();for(var i=0;i<s.length;i++)n(s[i]);var l=a;u.push([0,"chunk-vendors"]),t()})({0:function(e,n,t){e.exports=t("56d7")},"034f":function(e,n,t){"use strict";var r=t("85ec"),o=t.n(r);o.a},"56d7":function(e,n,t){"use strict";t.r(n);t("e260"),t("e6cf"),t("cca6"),t("a79d");var r,o=t("2b0e"),u=function(){var e=this,n=e.$createElement,t=e._self._c||n;return t("div",[t("pev2",{attrs:{"plan-source":e.plan,"plan-query":e.query}})],1)},c=[],s=t("7e84"),a=t.n(s),i={name:"sse-test",components:{pev2:a.a},data:function(){return{messages:[],query:"select 1;",plan:"                                     QUERY PLAN\n------------------------------------------------------------------------------------\n Result  (cost=0.00..0.01 rows=1 width=4) (actual time=0.002..0.002 rows=1 loops=1)\n Planning time: 2.710 ms\n Execution time: 0.870 ms\n(3 rows)\n"}},mounted:function(){var e=this;this.$sse("/queryChan",{format:"json"}).then((function(n){r=n,n.onError((function(e){console.error("lost connection; giving up!",e),n.close()})),n.subscribe("",(function(e,n){console.warn("Received a message w/o an event!",data,e)})),n.subscribe("visualise",(function(n,t){console.log(n),console.log(t),e.messages.push(n),e.query=n.query,e.plan=n.plan}))})).catch((function(e){console.error("Failed to connect to server",e)}))},beforeDestroy:function(){r.close()}},l=i,f=(t("034f"),t("2877")),p=Object(f["a"])(l,u,c,!1,null,null,null),d=p.exports,b=t("37b6");t("ab8b"),t("1f54");o["default"].config.productionTip=!1,o["default"].use(b["a"]),new o["default"]({render:function(e){return e(d)}}).$mount("#app")},"85ec":function(e,n,t){}});
//# sourceMappingURL=app.429671bf.js.map