import Vue from 'vue'
import App from './App.vue'

import VueSSE from 'vue-sse';
import 'bootstrap/dist/css/bootstrap.min.css';
import 'font-awesome/css/font-awesome.min.css';

Vue.config.productionTip = false
Vue.use(VueSSE);

new Vue({
  render: h => h(App),
}).$mount('#app')
