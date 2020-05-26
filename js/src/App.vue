<template>
  <div>
    <pev2 :plan-source="plan" :plan-query="query"></pev2>
  </div>
</template>



<script>
// We store the reference to the SSE object out here
// so we can access it from other methods
let msgServer;
import pev2 from "pev2";

export default {
  name: 'sse-test',
  components: {
    pev2: pev2
  },
  data() {
    return {
      messages: [],
      query: '',
      plan: ''
    };
  },
  mounted() {
    this.$sse('/queryChan', { format: 'json' }) // or { format: 'plain' }
      .then(sse => {
        // Store SSE object at a higher scope
        msgServer = sse;

        // Catch any errors (ie. lost connections, etc.)
        sse.onError(e => {
          console.error('lost connection; giving up!', e);

          // This is purely for example; EventSource will automatically
          // attempt to reconnect indefinitely, with no action needed
          // on your part to resubscribe to events once (if) reconnected
          sse.close();
        });

        // Listen for messages without a specified event
        sse.subscribe('', (message, rawEvent) => {
          console.warn('Received a message w/o an event!', data, message);
        });

        // Listen for messages based on their event (in this case, "chat")
        sse.subscribe('visualise', (message, rawEvent) => {
	  console.log(message);
	  console.log(rawEvent);
          this.messages.push(message);
	  this.query=message.query;
	  this.plan=message.plan;
        });

      })
      .catch(err => {
        // When this error is caught, it means the initial connection to the
        // events server failed.  No automatic attempts to reconnect will be made.
        console.error('Failed to connect to server', err);
      });
  },
  beforeDestroy() {
    // Make sure to close the connection with the events server
    // when the component is destroyed, or we'll have ghost connections!
    msgServer.close();
  },
};
</script>


<style>
#app {
  font-family: Avenir, Helvetica, Arial, sans-serif;
  -webkit-font-smoothing: antialiased;
  -moz-osx-font-smoothing: grayscale;
  text-align: center;
  color: #2c3e50;
  margin-top: 60px;
}
</style>
