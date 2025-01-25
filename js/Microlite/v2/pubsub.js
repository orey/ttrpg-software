"use strict";

class PubSub {
    constructor() {
        this.map = {};//key = event, value = array of functions to be notified
    }
    subscribe(event, f) {
        if ((typeof event != 'string') || (event == '')) {
            myconsole.log("Strange event " + String(event)
                        + ". Subscription recording failed.");
            return;
        }
        let tab = this.map[event];
        if (tab == undefined)
            this.map[event] = [f];
        else
            this.map[event].push(f);
        myconsole.log(`Event ${event} recorded.`);
    }
    publish(event, obj) {
        if ((typeof event != 'string') || (event == '')) {
            myconsole.log("Strange event " + String(event)
                        + " coming from source: " + source
                        + ". Skipping...");
            return;
        }
        let tab = this.map[event];
        if (tab == undefined) {
            myconsole.log("The event " + String(event)
                        + " has not been recorded by any source. Skipping...");
            return;
        }
        for (let elem of tab) {
            myconsole.log(`Dispatch event to function to ${elem.name}`);
            elem.call(null,obj);
        }
    }
}

//const PUBSUB = new PubSub();
  
