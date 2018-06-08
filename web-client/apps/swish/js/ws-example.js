
ws = new WebSocket('ws://localhost:3051/chat');
ws.onerror = function(message) {
    console.log(message);
};
ws.onopen = function(message) {
    ws.send("pengine_spawn([format('json-s')])");
};
ws.onmessage = function(message) {
    var event = JSON.parse(message.data);
    if (event.type == "create") {
        ws.send("pengine_ask('" + event.pid + "', member(X, [a,b,c]))");
    } else if (event.type == "success") {
        console.log(event.data);
        if (event.more) {
            ws.send("pengine_next('" + event.pid + "')");
        }
      }
    }
}; 