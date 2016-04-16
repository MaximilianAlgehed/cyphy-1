function createWebSocket(path) {
    var host = window.location.hostname;
    if(host == '') host = 'localhost';
    var uri = 'ws://' + host + ':9160' + path;
    var Socket = "MozWebSocket" in window ? MozWebSocket : WebSocket;
    return new Socket(uri);
}

$(document).ready(function () {
    var ws = createWebSocket('/');

    ws.onopen = function() {
        var p = $(document.createElement('p')).text('socket opened');
        $('#log').append(p);
        ws.send('register');
    };

    ws.onmessage = function() {
        var p = $(document.createElement('p')).text('received data');
        $('#log').append(p);

        var p = $(document.createElement('p')).text(event.data);
        $('#data').append(p);
    };

    ws.onclose = function() {
        var p = $(document.createElement('p')).text('socket closed');
        $('#log').append(p);
    };

    ws.onerror = function() {
        var p = $(document.createElement('p')).text('socket error');
        $('#log').append(p);
    };

    return false;
});
