'use strict';

function pad(n, width, z) {
    z = z || '0';
    n = n + '';
    return n.length >= width ? n : new Array(width - n.length + 1).join(z) + n;
}

angular.module('fabulor', []);

angular.module('fabulor').service('webSocket', function() {
    var url = document.URL.replace("http:", "ws:").replace("https:", "wss:"),
        connection;

    var connect = function() {
        console.log('Connecting to: ' + url);
        connection = new WebSocket(url);
    };

    this.setMessageCallback = function(callback) {
        connection.onmessage = callback;
    };

    this.send = function(message) {
        // TODO: try to catch exception and reconnect
        connection.send(message);
    };

    connect();
});

angular.module('fabulor').controller('RoomChatController', function($scope, $interval, webSocket) {
    $scope.message = '';

    $scope.roomUsers = [
        { name: 'Svetozar', status: 'Online' },
        { name: 'Kazimir', status: 'Online' }
    ];
    $scope.roomUsers2 = [
        { name: 'Barnabas', status: 'Offline for 45 mins' },
    ];

    var messageToStr = function(message) {
        var date = new Date(message.createdAt);
        var time = date.getHours() + ':' +
                   pad(date.getMinutes(), 2) + ':' +
                   pad(date.getSeconds(), 2);
        return '<b>' + message.username + '</b> (' + time + '): ' + message.value;
    }

    var createMessage = function(type, value) {
        return {
            username: "",
            type: type,
            value: value || '',
            roomId: 0,
            createdAt: "0000-00-00T00:00:00.000Z",
        };
    };

    var receiveMessage = function(event) {
        var message = JSON.parse(event.data);

        console.log(message);

        switch (message.type) {
            case 'message':
            case 'event':
                var p = document.createElement("p");
                p.innerHTML = messageToStr(message);
                var output = document.getElementById("output");
                output.appendChild(p);
                var messagesWrapper = document.querySelector('.messages-wrapper');
                messagesWrapper.scrollTop = messagesWrapper.scrollHeight;
                break;
            case 'status':
                console.log('status received');
                break;
        }
    };

    var sendStatusRequest = function() {
        webSocket.send(JSON.stringify(createMessage('status')));
    };

    $scope.sendMessage = function() {
      if (!$scope.message) {
          return;
      }

      webSocket.send(JSON.stringify(createMessage('message', $scope.message)));
      $scope.message = '';
    };

    webSocket.setMessageCallback(receiveMessage);

    window.onbeforeunload = function(e) {
        webSocket.send(JSON.stringify(createMessage('event', 'left the room')));
    };

    $interval(sendStatusRequest, 5000);
});

$(function() {
    var messagesWrapper = document.querySelector('.messages-wrapper');
    if (!messagesWrapper)
        return;

    $('.messages-wrapper').height(window.innerHeight - 120);
    window.scrollTo(0, document.body.scrollHeight);
    messagesWrapper.scrollTop = messagesWrapper.scrollHeight;
})

$(window).resize(function() {
    $('.messages-wrapper').height(window.innerHeight - 120);
});
