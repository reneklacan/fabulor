angular.module('fabulor', []);

angular.module('fabulor').controller('RoomChatController', function($scope) {
    $scope.roomUsers = [
        { name: 'Svetozar', status: 'Online' },
        { name: 'Kazimir', status: 'Online' }
    ];
    $scope.roomUsers2 = [
        { name: 'Barnabas', status: 'Offline for 45 mins' },
    ];
});

$(function() {
    $('.messages-wrapper').height(window.innerHeight - 120);
    window.scrollTo(0, document.body.scrollHeight);
    messagesWrapper = document.querySelector('.messages-wrapper');
    messagesWrapper.scrollTop = messagesWrapper.scrollHeight;
})

$(window).resize(function() {
    $('.messages-wrapper').height(window.innerHeight - 120);
});
