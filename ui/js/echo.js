A = {}
$(function() {
    var socket = A.socket = new WebSocket("ws://localhost:9000");

    socket.onopen = function() {
        console.log("Opened!")
    }

    socket.onmessage = function(msg) {
        console.log("Received: ", msg);
    }

    $('button').on('click', function(e) {
        socket.send($("#box")[0].value)
    })
 
    var menuStatus;

    var activePage = $("#window")
    var show = function() {
        if(menuStatus) {
            return;
        }
        $("#user-list").show();
        activePage.animate({
            marginLeft: "165px",
        }, 300, function () {
            menuStatus = true
        });
    };

    var hide = function() {
        if(!menuStatus) {
            return;
        }
        activePage.animate({
            marginLeft: "0px",
        }, 300, function () {
            menuStatus = false
            $("#user-list").hide();
        });
    };

    var toggle = A.toggle = function() {
        if (!menuStatus) {
            show();
        } else {
            hide();
        }
        return false;
    };
 
    // Show/hide the menu
    $("#user-toggle").click(toggle);
  
    // Menu behaviour
    $("#user-list li a").click(function () {
        var p = $(this).parent();
        p.siblings().removeClass('active');
        p.addClass('active');
    });
})



