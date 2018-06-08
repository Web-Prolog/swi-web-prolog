/* JavaScript for the Genealogist application */

var query = "";
var offset = 0;

function ask() {
  query = $("#query").val();
  if (query) {
    $.ajax({
      url:"http://localhost:3060/ask?query=" + query,
      success: function(event) {
              writeln(JSON.stringify(event.data));
              if (event.more) {
                  disableButtons(true, false, false, false);
              } else {
                  writeln("No more solutions");
                  offset = 0;
                  disableButtons(false, true, true, true);
              }
          }
    });
  }
}

function next() {
    offset ++;
    $.ajax({
      url:"http://localhost:3060/ask?query=" + query + "&offset=" + offset,
      success: function(event) {
              writeln(JSON.stringify(event.data));
              if (event.more) {
                  disableButtons(true, false, false, false);
              } else {
                  writeln("No more solutions");
                  offset = 0;
                  disableButtons(false, true, true, true);
              }
          }
    });
}

function stop() {
  offset = 0;
  disableButtons(false, true, true, true);
}

function abort() {
  offset = 0;
  disableButtons(false, true, true, true);
}


function update(op) {
  var pred = op + $("input[name=sex]:checked").val(),
      X = $("#X").val().toLowerCase() || '_',
      Y = $("#Y").val().toLowerCase() || '_',
      command = pred + '(' + X + ',' + Y + ')';
  $.ajax({
    url:"http://localhost:3060/ask?query=" + command,
    success: function(event) {
          writeln(command);
          $("#X,#Y").val("");
        }
    });
}


function writeln(string) {
  $('#output').append(string + "<br>");
}

function disableButtons(ask, next, stop, abort) {
  $("#ask-btn").prop("disabled", ask);
  $("#next-btn").prop("disabled", next);
  $("#stop-btn").prop("disabled", stop);
  $("#abort-btn").prop("disabled", abort);
}


$(document).ready(function() {
  $("#sample-queries").on("change", function() {
      $("#query").val($("#sample-queries option:selected").text());
  });
  $("#ask-btn").on("click", ask);
  $("#next-btn").on("click", next);
  $("#stop-btn").on("click", stop);
  $("#abort-btn").on("click", abort);
  $("#assert-btn").on("click", function() {
      update('assert_');
  });
  $("#retract-btn").on("click", function() {
      update('retract_');
  });
  $("#clear-btn").on("click", function() {
      $('#output').html('');
  });
});
