// Custom message for shiny

/*jshint
  jquery:true
*/
/*global Shiny */

$(function() {
  // enable/disable an input
  Shiny.addCustomMessageHandler("toggleInput", function(data) {
    $("#" + data.id).prop("disabled", !data.enable);
    if ($("#" + data.id).hasClass("selectpicker")) {
      $("#" + data.id).selectpicker("refresh");
    }
  });

  // hide or show an element
  Shiny.addCustomMessageHandler("toggleDisplay", function(data) {
    $("#" + data.id).css("display", data.display);
  });

  // Disable / enable a button
  Shiny.addCustomMessageHandler("togglewidget", function(data) {
    if (data.type == "disable") {
      $("#" + data.inputId).prop("disabled", true);
      $("#" + data.inputId).addClass("disabled");
    }
    if (data.type == "enable") {
      $("#" + data.inputId).prop("disabled", false);
      $("#" + data.inputId).removeClass("disabled");
    }
  });

  // Toggle button class
  Shiny.addCustomMessageHandler("toggleClass", function(data) {
    if (data.class == "success") {
      $("#" + data.id).removeClass("btn-primary");
      $("#" + data.id).addClass("btn-success");
    }
    if (data.class == "primary") {
      $("#" + data.id).removeClass("btn-success");
      $("#" + data.id).addClass("btn-primary");
    }
  });

  //$(".sw-dropdown").addClass("btn-group-esquisse");
  //$(".sw-dropdown > .btn").addClass("btn-charter");
  $("#sw-content-filterdrop").click(function(e) {
    e.stopPropagation();
  });
});

