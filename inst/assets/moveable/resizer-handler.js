/*jshint
  jquery:true,
  devel: true
*/
/*global Shiny, Moveable */
$(function() {
  Shiny.addCustomMessageHandler("resize", function(obj) {
    var el = document.getElementById(obj.id);
    if (el === null) {
      console.log("Resize : element not found");
      return;
    }

    var widthModal = 900;
    var moveable = new Moveable(document.querySelector(obj.container), {
      //document.body //
      target: el,
      resizable: true,
      keepRatio: false,
      throttleResize: 0,
      renderDirections: ["e", "se", "s"],
      edge: false,
      zoom: 1,
      origin: false,
      padding: { left: 0, top: 0, right: 0, bottom: 0 }
    });

    var frame = {
      translate: [0, 0]
    };
    moveable
      .on("resizeStart", function(e) {
        e.setOrigin(["%", "%"]);
        e.dragStart && e.dragStart.set(frame.translate);
      })
      .on("resize", function(e) {
        var beforeTranslate = e.drag.beforeTranslate;

        frame.translate = beforeTranslate;
        e.target.style.width = e.width + "px";
        e.target.style.height = e.height + "px";
        e.target.style.transform = "translate("
          .concat(beforeTranslate[0], "px, ")
          .concat(beforeTranslate[1], "px)");
        var browserWidth =
          window.innerWidth ||
          document.documentElement.clientWidth ||
          document.body.clientWidth;

        $("#" + obj.id).trigger("resize");
        if (obj.modal) {
          if ($(".modal-dialog").width() < 900) {
            widthModal = $(".modal-dialog").width();
          }
          if (e.width > widthModal) {
            if (e.width > browserWidth) {
              moveable.resizable = false;
              $(".modal-dialog").width(browserWidth - 40);
              $("#" + obj.id)
                .parent()
                .css("overflow-x", "scroll");
            } else {
              moveable.resizable = true;
              $(".modal-dialog").width(e.width + 30);
              $("#" + obj.id)
                .parent()
                .css("overflow-x", "hidden");
            }
          } else {
            moveable.resizable = true;
            $(".modal-dialog").width(widthModal);
            $("#" + obj.id)
              .parent()
              .css("overflow-x", "hidden");
          }
        }
      });

    Shiny.addCustomMessageHandler("resize-" + obj.id, function(obj) {
      moveable.request(
        "resizable",
        { offsetWidth: obj.width, offsetHeight: obj.height },
        true
      );
    });
  });
});

