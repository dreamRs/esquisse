
var dragulaBinding = new Shiny.InputBinding();
  $.extend(dragulaBinding, {
  find: function find(scope) {
    return $(scope).find('.shiny-input-dragula');
  },
  initialize: function initialize(el) {
    var opts = {};
    var $el = $(el);
    
    opts.removeOnSpill = false;
    
    var replaceold = $(el).data("replace");
    
    var containersId = [];
    
    var targetsContainer = $(el).data("targets");
    targetsContainer.forEach(function(element) {
      containersId.push(document.querySelector('#' + element));
    });
    
    var sourceContainer = $(el).data("source");
    sourceContainer.forEach(function(element) {
      containersId.push(document.querySelector('#' + element));
    });
    
    if (replaceold) {
      opts.copy = function (el, source) {
        return source === document.getElementById(sourceContainer);
      };
      opts.isContainer = function (el) {
        return el.classList.contains('target');
      };
    }
    
    var drake = dragula(containersId, opts).on('dragend', function(el) {
      $(el).trigger('change');
    });
    
    if (replaceold) {
      drake.on('drop', function(el, target) {

        if ( target !== document.getElementById(sourceContainer) ) { 
          if (target !== null) {
            $(target).children('.label-dragula').remove();
            target.appendChild(el);
          }
        } else {
          console.log($(el).attr("id"));
          $(target).find('#' + $(el).attr("id")).remove();
          //$('#' + $(el).attr("id")).remove();
          //target.appendChild(el);
        }
    
      });
    }
  },
  getValue: function getValue(el) {
    var values = {};
    values.source = [];
    values.target = {};
    var source = $(el).data("source");
    $('#' + source).find('span.label-dragula').each(function(){ 
      values.source.push( $(this).data('value') );
    });
    
    var targets = $(el).data("targets");
    var targetsname = [];
    targets.forEach(function(element) {
      targetsname.push(element.replace(/.*target-/, ''));
    });
    
    for (i = 0; i < targetsname.length; i++) { 
      values.target[targetsname[i]] = [];
      $('#' + targets[i]).find('span.label-dragula').each(function(){ 
        values.target[targetsname[i]].push( $(this).data('value') );
        if (values.target[targetsname[i]].length === 0) {
          values.target[targetsname[i]] = null;
        }
      });
    }

    if (values.source.length === 0) {
      values.source = null;
    }

    console.log(values);
    return values;
  },
  getType: function() {
    return "esquisse.dragula";
  },
  setValue: function setValue(el, value) {
    // Not implemented
  },
  subscribe: function subscribe(el, callback) {
    $(el).on('change.dragulaBinding', function (event) {
      callback();
    });
  },
  unsubscribe: function unsubscribe(el) {
    $(el).off('.dragulaBinding');
  },
  receiveMessage: function receiveMessage(el, data) {
    var $el = $(el);
    if (data.hasOwnProperty('choices')) {
      var targetsContainer = $(el).data("targets");
      targetsContainer.forEach(function(element) {
        $('#' + element).children('.label-dragula').remove();
      });
      var sourceContainer = $(el).data("source");
      sourceContainer.forEach(function(element) {
        $('#' + element).children('.label-dragula').remove();
        $('#' + element).html(data.choices);
      });
      $(el).trigger('change');
    }
  },
  getRatePolicy: function getRatePolicy() {
    return {
      policy: 'debounce',
      delay: 250
    };
  },
  getState: function getState(el) {}
});

Shiny.inputBindings.register(dragulaBinding, "shiny.dragula");

