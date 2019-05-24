
// dropInput binding
var dropInputBinding = new Shiny.InputBinding();

$.extend(dropInputBinding, {
  
  initialize: function(el) {
    $(el).find('.drop-input-button').on("click", function(e) {
      $(el).find('.drop-input-main').html(e.currentTarget.innerHTML);
      var value = $(e.currentTarget).data("value");
      $(el).find('.drop-input-main').data("value", value);
      $(el).trigger('change');
    });
  },

  find: function(scope) {
    return $(scope).find(".drop-input");
  },

  // Given the DOM element for the input, return the value
  getValue: function(el) {
    return $(el).find('.drop-input-main').data('value');
  },
  
  setValue: function setValue(el, value) {
    var selected = $(el).find(".drop-input-button[data-value='" + value + "']"); 
    if (typeof selected[0] !== 'undefined') {
      $(el).find('.drop-input-main').html(selected[0].innerHTML);
      var datavalue = selected.data("value");
      $(el).find('.drop-input-main').data("value", datavalue);
    }
  },

  subscribe: function(el, callback) {
    $(el).on("change.dropInputBinding", function(e) {
      callback();
    });
  },
  
  receiveMessage: function(el, data) {
    if (data.hasOwnProperty('selected')) {
      this.setValue(el, data.selected);
    }
    
    if (data.hasOwnProperty('disabled')) {
      var btns = $(el).find(".drop-input-button");
      btns.each(function(index) {
        $(this).prop('disabled', false);
      });
      data.disabled.map(function(value) {
        var btn = $(el).find(".drop-input-button[data-value='" + value + "']");
        btn.prop('disabled', true);
      });
    }
    
    $(el).trigger('change');
  },

  unsubscribe: function(el) {
    $(el).off(".dropInputBinding");
  }
});

Shiny.inputBindings.register(dropInputBinding);
