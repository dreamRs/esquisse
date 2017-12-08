
// Dragula bindings

/*
dragula([document.querySelector('#variables'), document.querySelector('#xvars')])
.on('dragend', function(el) {
var IDx = [];
$('#xvars').find('span').each(function(){ IDx.push(this.id); });
Shiny.onInputChange('xaxis', IDx);
});
*/

//var containers = $('.xyvar').toArray();
//containers.concat($('#variables').toArray());

dragula([document.querySelector('#variables'), document.querySelector('#xvars'), document.querySelector('#yvars')], {
  removeOnSpill: true,
  copy: function (el, source) {
    return source === document.getElementById('variables');
  },
  accepts: function (el, target) {
    return target !== document.getElementById('variables');
  },
  isContainer: function (el) {
    return el.classList.contains('xyvar');
  }
}).on('dragend', function(el) {

  var IDx = [];
  $('#xvars').find('span').each(function(){ IDx.push(this.id); });
  Shiny.onInputChange('xaxis', IDx);

  var IDy = [];
  $('#yvars').find('span').each(function(){ IDy.push(this.id); });
  Shiny.onInputChange('yaxis', IDy);

}).on('drop', function(el, target) {

  if ( target == document.getElementById('xvars') ) {
    document.getElementById('xvars').innerHTML = "";
    document.getElementById('xvars').appendChild(el);
  }

});

