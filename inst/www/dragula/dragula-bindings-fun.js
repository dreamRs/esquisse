
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

function dragAndDrop(idvar, idx, idy, idfill, idcolor, idsize) {
  $('.xyvar').find('.badge-dad').remove();
  dragula([document.querySelector('#' + idvar),
           document.querySelector('#' + idx),
           document.querySelector('#' + idy),
           document.querySelector('#' + idfill),
           document.querySelector('#' + idcolor),
           document.querySelector('#' + idsize)], {
    removeOnSpill: true,
    copy: function (el, source) {
      return source === document.getElementById(idvar);
    },
    accepts: function (el, target) {
      return target !== document.getElementById(idvar);
    },
    isContainer: function (el) {
      return el.classList.contains('xyvar');
    }
  }).on('dragend', function(el) {

    // element in the x axis box
    var elinx = [];
    $('#' + idx).find('span').each(function(){ elinx.push(this.id); });
    Shiny.onInputChange(idx, elinx);

    // element in the y axis box
    var eliny = [];
    $('#' + idy).find('span').each(function(){ eliny.push(this.id); });
    Shiny.onInputChange(idy, eliny);

    // element in the fill box
    var elinfill = [];
    $('#' + idfill).find('span').each(function(){ elinfill.push(this.id); });
    Shiny.onInputChange(idfill, elinfill);

    // element in the fill box
    var elincolor = [];
    $('#' + idcolor).find('span').each(function(){ elincolor.push(this.id); });
    Shiny.onInputChange(idcolor, elincolor);

    // element in the size box
    var elinsize = [];
    $('#' + idsize).find('span').each(function(){ elinsize.push(this.id); });
    Shiny.onInputChange(idsize, elinsize);

  }).on('drop', function(el, target) {

    if ( target !== document.getElementById(idvar) ) { // &  target !== document.getElementById(idy)
      $(target).children('.badge-dad').remove();
      //target.innerHTML = "";
      target.appendChild(el);
    }

  });
}

