
var copyCodeBtn = document.querySelector('.btn-copy-code');

/*
copyCodeBtn.addEventListener('click', function(event) {
  var copyCode = $('#codeggplot');
  var holderCode = $('#holderCode');
  //console.log(copyCode);
  holderCode.innerText = copyCode.innerText;
  Copied = holderCode.createTextRange();

  try {
    var successful = Copied.execCommand('Copy');
    var msg = successful ? 'successful' : 'unsuccessful';
    console.log('Copying text command was ' + msg);
  } catch (err) {
    console.log('Oops, unable to copy');
  }
});
*/


function ClipBoard() {
  var copyCode = $('#codeggplot');
  var holderCode = $('#holderCode');
  holderCode.innerText = copyCode.text();
  holderCode.select();
  document.execCommand("copy");
}
