/* If we try to send same message twice, Python will not receive it
 * because callback only gets called only on title change. As a
 * workaround, we prepand counter variable _icloak_i to each message
 * so they always trigger callback.
 */
_icloak_i = 0;


function sendToPy(jsonMsg) {
   jsonMsg._magic    = 'gentoo-sicp-wizards';
   jsonMsg._icloak_i = _icloak_i++;

   jsonMsgStr = JSON.stringify(jsonMsg);

   document.title = jsonMsgStr;
}

function UpdateUiFromPy(id, newVal) {
   getById(id).innerHTML = newVal;
}

function getById(id) {
   return document.getElementById(id);
}

function getByClass(className) {
   return document.getElementsByClassName(className);
}

function toggle(id, className) {
  getById(id).classList.toggle(className);
}

//TODO: is stuff in function($) {blah} visible in global scope
//TODO: REMOVE BLOAT
//BLOW UP YOUR COMPUTER
function removeClass(id, className) {
   //getById(id).className = '';
   $('#'+id).removeClass(className)
}

function addClass(id, className) {
   //getById(id).className = className;
   $('#'+id).addClass(className)
}

