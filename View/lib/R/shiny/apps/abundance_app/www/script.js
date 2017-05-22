
/*$(document).on('a1', function(event) {
  alert('Connected to the server');
});*/

function goToOtuTab(x) {
    var $select = $('#filterOTU').selectize();
    var selectize = $select[0].selectize;
    var search_id = selectize.search(x).items[0].id;
    selectize.setValue(search_id);
    $('.nav-tabs a:last').tab('show')
    var top = document.getElementById("app-content").offsetTop;
    window.scrollTo(0, top);
}