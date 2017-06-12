
shinyjs.newScrollTo = function(id){ window.scrollTo(0, document.getElementById(id).offsetTop);}

function scrollToBottom(){
  window.scrollTo(0,document.body.scrollHeight);
}

//jsCode <- "shinyjs.pageCol = function(params){$('body').css('background', params);}"


function goToOtuTab(x) {
    var $select = $('#filterOTU').selectize();
    var selectize = $select[0].selectize;
    var search_id = selectize.search(x).items[0].id;
    selectize.setValue(search_id);
    
    /*var second_tab_url = $(".nav-tabs a")[1].href;*/
    
    $(".nav-tabs a:last").tab('show');
    //var top = document.getElementById("app-content").offsetTop;
    //alert(top);
    window.scrollTo(0, 0);
}