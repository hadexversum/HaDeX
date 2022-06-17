$(document).ready(function(){
  $('.dropdown').on('click', function(e){
    $(this).toggleClass('open');
    
    e.stopPropagation();
    e.preventDefault();
  });
  
  $('[data-toggle=tab]').on('click', function(e){
    let dv = ($(this).attr('data-value'));
    
    //Set active element in tabcontents
    $('.tab-pane').removeClass('active');
    $('.tab-pane[data-value="' + dv + '"]').addClass('active');
    
    //Set active element in navbar
    $('a[data-toggle=tab]').parent().removeClass('active');
    $('a[data-value="' + dv + '"]').parent().addClass("active");
    
    //Close the dropdowns
    $('.dropdown').removeClass('open');
    
    e.stopPropagation();
    e.preventDefault();
  });
});