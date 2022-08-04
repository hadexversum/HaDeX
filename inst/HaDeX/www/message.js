Shiny.addCustomMessageHandler("hdx_handler", contact_hdx_viewer);

function contact_hdx_viewer(hdx_message){
  
  console.log(hdx_message)

  var extWindow = window.open("http://proteomique.ipbs.fr:8080/"); // my hosted app

  $(extWindow.document).ready(function() {
     console.log("extWindow is ready");
     extWindow.postMessage(JSON.stringify(hdx_message), '*');
  });

}