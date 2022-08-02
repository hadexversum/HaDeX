Shiny.addCustomMessageHandler("hdx_handler", contact_hdx_viewer);

function contact_hdx_viewer(hdx_message){
  
  console.log(hdx_message)

  var message = {};
  message.amino_results = hdx_message;
  message.hdx_number = 3.14;
  
  console.log(JSON.stringify(hdx_message));
  console.log("message below");
  
  var extWindow = window.open("http://127.0.0.1:5670"); // my hosted app

  console.log($(extWindow.document));
  console.log(JSON.stringify(message));
  
  $(extWindow.document).ready(function() {
     console.log("extWindow is ready");
     extWindow.postMessage(JSON.stringify(message), '*');
  });

  alert("Confirm sending the data:");

}