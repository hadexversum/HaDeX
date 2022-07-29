Shiny.addCustomMessageHandler("hdx_handler", contact_hdx_viewer);

function contact_hdx_viewer(hdx_message){
  
  var message = {};
  message.hdx_message = hdx_message;
  message.hdx_number = 3.14;
  
  alert(message.hdx_message);
  
  var extWindow = window.open("http://127.0.0.1:5670"); // my hosted app

  //extWindow.postMessage(JSON.stringify(message), '*');
  //extWindow.postMessage("hello", '*');
  
  console.log(extWindow.document);
  console.log(JSON.stringify(message));
  
  $(extWindow.document).ready(function() {
     console.log("extWindow is ready");
  extWindow.postMessage(JSON.stringify(message), '*');
  
});

  alert("end");
  
}