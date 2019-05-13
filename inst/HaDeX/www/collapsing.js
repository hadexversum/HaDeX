var buttons = document.getElementsByClassName("collapse-button");
var i;

var hiding = function() {
    this.classList.toggle("active");
    var panelToHide = this.nextElementSibling;
    if (panelToHide.style.display === "block") {
      panelToHide.style.display = "none";
    } else {
      panelToHide.style.display = "block";
    }
};

for (i = 0; i < buttons.length; i++) {
  buttons[i].addEventListener("click", hiding);
} 