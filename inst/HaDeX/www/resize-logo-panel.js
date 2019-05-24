let root = document.documentElement;
var logo_panel = document.getElementsByClassName('nav-pills')
logo_panel = logo_panel[0];
console.log(logo_panel);
var modify_scrollables = function() {
       var height = logo_panel.clientHeight + 25;
       console.log('logopanel + margin height: ' +  height);
       root.style.setProperty('--top-bar-height', height + 'px');
       console.log('topbar value: ' + root.style.getPropertyValue('--top-bar-height'));
    };
addResizeListener(logo_panel, modify_scrollables);