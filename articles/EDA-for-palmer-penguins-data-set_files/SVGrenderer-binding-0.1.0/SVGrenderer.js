HTMLWidgets.widget({

  name: 'SVGrenderer',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {
        el.innerHTML = x.message;
        children = el.children;
        svg = children.item('svg');
        el.setAttribute("style", "width:".concat(svg.getAttribute('width')));
        el.setAttribute("style", "height:".concat(svg.getAttribute('height')));
      },

      resize: function(width, height) {
        el.setAttribute("style", "width:".concat(width));
        el.setAttribute("style", "height:".concat(height));

      }

    };
  }
});
