HTMLWidgets.widget({

  name: 'SVGrenderer',

  type: 'output',

  factory: function(el, width, height) {

    return {

      renderValue: function(x) {

        el.innerHTML = x.message;

      },

      resize: function(width, height) {
        el.setAttribute("style", "width:".concat(width));
        el.setAttribute("style", "height:".concat(height));

      }

    };
  }
});
