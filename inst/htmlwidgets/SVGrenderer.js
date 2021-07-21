HTMLWidgets.widget({

  name: 'SVGrenderer',

  type: 'output',

  factory: function(el, width, height) {

    // TODO: define shared variables for this instance

    return {

      renderValue: function(x) {

        el.innerHTML = x.message;

      },

      resize: function(width, height) {

        // TODO: code to re-render the widget with a new size
      }

    };
  }
});
