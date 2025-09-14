var TextLabelBinding = new Shiny.InputBinding();
$.extend(TextLabelBinding, {
  find: function(scope) {
    return $(scope).find('.inline-text-label');
  },
  getValue: function(el) {
    return $(el).val();
  },
  setValue: function(el, value) {
    $(el).val(value);
  },
  subscribe: function(el, callback) {
    $(el).on('keyup.TextLabelBinding input.TextLabelBinding', function() {
      callback(true);
    });
    $(el).on('change.TextLabelBinding', function() {
      callback(false);
    });
  },
  unsubscribe: function(el) {
    $(el).off('.TextLabelBinding');
  },
  receiveMessage: function(el, data) {
    if(data.value)
      this.setValue(el, data.value);

    if(data.label)
      $(el).siblings('span').text(data.label);
  },
  getRatePolicy: function() {
    return {
      policy: 'debounce',
      delay: 250
    };
  }
});
Shiny.inputBindings.register(TextLabelBinding, 'abers.inlineTextLabel');
