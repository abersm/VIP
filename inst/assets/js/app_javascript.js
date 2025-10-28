$(document).ready(function () {
  $('body').on('click', function (e) {
    $('[data-bs-toggle=popover]').each(function () {
      if (!$(this).is(e.target) &&
      $(this).has(e.target).length === 0 &&
      $('.popover').has(e.target).length === 0) {
        $(this).popover('hide');
      }
    });
  });
})

$(document).ready(function() {
  $('.btn-close').addClass('btn-close-white');
});
