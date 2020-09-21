require(['require-config'], function() {

  require(["jquery"], function($) {

    // json save data

    $("#save-data-btn").on("click", function() {

      var data = partida;

        $.ajax("/save-data", {
          data: JSON.stringify(data),
          dataType: "json",
          type: "POST"
        }).done(function (response) {
         $("#saved-data").text(JSON.stringify(response));
        });
    });



  });

});
