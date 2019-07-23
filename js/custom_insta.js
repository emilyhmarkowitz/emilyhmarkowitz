$(document).ready(function() {


    var userFeed = new Instafeed({
        get: 'user',
        userId: '4975196235',
        limit: 12,
        resolution: 'thumbnail',
        accessToken: '4975196235.1677ed0.36bdc1396eec4d89a8651d63b19ad3e3',
        sortBy: 'most-recent',
        template: '<div class="col-lg-2 instaimg"><a href="{{image}}" title="{{caption}}" target="_blank"><img src="{{image}}" alt="{{caption}}" class="img-fluid"/></a></div>',
    });


    userFeed.run();

    
    // This will create a single gallery from all elements that have class "gallery-item"
    $('.gallery').magnificPopup({
        type: 'image',
        delegate: 'a',
        gallery: {
            enabled: true
        }
    });


});