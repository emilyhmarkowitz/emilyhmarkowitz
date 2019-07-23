$( document ).ready(function() {

var userFeed = new Instafeed({
get: 'user', 
userId: '4975196235', 
limit: 12, 
resolution: 'standard_resolution',
accessToken: '4975196235.1677ed0.36bdc1396eec4d89a8651d63b19ad3e3',
sortBy: 'most-recent',
template: '<div class="gallery"><a href="{{image}} title="{{caption}}" target="_blank"><img src="{{image}}" alt = "{{caption}}", class=img-fluid"/></a></div>', 
}); 
userFeed.run(); 




});