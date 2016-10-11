
function addRoute(name, sender, limit) {
	var $routes = $('.' + name);
    var clone = $routes.first().clone()
    $(clone).insertAfter($routes.last());
   	$(clone).find('textarea').val("");

   	if ($routes.length + 1 >= limit) {
		$(sender).attr("disabled","disabled"); 
	} 
}

function deleteRoute(name, sender, limit) {
	var $routes = $('.' + name);

   	if ($routes.length > 1 ) {
		$(sender).parent().remove();
		if ($routes.length - 1 < limit) {
			$('button[data-name="' + name + '-add"]').removeAttr("disabled");
		}
	}
}