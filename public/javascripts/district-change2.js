function cityChanges(ele, container, index) {
	var a = Number($(ele).attr("data-index"));
	var province = $(ele).val();

	var s = $(container).find('select[data-index="' + String(a + 1) + '"]');
	s.empty();

	$.each($('li[data-category="city"][data-filter="' + province + '"]'), function() {
		var v = $(this).attr("data-value");
		$("<option value='" + v  + "'>" + v + "</option>").appendTo(s);
	});
}

function cityChanges2(ele, container, index) {
	cityChanges(ele, container, index);
	districtChanges($(container).find('select[data-index="' + String(a + 1) + '"]'), container, index);
}

function districtChanges(ele, container, index) {
	var a = Number($(ele).attr("data-index"));
	var city = $(ele).val();

	var s = $(container).find('select[data-index="' + String(a + 1) + '"]');
	s.empty();

	$.each($('li[data-category="district"][data-filter="' + city + '"]'), function() {
		var v = $(this).attr("data-value");
		$("<option value='" + v  + "'>" + v + "</option>").appendTo(s);
	});
}