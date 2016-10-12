function cityChanges(ele, container, index) {
	var a = Number($(ele).attr("data-index"));
	var province = $(ele).val();
	$(container).find('select[data-index="' + String(a + 1) + '"]').children().css({"display":"none"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="choice"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + province + '"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + province + '"]').first().attr("selected", "true")
}

function cityChanges2(ele, container, index) {
	var a = Number($(ele).attr("data-index"));
	var province = $(ele).val();
	$(container).find('select[data-index="' + String(a + 1) + '"]').children().css({"display":"none"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="choice"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + province + '"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + province + '"]').first().attr("selected", "true")

	districtChanges($(container).find('select[data-index="' + String(a + 1) + '"]'), container, index);
}

function districtChanges(ele, container, index) {
	var a = Number($(ele).attr("data-index"));
	var city = $(ele).val();
	$(container).find('select[data-index="' + String(a + 1) + '"]').children().css({"display":"none"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="choice"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + city + '"]').css({"display":"block"});
	$(container).find('select[data-index="' + String(a + 1) + '"]').children('option[data-filter="' + city + '"]').first().attr("selected", "true");
}