
/**
 * 封装file upload
 */

function fileUpload(ele) {
	this.element = ele;
	this.preview_map = new Object();

	$(this.element).fileinput({
		language: "ca",
	    uploadUrl: "/file/upload",
	    allowedFileExtensions: ["jpg", "png", "gif"],
	    minImageWidth: 50,
	    minImageHeight: 50
	});

	var that = this;
	$(this.element).on('fileuploaded', function(event, data, previewId, index) {
		var filename = $('#' + previewId)[0].innerText.trim().split(" ")[0];
		that.preview_map[String(filename)] = data.response.result[index];
	});

	$(this.element).on('filesuccessremove', function(event, id) {
	    var filename = $('#' + id)[0].innerText.trim().split(" ")[0];
	    delete that.preview_map[String(filename)];
	});

	$(this.element).on('fileclear', function(event) {
	    this.preview_map = new Object();
	});

	return this;
}

fileUpload.prototype.queryFileNames = function() {
	var result = [];
	$.each(this.preview_map,function(key,value){ 
		result.push(value);	    
	});
	return result;
}

fileUpload.prototype.getElement = function() {
	return this.element;
}