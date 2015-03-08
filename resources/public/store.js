function EditorStore() {
	riot.observable(this);

	this.template = JSON.stringify(test_template,null,4);
	this.templates = [];
	this.errors = [];
	this.user = null;
	return this;
}

EditorStore.prototype.init = function() {
	// start loading data.
	this.templates = [
		{id :"ZIFJ35", name: "Template 1"},
		{id :"KDLI3J", name: "Template 3"},
		{id :"ADKIEJ", name: "Template 5"}
	];
	var me = this;
	console.log("triggering 'templates'", this.templates);
	$.ajax({
		url: "/my/templates",
		accepts: "application/json",
		dataType: "json",
		cache: false,
		error: me.onReceivedTemplatesError.bind(this),
		success: me.onReceivedTemplates.bind(this)
	})
}

EditorStore.prototype.onReceivedTemplatesError = function(err) {
	console.error("onReceivedTemplatesError", err);
	this.errors.push(err);
	this.trigger('received-error', err);
}

EditorStore.prototype.onReceivedTemplates = function(templatesData) {
	console.debug("onReceivedTemplates", templatesData);
	this.templates = templatesData;
	this.trigger('templates', templatesData);
}

EditorStore.prototype.login = function() {
	// maybe save here
	window.location.assign("/auth/github");
}

EditorStore.prototype.setTemplate = function(text) {
	this.template = text;
	this.emitChange();
}

EditorStore.prototype.emitChange = function(text) {
	this.trigger('change');
}
