function MyAccountStore() {
	riot.observable(this);
	this.errors = [];
	this.loggedIn = false;
	return this;
}


MyAccountStore.prototype.login = function() {
	// maybe save here
	window.location.assign("/auth/github");
}

MyAccountStore.prototype.init = function() {
	this.refresh();
}

MyAccountStore.prototype.refresh = function() {
	var me = this;
	$.ajax({
		url: "/my/account",
		accepts: "application/json",
		dataType: "json",
		cache: false,
		error: me.onReceivedAccountError.bind(this),
		success: me.onReceivedAccount.bind(this)
	});
}

MyAccountStore.prototype.onReceivedAccount = function(details) {
	this.loggedIn = details['logged-in'];
	this.trigger('received-account', details);
}

MyAccountStore.prototype.onReceivedAccountError = function(err) {
	console.error("onReceivedAccountError", err);
	this.errors.push(err);
	this.trigger('received-error', err);
}

function EditorStore() {
	riot.observable(this);

	this.template = JSON.stringify(test_template,null,4);
	this.templates = [];
	this.errors = [];
	this.user = null;
	return this;
}

EditorStore.prototype.init = function() {
	this.refresh();
}

EditorStore.prototype.refresh = function() {
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
	if (403 == err.status) return;
	console.error("onReceivedTemplatesError", err);
	this.errors.push(err);
	this.trigger('received-error', err);
}

EditorStore.prototype.onReceivedTemplates = function(templatesData) {
	console.debug("onReceivedTemplates", templatesData);
	this.templates = templatesData;
	this.trigger('templates', templatesData);
}

EditorStore.prototype.setTemplate = function(text) {
	this.template = text;
	this.emitChange();
}

EditorStore.prototype.emitChange = function(text) {
	this.trigger('change');
}
