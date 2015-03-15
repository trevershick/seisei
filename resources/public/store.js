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

MyAccountStore.prototype.logout = function() {
	window.location.assign("/auth/logout");
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
	this.templateOutput = null;
	this.templates = [];
	this.errors = [];
	this.user = null;
	return this;
}

EditorStore.prototype.onReceivedTemplateProcessed = function(data) {
	this.templateOutput = data.processed;
	if (data.errors && data.errors.length > 0) {
		data.errors.forEach(function(e) {
			this.errors.push(e);
		}.bind(this));
	}
	this.emitChange();
};

EditorStore.prototype.basicErrorHandler = function(err) {
	if (403 == err.status) return;
	console.error("onReceivedTemplatesError", err);
	this.errors.push(err);
	this.trigger('received-error', err);
}

EditorStore.prototype.init = function() {
	this.refresh();
}

EditorStore.prototype.refresh = function() {
	console.debug("triggering 'templates'", this.templates);
	$.ajax({
		url: "/my/templates",
		accepts: "application/json",
		dataType: "json",
		cache: false,
		error: this.basicErrorHandler.bind(this),
		success: this.onReceivedTemplates.bind(this)
	})
}

EditorStore.prototype.onReceivedTemplates = function(templatesData) {
	console.debug("onReceivedTemplates", templatesData);
	this.templates = templatesData;
	this.emitChange();
}

EditorStore.prototype.setTemplate = function(text) {
	this.template = text;
	this.emitChange();
}

EditorStore.prototype.tidy = function() {
	try {
		var o = JSON.parse(this.template);
		this.setTemplate(JSON.stringify(o, null, 2));
	} catch (e) {
		alert(e);
	}
};
EditorStore.prototype.process = function() {
	$.ajax({
		url: "/template/process",
		method: "POST",
		dataType: "json",
		contentType: "application/json",
		cache: false,
		data : JSON.stringify({
			template: this.template
		}),
		error: this.basicErrorHandler.bind(this),
		success: this.onReceivedTemplateProcessed.bind(this)
	});
};


EditorStore.prototype.emitChange = function(text) {
	this.trigger('change');
}
