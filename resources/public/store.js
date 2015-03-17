function MessageStore() {	
	riot.observable(this);
	this.messageNumber = 0;
	this._messages = [];
	this.defaultType = "info";
}

MessageStore.prototype.listen = function(to) {
	if (!to.on) return;
	to.on('on-message-ok', function(message) {
      this.ok(message);
    }.bind(this));
	to.on('on-message-warn', function(message) {
      this.warn(message);
    }.bind(this));
	to.on('on-message-info', function(message) {
      this.info(message);
    }.bind(this));
	to.on('on-message-error', function(message) {
      this.error(message);
    }.bind(this));
};

MessageStore.prototype.messages = function() {
	return this._messages;
};

MessageStore.prototype.error = function(message, opts) {
	return this.addMessage('danger', message);
};

MessageStore.prototype.warn = function(message, opts) {
	return this.addMessage('warning', message);
};

MessageStore.prototype.info = function(message, opts) {
	return this.addMessage('info', message);
};

MessageStore.prototype.ok = function(message, opts) {
	return this.addMessage('success', message);
};

MessageStore.prototype.removeMessage = function(message) {
	this._messages = this._messages.filter(function(m) {
		return m != message;
	});
	this.trigger("change");
};

MessageStore.prototype.addMessage = function(type,message) {
	message = _.isString(message) ? message : message.message ? message.message : message.toString();
	var m = {
		id: this.messageNumber++,
		when: new Date(),
		type: type,
		text: message
	};
	this._messages.push(m);
	setTimeout(function() {
		this.removeMessage(m);
	}.bind(this), 2000);

	return m;
};






function TemplateListStore() {
	riot.observable(this);
	this.templates = [];
	return this;
}

TemplateListStore.prototype.refresh = function() {
	$.ajax({
		url: "/my/templates",
		accepts: "application/json",
		dataType: "json",
		cache: false,
		error: this.basicErrorHandler.bind(this),
		success: this.onReceivedTemplates.bind(this)
	});
}

TemplateListStore.prototype.basicErrorHandler = function(err) {
	this.trigger("on-message-error", err);
};

TemplateListStore.prototype.onReceivedTemplates = function(templatesData) {
	// console.debug("onReceivedTemplates", templatesData);
	this.templates = templatesData;
	this.trigger('change');
}




function MyAccountStore() {
	riot.observable(this);
	this.loggedIn = false;
	return this;
};


MyAccountStore.prototype.login = function() {
	// maybe save here
	window.location.assign("/auth/github");
};

MyAccountStore.prototype.logout = function() {
	window.location.assign("/auth/logout");
};

MyAccountStore.prototype.init = function() {
	this.refresh();
};

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
};

MyAccountStore.prototype.onReceivedAccount = function(details) {
	this.loggedIn = details['logged-in'];
	this.trigger('received-account', details);
};

MyAccountStore.prototype.onReceivedAccountError = function(err) {
	console.error("onReceivedAccountError", err);
	this.trigger('on-message-error', err);
};



var test_template = {
  x: "{{city}}"
};

function EditorStore() {
	riot.observable(this);
	this.currentTemplate = {
		id: null,
		title: "Untitled",
		content: JSON.stringify(test_template,null,4)
	};
	//console.log("EditorStore, ctor", test_template);
	//console.log("EditorStore, ctor", this.currentTemplate);
	this.templateOutput = null;
	this.templates = [];
	this.messages = [];
	return this;
}

EditorStore.prototype.onReceivedTemplateProcessed = function(data) {
	this.templateOutput = data.processed;
	if (data.errors && data.errors.length > 0) {
		data.errors.forEach(function(e) {
			this.trigger('on-message-error', e);
		}.bind(this));
	}
	this.emitChange();
};

EditorStore.prototype.basicErrorHandler = function(err) {
	if (403 == err.status) return;
	console.error("onReceivedTemplatesError", err);
	this.trigger('on-message-error', err);
}

EditorStore.prototype.init = function() {

}



EditorStore.prototype.getCurrentTemplate = function() {
		//console.log("EditorStore, getCurrentTemplate", this.currentTemplate);
	return this.currentTemplate;
}

EditorStore.prototype.setTemplateContent = function(text) {
	this.currentTemplate.content = text;
	this.emitChange();
}

EditorStore.prototype.tidy = function() {
	try {
		var o = JSON.parse(this.currentTemplate.content);
		this.setTemplateContent(JSON.stringify(o, null, 2));
	} catch (e) {
		this.trigger('on-message-error', e);
	}
};

EditorStore.prototype.onLoadedTemplate = function(data) {
	this.currentTemplate = data.template;
	this.templateOutput = data.processed;
	this.emitChange();
};

EditorStore.prototype.loadTemplate = function(slug) {
	if (slug === this.currentTemplate.slug) {
		return;
	}
	$.ajax({
		url: "/my/templates/" + slug,
		accepts: "application/json",
		dataType: "json",
		cache: false,
		error: this.basicErrorHandler.bind(this),
		success: this.onLoadedTemplate.bind(this)
	});
};


EditorStore.prototype.onTemplateSaved = function(data) {
	//console.log("onTemplateSaved");
	this.trigger('on-message-ok', "Saved.");
	// update the template attributes
	var existingSlug = this.currentTemplate.slug;

	this.currentTemplate = data.template;
	this.trigger('template-saved', data.template);
	this.emitChange(); // notify everyone that stuff has changed

	if (existingSlug !== data.template.slug) {
		this.trigger('on-slug', data.template.slug); 
	}
	this.refresh(); // reload the template list
};

EditorStore.prototype.saveTemplate = function() {
	var body = {
		template : this.getCurrentTemplate()
	};
	$.ajax({
		url: "/my/templates" + (this.getCurrentTemplate().slug ? "/" + this.getCurrentTemplate().slug : ""),
		method: "POST",
		dataType: "json",
		contentType: "application/json",
		cache: false,
		data : JSON.stringify(body),
		error: this.basicErrorHandler.bind(this),
		success: this.onTemplateSaved.bind(this)
	});
};

EditorStore.prototype.process = function() {ing = true;
	var body = {
		template : this.getCurrentTemplate()
	};
	$.ajax({
		url: "/template/process",
		method: "POST",
		dataType: "json",
		contentType: "application/json",
		cache: false,
		data : JSON.stringify(body),
		error: this.onProcessError.bind(this),
		success: this.onReceivedTemplateProcessed.bind(this)
	});
};
EditorStore.prototype.onProcessError = function(err) {
	this.trigger('on-message-error', err);
};

EditorStore.prototype.emitChange = function(text) {
	this.trigger('change');
}
