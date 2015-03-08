function EditorStore() {
	riot.observable(this);

	this.template = JSON.stringify(test_template,null,4);
	this.templates = [];
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
	console.log("triggering 'templates'", this.templates);

	this.trigger('templates', this.templates);
}

EditorStore.prototype.login = function() {
	var url="https://github.com/login/oauth/authorize";
	var onSuccess = function(data) {
		console.log("data", data);
		this.user = data;
		this.trigger('loggedin', this.user);
	}.bind(this);

	$.ajax({
	  dataType: "json",
	  url: url,
	  data: {
	  	"client_id": "742c2c34f988aa8658a3",
	  	"scope": "user:email"
	  },
	  success: onSuccess
	});

	// do the login here.

}

EditorStore.prototype.setTemplate = function(text) {
	this.template = text;
	this.emitChange();
}

EditorStore.prototype.emitChange = function(text) {
	this.trigger('change');
}
