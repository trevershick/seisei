<template-dropdown>
	<li each={items}><a href="#">{name}</a></li>

	var self = this;


	this.on('update', function(eventName) {
		console.debug("dropdown update");
		this.items = this.opts.state.templates;
	}.bind(this));

	this.on('mount', function(eventName) {
		console.debug("dropdown mount");
	});

	console.debug("Setting up listener on", this.opts.state);
	this.opts.state.on('templates', function(templates) {
		console.log("template-dropdown::'on templates'", templates);
		this.update();
	}.bind(this));
</template-dropdown>
