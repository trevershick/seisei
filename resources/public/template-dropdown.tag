<template-dropdown>
    <li each={items}><a href="#">{name}</a></li>

	var self = this;


	this.on('update', function(eventName) {
		console.log("dropdown update");
		this.items = this.opts.state.templates;
	}.bind(this));

	this.on('mount', function(eventName) {
		console.log("dropdown mount");
	});

	console.log("Setting up listener on", this.opts.state);
    this.opts.state.on('templates', function() {
    	console.log("Tempaltes Received");
    	self.update();
    });
</template-dropdown>
