<template-dropdown>
      <a if={items.length > 0} href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Templates <span class="caret"></span></a>
      <ul if={items.length > 0} class="dropdown-menu" role="menu" >
		<li each={items}><a href="#">{title}</a></li>
		</ul>


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
