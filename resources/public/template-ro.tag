<template-ro>
	<div name="editorElement" class="editor">


	</div>
	<style scoped>
    .editor {
      width:100%;
      height:100%;
    }
  </style>

	</style>
	var self = this;

	this.on('update', function() {
		this.editor && this.editor.setValue(opts.editor.template);
	}.bind(this));

	this.on('unmount', function() {
		this.editor.destroy();
	}.bind(this));

	this.on('mount', function(eventName) {
		this.editor = ace.edit(this.editorElement);
		this.editor.setTheme("ace/theme/twilight");
		this.editor.getSession().setMode("ace/mode/javascript");
		this.editor.setReadOnly(true);
		this.update();
  	}.bind(this));


</template-ro>
