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
		if (!this.aceEditor) { return; }
		if (opts.editor.templateOutput) {
			this.aceEditor.setValue(JSON.stringify(opts.editor.templateOutput, null,4), -1);
		} else {
			this.aceEditor.setValue("", -1);
		}
	}.bind(this));

	this.on('unmount', function() {
		this.aceEditor.destroy();
	}.bind(this));

	this.on('mount', function(eventName) {
		this.aceEditor = ace.edit(this.editorElement);
		this.aceEditor.setTheme("ace/theme/twilight");
		this.aceEditor.getSession().setMode("ace/mode/javascript");
		this.aceEditor.setReadOnly(true);
		this.update();
  	}.bind(this));


</template-ro>
