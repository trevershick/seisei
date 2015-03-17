<template-editor>
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
		// console.debug("update template-editor");
		var currentTemplateContent = opts.editor.getCurrentTemplate().content;
		if (this.text !== currentTemplateContent) {
			// console.log("template-editor, onUpdate", currentTemplateContent);
			this.text = currentTemplateContent;
			this.editor && this.editor.setValue(this.text,-1);
		}
	}.bind(this));

	this.on('unmount', function() {
		this.editor.destroy();
	}.bind(this));

	this.on('mount', function(eventName) {
		// console.debug("mounting the editor");
		this.editor = ace.edit(this.editorElement);
		this.editor.setTheme("ace/theme/twilight");
		this.editor.getSession().setMode("ace/mode/javascript");
		this.editor.setHighlightActiveLine(false);
		this.editor.setValue(this.text, -1);
		this.editor.on("change", function() {
			self.text = self.editor.getValue();
			opts.editor.setTemplateContent(self.text);
		});
  	}.bind(this));


</template-editor>
