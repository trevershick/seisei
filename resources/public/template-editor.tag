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
		console.debug("update template-editor");
		this.template = opts.editor.template
		this.editor && this.editor.setValue(this.template);
	}.bind(this));

	this.on('unmount', function() {
		this.editor.destroy();
	}.bind(this));

	this.on('mount', function(eventName) {
		console.debug("mounting the editor");
		this.editor = ace.edit(this.editorElement);
		this.editor.on("change", function() {
			self.text = self.editor.getValue();
			opts.editor.setTemplate(self.text);
		});
		this.editor.setTheme("ace/theme/twilight");
		this.editor.getSession().setMode("ace/mode/javascript");
		this.editor.setHighlightActiveLine(false);
		this.editor.setValue(this.template);

		this.editor.commands.addCommand({
    		name: 'myCommand',
    		bindKey: {win: 'Ctrl-M',  mac: 'Command-M'},
    		exec: function(editor) {
        		alert('m');
    		},
    		readOnly: true // false if this command should not apply in readOnly mode
		});
  	}.bind(this));


</template-editor>
