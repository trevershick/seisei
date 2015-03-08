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
		this.editor && this.editor.setValue(opts.state.template);
	}.bind(this));

	this.on('unmount', function() {
		this.editor.destroy();
	}.bind(this));

	this.on('mount', function(eventName) {
		this.editor = ace.edit(this.editorElement);
		this.editor.on("change", function() {
			self.text = self.editor.getValue();
			opts.state.setTemplate(self.text);
		});
		this.editor.setTheme("ace/theme/monokai");
		this.editor.getSession().setMode("ace/mode/javascript");
		this.editor.setHighlightActiveLine(false);

		this.editor.commands.addCommand({
    		name: 'myCommand',
    		bindKey: {win: 'Ctrl-M',  mac: 'Command-M'},
    		exec: function(editor) {
        		alert('m');
    		},
    		readOnly: true // false if this command should not apply in readOnly mode
		});
		this.update();

  	}.bind(this));


</template-editor>