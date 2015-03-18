<messages>
	<style scope>
	.messages-overlay {
		position: absolute;
		width: 100%;
		z-index: 2000;
		bottom: 0;
	}
	.alert {
		margin-left:auto;
		margin-right:auto;
		width:50%;
	}
	</style>
	<div class="messages-overlay">
		<div each={this._messages}>
		<div class={parent.messageClasses(type)}>
			<button type="button" class="close" onclick={parent.remove} aria-label="Close"><span aria-hidden="true">&times;</span></button>
			&nbsp;{text}
		</div>
		</div>
	</div>
	
	messageClasses(type) {
		return "alert alert-" + type + " message"
	}

	this.on('update', function() {
		if (_.isFunction(this.opts.messages)) {
			this._messages = this.opts.messages();	
		} else {
			this._messages = this.opts.messages;
		}
	});

	remove(e) {
		var itemToRemove = e.item;
		this.opts.close(itemToRemove);
	}
</messages>