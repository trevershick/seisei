<alert-dialog>

<style scoped>
.modal {
	z-index: 10000;
}
.modal-backdrop {
	z-index: 9999;
}
</style>

<div class={ modalClasses() } style={ modalStyle() }>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" onclick={onOk} aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <h4 class="modal-title">{ dialogTitle() }</h4>
      </div>
      <div class="modal-body">
	  	{ dialogContent() }
	  </div>
	  <div class="modal-footer">
		  	<button class="btn btn-primary" onclick={ this.onOk }>OK</button>
	  </div>
    </div>
  </div>
</div>

	dialogTitle() {
		return this.opts.messages.alertTitle || "alert";
	}

	dialogContent() {
		return this.opts.messages.alertMessage || "Are you sure?";
	}

	onOk() {
		opts.messages.clearAlert();
	}

	modalClasses() {
		return ['modal','modal-backdrop'].join(" ");
	}

	modalStyle() {
		return this.showalert ? "display:block;":"";
	}
	this.on('update', function() {
		this.showalert = !!opts.messages.alertMessage;
	});


</alert-dialog>