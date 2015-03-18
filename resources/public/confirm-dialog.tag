<confirm-dialog>

<div class={ modalClasses() } style={ modalStyle() }>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <h4 class="modal-title">{ dialogTitle() }</h4>
      </div>
      <div class="modal-body">
	  	{ dialogContent() }
	  </div>
	  <div class="modal-footer">
		  	<button class="btn btn-primary" onclick={ this.onYes }>Yes</button>
		  	<button class="btn btn-default" onclick={ this.onNo }>No</button>
	  </div>
    </div>
  </div>
</div>

	dialogTitle() {
		return this.opts.title || "Confirm";
	}

	dialogContent() {
		return this.opts.message || "Are you sure?";
	}

	onYes() {
		opts.callback(true);
	}

	onNo() {
		opts.callback(false);
	}

	modalClasses() {
		return ['modal','modal-backdrop'].join(" ");
	}

	modalStyle() {
		return opts.showconfirm ? "display:block;":"";
	}



</confirm-dialog>