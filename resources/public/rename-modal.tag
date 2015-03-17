<rename-modal>


<div class={ modalClasses() } style={ modalStyle() }>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <h4 class="modal-title">Rename Template</h4>
      </div>
      <div class="modal-body">
	  	Rename the template to : <input type="text" name="newName" id="newName" value={opts.title}/>
	  </div>
	  <div class="modal-footer">

		  	<button class="btn btn-primary" onclick={onSave}>Save</button>
		  	<button class="btn btn-default" onclick={opts.close}>Close</button>

	  </div>
    </div>
  </div>
</div>

	onSave() {
		opts.save(this.newName.value);
	}

	modalClasses() {
		return ['modal','modal-backdrop'].join(" ");
	}

	modalStyle() {
		return opts.x ? "display:block;":"";
	}



</rename-modal>