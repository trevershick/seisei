<modal>


<div class={ modalClasses() } style={ modalStyle() }>
  <div class="modal-dialog">
    <div class="modal-content">
      <div class="modal-header">
        <button type="button" class="close" data-dismiss="modal" aria-label="Close"><span aria-hidden="true">&times;</span></button>
        <h4 class="modal-title" if={opts.title}>{opts.title}</h4>
      </div>
      <div class="modal-body">
	  	<inner-html/>
	  </div>
	  <div class="modal-footer">
		  <span each={opts.buttons}>
		  	<button if={click} class={class} onclick={click}>{label}</button>
		  </span>
	  </div>
    </div>
  </div>
</div>

	modalClasses() {
		return ['modal','modal-backdrop'].join(" ");
	}

	modalStyle() {
		return opts.x ? "display:block;":"";
	}

  	onHandleBackdropClick(e) {
    if (e.target !== e.currentTarget) {
      return;
    }

    opts.onRequestHide();
  }

</modal>