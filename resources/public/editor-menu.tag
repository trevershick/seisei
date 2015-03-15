<editor-menu>
	<nav class="navbar navbar-default">
      <div class="container-fluid">
        <div class="navbar-header">
            <button type="button" class="navbar-toggle collapsed" data-toggle="collapse" data-target="#bs-example-navbar-collapse-1">
            <span class="sr-only">Toggle navigation</span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            <span class="icon-bar"></span>
            </button>
            <a class="navbar-brand" href="#">生成</a>
        </div>

        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
            <ul class="nav navbar-nav">
<!--                <li class="active"><a href="#">Link <span class="sr-only">(current)</span></a></li> -->
                <li><a href="#">Save</a></li>
                <li><a href="#" onClick={onRun}>Run</a></li>
                <li><a href="#" onClick={onTidy}>Tidy</a></li>
                <li>
				 	<a if={items.length > 0} href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Templates <span class="caret"></span></a>
				    <ul if={items.length > 0} class="dropdown-menu" role="menu" >
						<li each={items}><a href="#">{title}</a></li>
					</ul>
                </li>
            </ul>

            <ul class="nav navbar-nav navbar-right">
                <li><login-with-github accounts={this.opts.accounts}></login-with-github></li>
            </ul>
        </div>
    </nav>


    onRun() {
        this.opts.editor.process();
    }
    onTidy() {
        this.opts.editor.tidy();
    }

	this.on('update', function(eventName) {
		console.debug("dropdown update");
		this.items = this.opts.editor.templates;
	}.bind(this));

	this.on('mount', function(eventName) {
		console.debug("dropdown mount");
	});

</editor-menu>