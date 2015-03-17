<editor-menu>
	<nav class="navbar navbar-default">
      <div class="container-fluid">
        <div class="navbar-header">
            <a class="navbar-brand" href="#">生成</a>
        </div>

        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
            <ul class="nav navbar-nav">
<!--                <li class="active"><a href="#">Link <span class="sr-only">(current)</span></a></li> -->
                <li onClick={opts.save} if={ opts.save && opts.loggedin }><a href="#"><span class="glyphicon glyphicon-pencil"></span> Save</a></li>
                <li><a href="#" onClick={opts.run} if={opts.run}><span class="glyphicon glyphicon-play"></span> Run</a></li>
                <li><a href="#" onClick={opts.tidy} if={opts.tidy}><span class="glyphicon glyphicon-indent-left"></span> Tidy</a></li>
                <li>
				 	<a if={items.length > 0} href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Templates <span class="caret"></span></a>
				    <ul if={items.length > 0} class="dropdown-menu" role="menu" >
						<li each={items}><a href="#template/{slug}">{title}</a></li>
					</ul>
                </li>
            </ul>

            <ul class="nav navbar-nav navbar-right">
                <li><login-with-github login={opts.login} loggedin={opts.loggedin} logout={opts.logout}></login-with-github></li>
            </ul>
            <ul class="nav navbar-nav navbar-right">
                <li if={opts.title}><a onclick={ onTitleClick }>{opts.title}</a></li>
            </ul>
        </div>
    </nav>

    onTitleClick() {
        if (opts.titleclick) {
            opts.titleclick(arguments);
        }
        riot.update(); // don't know why i have to have this
    }

	this.on('update', function(eventName) {
		console.debug("dropdown update");
		this.items = this.opts.templates;
	}.bind(this));

	this.on('mount', function(eventName) {
		console.debug("dropdown mount");
	});

</editor-menu>