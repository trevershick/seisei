<editor-menu>
    <style scoped>
    .dropdown-menu {
        max-height:350px;
        overflow-y:scroll;
    }
    </style>
    <hotkeys show={ this.showhk }></hotkeys>

	<nav class="navbar navbar-default">
      <div class="container-fluid">
        <div class="navbar-header">
            <a class="navbar-brand" href="#">生成</a>
        </div>

        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
            <ul class="nav navbar-nav">
<!--                <li class="active"><a href="#">Link <span class="sr-only">(current)</span></a></li> -->
                <li onClick={opts.save} if={ opts.save && opts.loggedin }><a href="#"><span class="glyphicon glyphicon-pencil"></span> <u>S</u>ave</a></li>
                <li><a href="#" onClick={opts.run} if={opts.run}><span class="glyphicon glyphicon-play"></span> <u>R</u>un</a></li>
                <li><a href="#" onClick={opts.tidy} if={opts.tidy}><span class="glyphicon glyphicon-indent-left"></span> T<u>i</u>dy</a></li>
                <li>
				 	<a if={items.length > 0} href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Templates <span class="caret"></span></a>
				    <ul if={items.length > 0} class="dropdown-menu" role="menu">
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
    showHotkeys() {
        this.showhk = !this.showhk;
        riot.update(this);
    }

	this.on('update', function(eventName) {
		// console.debug("dropdown update");
		this.items = this.opts.templates;
	}.bind(this));

	this.on('mount', function(eventName) {
        this.showhk = false;
        Mousetrap.bindGlobal('mod+s', function() { !this.showhk && this.opts.save();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+r', function() { !this.showhk && this.opts.run();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+i', function() { !this.showhk && this.opts.tidy();return false; }.bind(this));
        Mousetrap.bindGlobal('esc', function() { this.showHotkeys();return false; }.bind(this));
	});

</editor-menu>