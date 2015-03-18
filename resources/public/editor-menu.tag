<editor-menu>
    <style scoped>
    .dropdown-menu {
        max-height:350px;
        overflow-y:auto;
    }
    </style>
    <hotkeys show={ this.showhk }></hotkeys>

	<nav class="navbar navbar-default">
      <div class="container-fluid">
        <div class="navbar-header">
            <a class="navbar-brand" href="#">生成</a>
        </div>

        <div class="collapse navbar-collapse" id="bs-example-navbar-collapse-1">
        <div>
            <ul class="nav navbar-nav">
                <li onClick={opts.new} if={ opts.shownew && _.isFunction(opts.new) }><a href="#"><span class="glyphicon glyphicon-plus"></span> N<u>e</u>w</a></li>
                <li onClick={opts.save} if={ _.isFunction(opts.save) && opts.loggedin }><a href="#"><span class="glyphicon glyphicon-pencil"></span> <u>S</u>ave</a></li>
                <li><a href="#" onClick={opts.run} if={ _.isFunction(opts.run) }><span class="glyphicon glyphicon-play"></span> <u>R</u>un</a></li>
                <li><a href="#" onClick={ opts.tidy } if={ _.isFunction(opts.tidy) }><span class="glyphicon glyphicon-indent-left"></span> T<u>i</u>dy</a></li>
                <li><a href="#" onClick={ opts.delete } if={ opts.showdelete &&  _.isFunction(opts.delete) }><span class="glyphicon glyphicon-delete"></span> <u>D</u>elete</a></li>
                <li>
				 	<a if={items.length > 0} href="#" class="dropdown-toggle" data-toggle="dropdown" role="button" aria-expanded="false">Templates <span class="caret"></span></a>
				    <ul if={items.length > 0} class="dropdown-menu" role="menu">
						<li each={items}><a href="#template/{slug}">{title}</a></li>
					</ul>
                </li>
            </ul>

            <ul class="nav navbar-nav navbar-right">
                <li><a href="#" onClick={ opts.feedback } if={ _.isFunction(opts.feedback) }><span class="glyphicon glyphicon-feedback"></span> Feedback</a></li>
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
        Mousetrap.bindGlobal('mod+e', function() { (opts.showdelete &&  _.isFunction(opts.new)) && this.opts.new();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+d', function() { (opts.showdelete &&  _.isFunction(opts.delete)) && this.opts.delete();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+s', function() { !this.showhk && this.opts.save();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+r', function() { !this.showhk && this.opts.run();return false; }.bind(this));
        Mousetrap.bindGlobal('mod+i', function() { !this.showhk && this.opts.tidy();return false; }.bind(this));
        Mousetrap.bindGlobal('esc', function() { this.showHotkeys();return false; }.bind(this));
	});

</editor-menu>