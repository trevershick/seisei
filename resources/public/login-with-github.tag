<login-with-github>
	<a onclick={onLogin} class="btn btn-block btn-social btn-github" if={ !loggedIn }>
		<i class="fa fa-github"></i>
		Sign in with Github
	</a>

	this.on('update', function(){
		this.loggedIn = this.opts.state.loggedIn;
		console.log("login-with-github on update", this.loggedIn);
	}.bind(this));


	this.opts.state.on('received-account received-error', function(eventName) {
		console.log("login-with-github on " + eventName);
		this.update();
	}.bind(this));


	onLogin() {
		console.log("onLogin Clicked");
		this.opts.state.login();
		return false;
	}
</login-with-github>
