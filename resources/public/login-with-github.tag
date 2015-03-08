<login-with-github>
	<a onclick={onLogin}>Login</a>

	onLogin() {
		console.log("onLogin Clicked");
		this.opts.state.login();
		return false;
	}
</login-with-github>