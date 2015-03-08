<login-with-github>

	<a onclick={onLogin} class="btn btn-block btn-social btn-github">
		<i class="fa fa-github"></i>
		Sign in with Github
	</a>

	onLogin() {
		console.log("onLogin Clicked");
		this.opts.state.login();
		return false;
	}
</login-with-github>
