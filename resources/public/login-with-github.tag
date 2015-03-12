<login-with-github>
	<a onclick={onLogin} class="btn btn-block btn-social btn-github" if={ ! opts.accounts.loggedIn }>
		<i class="fa fa-github"></i>
		Sign in with Github
	</a>
	<a onclick={ onLogout } class="btn btn-block" if={ opts.accounts.loggedIn }>
		Logout
	</a>

	onLogout() {
		console.debug("onLogout Clicked");
		opts.accounts.logout();
		return false;
	}
	onLogin() {
		console.debug("onLogin Clicked");
		opts.accounts.login();
		return false;
	}
</login-with-github>
