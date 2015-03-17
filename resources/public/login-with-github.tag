<login-with-github>
	<a onclick={opts.login} class="btn btn-block btn-social btn-github" if={! opts.loggedin && opts.login }>
		<i class="fa fa-github"></i>
		Sign in with Github
	</a>
	<a onclick={ opts.logout } class="btn btn-block" if={ opts.logout && opts.loggedin }>
		Logout
	</a>

</login-with-github>
