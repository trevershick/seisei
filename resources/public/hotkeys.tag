<hotkeys>
	
	<style scoped>
  .backdrop {
    background-color: black;
    opacity: 0.9;
    position: absolute;
    top: 0;
    right: 0;
    bottom: 0;
    left: 0;
    z-index: 1000;
  }
  kbd {
    min-width:25px;
  }
  kbd.cmd {
    width: 40px;
  }
  kbd {
    margin-right:5px;
    margin-left:5px;
  }
  dl {
    display: block;
    margin-top: 20%;
  }
  ul {
    margin-top: 5%;
  }
  li {
    width: 30%;
    margin-left: 10%;
    margin-top: 20px;
    font-weight: bold;
    display: inline-block;
  }
  li p {
    display: inline-block;
    position: absolute;
  }
  dd {
    display: inline-block;
    max-wilih: 30%;
    border: 1px solid yellow;
    margin-left: 20px;
  }
  .kk {
    margin-right: 20px;
  }


  	</style>
	<div class="backdrop">
		<div class="hotkeys">	
			<ul>
				<li><span class="kk"><kbd class="light cmd">&#8984;</kbd> + <kbd class="light">i</kbd></span><p>Tidy up your JSON</p></li>
				
				<li><span class="kk"><kbd class="light cmd">&#8984;</kbd> + <kbd class="light">r</kbd></span><p>Run your template on the Server</p></li>
				
				<li><span class="kk"><kbd class="light cmd">&#8984;</kbd> + <kbd class="light">s</kbd></span><p>Save your template on the server</p></li>

        <li><span class="kk"><kbd class="light cmd">&#8984;</kbd> + <kbd class="light">e</kbd></span><p>Create a new template</p></li>
				
        <li><span class="kk"><kbd class="light cmd">&#8984;</kbd> + <kbd class="light">s</kbd></span><p>Delete the current template</p></li>

			</ul>
		</div>
		<hotkey-prompt>
			<kbd>esc</kbd> to dismiss
		</hotkey-prompt>

	</div>


</hotkeys>