<editor-app>

    <style scoped>
    b.floater {
        z-index: 99;
        position: absolute;
        bottom: 10px;
        right: 10px;
    }   
    .glyphicon-remove {
        color:red;
    }
    .glyphicon-ok {
        color: green;
    }
    .editor-row {
        position: absolute;
        left:2px;
        right:2px;
        top:43px;
        bottom:35px;
    }
    </style>

    <rename-modal save={onRenameSave} 
        close={onRenameClose} 
        x={ this.showRename }
        title={opts.editor.getCurrentTemplate().title}></rename-modal>

    <div class="row">
        <editor-menu save={onSave} run={onRun} tidy={onTidy} 
            login={onLogin}
            logout={onLogout}
            loggedin={opts.accounts.loggedIn} 
            templates={opts.templates.templates}
            editor={opts.editor}
            titleclick={onRenameClick}
            title={currentTemplateTitle()} ></editor-menu>
    </div>

    <div class="row editor-row">
        <div class="row" style="height:100%;">
            <div class="col-sm-6" style="height:100%;padding:0px;">
                <template-editor editor={opts.editor} run={ onRun } save={ onSave } tidy={ onTidy }></template-editor>
                <div style='text-align:right'>
                <b class="floater"><span class="glyphicon glyphicon-console"></span></b>
                </div>
            </div>
            <div class="col-sm-6" style="height:100%;padding:0px;">
                <template-ro editor={opts.editor}></template-ro>
                <div style='text-align:right'>
                <b class="floater"><span class={this.getProcessedClasses()}></span></b>
                </div>
            </div>
        </div>
    </div>

    <hotkey-prompt>
        <kbd>esc</kbd> to show hotkeys
    </hotkey-prompt>


    currentTemplateTitle() {
        if (opts.editor.getCurrentTemplate().slug) {
            return opts.editor.getCurrentTemplate().title
        }
    }
    onLogout() {
        // console.debug("onLogout Clicked");
        opts.accounts.logout();
    }

    onLogin() {
        // console.debug("onLogin Clicked");
        opts.accounts.login();
    }

    onSave() {
        this.opts.editor.saveTemplate();
    }
    onRun() {
        this.opts.editor.process();
    }
    onTidy() {
        this.opts.editor.tidy();
    }

    onRenameClose() {
        this.showRename = false;
    }
    onRenameSave(newName) {
        // do rename here
        this.showRename = false;
        this.opts.editor.getCurrentTemplate().title = newName;
        this.opts.editor.saveTemplate();
    }
    onRenameClick() {
        this.showRename = true;
    }
    getProcessedClasses() {
        var cx = ['glyphicon'];
        if (this.opts.editor.templateOutput) {
            cx.push('glyphicon-ok');
        } else {
            cx.push('glyphicon-remove');
        }
        return cx.join(' ');
    }

    this.on('update', function(eventName) {
        // console.debug("editor-app update");
    }.bind(this));

    this.on('mount', function(eventName) {
        // console.debug("editor-app mount");
        this.open = false;
        this.showRename = false;
    }.bind(this));

</editor-app>

