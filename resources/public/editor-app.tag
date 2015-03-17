<editor-app>

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

    <div class="row" style="height:75%">
        <div class="row" style="height:100%;">
            <div class="col-sm-6" style="height:100%;padding:0px;">
                <template-editor editor={opts.editor}></template-editor>
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


    currentTemplateTitle() {
        if (opts.editor.getCurrentTemplate().slug) {
            return opts.editor.getCurrentTemplate().title
        }
    }
    onLogout() {
        console.debug("onLogout Clicked");
        opts.accounts.logout();
    }

    onLogin() {
        console.debug("onLogin Clicked");
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
        console.debug("editor-app update");
    }.bind(this));

    this.on('mount', function(eventName) {
        console.debug("editor-app mount");
        this.open = false;
        this.showRename = false;
    }.bind(this));

</editor-app>

