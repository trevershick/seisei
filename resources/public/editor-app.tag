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
        left:0px;
        right:0px;
        top:40px;
        bottom:35px;
    }
    .left-side {
        border: 1px solid rgb(78, 93, 108);
    }
    .right-side {
        border: 1px solid rgb(78, 93, 108); 
    }
    </style>

    <confirm-dialog 
        showConfirm={ this.showConfirm } 
        callback={ onConfirm } 
        title={ confirmTitle }
        message={ confirmMessage} ></confirm-dialog>

    <rename-modal save={onRenameSave} 
        close={onRenameClose} 
        x={ this.showRename }
        title={opts.editor.getCurrentTemplate().title}></rename-modal>

    <div class="row">
        <editor-menu save={ onSave } run={ onRun } tidy={ onTidy } 
            showDelete={ this.hasSavedTemplate }
            delete={ onDelete }
            new={ onNew }
            publish={ onPublish }
            showNew={ this.hasSavedTemplate }
            feedback={ onFeedback }
            login={onLogin}
            logout={onLogout}
            loggedin={opts.accounts.loggedIn} 
            templates={opts.templates.templates}
            editor={opts.editor}
            titleclick={onRenameClick}
            help={onHelp}
            title={currentTemplateTitle()} ></editor-menu>
    </div>

    <div class="row editor-row">
        <div class="row" style="height:100%;">
            <div class="col-sm-6 left-side" style="height:100%;padding:0px;">
                <template-editor editor={opts.editor} run={ onRun } save={ onSave } tidy={ onTidy }></template-editor>
                <div style='text-align:right'>
                <b class="floater"><span class={this.getProcessedClasses()}></span></b>
                </div>
            </div>
            <div class="col-sm-6 right-side" style="height:100%;padding:0px;">
                <template-ro editor={opts.editor}></template-ro>
                <editor-help onsampleclick={onSampleClick}></editor-help>
            </div>
        </div>
    </div>
    <div style="position:absolute;bottom:0;left:0;right:0;margin-left:20px;">
        <tweet></tweet>
    </div>
    <hotkey-prompt>
        <kbd>esc</kbd> to show hotkeys
    </hotkey-prompt>


    currentTemplateTitle() {
        if (opts.editor.getCurrentTemplate().slug) {
            return opts.editor.getCurrentTemplate().title
        }
    }

    onSampleClick(obj) {
        opts.editor.setTemplateContent(JSON.stringify(obj.input, null, 2));
    }

    onLogout() {
        opts.accounts.logout();
    }

    onLogin() {
        opts.accounts.login();
    }

    onSave() {
        this.opts.editor.saveTemplate();
    }

    onPublish() {
        this.opts.editor.publishTemplate();
    }

    onDelete() {
        this.confirm(function(yesOrNo) {
            if (yesOrNo) {
                this.opts.editor.deleteTemplate();
            }
        }.bind(this), "Are you sure you want to delete " + this.currentTemplateTitle() + " ?", "Delete your template?");
        riot.update(this);
    }

    onHelp() {
        this.showHelp = !this.showHelp;
        riot.update(this);

    }

    onNew() {
        this.opts.editor.newTemplate();
    }

    onFeedback() {
        window.open("https://github.com/trevershick/seisei/issues","_blank");
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

    confirm(callback, msg, title) {
        this.confirmTitle = title;
        this.confirmMessage = msg;
        this.confirmcallback = callback;
        this.showConfirm = true;
    }

    onConfirm(trueOrFalse) {
        this.showConfirm = false;
        this.confirmcallback(trueOrFalse);
    }

    this.on('update', function(eventName) {
        this.hasSavedTemplate = this.opts.editor.isTemplateSaved();
    }.bind(this));

    this.on('mount', function(eventName) {
        this.open = false;
        this.showRename = false;
        this.showConfirm = false;
        this.showHelp = false;
    }.bind(this));

</editor-app>

