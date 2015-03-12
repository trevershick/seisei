<editor-app>
    <div class="row">
        <editor-menu editor={opts.editor} accounts={opts.accounts}></editor-menu>
    </div>
    <div class="row" style="height:75%">
        <div class="row" style="height:100%">
            <div class="col-sm-6" style="height:100%">
                <template-editor editor={opts.editor}></template-editor>
            </div>
            <div class="col-sm-6" style="height:100%">
                <template-ro editor={opts.editor}></template-ro>
            </div>
        </div>
    </div>
    this.on('update', function(eventName) {
        console.debug("editor-app update");
    }.bind(this));

    this.on('mount', function(eventName) {
        console.debug("editor-app mount");
    }.bind(this));
</editor-app>

