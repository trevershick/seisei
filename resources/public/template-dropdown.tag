<template-dropdown>
    <li each={opts.state.templates}><a href="#">{name}</a></li>

    this.opts.state.on('templates', this.update);
</template-dropdown>