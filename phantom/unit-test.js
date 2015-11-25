/** borrowed from https://github.com/emezeske/lein-cljsbuild/tree/1.1.1/example-projects/advanced **/
var system = require('system');
var url,args;

if (phantom.version.major > 1) {
    args = system.args;
    if (args.length < 2) {
        system.stderr.write('Expected a target URL parameter.');
        phantom.exit(1);
    }
    url = args[1];
} else {
    args = phantom.args;
    if (args.length < 1) {
        system.stderr.write('Expected a target URL parameter.');
        phantom.exit(1);
    }
    url = args[0];
}

var page = require('webpage').create();

page.onConsoleMessage = function (message) {
    console.log(" > " + message);
};

console.log("Loading URL: " + url);

page.open(url, function (status) {
    if (status != "success") {
        console.log('Failed to open ' + url);
        phantom.exit(1);
    }

    var result = page.evaluate(function() {
      console.log("Running test.");
      return seisei.test.run();
    });

    // NOTE: PhantomJS 1.4.0 has a bug that prevents the exit codes
    //        below from being returned properly. :(
    //
    // http://code.google.com/p/phantomjs/issues/detail?id=294
    console.log("result is " + result);
    if (result != 0) {
        console.log("*** Test failed! ***");
        phantom.exit(1);
    }

    console.log("Test succeeded.");
    phantom.exit(0);
});
