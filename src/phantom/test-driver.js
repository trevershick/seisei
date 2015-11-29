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
/* wait for a callback (from inside our evaluate function below to
  notify when the test suite is complete */
page.onCallback = function(result) {
  if (result != 0) {
    console.log("*** Test failed! ***");
  } else {
    console.log("Test succeeded.");
  }
  phantom.exit(result);
};

page.open(url, function (status) {
    if (status != "success") {
        console.log('Failed to open ' + url);
        phantom.exit(1);
    }

    page.evaluate(function() {
      console.log("Running test.");
      /* The callback passed in is called when the test suite is finished.
      So we then call window.callPhantom which invokes our page.onCallback method
      above */
      seisei.test.run(function(result) {
        window.callPhantom(result);
      });
    });
});
