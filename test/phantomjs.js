/*
 * Copyright (c) 2015 Lars Kuhtz <lakuhtz@gmail.com>
 */

var page = require('webpage').create();
var system = require('system');
var index = null;

if (system.args.length === 1) {
    index = 'https://rawgit.com/alephcloud/wai-cors/master/test/index.html'
} else if (system.args.length === 2) {
    index = 'file://' + system.args[1];
} else {
    console.log('Usage:');
    console.log('   phantomjs phanomjs.js <absolute-index-file-path>');
    console.log('   phantomjs phanomjs.js');
    phantom.exit(3);
}

page.open(index, function(status) {
  console.log("Status: " + status);
  if(status === "success") {
        console.log("test page loaded successfully");
  } else {
      console.log("failed to load test page");
      phantom.exit(2);
  }
});

page.onCallback = function(data) {
    if (data.failures > 0) {
        console.log("Some tests failed");
        console.log( page.plainText );
        phantom.exit(1);
    } else {
        console.log( page.plainText );
        phantom.exit(0);
    }
}

page.onConsoleMessage = function(msg) {
    system.stderr.writeLine('[CONSOLE] ' + msg);
};

