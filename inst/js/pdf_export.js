var casper = require("casper").create();
casper.start();

casper.page.paperSize = {
  width: '21cm',
  height: '29.7cm',
  orientation: 'portrait',
  border: '3mm'
};

var utils = require('utils');

casper.thenOpen(casper.cli.raw.get("htmlPath"), function() {
  this.capture(casper.cli.raw.get("pdfPath"));
  this.echo('Created pdf.');
});

casper.run();


