var configure = require('@veupathdb/site-webpack-config');

module.exports = configure({
  entry: {
    'site-client': './webapp/wdkCustomization/js/client/main.js'
  }
});
