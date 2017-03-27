'use strict';

var raven = require('raven');

exports.newClientFFI = function(dsn) {
  return function() {
    return raven.Client(dsn);
  };
};

exports.captureErrorFFI = function(client) {
  return function(error) {
    return function(onError) {
      return function(onSuccess) {
        return function() {
          client.captureException(error, function(err, eventID) {
            if (err !== null) {
              onError(err);
              return;
            }
            onSuccess(eventID);
          });
        };
      };
    };
  };
};

exports.captureMessageFFI = function(client) {
  return function(message) {
    return function(onError) {
      return function(onSuccess) {
        return function() {
          client.captureMessage(message, function(err, eventID) {
            if (err !== null) {
              onError(err);
              return;
            }
            onSuccess(eventID);
          });
        };
      };
    };
  };
};
