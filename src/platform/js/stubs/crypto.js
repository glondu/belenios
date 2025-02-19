// BELENIOS
// Copyright © 2025 Inria

// The following is an implementation of crypto operations needed by Belenios

if (window.crypto) {
  belenios.crypto = {
    getRandomBytes: function(len) {
      const buf = new Uint8Array(len);
      window.crypto.getRandomValues(buf);
      return buf;
    }
  }
  const subtle = window.crypto.subtle;
  if (subtle) {
    belenios.crypto.aesgcm = {
      encrypt: function(keyData, iv, data, onFulfilled, onRejected) {
        const params = { name: "AES-GCM", iv };
        subtle.importKey("raw", keyData, "AES-GCM", false, ["encrypt"])
          .then((key) => {
            subtle.encrypt(params, key, data).then(onFulfilled, onRejected)
          }, onRejected);
      },
      decrypt: function(keyData, iv, data, onFulfilled, onRejected) {
        const params = { name: "AES-GCM", iv };
        subtle.importKey("raw", keyData, "AES-GCM", false, ["decrypt"])
          .then((key) => {
            subtle.decrypt(params, key, data).then(onFulfilled, onRejected)
          }, onRejected);
      }
    }
  } else {
    window.alert("SubtleCrypto object is missing. You must use a more recent browser!");
  }
} else {
  window.alert("Crypto object is missing. You must use a more recent browser!");
}
