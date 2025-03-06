// BELENIOS
// Copyright © 2025 Inria

// The following is an implementation of crypto operations needed by Belenios

if (crypto) {
  belenios.crypto = {
    getRandomBytes: function(len) {
      const buf = new Uint8Array(len);
      crypto.getRandomValues(buf);
      return buf;
    }
  }
  const subtle = crypto.subtle;
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
    const msg = "SubtleCrypto object is missing. You must use a more recent browser!";
    if (window) {
      window.alert(msg);
    } else {
      console.log(msg);
    }
  }
} else {
  const msg = "Crypto object is missing. You must use a more recent browser!";
  if (window) {
    window.alert(msg);
  } else {
    console.log(msg);
  }
}
