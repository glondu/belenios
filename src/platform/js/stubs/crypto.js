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
} else {
  window.alert("Crypto object is missing. You must use a more recent browser!");
}
