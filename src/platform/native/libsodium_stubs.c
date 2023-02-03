/**************************************************************************/
/*                                BELENIOS                                */
/*                                                                        */
/*  Copyright © 2021-2023 Inria                                           */
/*                                                                        */
/*  This program is free software: you can redistribute it and/or modify  */
/*  it under the terms of the GNU Affero General Public License as        */
/*  published by the Free Software Foundation, either version 3 of the    */
/*  License, or (at your option) any later version, with the additional   */
/*  exemption that compiling, linking, and/or using OpenSSL is allowed.   */
/*                                                                        */
/*  This program is distributed in the hope that it will be useful, but   */
/*  WITHOUT ANY WARRANTY; without even the implied warranty of            */
/*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     */
/*  Affero General Public License for more details.                       */
/*                                                                        */
/*  You should have received a copy of the GNU Affero General Public      */
/*  License along with this program.  If not, see                         */
/*  <http://www.gnu.org/licenses/>.                                       */
/**************************************************************************/

#include <caml/mlvalues.h>
#include <sodium.h>

value belenios_libsodium_ed25519_bytes() {
  return Val_long(crypto_core_ed25519_BYTES);
}

value belenios_libsodium_ed25519_scalarbytes() {
  return Val_long(crypto_core_ed25519_SCALARBYTES);
}

value belenios_libsodium_ed25519_is_valid_point(value p) {
  return Val_int(crypto_core_ed25519_is_valid_point(Bytes_val(p)));
}

value belenios_libsodium_ed25519_scalarmult(value q, value n, value p) {
  return Val_int(crypto_scalarmult_ed25519_noclamp(Bytes_val(q), Bytes_val(n), Bytes_val(p)));
}

value belenios_libsodium_ed25519_add(value r, value p, value q) {
  return Val_int(crypto_core_ed25519_add(Bytes_val(r), Bytes_val(p), Bytes_val(q)));
}
