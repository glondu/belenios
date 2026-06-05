(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2026 VCAST                                                *)
(*                                                                        *)
(*  This program is free software: you can redistribute it and/or modify  *)
(*  it under the terms of the GNU Affero General Public License as        *)
(*  published by the Free Software Foundation, either version 3 of the    *)
(*  License, or (at your option) any later version, with the additional   *)
(*  exemption that compiling, linking, and/or using OpenSSL is allowed.   *)
(*                                                                        *)
(*  This program is distributed in the hope that it will be useful, but   *)
(*  WITHOUT ANY WARRANTY; without even the implied warranty of            *)
(*  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU     *)
(*  Affero General Public License for more details.                       *)
(*                                                                        *)
(*  You should have received a copy of the GNU Affero General Public      *)
(*  License along with this program.  If not, see                         *)
(*  <http://www.gnu.org/licenses/>.                                       *)
(**************************************************************************)

open Belenios_core.Crypto_std

module Expand_message = struct
  type vector = { msg : string; len_in_bytes : int; uniform_bytes : string }

  type vectors = {
    name : string;
    dst : string;
    hash : (module HASH_FUNCTION);
    vectors : vector list;
  }

  let test_vectors { name; dst; hash; vectors } =
    let module H = (val hash) in
    let module E = Expand_message (H) in
    let expand_message =
      match name with
      | "expand_message_xmd" -> E.expand_message_xmd
      | _ -> invalid_arg __FUNCTION__
    in
    let test_vector { msg; len_in_bytes; uniform_bytes } =
      let (`Hex uniform_bytes') =
        expand_message ~dst msg len_in_bytes |> Hex.of_string
      in
      if uniform_bytes' <> uniform_bytes then
        Printf.ksprintf failwith "%s: %s(%s): %s" __FUNCTION__ name dst msg
    in
    List.iter test_vector vectors;
    Printf.printf "%s(%s): %d tests successful\n" name dst (List.length vectors)

  let rfc9380_k1 =
    {
      name = "expand_message_xmd";
      dst = "QUUX-V01-CS02-with-expander-SHA256-128";
      hash = (module SHA256);
      vectors =
        [
          {
            msg = "";
            len_in_bytes = 0x20;
            uniform_bytes =
              "68a985b87eb6b46952128911f2a4412bbc302a9d759667f87f7a21d803f07235";
          };
          {
            msg = "abc";
            len_in_bytes = 0x20;
            uniform_bytes =
              "d8ccab23b5985ccea865c6c97b6e5b8350e794e603b4b97902f53a8a0d605615";
          };
          {
            msg = "abcdef0123456789";
            len_in_bytes = 0x20;
            uniform_bytes =
              "eff31487c770a893cfb36f912fbfcbff40d5661771ca4b2cb4eafe524333f5c1";
          };
          {
            msg =
              "q128_qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq";
            len_in_bytes = 0x20;
            uniform_bytes =
              "b23a1d2b4d97b2ef7785562a7e8bac7eed54ed6e97e29aa51bfe3f12ddad1ff9";
          };
          {
            msg =
              "a512_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
            len_in_bytes = 0x20;
            uniform_bytes =
              "4623227bcc01293b8c130bf771da8c298dede7383243dc0993d2d94823958c4c";
          };
          {
            msg = "";
            len_in_bytes = 0x80;
            uniform_bytes =
              "af84c27ccfd45d41914fdff5df25293e221afc53d8ad2ac06d5e3e29485dadbee0d121587713a3e0dd4d5e69e93eb7cd4f5df4cd103e188cf60cb02edc3edf18eda8576c412b18ffb658e3dd6ec849469b979d444cf7b26911a08e63cf31f9dcc541708d3491184472c2c29bb749d4286b004ceb5ee6b9a7fa5b646c993f0ced";
          };
          {
            msg = "abc";
            len_in_bytes = 0x80;
            uniform_bytes =
              "abba86a6129e366fc877aab32fc4ffc70120d8996c88aee2fe4b32d6c7b6437a647e6c3163d40b76a73cf6a5674ef1d890f95b664ee0afa5359a5c4e07985635bbecbac65d747d3d2da7ec2b8221b17b0ca9dc8a1ac1c07ea6a1e60583e2cb00058e77b7b72a298425cd1b941ad4ec65e8afc50303a22c0f99b0509b4c895f40";
          };
          {
            msg = "abcdef0123456789";
            len_in_bytes = 0x80;
            uniform_bytes =
              "ef904a29bffc4cf9ee82832451c946ac3c8f8058ae97d8d629831a74c6572bd9ebd0df635cd1f208e2038e760c4994984ce73f0d55ea9f22af83ba4734569d4bc95e18350f740c07eef653cbb9f87910d833751825f0ebefa1abe5420bb52be14cf489b37fe1a72f7de2d10be453b2c9d9eb20c7e3f6edc5a60629178d9478df";
          };
          {
            msg =
              "q128_qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq";
            len_in_bytes = 0x80;
            uniform_bytes =
              "80be107d0884f0d881bb460322f0443d38bd222db8bd0b0a5312a6fedb49c1bbd88fd75d8b9a09486c60123dfa1d73c1cc3169761b17476d3c6b7cbbd727acd0e2c942f4dd96ae3da5de368d26b32286e32de7e5a8cb2949f866a0b80c58116b29fa7fabb3ea7d520ee603e0c25bcaf0b9a5e92ec6a1fe4e0391d1cdbce8c68a";
          };
          {
            msg =
              "a512_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
            len_in_bytes = 0x80;
            uniform_bytes =
              "546aff5444b5b79aa6148bd81728704c32decb73a3ba76e9e75885cad9def1d06d6792f8a7d12794e90efed817d96920d728896a4510864370c207f99bd4a608ea121700ef01ed879745ee3e4ceef777eda6d9e5e38b90c86ea6fb0b36504ba4a45d22e86f6db5dd43d98a294bebb9125d5b794e9d2a81181066eb954966a487";
          };
        ];
    }

  let rfc9380_k2 =
    {
      name = "expand_message_xmd";
      dst =
        "QUUX-V01-CS02-with-expander-SHA256-128-long-DST-1111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111111";
      hash = (module SHA256);
      vectors =
        [
          {
            msg = "";
            len_in_bytes = 0x20;
            uniform_bytes =
              "e8dc0c8b686b7ef2074086fbdd2f30e3f8bfbd3bdf177f73f04b97ce618a3ed3";
          };
          {
            msg = "abc";
            len_in_bytes = 0x20;
            uniform_bytes =
              "52dbf4f36cf560fca57dedec2ad924ee9c266341d8f3d6afe5171733b16bbb12";
          };
          {
            msg = "abcdef0123456789";
            len_in_bytes = 0x20;
            uniform_bytes =
              "35387dcf22618f3728e6c686490f8b431f76550b0b2c61cbc1ce7001536f4521";
          };
          {
            msg =
              "q128_qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq";
            len_in_bytes = 0x20;
            uniform_bytes =
              "01b637612bb18e840028be900a833a74414140dde0c4754c198532c3a0ba42bc";
          };
          {
            msg =
              "a512_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
            len_in_bytes = 0x20;
            uniform_bytes =
              "20cce7033cabc5460743180be6fa8aac5a103f56d481cf369a8accc0c374431b";
          };
          {
            msg = "";
            len_in_bytes = 0x80;
            uniform_bytes =
              "14604d85432c68b757e485c8894db3117992fc57e0e136f71ad987f789a0abc287c47876978e2388a02af86b1e8d1342e5ce4f7aaa07a87321e691f6fba7e0072eecc1218aebb89fb14a0662322d5edbd873f0eb35260145cd4e64f748c5dfe60567e126604bcab1a3ee2dc0778102ae8a5cfd1429ebc0fa6bf1a53c36f55dfc";
          };
          {
            msg = "abc";
            len_in_bytes = 0x80;
            uniform_bytes =
              "1a30a5e36fbdb87077552b9d18b9f0aee16e80181d5b951d0471d55b66684914aef87dbb3626eaabf5ded8cd0686567e503853e5c84c259ba0efc37f71c839da2129fe81afdaec7fbdc0ccd4c794727a17c0d20ff0ea55e1389d6982d1241cb8d165762dbc39fb0cee4474d2cbbd468a835ae5b2f20e4f959f56ab24cd6fe267";
          };
          {
            msg = "abcdef0123456789";
            len_in_bytes = 0x80;
            uniform_bytes =
              "d2ecef3635d2397f34a9f86438d772db19ffe9924e28a1caf6f1c8f15603d4028f40891044e5c7e39ebb9b31339979ff33a4249206f67d4a1e7c765410bcd249ad78d407e303675918f20f26ce6d7027ed3774512ef5b00d816e51bfcc96c3539601fa48ef1c07e494bdc37054ba96ecb9dbd666417e3de289d4f424f502a982";
          };
          {
            msg =
              "q128_qqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqqq";
            len_in_bytes = 0x80;
            uniform_bytes =
              "ed6e8c036df90111410431431a232d41a32c86e296c05d426e5f44e75b9a50d335b2412bc6c91e0a6dc131de09c43110d9180d0a70f0d6289cb4e43b05f7ee5e9b3f42a1fad0f31bac6a625b3b5c50e3a83316783b649e5ecc9d3b1d9471cb5024b7ccf40d41d1751a04ca0356548bc6e703fca02ab521b505e8e45600508d32";
          };
          {
            msg =
              "a512_aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";
            len_in_bytes = 0x80;
            uniform_bytes =
              "78b53f2413f3c688f07732c10e5ced29a17c6a16f717179ffbe38d92d6c9ec296502eb9889af83a1928cd162e845b0d3c5424e83280fed3d10cffb2f8431f14e7a23f4c68819d40617589e4c41169d0b56e0e3535be1fd71fbb08bb70c5b5ffed953d6c14bf7618b35fc1f4c4b30538236b4b08c9fbf90462447a8ada60be495";
          };
        ];
    }

  let () = List.iter test_vectors [ rfc9380_k1; rfc9380_k2 ]
end

module Pbkdf2 = struct
  type vector = {
    password : string;
    salt : string;
    iterations : int;
    dkLen : int;
    output : string;
  }

  type vectors = {
    name : string;
    prf : (module PSEUDO_RANDOM_FUNCTION);
    vectors : vector list;
  }

  let test_vectors { name; prf; vectors } =
    let module H = (val prf) in
    let module P = Pbkdf2 (H) in
    let test_vector i { password; salt; iterations; dkLen; output } =
      let (`Hex computed) =
        P.pbkdf2 ~iterations ~salt password dkLen |> Hex.of_string
      in
      if computed <> output then
        Printf.ksprintf failwith "%s: %s(%d)" __FUNCTION__ name i
    in
    List.iteri test_vector vectors;
    Printf.printf "%s: %d tests successful\n" name (List.length vectors)

  let rfc6070 =
    {
      name = "RFC6070";
      prf = (module HMAC_SHA1);
      vectors =
        [
          {
            password = "password";
            salt = "salt";
            iterations = 1;
            dkLen = 20;
            output = "0c60c80f961f0e71f3a9b524af6012062fe037a6";
          };
          {
            password = "password";
            salt = "salt";
            iterations = 2;
            dkLen = 20;
            output = "ea6c014dc72d6f8ccd1ed92ace1d41f0d8de8957";
          };
          {
            password = "password";
            salt = "salt";
            iterations = 4096;
            dkLen = 20;
            output = "4b007901b765489abead49d926f721d065a429c1";
          };
          {
            password = "password";
            salt = "salt";
            iterations = 16777216;
            dkLen = 20;
            output = "eefe3d61cd4da4e4e9945b3d6ba2158c2634e984";
          };
          {
            password = "passwordPASSWORDpassword";
            salt = "saltSALTsaltSALTsaltSALTsaltSALTsalt";
            iterations = 4096;
            dkLen = 25;
            output = "3d2eec4fe41c849b80c8d83662c0e44a8b291a964cf2f07038";
          };
          {
            password = "pass\000word";
            salt = "sa\000lt";
            iterations = 4096;
            dkLen = 16;
            output = "56fa6aa75548099dcc37d7f03425e0c3";
          };
        ];
    }

  let () = List.iter test_vectors [ rfc6070 ]
end
