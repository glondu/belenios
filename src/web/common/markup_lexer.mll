(**************************************************************************)
(*                                BELENIOS                                *)
(*                                                                        *)
(*  Copyright © 2023-2023 Inria                                           *)
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

{
  open Markup_parser
}

rule text b = parse
| ("<br>" | "<b>" | "</b>" | "<i>" | "</i>")
{
  lexbuf.lex_curr_pos <- lexbuf.lex_curr_pos - Lexing.lexeme_end lexbuf + Lexing.lexeme_start lexbuf;
  TEXT (Buffer.contents b)
}
| eof { TEXT (Buffer.contents b) }
| "&amp;" { Buffer.add_char b '&'; text b lexbuf }
| "&lt;" { Buffer.add_char b '<'; text b lexbuf }
| "&gt;" { Buffer.add_char b '>'; text b lexbuf }
| _ as c { Buffer.add_char b c; text b lexbuf }

and token = parse
| "<br>" { BR }
| "<b>" { BOPEN }
| "</b>" { BCLOSE }
| "<i>" { IOPEN }
| "</i>" { ICLOSE }
| _ as c { let b = Buffer.create 128 in Buffer.add_char b c; text b lexbuf }
| eof { EOF }
