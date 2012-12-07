open Eliom_service
open Eliom_parameter

let heliosvotingorg_s = external_service
  ~prefix:"http://heliosvoting.org"
  ~path:[]
  ~get_params:unit
  ()

let home_s = service
  ~path:[]
  ~get_params:unit
  ()
