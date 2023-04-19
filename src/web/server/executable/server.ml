let () =
  Eliom_service.register_eliom_module "belenios" Belenios_server.Web_main.main

let () =
  let alt_msg =
    "Alternate config file (default " ^ Ocsigen_config.get_config_file () ^ ")"
  and silent_msg = "Silent mode (error messages in errors.log only)"
  and pid_msg = "Specify a file where to write the PIDs of servers"
  and daemon_msg = "Daemon mode (detach the process)"
  and verbose_msg = "Verbose mode"
  and debug_msg = "Extremely verbose mode (debug)"
  and version_msg = "Display version number and exit" in
  try
    Arg.parse_argv Sys.argv
      [
        ("-c", Arg.String Ocsigen_config.set_configfile, alt_msg);
        ("--config", Arg.String Ocsigen_config.set_configfile, alt_msg);
        ("-s", Arg.Unit Ocsigen_config.set_silent, silent_msg);
        ("--silent", Arg.Unit Ocsigen_config.set_silent, silent_msg);
        ("-p", Arg.String Ocsigen_config.set_pidfile, pid_msg);
        ("--pidfile", Arg.String Ocsigen_config.set_pidfile, pid_msg);
        ("-v", Arg.Unit Ocsigen_config.set_verbose, verbose_msg);
        ("--verbose", Arg.Unit Ocsigen_config.set_verbose, verbose_msg);
        ("--debug", Arg.Unit Ocsigen_config.set_debug, debug_msg);
        ("-d", Arg.Unit Ocsigen_config.set_daemon, daemon_msg);
        ("--daemon", Arg.Unit Ocsigen_config.set_daemon, daemon_msg);
        ("--version", Arg.Unit Ocsigen_config.display_version, version_msg);
      ]
      (fun _ -> ())
      "usage: belenios-server [-c configfile]"
  with Arg.Help s ->
    print_endline s;
    exit 0

let () = Ocsigen_server.start ~config:(Ocsigen_parseconfig.parse_config ()) ()
