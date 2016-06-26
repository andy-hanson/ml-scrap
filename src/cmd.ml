open Core.Std

let run_command: Command.t =
	Command.basic ~summary:"RUNS A COMMAND"
		Command.Spec.(
			empty
			+> flag "-s" (optional string) ~doc:"string Your favorite letter"
			+> anon ("filename" %: file)
		)
		@@ fun use_string filename () ->
			OutputU.printf "%s   %s\n" (match use_string with | Some x -> x | None -> "Y") filename

let poop_command: Command.t =
	Command.basic ~summary: "DIFF"
		Command.Spec.(
			empty
		)
		@@ fun () ->
			OutputU.printf "%s\n" "ONE HUNDRED!"

let command =
  Command.group
    ~summary:"NOZE"
    ~readme:(fun () -> "readme.md")
		["run", run_command; "poop", poop_command]

let () =
  Command.run ~version:"0.0.0" command
