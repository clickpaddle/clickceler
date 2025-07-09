read_json_file_to_atom(File, Atom) :-
    setup_call_cleanup(
        open(File, read, Stream, [encoding(utf8)]),
        read_string(Stream, _, String),
        close(Stream)
    ),
    atom_string(Atom, String).

