from events.base import ErrorEvent, InfoEvent

def write_prolog_facts(events, filename):
    with open(filename, 'w', encoding='utf-8') as f:
        for e in events:
            e_type = e.type.lower() + "_event"
            msg = e.message.replace("'", "\\'")
            ts = e.timestamp.replace("'", "\\'")
            sev = e.severity.lower()
            f.write(f"event({e.id}, {e_type}, '{msg}', '{ts}').\n")
            f.write(f"severity({e.id}, {sev}).\n")
            if isinstance(e, ErrorEvent):
                f.write(f"error_code({e.id}, {e.error_code}).\n")
            elif isinstance(e, InfoEvent):
                src = e.source.replace("'", "\\'")
                f.write(f"info_source({e.id}, '{src}').\n")

