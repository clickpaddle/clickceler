import json
from classes.error_event import ErrorEvent
from classes.info_event import InfoEvent

def load_events_from_json(filename):
    with open(filename, "r", encoding="utf-8") as f:
        raw_events = json.load(f)

    events = []
    for e in raw_events:
        etype = e.get("type", "").lower()
        if etype == "error":
            event = ErrorEvent(
                id=e["id"],
                message=e.get("message", ""),
                timestamp=e.get("timestamp", ""),
                severity=e.get("severity", "unknown"),
                error_code=e.get("error_code")
            )
        elif etype == "info":
            event = InfoEvent(
                id=e["id"],
                message=e.get("message", ""),
                timestamp=e.get("timestamp", ""),
                severity=e.get("severity", "unknown"),
                source=e.get("source", "")
            )
        else:
            # Ignorer les types inconnus
            continue
        events.append(event)
    return events
