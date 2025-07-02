import json
import subprocess

# --- Classes Python avec héritage et sévérité ---
class Event:
    def __init__(self, id, type_, message, timestamp, severity):
        self.id = id
        self.type = type_
        self.message = message
        self.timestamp = timestamp
        self.severity = severity

class ErrorEvent(Event):
    def __init__(self, id, message, timestamp, severity, error_code):
        super().__init__(id, 'ERROR', message, timestamp, severity)
        self.error_code = error_code

class InfoEvent(Event):
    def __init__(self, id, message, timestamp, severity, source):
        super().__init__(id, 'INFO', message, timestamp, severity)
        self.source = source

# --- Lecture JSON vers objets Python ---
def load_events_from_json(file_path):
    with open(file_path, 'r', encoding='utf-8') as f:
        data = json.load(f)
    events = []
    for item in data:
        if item["type"] == "ERROR":
            events.append(ErrorEvent(
                item["id"], item["message"], item["timestamp"],
                item["severity"], item["error_code"]
            ))
        elif item["type"] == "INFO":
            events.append(InfoEvent(
                item["id"], item["message"], item["timestamp"],
                item["severity"], item["source"]
            ))
    return events

# --- Génération de faits Prolog ---
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

# --- Appel à Prolog ---
def run_prolog_query(facts_file, rules_file, query):
    prolog_script = f"""
    ['{facts_file}'],
    ['{rules_file}'],
    findall(ID, {query}, Result),
    writeln(Result),
    halt.
    """
    result = subprocess.run(['swipl', '-q'], input=prolog_script.encode(), capture_output=True)
    return result.stdout.decode().strip()

# --- Main ---
def main():
    events = load_events_from_json("events.json")
    write_prolog_facts(events, "events_facts.pl")

    # Requête : événements de sévérité critique
    result = run_prolog_query("events_facts.pl", "rules.pl", "is_critical(ID)")
    print("Critical events:", result)

if __name__ == "__main__":
    main()

