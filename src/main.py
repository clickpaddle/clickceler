from events.loader import load_events_from_json

def write_prolog_facts(events, filename):
    with open(filename, 'w', encoding='utf-8') as f:
        for event in events:
            fact = event.to_prolog_fact()
            f.write(fact + "\n")

if __name__ == "__main__":
    events = load_events_from_json("data/events.json")
    write_prolog_facts(events, "rules/facts.pl")
    print(f"{len(events)} événements traités et convertis en faits Prolog.")
