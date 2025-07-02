import json
from pyswip import Prolog
from classes.loader import load_events_from_json

def write_prolog_facts(events, filename):
    """
    Écrit les faits Prolog dans un fichier au format event(ID, _{...}).
    """
    def format_prolog_dict(event):
        parts = []
        for key, value in event.items():
            if isinstance(value, str):
                val_str = f'"{value}"'
            elif isinstance(value, bool):
                val_str = "true" if value else "false"
            else:
                val_str = str(value)
            parts.append(f"{key}:{val_str}")
        return "_{" + ", ".join(parts) + "}"

    with open(filename, 'w', encoding='utf-8') as f:
        for event in events:
            eid = event.id if hasattr(event, 'id') else event.get('id')
            # retirer 'id' du dict avant conversion
            event_dict = event if isinstance(event, dict) else event.__dict__
            event_body = {k: v for k, v in event_dict.items() if k != "id"}
            dict_str = format_prolog_dict(event_body)
            fact = f"event({eid}, {dict_str})."
            f.write(fact + "\n")

def load_prolog_facts(prolog, filename):
    """
    Appelle la règle Prolog load_facts pour charger le fichier facts.pl.
    """
    try:
        list(prolog.query(f"load_facts('{filename}')"))
        print(f"✅ Faits chargés depuis {filename}")
    except Exception as e:
        print(f"❌ Erreur lors du chargement des faits Prolog: {e}")

def main():
    # Charger les événements JSON via ta fonction (adaptée)
    events = load_events_from_json("data/list_of_events.json")
    print(f"{len(events)} événements chargés depuis JSON.")

    # Écrire les faits dans un fichier
    fact_file = "rules/facts.pl"
    write_prolog_facts(events, fact_file)
    print(f"Faits Prolog écrits dans {fact_file}")

    # Initialiser Prolog et charger les règles
    prolog = Prolog()
    prolog.consult("rules/rules.pl")
    print("Règles Prolog chargées.")

    # Charger les faits via la règle load_facts/1
    load_prolog_facts(prolog, fact_file)

    # Exemple : récupérer et afficher tous les faits event/2
    results = list(prolog.query("event(Id, Event)"))
    print(f"{len(results)} faits event/2 lus depuis Prolog :")
    for r in results:
        print(r)

if __name__ == "__main__":
    main()

