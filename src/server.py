import socket
import threading
import json
import struct
from pyswip import Prolog

HOST = '127.0.0.1'
PORT = 65432

prolog = Prolog()
try:
    success = prolog.consult('rules/rules.pl')
    print("Consult success:", success)
except Exception as e:
    print("Erreur consult:", e)

# Vérifie si critical_event/2 est bien défini
for sol in prolog.query("current_predicate(critical_event/2)"):
    print("critical_event/2 existe dans la base.")

events = []
lock = threading.Lock()  # Pour l'accès concurrent

def format_prolog_dict(event):
    """
    Convertit un dictionnaire Python en _{key:value, ...}.
    Chaînes entourées de guillemets sauf pour les atomes connus.
    """
    parts = []
    for key, value in event.items():
        if isinstance(value, str):
            if key in ["message", "timestamp"]:
                val_str = f'"{value}"'
            else:
                val_str = value.lower().replace(" ", "_")
        elif isinstance(value, bool):
            val_str = "true" if value else "false"
        else:
            val_str = str(value)
        parts.append(f"{key}:{val_str}")
    return "_{" + ", ".join(parts) + "}"

def add_event_to_prolog(event):
    """
    Ajoute un fait Prolog sous la forme : event(ID, _{...}).
    """
    with lock:
        eid = event.get("id")
        if eid is None:
            print("Erreur : l'événement n'a pas de champ 'id'")
            return

        # Retirer 'id' du dict
        event_body = {k: v for k, v in event.items() if k != "id"}
        dict_str = format_prolog_dict(event_body)
        fact = f"event({eid}, {dict_str})"

        try:
            prolog.assertz(fact)
            print(f"✅ Ajouté dans Prolog : {fact}")

            # Vérification immédiate
            result = list(prolog.query("event(ID, Evt)"))
            if result:
                print(f"📌 Faits event/2 enregistrés : {len(result)}. Exemple : {result[0]}")
            else:
                print("⚠️ Aucun fait event/2 trouvé après insertion.")
        except Exception as e:
            print(f"❌ Erreur lors de l'insertion Prolog : {e}")
            print(f"⛔ Fait problématique : {fact}")

def execute_prolog_query(query_str):
    """
    Exécute une query Prolog.
    """
    try:
        with lock:
            results = list(prolog.query(query_str))
            return True, results
    except Exception as e:
        return False, f"Erreur Prolog : {str(e)}"

def process_message(message: str) -> str:
    if message.startswith("EVENT "):
        json_part = message[len("EVENT "):].strip()
        try:
            parsed = json.loads(json_part)
            if isinstance(parsed, list):
                for event in parsed:
                    if not isinstance(event, dict):
                        return "ERROR: Chaque élément doit être un objet JSON"
                    add_event_to_prolog(event)
                return f"OK: {len(parsed)} événements ajoutés"
            elif isinstance(parsed, dict):
                add_event_to_prolog(parsed)
                return "OK: 1 événement ajouté"
            else:
                return "ERROR: JSON doit être un objet ou une liste d'objets"
        except json.JSONDecodeError:
            return "ERROR: JSON invalide"

    elif message.startswith("QUERY "):
        query_str = message[len("QUERY "):].strip()
        success, results_or_error = execute_prolog_query(query_str)
        if success:
            if results_or_error:
                return f"OK: {results_or_error}"
            else:
                return "OK: Aucun résultat"
        else:
            return f"ERROR: {results_or_error}"

    else:
        return "ERROR: Commande inconnue, doit commencer par EVENT ou QUERY"

def handle_client(conn, addr):
    print(f"Connexion de {addr}")
    buffer_lines = []

    try:
        with conn:
            while True:
                data = conn.recv(1024)
                if not data:
                    print(f"Déconnexion de {addr}")
                    break
                text = data.decode('utf-8')
                lines = text.splitlines()
                for line in lines:
                    if line.strip() == '':
                        full_message = '\n'.join(buffer_lines).strip()
                        if full_message:
                            response = process_message(full_message)
                            conn.sendall((response + "\n").encode('utf-8'))
                        buffer_lines = []
                    else:
                        buffer_lines.append(line)
    except Exception as e:
        print(f"Erreur avec client {addr}: {e}")
    finally:
        try:
            conn.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        conn.close()

def main():
    print(f"🟢 Démarrage serveur sur {HOST}:{PORT}")
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        # Pour relancer sans TIME_WAIT bloquant
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        try:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
        except OSError:
            pass  # Non dispo partout

        s.bind((HOST, PORT))
        s.listen()

        while True:
            conn, addr = s.accept()
            thread = threading.Thread(target=handle_client, args=(conn, addr), daemon=True)
            thread.start()

if __name__ == "__main__":
    main()
