import socket
import threading
import json
from pyswip import Prolog

HOST = '127.0.0.1'
PORT = 65432

prolog = Prolog()
try:
    success = prolog.consult('../rules/rules.pl')
    print("Consult success:", success)
except Exception as e:
    print("Consult error:", e)


events = []
lock = threading.Lock()
event_counter = 1  # auto-incrementing ID

def format_prolog_dict(event):
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
    global event_counter
    with lock:
        eid = event_counter
        event_counter += 1

        event_body = {k: v for k, v in event.items() if k != "id"}
        dict_str = format_prolog_dict(event_body)
        fact = f"event({eid}, {dict_str})"

        try:
            prolog.assertz(fact)
            print(f"✅ Fact added to Prolog: {fact}")
        except Exception as e:
            print(f"❌ Prolog insertion error: {e}")
            print(f"⛔ Problematic fact: {fact}")

def execute_prolog_query(query_str):
    try:
        with lock:
            results = list(prolog.query(query_str))
            return True, results
    except Exception as e:
        return False, f"Prolog error: {str(e)}"

def process_message(message: str) -> str:
    if message.startswith("EVENT "):
        json_part = message[len("EVENT "):].strip()
        try:
            parsed = json.loads(json_part)
            if not isinstance(parsed, list):
                return "ERROR: The event must be a JSON list of event objects"

            for event in parsed:
                if not isinstance(event, dict):
                    return "ERROR: Each list element must be a JSON object"
                add_event_to_prolog(event)

            return f"OK: {len(parsed)} event(s) added"

        except json.JSONDecodeError as e:
            return f"ERROR: Invalid JSON: {str(e)}"

    elif message.startswith("QUERY "):
        query_str = message[len("QUERY "):].strip()
        success, results_or_error = execute_prolog_query(query_str)
        if success:
            if results_or_error:
                return f"OK: {results_or_error}"
            else:
                return "OK: No results"
        else:
            return f"ERROR: {results_or_error}"

    else:
        return "ERROR: Unknown command. Use EVENT or QUERY."

def handle_client(conn, addr):
    print(f"Connection from {addr}")
    buffer_lines = []

    try:
        with conn:
            while True:
                data = conn.recv(1024)
                if not data:
                    print(f"Disconnection from {addr}")
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
        print(f"Error with client {addr}: {e}")
    finally:
        try:
            conn.shutdown(socket.SHUT_RDWR)
        except OSError:
            pass
        conn.close()

def main():
    print(f"🟢 Server started on {HOST}:{PORT}")
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEADDR, 1)
        try:
            s.setsockopt(socket.SOL_SOCKET, socket.SO_REUSEPORT, 1)
        except OSError:
            pass
        s.bind((HOST, PORT))
        s.listen()

        while True:
            conn, addr = s.accept()
            thread = threading.Thread(target=handle_client, args=(conn, addr), daemon=True)
            thread.start()

if __name__ == "__main__":
    main()
