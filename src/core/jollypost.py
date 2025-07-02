import socket

HOST = '127.0.0.1'  # Adresse du serveur
PORT = 65432        # Port pour envoi d'événements

def main():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))
        print(f"Connecté au serveur {HOST}:{PORT}")
        print("Entrez un événement JSON (multi-lignes supporté). Tapez une ligne vide pour valider, 'exit' pour quitter.")

        buffer_lines = []

        while True:
            try:
                line = input()
            except EOFError:
                break

            if line.strip().lower() == 'exit':
                print("Fermeture du client.")
                break

            if line.strip() == '':
                # Fin de l'input, envoyer le JSON
                if not buffer_lines:
                    continue
                json_str = '\n'.join(buffer_lines)
                message = f"EVENT {json_str}\n\n"  # Important : double \n pour marquer fin
                try:
                    s.sendall(message.encode('utf-8'))
                    response = s.recv(4096).decode('utf-8').strip()
                    print("Réponse serveur:", response)
                except Exception as e:
                    print(f"Erreur communication : {e}")
                    break
                buffer_lines = []
            else:
                buffer_lines.append(line)

if __name__ == "__main__":
    main()
