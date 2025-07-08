#!/home/luc/python3/bin/python
import socket
import sys

HOST = "127.0.0.1"  # Server address
PORT = 65432  # Port for sending events


def main():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))
        print(f"Connected to server {HOST}:{PORT}")
        print(
            "Enter a JSON event (multi-line supported). Type an empty line to send, 'exit' to quit."
        )

        buffer_lines = []

        for line in sys.stdin:
            line = line.rstrip("\n")

            if line.strip().lower() == "exit":
                print("Closing client.")
                break

            if line.strip() == "":
                if not buffer_lines:
                    continue
                json_str = "\n".join(buffer_lines)
                message = f"EVENT {json_str}\n\n"
                print("Message prêt à l’envoi :", message)
                s.sendall(message.encode("utf-8"))  
                buffer_lines = []
            else:
                buffer_lines.append(line)

        if buffer_lines:
            json_str = "\n".join(buffer_lines)
            message = f"EVENT {json_str}\n\n"
            print("Dernier message à envoyer :", message)
            s.sendall(message.encode('utf-8'))


if __name__ == "__main__":
    main()
