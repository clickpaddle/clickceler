import socket

HOST = '127.0.0.1'
PORT = 65432

def main():
    print(f"Connected to Server  {HOST}:{PORT}")

    buffer_lines = []

    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))

        while True:
            try:
                line = input("> ")
            except EOFError:
                break

            if line.strip().lower() == "exit":
                print("Client Closed")
                break

            if line.strip() == "":
                if not buffer_lines:
                    continue
                query = '\n'.join(buffer_lines).strip()
                message = f"QUERY {query}\n\n"  #  Double CR for the end of the query
                s.sendall(message.encode('utf-8'))

                # Read Full Answer
                response_chunks = []
                while True:
                    chunk = s.recv(4096)
                    if not chunk:
                        break
                    response_chunks.append(chunk)
                    # On suppose que la réponse se termine par \n
                    if b'\n' in chunk:
                        break
                response = b''.join(response_chunks).decode('utf-8').strip()
                print("Server Answer:", response)
                buffer_lines = []
                break  
            else:
                buffer_lines.append(line)

if __name__ == "__main__":
    main()
