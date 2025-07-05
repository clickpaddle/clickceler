import socket

HOST = '127.0.0.1'
PORT = 65433

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
                message = f"{query}"  # Double CR to mark end of query
                s.sendall(message.encode('utf-8'))
                #s.sendall(message)

                # Read full response
                response_chunks = []
                while True:
                    chunk = s.recv(4096)
                    if not chunk:
                        break
                    response_chunks.append(chunk)
                    # We assume the response ends with a newline character
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
