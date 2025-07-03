import socket

HOST = '127.0.0.1'  # Server address
PORT = 65432        # Port for sending events

def main():
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((HOST, PORT))
        print(f"Connected to server {HOST}:{PORT}")
        print("Enter a JSON event (multi-line supported). Type an empty line to send, 'exit' to quit.")

        buffer_lines = []

        while True:
            try:
                line = input()
            except EOFError:
                break

            if line.strip().lower() == 'exit':
                print("Closing client.")
                break

            if line.strip() == '':
                # End of input, send the JSON
                if not buffer_lines:
                    continue
                json_str = '\n'.join(buffer_lines)
                message = f"EVENT {json_str}\n\n"  # Important: double \n to mark end
                try:
                    s.sendall(message.encode('utf-8'))
                    response = s.recv(4096).decode('utf-8').strip()
                    print("Server response:", response)
                except Exception as e:
                    print(f"Communication error: {e}")
                    break
                buffer_lines = []
            else:
                buffer_lines.append(line)

if __name__ == "__main__":
    main()
