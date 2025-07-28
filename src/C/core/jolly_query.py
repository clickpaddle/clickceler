import socket
import sys
        
def recv_all(sock, buffer_size=4096):
    data = bytearray()
    while True:
        part = sock.recv(buffer_size)
        if not part:
            break
        data.extend(part)
    return data.decode('utf-8') or ""  


def call_prolog_predicate(predicate: str, host: str = 'localhost', port: int = 65432) -> str:
    """Send a Prolog predicate to jollyclick and get the output response."""
    try:
        with socket.create_connection((host, port), timeout=10) as sock:
            sock.sendall(b"QUERY ")
            sock.sendall(predicate.encode('utf-8'))

            full_response = recv_all(sock)
            return full_response or ""  
    except Exception as e:
        return f"[ERROR] Failed to connect or send query: {e}"

if __name__ == '__main__':

            try:
                predicate = input('').strip()
                if not predicate:
                    exit(1)

                result = call_prolog_predicate(predicate)
                print(result)

            except EOFError:
                exit(1) 


