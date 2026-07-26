import http.server
import select
import socket
import ssl
import sys
import threading
import time


(
    certificate,
    key,
    port_file,
    request_log,
    handshake_log,
    hang_file,
    closed_file,
) = sys.argv[1:]
log_lock = threading.Lock()


def record_server_name(_socket, server_name, _context):
    with log_lock:
        with open(handshake_log, "a", encoding="utf-8") as output:
            output.write((server_name or "<none>") + "\n")


class Server(http.server.ThreadingHTTPServer):
    address_family = socket.AF_INET6
    daemon_threads = True

    def server_bind(self):
        self.socket.setsockopt(socket.IPPROTO_IPV6, socket.IPV6_V6ONLY, 0)
        super().server_bind()


class Handler(http.server.BaseHTTPRequestHandler):
    protocol_version = "HTTP/1.1"

    def log_message(self, format, *args):
        pass

    def record(self):
        fields = [
            self.path,
            "host=" + self.headers.get("Host", "-"),
            "ua=" + self.headers.get("User-Agent", "-"),
            "language=" + self.headers.get("Accept-Language", "-"),
            "authorization=" + str("Authorization" in self.headers).lower(),
            "cookie=" + str("Cookie" in self.headers).lower(),
        ]
        with log_lock:
            with open(request_log, "a", encoding="utf-8") as output:
                output.write(" ".join(fields) + "\n")

    def send(self, status, body=b"", headers=()):
        self.send_response(status)
        for name, value in headers:
            self.send_header(name, value)
        self.send_header("Connection", "close")
        self.send_header("Content-Length", str(len(body)))
        self.end_headers()
        if body:
            self.wfile.write(body)

    def wait_for_disconnect(self, timeout):
        deadline = time.monotonic() + timeout
        while time.monotonic() < deadline:
            readable, _, _ = select.select((self.connection,), (), (), 0.1)
            if not readable:
                continue
            try:
                data = self.connection.recv(1)
            except (OSError, ssl.SSLError):
                data = b""
            if not data:
                return True
        return False

    def do_GET(self):
        self.record()
        if self.path == "/same":
            self.send(302, headers=(("Location", "/ok"),))
        elif self.path == "/ok":
            self.send(
                200,
                b"transport ok",
                headers=(("Content-Type", "text/plain; charset=utf-8"),),
            )
        elif self.path == "/cross":
            port = self.server.server_address[1]
            target = "https://127.0.0.1:%d/not-contacted" % port
            self.send(302, headers=(("Location", target),))
        elif self.path == "/large":
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Connection", "close")
            self.send_header("Content-Length", "4096")
            self.end_headers()
        elif self.path == "/stream":
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Connection", "close")
            self.end_headers()
            self.wfile.write(b"x" * 64)
        elif self.path == "/invalid-length":
            self.send_response(200)
            self.send_header("Content-Type", "text/plain")
            self.send_header("Connection", "close")
            self.send_header("Content-Length", "+1")
            self.end_headers()
            self.wfile.write(b"x")
        elif self.path.startswith("/redirect-limit/"):
            step = int(self.path.rsplit("/", 1)[1])
            if step < 11:
                self.send(
                    302,
                    headers=(("Location", "/redirect-limit/%d" % (step + 1)),),
                )
            else:
                self.send(
                    200, b"too many", headers=(("Content-Type", "text/plain"),)
                )
        elif self.path.startswith("/chain/"):
            step = int(self.path.rsplit("/", 1)[1])
            time.sleep(0.2)
            if step < 2:
                self.send(302, headers=(("Location", "/chain/%d" % (step + 1)),))
            else:
                self.send(200, b"late body", headers=(("Content-Type", "text/plain"),))
        elif self.path == "/hang":
            with open(hang_file, "w", encoding="utf-8") as output:
                output.write("request received\n")
            if self.wait_for_disconnect(30):
                with open(closed_file, "w", encoding="utf-8") as output:
                    output.write("connection closed\n")
                return
            self.send(200, b"too late", headers=(("Content-Type", "text/plain"),))
        else:
            self.send(404, b"not found", headers=(("Content-Type", "text/plain"),))


server = Server(("::", 0), Handler)
context = ssl.SSLContext(ssl.PROTOCOL_TLS_SERVER)
context.load_cert_chain(certificate, key)
context.set_servername_callback(record_server_name)
server.socket = context.wrap_socket(server.socket, server_side=True)
with open(port_file, "w", encoding="utf-8") as output:
    output.write(str(server.server_address[1]) + "\n")
server.serve_forever()
