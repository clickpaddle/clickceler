import json
import random
import socket
from datetime import datetime, timedelta
import os

categories = ["ERROR", "INFO", "DEBUG"]
classes = ["Log", "Server", "CPU", "Disk", "Router", "Memory"]

log_messages = [
    "Disk full", "Backup completed", "CPU Overheat", "Memory usage high",
    "Network unreachable", "User login successful", "Disk read error",
    "Temperature sensor malfunction", "Scheduled maintenance started",
    "Power supply failure"
]

server_statuses = ["up", "down"]
cpu_states = ["normal", "overheated", "throttled"]
disk_states = ["healthy", "degraded", "failed"]
router_states = ["online", "offline", "congested"]
memory_states = ["normal", "high_usage", "error"]
status_states = ["open","ack","closed"]

hostnames = ["server01", "server02", "server03", "backup01", "router01", "auth01", "sensor01", "maint01"]
sources = ["StorageSystem", "System Monitor", "ThermalSensor", "MemoryMonitor", "NetworkMonitor", "Auth Service", "Maintenance Scheduler", "PowerManagement"]
sub_sources = ["DiskMonitor", "BackupService", "CPUUnit", "RAMModule", "InterfaceEth0", "LoginModule", "TempProbe", "SchedulerCore", "PSUUnit"]
origins = ["datacenter1", "datacenter2", "datacenter3"]

def random_timestamp():
    base = datetime(2025, 7, 1, 0, 0, 0)
    delta = timedelta(minutes=random.randint(0, 24*60))
    return (base + delta).isoformat() + "Z"

def generate_event():
    category = random.choice(categories)
    event_class = random.choice(classes)
    host = random.choice(hostnames)
    src = random.choice(sources)
    sub_src = random.choice(sub_sources)
    orig = random.choice(origins)
    ts = random_timestamp()

    event = {
        "type": event_class,
        "category": category,
        "timestamp": ts,
        "hostname": host,
        "source": src,
        "sub_source": sub_src,
        "origin": orig
    }
    event["status"] = random.choice(status_states)
    if event_class == "Log":
        event["message"] = random.choice(log_messages)
    elif event_class == "Server":
        event["state"] = random.choice(server_statuses)
    elif event_class == "CPU":
        event["state"] = random.choice(cpu_states)
        event["usage_percent"] = random.randint(0, 100)
    elif event_class == "Disk":
        event["state"] = random.choice(disk_states)
        event["free_space_gb"] = round(random.uniform(0, 500), 2)
    elif event_class == "Router":
        event["state"] = random.choice(router_states)
        event["packet_loss_percent"] = round(random.uniform(0, 10), 2)
    elif event_class == "Memory":
        event["state"] = random.choice(memory_states)
        event["used_mb"] = random.randint(0, 32000)

    return event

def send_events(host="127.0.0.1", port=65432):
    events = [generate_event() for _ in range(10)]
    json_data = json.dumps(events, indent=2)

    # Write JSON to file
    output_path = os.path.join("..", "data", "events.json")
    os.makedirs(os.path.dirname(output_path), exist_ok=True)
    with open(output_path, "w") as f:
        f.write(json_data)
    print(f"Written events to {output_path}")

    # Send JSON over TCP socket
    with socket.socket(socket.AF_INET, socket.SOCK_STREAM) as s:
        s.connect((host, port))
        s.sendall(b"EVENT ")
        s.sendall(json_data.encode('utf-8'))
        print(f"Sent {len(json_data)} bytes to {host}:{port}")

if __name__ == "__main__":
    send_events()

