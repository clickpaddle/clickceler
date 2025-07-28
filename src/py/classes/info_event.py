from .base import Event

class InfoEvent(Event):
    def __init__(self, id, message, timestamp, severity="unknown", source=None):
        super().__init__(id, message, timestamp, severity)
        self.source = source or ""

    def to_prolog_fact(self):
        msg = self.message.replace("'", "\\'")
        ts = self.timestamp.replace("'", "\\'")
        sev = self.severity
        src = self.source.replace("'", "\\'")
        return f"event({self.id}, info_event{{message:'{msg}', timestamp:'{ts}', severity:{sev}, source:'{src}'}})."
