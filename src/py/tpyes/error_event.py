from .base import Event

class ErrorEvent(Event):
    def __init__(self, id, message, timestamp, severity="unknown", error_code=None):
        super().__init__(id, message, timestamp, severity)
        self.error_code = error_code

    def to_prolog_fact(self):
        msg = self.message.replace("'", "\\'")
        ts = self.timestamp.replace("'", "\\'")
        sev = self.severity
        code = self.error_code if self.error_code is not None else "null"
        return f"event({self.id}, error_event{{message:'{msg}', timestamp:'{ts}', severity:{sev}, error_code:{code}}})."
