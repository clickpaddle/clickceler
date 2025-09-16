class Event:
    def __init__(self, id, message, timestamp, severity="unknown"):
        self.id = id
        self.message = message
        self.timestamp = timestamp
        self.severity = severity.lower()

    def to_prolog_fact(self):
        raise NotImplementedError("Cette méthode doit être surchargée dans la sous-classe")
