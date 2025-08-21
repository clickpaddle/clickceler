flowchart TD
    A["New Event Received (sensor/probe)"] --> B{"Event matches abstract rule conditions?"}
    B -- "Yes" --> C{"Matching Abstract Exists?"}
    C -- "Yes" --> D["Add Event ID to Abstract.abstract_contrib"]
    D --> E["Add Abstract ID to Event.linked_abstracts"]
    E --> F["Log: Event updated"]
    C -- "No" --> G["Create New Abstract Event"]
    G --> H["Event.linked_abstracts = [Abstract ID]"]
    H --> I["Set Abstract.type"]
    I --> J["Set Abstract.core_fields"]
    J --> K["Log: Abstract created from sensor or probe event"]

    %% Closure handling
    N["Event status changed to closed"] --> O["Remove Event ID from all Abstract.abstract_contrib"]
    O --> P{"Abstract.abstract_contrib empty?"}
    P -- "Yes" --> Q["Set Abstract.status = closed"]
    Q --> R["Log: Abstract updated"]
    P -- "No" --> S["Abstract remains open"]
    S --> T["Log: Abstract updated"]


