flowchart TD
    A["Source Event Received"] --> B{"Matching Abstract Exists?"}
    B -- "Yes" --> C["Add Event ID to Abstract.abs_contrib"]
    C --> D["Add Abstract ID to Event.linked_abstracts"]
    B -- "No" --> E["Create New Abstract Event"]
    E --> F["Abstract.abs_contrib = [Event ID]"]
    F --> G["Event.linked_abstracts = [Abstract ID]"]

    %% Closure handling
    H["Source Event Closed"] --> I["Remove Event ID from all Abstract.abs_contrib"]
    I --> J{"Abstract.abs_contrib empty?"}
    J -- "Yes" --> K["Mark Abstract Closed"]
    J -- "No" --> L["Abstract remains open"]
