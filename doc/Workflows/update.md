```mermaid
flowchart TD
    A["Incoming Event from ABSTRACT Phase (Normalized Event)"] --> B["UPDATE Phase Apply Update Rules"]
    B --> C["Search Stored / Transformed Events within Time Window"]
    C -->|"Combine / Enrich"| D["Event Transformed / Enriched"]
    D --> E["Re-inject into Internal Queue"]
    E --> F["Next Phase: CORRELATE Apply Correlation Rules"]

    subgraph Update_Phase ["UPDATE Phase"]
        B
        C
        D
    end

    subgraph Internal_Queue ["Internal Event Queue"]
        E
    end

    subgraph Correlate_Phase ["CORRELATE Phase"]
        F
    end

