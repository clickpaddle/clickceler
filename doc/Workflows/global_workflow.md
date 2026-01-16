flowchart TD
    A["Incoming Event"] --> B["REFINE Phase Apply Refinement Rules"]
    B --> C["FILTER Phase Apply Filter Rules"]
    C --> D["THROTTLE Phase Apply Throttling Rules"]
    D --> E["UPDATE Phase Apply Update Rules and Enrichment"]
    E --> F["EXECUTE Phase Apply Execution Enrichment / Attributes"]
    F --> G["TIME Phase Apply Time-Based Attributes / Rules"]
    G --> H["PROPAGATE Phase Prepare Event for Propagation / Attach Clauses 'when'"]
    H --> I["TRIGGERS Phase Evaluate All 'when' Clauses and Execute Actions"]

    %% Subgraphs for clarity
    subgraph Refine_Phase ["REFINE Phase"]
        B
    end

    subgraph Filter_Phase ["FILTER Phase"]
        C
    end

    subgraph Throttle_Phase ["THROTTLE Phase"]
        D
    end

    subgraph Update_Phase ["UPDATE Phase"]
        E
    end

    subgraph Execute_Phase ["EXECUTE Phase"]
        F
    end

    subgraph Time_Phase ["TIME Phase"]
        G
    end

    subgraph Propagate_Phase ["PROPAGATE Phase"]
        H
    end

    subgraph Triggers_Phase ["TRIGGERS Phase"]
        I
    end

