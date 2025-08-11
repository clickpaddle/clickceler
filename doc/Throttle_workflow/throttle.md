lowchart TD
    Start["Start: handle_event_throttle(event(EventType, DictIn))"] --> FindRules{"throttle_rule_match(EventType, RuleID, Priority, Pattern, CondsDict, Params, TransDict, DictIn) ?"}
    
    FindRules -- No --> SendDirect["No matching rule<br>safe_thread_send_message(event) direct"]
    FindRules -- Yes --> SortRules["Sort matched rules by priority (desc)"]
    
    SortRules --> CheckBufferEmpty{"Is buffer empty?"}
    
    CheckBufferEmpty -- Yes --> CheckSendMethod{"Send method in Params?"}
    CheckBufferEmpty -- No --> BufferEvent["assertz(buffered_event(event(EventType, DictIn)))"]
    
    CheckSendMethod -- send_first --> SendFirstImmediate["Send first event immediately<br>safe_thread_send_message(event)"]
    CheckSendMethod -- send_last --> BufferEvent
    
    SendFirstImmediate --> BufferEventAfterFirst["Buffer event<br>assertz(buffered_event(event(EventType, DictIn)))"]
    BufferEventAfterFirst --> CheckLimitWindow
    
    BufferEvent --> CheckLimitWindow{"Buffer limit exceeded or window expired?"}
    
    CheckLimitWindow -- No --> WaitMore["Continue buffering events"]
    CheckLimitWindow -- Yes --> GetSendMethod["Get send method from Params<br>send_first or send_last"]
    
    GetSendMethod --> PrepareEvent{"Select event to keep based on method"}
    
    PrepareEvent -- send_first --> KeepFirst["Keep first buffered event"]
    PrepareEvent -- send_last --> KeepLast["Keep last buffered event"]
    
    KeepFirst --> ClearBuffer["retractall(buffered_event)"]
    KeepLast --> ClearBuffer
    
    ClearBuffer --> StartDelayThread["Start thread to send event after delay<br>(sleep Delay seconds)"]
    
    StartDelayThread --> WaitDelay["Wait delay seconds..."]
    WaitDelay --> SendEvent["safe_thread_send_message(event)"]
    
    SendEvent --> End["End"]
    WaitMore --> End
    SendDirect --> End
