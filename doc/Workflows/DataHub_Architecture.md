# ARCHITECTURE DOCUMENT: Central Control Data Hub for Clickceler (Open Source Edition)
## Open Source Hybrid Infrastructure Blueprint (Proxmox / Ceph / Kafka)

**Project Identity:** Clickceler Event Management  
**Context:** SCADA / Industry v4.0 Critical Infrastructure  
**Infrastructure Stack:** Proxmox VE 8.x + Ceph Quincy/Reef + Apache Kafka  
**Scope:** 60 Global Edge Branches to Central Stretched Hub  
**Compliance:** Enterprise-Grade HA (RPO=0) & Zero-Trust mTLS Standards

---

## 1. Executive Summary
This document defines an ultra-resilient infrastructure using **Open Source** technologies for real-time security data centralization. The architecture combines a **Proxmox VE Cluster** with **Ceph Distributed Storage** and a Rack-Aware Kafka Cluster.

Emphasis is placed on:
* **Zero Data Loss (RPO=0):** Achieved via Ceph synchronous replication across sites.
* **Vendor Independence:** Leveraging KVM, Ceph, and Linux-native security.
* **Complex Event Handling:** Optimized via Claim Check patterns and S3-compatible storage.

---

## 2. Central Hub: Infrastructure
The central hub consists of **8 high-performance nodes** (4 per site).

### Hardware Specifications
| Component | Technical Details | Business Value |
| :--- | :--- | :--- |
| **Processors** | 2 x Intel® Xeon® Gold 6430 (64 physical cores total per node) | High-throughput message processing & low latency. |
| **Memory** | 512GB DDR5-4800 RAM | Optimized for Ceph OSD caching & Kafka Page Cache. |
| **Storage (OSD)** | 10 x 7.68TB NVMe/SAS SSD per node | Unified Ceph pool (Block & Object storage). |
| **Network Fabric** | Quad-port 25GbE SFP28 | Dedicated links for Ceph Cluster traffic (Replication). |
| **Virtualization** | **Proxmox VE 8.x (KVM/LXC)** | Enterprise-grade HA without licensing overhead. |

---

## 3. Storage & Kafka Alignment (Ceph Stretched Layer)

### 3.1 Ceph CRUSH Map & Quorum
* **Site-Aware Replication:** The Ceph CRUSH map is configured with `Site` as the primary failure domain. Every write is replicated to ensure copies exist on both **Site A** and **Site B**.
* **PFTT=1 (Primary Failure):** The cluster survives the total loss of one site.
* **QDevice (Site C):** A lightweight Proxmox QDevice and Ceph Monitor on Site C act as a tie-breaker to maintain Quorum and prevent split-brain.

### 3.2 Kafka & Storage Architecture
* **Ceph RBD (Block):** Used for Kafka persistent logs.
* **Kafka Rack-Aware:** Brokers are tagged with site locations to ensure partition replicas are never stored on the same site.
* **Net usable capacity:** ~61 TB (after synchronous replication and 25% safety slack).

---

## 4. Advanced Data Reliability Patterns

### 4.1 Edge Node & Transactional Outbox (CDC)
* **Debezium Agent:** Captures changes from local branch databases.
* **Outbox Pattern:** Ensures atomic commits (Data + Outbox table). If WAN drops, data is buffered locally and resumed automatically.

### 4.2 Claim Check (Large Files/Biometrics)
* **Problem:** Large biometric payloads bloat Kafka.
* **Solution:** Files stored in **Ceph Object Gateway (S3-compatible)**. Kafka carries only the metadata/URL.
* **Benefit:** Lean Kafka topics and high-speed processing.

### 4.3 Schema Evolution (Schema Registry)
* **Standard:** Apache Avro (Binary).
* **Governance:** **BACKWARD** compatibility ensures branches can be updated independently without breaking central consumers.

---

## 5. Security & Zero-Trust

### 5.1 Triple-Layer Authentication Matrix

| Entity | Auth Method | Use Case | Governance |
| :--- | :--- | :--- | :--- |
| **Edge Brokers** | mTLS 1.3 | Inter-site bridging | Private Root CA via Vault. |
| **Debezium Agents** | SASL/SCRAM | Local DB ingestion | Managed credentials in Vault. |
| **Admins/Analysts** | OIDC / OAuth2 | Dashboard & Monitoring | Keycloak / Authelia (SSO). |
| **Applications** | mTLS + ACLs | Producer/Consumer logic | Branch-level isolation. |

### 5.2 Encryption & HashiCorp Vault
* **Encryption at Rest:** Ceph OSDs are encrypted via **LUKS/dm-crypt**.
* **Vault Roles:**
    1. **PKI:** Dynamic issuance of mTLS certs for 60 branches.
    2. **KMS:** Secure management of Ceph encryption keys.
    3. **Secrets:** Automated rotation of SASL credentials.

---

## 6. Network Configuration & Performance
* **Linux Bridge / Open vSwitch:** Managed via Proxmox for network isolation.
* **Jumbo Frames (MTU 9000):** Strictly enforced for **Hub-to-Hub Ceph replication** to minimize CPU overhead and maximize throughput.
* **Standard MTU (1500):** For Edge-to-Hub WAN traffic to ensure global compatibility.
* **Load Balancing:** **HAProxy / Keepalived** configured for SNI (Server Name Indication) routing of mTLS Kafka traffic.

---

## 7. Observability: OpenTelemetry & Grafana
* **OpenTelemetry Collector:** Scrapes JMX metrics from Kafka and OSD metrics from Ceph.
* **Unified Tracing:** End-to-end Trace IDs from the Edge Debezium agent to the Central Hub.
* **Loki & Grafana:** Centralized log aggregation and real-time dashboards for cluster health and branch consumer lag.

---

## 8. Annex: Network Ports & Firewall Rules

| Service | Port | Protocol | Traffic Flow |
| :--- | :--- | :--- | :--- |
| **Kafka Replication** | 9092 | TCP | Intra-Hub (Site A ↔ B) |
| **Kafka mTLS (DNAT)** | 9093-909X | TCP | Edge → Hub (Direct to Broker) |
| **Schema Registry** | 8081 | TCP | Edge → Hub |
| **Ceph S3 (HTTPS)** | 443 | TCP | Branch File Uploads |
| **Ceph OSD/MON** | 6789, 6800+ | TCP | Intra-Hub Sync (Site A ↔ B) |
| **Proxmox Corosync** | 5405 | UDP | Quorum Heartbeat (A, B, C) |

---

## 9. Annex: Solution Architecture Diagram

```mermaid
graph TD
    %% ---------------- Producers / Edge ----------------
    subgraph Producers [Edge: 60 Remote Branches / Producers]
        Edge_Deb[Debezium Agent]
        Edge_DB[Local DB / Outbox]
        Edge_App[Clickceler Local / Producer]
        Edge_S3[(S3 Local Storage / Claim Check)]
        subgraph Auth_Edge [Security Context]
            Cert_B{Cert mTLS}
            Creds[SCRAM User/Pass]
        end
        subgraph FW_Edge [Edge Firewall]
        end
    end
    Edge_S3 --> FW_Edge
    
    %% ---------------- Central Hub ----------------
    subgraph Hub ["Central Hub (Proxmox + Ceph)"]
        subgraph FW_Central [Central Firewall - DNAT / DMZ]
        end

        %% Site A
        subgraph SiteA [Site A - 4 Nodes]
            Brokers_A[Kafka Brokers A0-A3]
            BR_A_Comment(("RF=3, ISR=2"))
            SR_A[Schema Registry A]
            S3_A[(Ceph S3 Pool A)]
            LB_S3_A[LB S3 A]
            LB_Schema_A[LB Schema A]
            
            Brokers_A --> BR_A_Comment
            SR_A -->|_schemas topic| Brokers_A
            LB_Schema_A --> SR_A
        end

        %% Site B
        subgraph SiteB [Site B - 4 Nodes]
            Brokers_B[Kafka Brokers B0-B3]
            BR_B_Comment(("RF=3, ISR=2"))
            SR_B[Schema Registry B]
            S3_B[(Ceph S3 Pool B)]
            LB_S3_B[LB S3 B]
            LB_Schema_B[LB Schema B]

            Brokers_B --> BR_B_Comment
            SR_B -->|_schemas topic| Brokers_B
            LB_Schema_B --> SR_B
        end
    
        %% Ceph Stretched Layer (Replacing vSAN)
        CEPH{Ceph CRUSH Map: Site-Aware}
        Brokers_A & Brokers_B & S3_A & S3_B --- CEPH

        %% Site C - The Tie-Breaker
        subgraph SiteC [Site C - Witness]
            QDev[Proxmox QDevice]
            MON_C[Ceph Monitor C]
            note_C(("Quorum Arbitrator"))
        end
        SiteC -.->|Voting & Quorum| CEPH

        %% ---------------- Security ----------------
        subgraph Security [Central Security]
            Vault[HashiCorp Vault / PKI & KMS / Secrets]
            AD[(Active Directory / LDAP)]
        end

        %% ---------------- Observability ----------------
        subgraph Observability [Monitoring & Logs]
            OT[OpenTelemetry]
            Loki[Loki]
            Clickceler 
            GF[Grafana Dashboards]
        end
    end

    %% ---------------- Consumers ----------------
    subgraph Consumers [Consumers / Dashboards]
        C_Edge[Edge Dashboards / Consumers]
        C_Central[Clickceler Central / Analytics / Reporting]
    end

    %% ---------------- Data Flows ----------------
    Edge_Deb -->|Produce events| FW_Edge
    Edge_App -->|Produce events| Edge_DB
    FW_Edge --> FW_Central
    
    FW_Central --> LB_Schema_A & LB_Schema_B
    FW_Central --> LB_S3_A & LB_S3_B
    LB_S3_A --> S3_A
    LB_S3_B --> S3_B
    
    Vault -.->|KMS / LUKS| CEPH
    
    Edge_App -->|Files| Edge_S3
    Edge_DB --> Edge_Deb
    Edge_Deb -.->|Uses| Cert_B
    Edge_Deb -.->|Uses| Creds

    FW_Central --> Brokers_A & Brokers_B
    FW_Central --> Loki

    Brokers_A & Brokers_B -->|Consume messages| C_Central & C_Edge
    Brokers_A & Brokers_B -->|Metrics & Logs| OT
    OT & Loki --> Clickceler 
    Clickceler --> GF

  Vault -->|mTLS / SASL| Brokers_A & Brokers_B & Auth_Edge
    AD -->|ACLs / Users| Brokers_A & Brokers_B & Auth_Edge

%% ---------------- Styles ----------------%% Security - Violet Pastel Net
    style Security fill:#f3e5f5,stroke:#9575cd,stroke-width:2px,color:#4a148c
    style Vault fill:#ffffff,stroke:#9575cd,color:#4a148c
    style AD fill:#ffffff,stroke:#9575cd,color:#4a148c
    style Auth_Edge fill:#f3e5f5,stroke:#9575cd,color:#4a148c

    %% Central Infrastructure - Blue Net
    style Hub fill:#f5f5f5,stroke:#263238,stroke-width:2px
    style SiteA fill:#e3f2fd,stroke:#1565c0,stroke-width:2px
    style SiteB fill:#e3f2fd,stroke:#1565c0,stroke-width:2px
    style CEPH fill:#1565c0,stroke:#0d47a1,stroke-width:3px,color:#ffffff
    style SiteC fill:#fff8e1,stroke:#ff8f00,stroke-width:2px

    %% Edge & Consumers - Harmonized Orange/Ocre Net (SANS HACHURES)
    style Producers fill:#fff3e0,stroke:#e65100,stroke-width:2px
    style Edge_App fill:#ef6c00,stroke:#e65100,color:#ffffff
    style Edge_DB fill:#ef6c00,stroke:#e65100,color:#ffffff
    style Edge_Deb fill:#ef6c00,stroke:#e65100,color:#ffffff
    style Edge_S3 fill:#006064,stroke:#00838f,color:#ffffff
    style Consumers fill:#fff3e0,stroke:#e65100,stroke-width:2px
    style C_Edge fill:#ef6c00,stroke:#e65100,color:#ffffff
    style C_Central fill:#ef6c00,stroke:#e65100,color:#ffffff

    %% Observability - Green Net
    style Observability fill:#e8f5e9,stroke:#2e7d32,stroke-width:2px
    style OT fill:#1b5e20,stroke:#2e7d32,color:#ffffff
    style Loki fill:#004d40,stroke:#00695c,color:#ffffff
    style GF fill:#33691e,stroke:#558b2f,color:#ffffff

    %% Firewalls - Grey Net
    style FW_Edge fill:#37474f,stroke:#263238,color:#ffffff
    style FW_Central fill:#37474f,stroke:#263238,color:#ffffff

    %% ---------------- Arrow Thickness (SOLID) ----------------
    linkStyle default stroke-width:2px,stroke:#222222
  
```

