```mermaid
flowchart LR
    classDef ghost-node fill:none, stroke:none, color:none, height:0, padding:0, margin:0;

    subgraph DEV["🧑‍💻 Local Development"]
        A1[Write source code\nCommit signed with GPG key] --> A2[Push to feature branch]
        A2 --> A3["Create Pull Request (PR)"]
        A3 --> A4[Code Review & Approval]
        A4 --> A5[Merge PR to main branch]
    end
    
    subgraph CI["⚙️ Continuous Integration (GitHub Actions)"]
        CI_SPACER[" "] --- B1[Trigger CI Workflow\n.github/workflows/ci.yml]
        class CI_SPACER ghost-node
        B1 --> B2[Checkout source code]
        B2 --> B3[Static code analysis with SonarQube\ncode_quality.yml]
        B3 --> B4[Dependency vulnerability scan\nDependabot + Snyk]
        B4 --> B5[Build and compile the code\nbuild.yml]
        B5 --> B6[Run unit and integration tests]
        B6 --> B7[Sign binary or build artifact\ncosign sign --key cosign.key]
        B7 --> B8[Build and push Docker image\ndocker_build.yml]
        B8 --> B9[Security scan of Docker image\nXray / Trivy]
        B9 --> B10[Push signed image to container registry\nArtifactory, GHCR, Harbor, etc.]
    end
    
    subgraph CD["🚀 Continuous Deployment (ArgoCD)"]
        CD_SPACER[" "]  --- C1[Push Kubernetes manifests\nmanifests/deployment.yaml]
        class CD_SPACER ghost-node
        C1 --> C2[ArgoCD sync trigger]
        C2 --> C3[ArgoCD compares desired vs actual state]
        C3 --> C4[Deploy signed image\nto target namespace]
        C4 --> C5[Health checks + automatic rollback on failure]
    end
    
    subgraph MON["📊 Monitoring & Security Auditing"]
        MON_SPACER[" "] --- D1[Prometheus + Grafana\nPerformance metrics]
        class MON_SPACER ghost-node
        D1 --> D2[Loki + Tempo\nLogs and traces]
        D2 --> D3[ArgoCD audit trail\nDeployment history]
        D3 --> D4[Slack / Teams webhook\nCI/CD notifications]
    end
    
    DEV --> CI
    CI --> CD
    CD --> MON
```
