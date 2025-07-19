```mermaid
graph TD
    subgraph Utilisateur
        User[Utilisateur<br/>(Organisateur ou Participant)]
    end

    subgraph Frontend
        FE[Application Web<br/>(React.js)]
    end

    subgraph Backend
        API[API Backend<br/>(Node.js/Express)]
    end

    subgraph Services
        DB[(MongoDB Atlas)]
        Auth[Service d'authentification<br/>(Auth0, JWT...)]
        Storage[Stockage de fichiers<br/>(Cloudinary, etc.)]
    end

    User <-->|HTTP(S)| FE
    FE -->|API REST| API
    API --> DB
    API --> Auth
    API --> Storage
```
