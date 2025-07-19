```mermaid
graph TD
    subgraph Utilisateur
        User[Utilisateurbr(Organisateur ou Participant)]
    end

    subgraph Frontend
        FE[Application Web(React.js)]
    end

    subgraph Backend
        API[API Backend(Node.js/Express)]
    end

    subgraph Services
        DB[(MongoDB Atlas)]
        Auth[Service d'authentification(Auth0, JWT...)]
        Storage[Stockage de fichiers<br/>(Cloudinary, etc.)]
    end

    User <-->|HTTP(S)| FE
    FE -->|API REST| API
    API --> DB
    API --> Auth
    API --> Storage
```
