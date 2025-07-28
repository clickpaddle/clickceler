# 🔐 Security Policy for ClickCeler

## 📅 Supported Versions

ClickCeler is an educational and community-driven project. As such, it is not maintained with commercial-grade security patching timelines, but we still care about responsible disclosures and the safety of contributors and users.

| Version         | Supported          |
|-----------------|--------------------|
| `main` branch   | ✅ Active support   |
| Older branches  | ❌ Not maintained   |

If you’re using the codebase, always pull from the `main` branch for the latest fixes and contributions.

## 🛡️ Reporting a Vulnerability

If you discover a security issue related to the ClickCeler codebase or one of its dependencies:

1. **Do not** open a public GitHub issue.
2. **Email us instead** at:  
   📧 **security@clickpaddle.com** *(replace with actual contact email)*

Please include:

- A detailed description of the vulnerability
- Steps to reproduce, if applicable
- Any known exploits or proof of concept (optional but helpful)

We will:

- Acknowledge your report within 72 hours
- Investigate and confirm the issue
- Patch it responsibly (when applicable)
- Credit you (if desired) in the changelog or release notes

## 🔁 Coordinated Disclosure

We strongly support **coordinated vulnerability disclosure**. If you report responsibly, we’ll work with you collaboratively and respectfully.

## 🚫 Out of Scope

Since ClickCeler is a project for learning and experimentation, the following are considered **out of scope** for responsible disclosure:

- Attacks requiring physical access to the contributor's or user's machine
- Issues in third-party dependencies unless they directly affect ClickCeler
- Social engineering attacks against maintainers or contributors
- Denial of service via voluntary misuse or unrealistic abuse

## 🔒 General Security Principles

We encourage all contributors to follow basic security best practices:

- Avoid committing secrets or credentials to the repo
- Validate and sanitize any user-generated data (in code examples)
- Prefer secure defaults in educational demos
- Use GitHub's built-in Dependabot alerts and code scanning when possible

## 📜 Licensing Note

All code in this repository is under the **GPL license**. Please be aware that while reuse and modification are encouraged under this license, **you assume responsibility for the security and safety of your own deployments.**

---

*Thank you for helping keep ClickCeler safe, responsible, and open for everyone.*
