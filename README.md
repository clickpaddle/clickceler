# ClickCeler — Educational Event Management Expert System

## Overview

ClickCeler is simple educational implementation inspired by early expert systems for event management. Its goal is to help new generations understand how event-driven expert systems were designed and operated, using logic programming (Prolog) and rule-based reasoning.

This project demonstrates the core concepts of:

- Representing events as structured facts
- Applying rule phases such as enrich, reset, link, launch
- Using logic inference to reason on event data
- Handling concurrent event inputs and queries via a simple server-client architecture

It is not a production-ready system but a didactic tool to explore foundational ideas in expert systems and event or facts management.

## How It Works

- Events are modeled as Prolog facts with rich attributes: swipl on Linux
- Rules written in Prolog are applied to update and correlate events.
- A Python server receives event data (in JSON), asserts facts in Prolog, and allows queries: Python module pyswip

This bridges modern application programming with classic logic-based reasoning engines.

## Usage

1. Load your events as JSON objects: events.json
2. Convert them to Prolog facts using the provided Python scripts: clickpost.py < events.json
3. Define your own Prolog rules for event enrich, reset, link, launch, filter: rules.pl
4. Run the server to accept events and queries over TCP: ClickCeler.py
5. Explore how logic rules process your events step by step: clickquery.py

---

## Future Plans

ClickCeler is an evolving educational project, and we have several exciting plans to enhance its capabilities and demonstrate more advanced concepts in event management and expert systems. Our future roadmap focuses on improving the solution's architecture, user interaction, automation, and deployment.

### Architectural Documentation

We aim to create comprehensive documentation detailing the **As-Is** and **To-Be** architecture of ClickCeler. This will provide a clear understanding of the current system design and how we envision its evolution, particularly regarding event flow, data persistence, and rule execution.

### Event Visualization Web Interface

To make the system more accessible and visually engaging, we plan to develop a small **web interface for visualizing events**. This interface will allow users to:

* See incoming events in real-time.
* Observe how rules process and transform events.
* Query event data dynamically through a user-friendly graphical interface.

### Automated Rule Execution

Currently, rule execution might require manual triggers. Our goal is to **automate the execution of rules** upon the arrival of new events or changes to existing events. This will simulate a more reactive and autonomous expert system, where events are processed instantly as they occur.

### NoSQL Database Integration

To ensure robust event persistence and enable seamless server restarts without data loss, we intend to integrate ClickCeler with a **NoSQL database**. This will allow us to:

* Store all incoming and processed events reliably.
* Reload events efficiently upon server restarts, maintaining the system's state.

### OCI Container Packaging

For easier distribution, deployment, and portability, we will **package ClickCeler within an Open Container Initiative (OCI) compliant container**. This will encapsulate all necessary dependencies, ensuring the application runs consistently across different environments.

### Kubernetes Deployment with ArgoCD

Finally, to demonstrate modern deployment practices for event-driven systems, we plan to **deploy the ClickCeler OCI image on a Kubernetes cluster using a GitOps tool like ArgoCD**. This will showcase:

* Automated, declarative deployments.
* Scalability and resilience within a container orchestration environment.
* Best practices for managing application lifecycles in cloud-native infrastructures.

---

## Disclaimer

This project is provided for educational and illustrative purposes only. It aims to demonstrate fundamental concepts of early expert systems for event management and is not intended as a production-ready solution.

No guarantee or commitment is made regarding the future development, maintenance, or support of this project. Users and contributors engage with this codebase at their own discretion.

The author expressly disclaims any liability for the use or misuse of this software.

