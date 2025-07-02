# JollyClick — Educational Event Management Expert System

## Overview

JollyClick is a simple educational implementation inspired by early expert systems for event management. Its goal is to help new generations understand how event-driven expert systems were designed and operated, using logic programming (Prolog) and rule-based reasoning.

This project demonstrates the core concepts of:

- Representing events as structured facts  
- Applying rule phases such as refine, update, filter, correlate, and execute  
- Using logic inference to reason on event data  
- Handling concurrent event inputs and queries via a server-client architecture  

It is not a production-ready system but a didactic tool to explore foundational ideas in expert systems and event management.

## How It Works

- Events are modeled as Prolog facts with rich attributes.  
- Rules written in Prolog are applied to update and correlate events.  
- A Python server receives event data (in JSON), asserts facts in Prolog, and allows queries.  

This bridges modern application programming with classic logic-based reasoning engines.

## Usage

1. Load your events as JSON objects.  
2. Convert them to Prolog facts using the provided Python scripts.  
3. Define your own Prolog rules for event refinement, filtering, and correlation.  
4. Run the server to accept events and queries over TCP.  
5. Explore how logic rules process your events step by step.

## Disclaimer

This project is provided for educational and illustrative purposes only. It aims to demonstrate fundamental concepts of early expert systems for event management and is not intended as a production-ready solution.

No guarantee or commitment is made regarding the future development, maintenance, or support of this project. Users and contributors engage with this codebase at their own discretion.

The author expressly disclaims any liability for the use or misuse of this software.

