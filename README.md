# Routing Table Simulation

## Overview

This project, part of the Imperative Programming course at ENSEEIHT
(N7), focuses on implementing and comparing different data structures for
storing and exploiting network routing tables. The simulation includes
longest prefix matching and cache management to optimize routing
performance.

## Technical Specifications

The project compares two main architectures for the routing cache:

1. routeur_LL: Linked List for the routing table + Linked List for the cache.

2. routeur_LA: Linked List for the routing table + Prefix Tree (Trie) for the cache.

## Features

Longest Prefix Matching: Selects the most specific route based on the subnet mask.

Cache Management: Supports three replacement policies:

- FIFO: First In, First Out.

- LRU: Least Recently Used.

- LFU: Least Frequently Used (for teams of 3).

Performance Statistics: Tracking cache miss rates and routing requests.

## Compilation

The project is written in Ada 2022. Use the GNAT compiler:

```
gnatmake -gnatwa -gnata -g ./routeur_ll.adb
gnatmake -gnatwa -gnata -g ./routeur_la.adb
```

## Usage

Run the executable with the following command-line arguments:

```
./routeur_ll [-c size] [-p policy] [-t table_file] [-q packets_file] [-r results_file] [-s|-S]
```
