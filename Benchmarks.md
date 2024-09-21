# Benchmarks (YMMV)

All benchmarks were done using reference scripts and the cardano-node emulator
([github](https://github.com/IntersectMBO/cardano-node-emulator)). The scripts for this protocol are
too large to be used locally so it is required that reference scripts are used. The emulator uses
the same parameters as the mainnet.

## Table of Contents 
- [Required Deposits for Reference Scripts](#required-deposits-for-reference-scripts)
- [Creating Proposal UTxOs](#creating-proposal-utxos)
- [Updating Proposal UTxOs](#updating-proposal-utxos)
- [Closing Proposal UTxOs](#closing-proposal-utxos)
- [Purchasing Proposals UTxOs](#purchasing-proposals-utxos)
- [Executing Active UTxOs](#executing-active-utxos)
- [Closing Expired Active UTxOs](#closing-expired-active-utxos)
- [Updating Payment Addresses of Active UTxOs](#updating-payment-addresses-of-active-utxos)

## Required Deposits for Reference Scripts

| Script | Deposit |
|:------:|:-------:|
| options spending script | 15 ADA |
| proposal beacon script | 25 ADA |
| active beacon script | 52 ADA |
| address update observer script | 15 ADA |

It is recommended that users share reference scripts using something like
[cardano-reference-scripts](https://github.com/fallen-icarus/cardano-reference-scripts) so that there
is only one copy of each script stored on chain at a time.

## Creating Proposal UTxOs

A single proposal UTxO is capable of holding 400+ possible terms; the bottleneck is the transaction
size.

#### All proposals are for the same conditions, and have three possible terms.
| Proposals Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.257341 ADA | 0.386012 ADA |
| 5 | 0.510412 ADA | 0.765618 ADA |
| 10 | 0.826750 ADA | 1.240125 ADA |
| 15 | 1.143089 ADA | 1.714634 ADA |
| 20 | 1.459427 ADA | 2.189141 ADA |
| 25 | 1.775986 ADA | 2.663979 ADA |

Max: 25 Proposals  
Bottleneck: Tx Size

#### All proposals are for different conditions, and have three possible terms.
| Proposals Created | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.274393 ADA | 0.411590 ADA |
| 5 | 0.589333 ADA | 0.884000 ADA |
| 10 | 0.983865 ADA | 1.475798 ADA |
| 15 | 1.378309 ADA | 2.067464 ADA |
| 16 | 1.457198 ADA | 2.185797 ADA |

Max: 16 Proposals  
Bottleneck: Tx Size

## Updating Proposal UTxOs

Only the worst case scenarios are shown here. If all proposals are for the same conditions, it is
possible to update 18 proposals in a single transaction (even if beacons need to be changed).

#### All proposals are for different conditions, and have three possible terms. No beacons need to be changed.
| Proposals Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.401124 ADA | 0.601686 ADA |
| 5 | 0.778689 ADA | 1.168034 ADA |
| 10 | 1.293724 ADA | 1.940586 ADA |
| 15 | 1.856626 ADA | 2.784939 ADA |
| 16 | 1.974950 ADA | 2.962425 ADA |

Max: 16 Proposals  
Bottleneck: Tx Size

#### All proposals are for different conditions, and have three possible terms. Beacons are changed for all proposals.
| Proposals Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.366034 ADA | 0.549051 ADA |
| 5 | 0.752844 ADA | 1.129266 ADA |
| 10 | 1.279838 ADA | 1.919757 ADA |
| 12 | 1.504038 ADA | 2.256057 ADA |

Max: 12 Proposals  
Bottleneck: Tx Size

## Closing Proposal UTxOs

#### All proposals are for different conditions, and have three possible terms.
| Proposals Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.276683 ADA | 0.415025 ADA |
| 5 | 0.397682 ADA | 0.596523 ADA |
| 10 | 0.592055 ADA | 0.888083 ADA |
| 15 | 0.834163 ADA | 1.251245 ADA |
| 20 | 1.124182 ADA | 1.686273 ADA |
| 25 | 1.462287 ADA | 2.193431 ADA |
| 30 | 1.848303 ADA | 2.772455 ADA |

Max: 30 Proposals  
Bottleneck: Memory

## Purchasing Proposal UTxOs

#### All proposals are for the same conditions, and have three possible terms.
| Proposals Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.333139 ADA | 0.499709 ADA |
| 2 | 0.448848 ADA | 0.673272 ADA |
| 3 | 0.567382 ADA | 0.851073 ADA |
| 4 | 0.693589 ADA | 1.040384 ADA |
| 5 | 1.826127 ADA | 1.239191 ADA |
| 6 | 1.951956 ADA | 1.427934 ADA |
| 7 | 1.070274 ADA | 1.605411 ADA |
| 8 | 1.216039 ADA | 1.824059 ADA |
| 9 | 1.346737 ADA | 2.020106 ADA |
| 10 | 1.502011 ADA | 2.253017 ADA |
| 11 | 1.651063 ADA | 2.476595 ADA |
| 12 | 1.794166 ADA | 2.691249 ADA |

Max: 12 Proposals  
Bottleneck: Memory

#### All proposals are for different conditions, and have three possible terms.
| Proposals Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.346103 ADA | 0.519155 ADA |
| 2 | 0.495061 ADA | 0.742592 ADA |
| 3 | 0.663553 ADA | 0.995330 ADA |
| 4 | 0.827242 ADA | 1.240863 ADA |
| 5 | 1.013242 ADA | 1.519863 ADA |
| 6 | 1.235577 ADA | 1.853366 ADA |
| 7 | 1.490055 ADA | 2.235083 ADA |
| 8 | 1.716877 ADA | 2.575316 ADA |

Max: 8 Proposals  
Bottleneck: Memory

## Executing Active UTxOs

#### All contracts have the same beacons.
| Contracts Executed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.265028 ADA | 0.397542 ADA |
| 5 | 0.523162 ADA | 0.784743 ADA |
| 10 | 0.943173 ADA | 1.414760 ADA |
| 15 | 1.425843 ADA | 2.138765 ADA |
| 18 | 1.737500 ADA | 2.606250 ADA |

Max: 18 Contracts  
Bottleneck: Memory

#### All contracts have different beacons.
| Contracts Executed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.364356 ADA | 0.546534 ADA |
| 5 | 0.651614 ADA | 0.977421 ADA |
| 10 | 1.281881 ADA | 1.922822 ADA |
| 13 | 1.634363 ADA | 2.451545 ADA |

Max: 13 Contracts  
Bottleneck: Memory

## Closing Expired Active UTxOs

#### All contracts have the same beacons.
| Expired Contracts Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.235505 ADA | 0.353258 ADA |
| 5 | 0.426865 ADA | 0.640298 ADA |
| 10 | 0.764329 ADA | 1.146494 ADA |
| 15 | 1.165457 ADA | 1.748186 ADA |
| 19 | 1.517566 ADA | 2.276349 ADA |

Max: 19 Contracts  
Bottleneck: Memory

#### All contracts have different beacons.
| Expired Contracts Closed | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.237353 ADA | 0.356030 ADA |
| 5 | 0.498704 ADA | 0.748056 ADA |
| 10 | 1.100580 ADA | 1.650870 ADA |
| 13 | 1.448727 ADA | 2.173091 ADA |

Max: 13 Contracts  
Bottleneck: Memory

## Updating Payment Addresses of Active UTxOs

#### All contracts have the same beacons.
| Contracts Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.256667 ADA | 0.385001 ADA |
| 5 | 0.529000 ADA | 0.793500 ADA |
| 10 | 0.884023 ADA | 1.326035 ADA |
| 15 | 1.257796 ADA | 1.886694 ADA |
| 20 | 1.647552 ADA | 2.471328 ADA |

Max: 20 Contracts  
Bottleneck: Tx Size

#### All contracts have different beacons.
| Contracts Updated | Tx Fee | Req. Collateral |
|:--:|:--:|:--:|
| 1 | 0.261055 ADA | 0.391583 ADA |
| 5 | 0.552084 ADA | 0.828126 ADA |
| 10 | 0.930806 ADA | 1.396209 ADA |
| 15 | 1.325625 ADA | 1.988438 ADA |
| 18 | 1.570385 ADA | 2.355578 ADA |

Max: 18 Contracts  
Bottleneck: Tx Size
