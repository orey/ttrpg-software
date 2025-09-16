# README

## About

[Unicreon](https://www.rolis.net/unicreon) is a French game created by Amaury Bouchgard.

This game probably started as a hack of Savage Worlds. It is simpler but quite efficient.

## Probabilities

Attributes are 4 or 5 depending if you are working with powers (attributes are noted in a D&D way):

| Attribute | Name                         |
|-----------|------------------------------|
| STR       | Strength (Might)             |
| DEX       | Dexterity (Agility)          |
| PER       | Perception                   |
| INT       | Intelligence (Willpower)     |
| POW       | Power (D&D would say Wisdom) |

They are noted from d4 to d12 and have a cost (4 points for d4, 6 for d6, etc.).

| Attribute value | Description |
|-----------------|-------------|
| d4              | Weak        |
| d6              | Average     |
| d8              | Good        |
| d10             | Strong      |
| d12             | Expert      |

PCs start with 26 points is using the 4 first attributes only and 34 if using Power.

That means that a starting PC with no powers can have 3 average attributes (18) and 1 good (8), with powers would be 3 average and 2 good.

Here are the various difficulty ratings (DRs)


| DR | Description |
|----|-------------|
| 3  | easy        |
| 4  | average     |
| 5  | hard        |
| 6  | very hard   |
| 8  | unprobable  |
| 10 | impossible  |

These are the probabilities to reach or exceed the difficulty rating.

| DR  | **3** | **4** | **5** | **6** | 7   | **8** | 9   | **10** |
|-----|-------|-------|-------|-------|-----|-------|-----|--------|
| d4  | 50%   | 25%   | 0%    | 0%    | 0%  | 0%    | 0%  | 0%     |
| d6  | 67%   | 50%   | 33%   | 17%   | 0%  | 0%    | 0%  | 0%     |
| d8  | 75%   | 62%   | 50%   | 38%   | 25% | 12%   | 0%  | 0%     |
| d10 | 80%   | 70%   | 60%   | 50%   | 40% | 30%   | 20% | 10%    |
| d12 | 83%   | 75%   | 67%   | 58%   | 50% | 42%   | 33% | 25%    |

There is no wild die, and dice are not exploding.

## Skills

Skills are noted the same as attributes, from d4 to D12, with the same cost (the value of the die).

Skill points are depending on the INT:

These are the probabilities to reach or exceed the difficulty rating.

| INT | Skill points (no POW) | Skill points for powers |
|-----|-----------------------|-------------------------|
| d4  | 34                    | 12                      |
| d6  | 36                    | 18                      |
| d8  | 38                    | 24                      |
| d10 | 42                    | 30                      |
| d12 | 48                    | 36                      |

To succeed, you launch the dice of the attribute and the dice of the skill and if one of them equals of beats the DR, you're OK.

## Probabilities

Here they are per attribute/skill rating.

```
----------
Dif     |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 | 
d4/d4   | 75% | 44% |  0% |  0% |  0% |  0% |  0% |  0% | 
d6/d4   | 83% | 62% | 33% | 17% |  0% |  0% |  0% |  0% | 
d8/d4   | 88% | 72% | 50% | 38% | 25% | 12% |  0% |  0% | 
d10/d4  | 90% | 78% | 60% | 50% | 40% | 30% | 20% | 10% | 
d12/d4  | 92% | 81% | 67% | 58% | 50% | 42% | 33% | 25% | 
----------
Dif     |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 | 
d4/d6   | 83% | 62% | 33% | 17% |  0% |  0% |  0% |  0% | 
d6/d6   | 89% | 75% | 56% | 31% |  0% |  0% |  0% |  0% | 
d8/d6   | 92% | 81% | 67% | 48% | 25% | 12% |  0% |  0% | 
d10/d6  | 93% | 85% | 73% | 58% | 40% | 30% | 20% | 10% | 
d12/d6  | 94% | 88% | 78% | 65% | 50% | 42% | 33% | 25% | 
----------
Dif     |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 | 
d4/d8   | 88% | 72% | 50% | 38% | 25% | 12% |  0% |  0% | 
d6/d8   | 92% | 81% | 67% | 48% | 25% | 12% |  0% |  0% | 
d8/d8   | 94% | 86% | 75% | 61% | 44% | 23% |  0% |  0% | 
d10/d8  | 95% | 89% | 80% | 69% | 55% | 39% | 20% | 10% | 
d12/d8  | 96% | 91% | 83% | 74% | 62% | 49% | 33% | 25% | 
----------
Dif     |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 | 
d4/d10  | 90% | 78% | 60% | 50% | 40% | 30% | 20% | 10% | 
d6/d10  | 93% | 85% | 73% | 58% | 40% | 30% | 20% | 10% | 
d8/d10  | 95% | 89% | 80% | 69% | 55% | 39% | 20% | 10% | 
d10/d10 | 96% | 91% | 84% | 75% | 64% | 51% | 36% | 19% | 
d12/d10 | 97% | 92% | 87% | 79% | 70% | 59% | 47% | 32% | 
----------
Dif     |   3 |   4 |   5 |   6 |   7 |   8 |   9 |  10 | 
d4/d12  | 92% | 81% | 67% | 58% | 50% | 42% | 33% | 25% | 
d6/d12  | 94% | 88% | 78% | 65% | 50% | 42% | 33% | 25% | 
d8/d12  | 96% | 91% | 83% | 74% | 62% | 49% | 33% | 25% | 
d10/d12 | 97% | 92% | 87% | 79% | 70% | 59% | 47% | 32% | 
d12/d12 | 97% | 94% | 89% | 83% | 75% | 66% | 56% | 44% | 

```

## Other rules

| Attribute | HP (STR) | Karma (PER) | Wealth (DEX) |
|-----------|----------|-------------|--------------|
| d4        | 10       | 1           | 30           |
| d6        | 11       | 2           | 40           |
| d8        | 12       | 3           | 50           |
| d10       | 13       | 4           | 60           |
| d12       | 15       | 5           | 80           |

Skills:

| Attribute | Skill                     |
|-----------|---------------------------|
| STR       | Athletics                 |
| STR       | Physical resistance       |
| STR       | Unarmed combat            |
| STR       | Melee combat (per weapon) |
| DEX       | Range combat (per weapon) |
| DEX       | Acrobatics                |
| DEX       | Swimming/Diving           |
| DEX       | Climbing                  |
| DEX       | Ride/drive                |
| DEX       | Pick lock                 |
| DEX       | Craft                     |
| DEX       | Theft                     |
| INT       | Botanic                   |
| INT       | Cooking                   |
| INT       | Ethnology                 |
| INT       | Geography                 |
| INT       | Intuition                 |
| INT       | Medicine                  |
| INT       | Languages (per language)  |
| INT       | Animal care               |
| INT       | Zoology                   |
| PER       | Beautiful                 |
| PER       | Influence (baratin)       |
| PER       | Representation            |
| PER       | Seduce                    |
| PER       | Disguise                  |
| PER       | Detect lies               |
| PER       | Detect traps              |
| PER       | Stealth                   |
| PER       | Animal training           |
| PER       | Negotiate                 |
| PER       | Tracking                  |
| PER       | Seek                      |
| PER       | Enhanced vision           |
| POW       | Mental resistance         |
 



