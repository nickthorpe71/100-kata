# Interview Week Plan

Use this plan for the final practice block before technical interviews next week.

## Goal

This is not a breadth plan.
This is a sharpness plan.

The objectives are:
- get interview structure tighter
- use `JS` as the default interview language
- use `C++` selectively when types, pointers, or references clarify a concept
- cover one clean pass of system design

## Current Direction

The language decision has been made:

- Use `JS` for interview execution.
- Use short `C++` comparisons only when they help explain concepts like pointer mutation, pass-by-value vs reference, or tree/list structure.

Do not spend the remaining practice time trying to make `C++` interview-ready from memory.
Use `C++` as a learning aid, not as the primary output language.

## Total Time

About 4-5 hours remain.

If energy drops, cut scope before quality.
Do not cram all of kata `75-100`.

## High-Yield Version

With the remaining time, optimize for repetition quality, not coverage.

The priority is:
- sharpen `JS` interview execution
- keep one linked-list / recursion confidence pass
- refresh one system design prompt cleanly
- finish with a short verbal rep

Do not try to complete every remaining kata unless the earlier rounds move unusually fast.

## Completed Warmup

Block 1 was useful and should remain as reference material:

- reverse linked list
- max depth of binary tree
- number of islands
- top k frequent elements

Carry forward:
- explicit edge-case capture
- exact pointer / recursive state wording
- complexity discipline

## Block 2: Core Coding Rounds In JS

Time: 2-2.5 hours

Purpose:
- cover high-yield interview patterns with real interview structure

Default priority order:
1. [75-three-small-problems](/home/nickthorpe71/code/100-kata/75-three-small-problems)
2. [77-phased-vote-counter](/home/nickthorpe71/code/100-kata/77-phased-vote-counter)
3. [76-alien-dictionary](/home/nickthorpe71/code/100-kata/76-alien-dictionary)
4. [93-three-dp-problems](/home/nickthorpe71/code/100-kata/93-three-dp-problems)

High-yield cut rule:
- definitely do `75`
- definitely do `77`
- do `76` if energy is still strong
- do `93` only if time remains after system design and verbal work

Round rules:
- restate the problem before coding
- list edge cases and assumptions
- state the plan before implementation
- test one normal case, one tricky case, one edge case
- state time and space complexity

What this block covers:
- arrays and hash maps
- changing requirements
- graphs and topological sort
- dynamic programming

Language rules for this block:
- code in `JS`
- if a concept would benefit from comparison, add a short `C++` note and then return to `JS`
- do not let `C++` syntax consume round time

## Block 3: JS Confidence Check With C++ Side Notes

Time: 45-60 minutes

Purpose:
- reinforce linked-list and tree/graph patterns in `JS`
- optionally use `C++` to clarify pointer/reference concepts without making it the implementation language

Do:
1. [78-three-linked-list-problems](/home/nickthorpe71/code/100-kata/78-three-linked-list-problems)
2. tree recursion items from [84-three-tree-graph-problems](/home/nickthorpe71/code/100-kata/84-three-tree-graph-problems)

High-yield cut rule:
- do one linked-list problem
- do one tree or graph recursion problem
- stop once pointer / recursion confidence feels restored

Focus:
- not losing references during list edits
- clean base cases
- recursive state definition
- `null` checks in `JS`, with optional `nullptr` comparison in `C++`

Language checkpoint:
- if this block feels solid in `JS`, stay in `JS` for all final reps
- only use `C++` for brief conceptual cross-checks

## Block 4: System Design

Time: 60-90 minutes

Purpose:
- refresh the communication structure for design interviews

Prompt 1:
- social feed / timeline

Prompt 2:
- rate limiter or chat system

High-yield cut rule:
- do one prompt only
- prefer `social feed / timeline` unless a target company strongly suggests the other prompt

For each design, force this structure:
1. functional requirements
2. non-functional requirements
3. core entities
4. APIs
5. high-level architecture
6. data model and storage choices
7. bottlenecks
8. scaling path
9. tradeoffs

Do not chase every micro-detail.
The goal is calm, structured system design communication.

## Block 5: Verbal Mock In JS

Time: 30-45 minutes

Purpose:
- improve interview signal without more coding fatigue

Run three verbal-only problem rehearsals.

High-yield cut rule:
- if time is tight, do two rehearsals instead of three

For each one, practice only:
- problem restatement
- constraints and assumptions
- edge cases
- high-level approach
- complexity
- test plan

Strong target phrasing:

> Let me restate the problem to make sure I have it right. We are given __ and need to return __. The key edge cases are __. My current plan is __ because __. I’ll validate it with __ and then I’ll give time and space complexity.

## Tomorrow Morning Kickoff

Start the session with:

> Use `interview-prep/CODEX_INTERVIEW_DRILL_COACH.md` and `interview-prep/CPP_INTERVIEW_WEEK_PLAN.md`. We are using `JS` as the interview language and `C++` only for helpful concept comparisons. Start Block 2 in interview mode.

If you want to skip straight into coding rounds, start with:

> Use the drill coach doc and the interview week plan. Start Block 2 with kata 75 in `JS` interview mode.

## What To Record After Each Block

Write down:
- what slowed you down
- what syntax or concepts felt shaky in `JS`
- what `C++` comparisons were actually helpful
- what patterns felt comfortable
- whether `JS` feels automatic enough for Monday

## Non-Negotiables

- Do not code before restating the problem.
- Do not skip edge cases.
- Do not finish without complexity.
- Do not let language syntax derail algorithm reasoning for more than a few minutes before simplifying.
- Default to `JS` unless there is a specific reason to show a `C++` comparison.

## If Time Runs Short

Cut in this order:
1. shorten Block 5 to two rehearsals
2. keep Block 4 to one prompt only
3. cut `93-three-dp-problems`
4. cut `76-alien-dictionary` if needed

Do not cut `75`, `77`, the pointer/recursion check, or the single system design refresh.

## Recommended Remaining Sequence

If about 4 hours remain, use this order:

1. `75-three-small-problems` in `JS`
2. `77-phased-vote-counter` in `JS`
3. one system design prompt
4. one linked-list problem and one tree/graph recursion problem in `JS`
5. two short verbal rehearsals

If you have closer to 5 hours and energy remains:

1. `75-three-small-problems`
2. `77-phased-vote-counter`
3. `76-alien-dictionary`
4. one system design prompt
5. one linked-list problem and one tree/graph recursion problem
6. two verbal rehearsals
