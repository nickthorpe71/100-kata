# Google Two-Week Interview Plan

Start date: April 10, 2026  
End date: April 23, 2026

Use this plan if you have about `60 minutes` in the morning and `60 minutes` at night.

## Why This Plan Looks Like This

This plan is tuned to your current state:

- Block 1 warmup is already useful reference material
- `75-three-small-problems` is done
- `77-phased-vote-counter` is done
- `76-alien-dictionary` is the next highest-yield coding round
- current coding weaknesses are:
  - coding too early
  - incomplete requirement capture
  - not always identifying the best approach without help
- you are specifically targeting Google in the next few weeks

This role also specifically points toward:

- Google Cloud
- web solutions / full-stack work
- cloud support tooling
- scalable application design
- technical design documents
- stakeholder communication
- customer and agent pain-point analysis
- performance monitoring and improvement
- business systems familiarity:
  - CRM
  - billing
  - SQL / NoSQL
  - logs analysis
  - protocol buffers
  - MapReduce

Google interview guidance emphasizes:

- work-sample problem solving
- general cognitive ability / problem solving
- structured interviewing
- thinking out loud
- behavioral stories with clear structure

Sources used:

- Google Student Blog, “How are interviews at Google different?” (June 19, 2015): https://students.googleblog.com/2015/06/how-are-interviews-at-google-different.html
- Google Student Blog, “Google Interns' Top 5 Interview Tips” (July 29, 2015): https://students.googleblog.com/2015/07/google-interns-top-5-interview-tips.html

Inference from those sources:

- the highest-value short-term prep is not pure LeetCode volume
- it is coding reps plus clear thinking-out-loud plus structured behavioral stories

Inference from the job posting:

- do not prep as if this is only a generic backend SWE loop
- add role-specific reps around:
  - application/tooling design
  - production debugging and performance
  - full-stack tradeoff communication
  - customer-facing prioritization
  - design-doc style explanation

## Daily Structure

Morning block: `60 min`

- use for the hardest work
- coding interview rounds
- system design primary reps
- full verbal restatement + edge cases + plan before coding

Night block: `60 min`

- use for lower-fatigue but still high-value reps
- `code-defense`
- verbal rehearsal
- behavioral stories
- technical experience discussion drills
- review and error logging

## Non-Negotiables

- Do not code before restating the problem.
- Do not skip assumptions or edge cases.
- Do not finish without complexity.
- For Google-style coding reps, think out loud throughout.
- For behavioral reps, use `CAR`:
  - context
  - action
  - result

## What To Track Every Day

Write down three bullets after the night block:

- one thing that slowed you down
- one signal improvement for tomorrow
- one pattern or story to revisit

## Role-Specific Overlays

For this posting, make sure your answers naturally show:

- strong `JS` / web application comfort
- backend and data-system reasoning
- production pragmatism
- design-doc clarity
- stakeholder communication

Add these recurring themes to your prep:

- monitoring and debugging live systems
- performance bottleneck diagnosis
- customer pain-point translation into technical work
- prioritization under ambiguity
- integration-heavy full-stack thinking

## Extra Topics To Be Ready To Speak About

You do not need deep interview drills on all of these, but you should be able to discuss them cleanly:

- SQL vs NoSQL tradeoffs
- when logs, metrics, and traces help different problems
- basic protocol buffer use cases
- what MapReduce-style batch processing is good at
- testing strategy:
  - unit
  - integration
  - end-to-end
- CRM / billing style systems:
  - correctness
  - auditability
  - idempotency
  - failure handling

## Technical Experience Discussion Format

For technical experience questions, do not answer them like vague behavioral prompts.

Use this structure:

1. context
2. symptoms
3. hypotheses
4. debugging or analysis steps
5. root cause
6. fix
7. prevention or follow-up
8. outcome

This is the best format for prompts like:

- “Tell me about a time you had to fix a production issue.”
- “How did you debug it step by step?”
- “Tell me about a performance problem you investigated.”

## Technical Experience Prompt Bank

Use these in evening drills:

- Tell me about a time you had to fix a production issue. How did you debug it step by step?
- Tell me about a time you improved the performance of a slow system. How did you isolate the bottleneck?
- Tell me about a difficult integration issue across systems. How did you narrow down the source of truth?
- Tell me about a time you had incomplete information during an incident. How did you make decisions safely?
- Tell me about a time you had to prioritize urgent support work against planned roadmap work.
- Tell me about a technical design document you wrote. What decisions did you make and why?
- Tell me about a time logs, metrics, or traces changed your understanding of a problem.
- Tell me about a time a customer or support agent pain point changed the roadmap or architecture.
- Tell me about a time you had to make a system more reliable without slowing delivery too much.
- Tell me about a bug that crossed front-end, back-end, and data layers. How did you untangle it?

## Week 1

### Day 1: Friday, April 10, 2026

AM:
- Full coding interview round: [76-alien-dictionary](/home/nickthorpe71/code/100-kata/76-alien-dictionary) in `JS`
- force:
  - restatement
  - assumptions
  - graph model
  - indegree / traversal explanation
  - complexity

PM:
- `code-defense`: 2-3 waves only
- 20 min approach-selection drill:
  - brute force
  - bottleneck
  - optimal approach
- 20 min review notes from `76`

### Day 2: Saturday, April 11, 2026

AM:
- System design: cloud support tooling or agent workflow system
- force this structure:
  - requirements
  - non-functional requirements
  - APIs
  - high-level design
  - data model
  - scaling path
  - bottlenecks
  - tradeoffs
- role-specific focus:
  - ticket / case lifecycle
  - customer and agent workflows
  - auditability
  - integrations with CRM / billing / logs systems

PM:
- Behavioral prep:
  - 3 Google-relevant stories in `CAR`
  - conflict / disagreement
  - ambiguous problem
  - leadership without authority
- 15 min spoken recap of your support-tooling design
- 20 min technical experience drill:
  - production issue debugging, step by step

### Day 3: Sunday, April 12, 2026

AM:
- Coding round: [78-three-linked-list-problems](/home/nickthorpe71/code/100-kata/78-three-linked-list-problems)
- do one linked-list problem in full interview mode
- focus:
  - mutation safety
  - pointer/reference explanation in `JS`
  - no lost references

PM:
- Light day
- 30 min `code-defense`
- 30 min technical experience drill:
  - integration issue or cross-stack bug

### Day 4: Monday, April 13, 2026

AM:
- Coding round: tree/graph recursion item from [84-three-tree-graph-problems](/home/nickthorpe71/code/100-kata/84-three-tree-graph-problems)
- focus:
  - recursive state
  - base cases
  - return value meaning

PM:
- Verbal-only rehearsal:
  - 1 medium problem
  - no coding
  - restatement, assumptions, plan, complexity, test plan
- 25 min technical experience drill:
  - performance bottleneck investigation

### Day 5: Tuesday, April 14, 2026

AM:
- System design: rate limiter or logs/monitoring pipeline
- keep it narrower and cleaner than the larger application prompts
- focus:
  - APIs
  - ingestion path
  - storage choices
  - backpressure
  - performance tradeoffs
  - if rate limiter:
    - sliding window / token bucket
    - distributed enforcement
    - hot keys
  - if monitoring pipeline:
    - logs vs metrics
    - indexing
    - retention
    - alerting

PM:
- Behavioral prep:
  - failure / setback
  - learning quickly
  - making a decision with incomplete information
- 15 min recap from memory
- 15 min data-systems review:
  - SQL vs NoSQL
  - logs vs metrics
- 15 min technical experience drill:
  - incident with incomplete information

### Day 6: Wednesday, April 15, 2026

AM:
- Coding round: [93-three-dp-problems](/home/nickthorpe71/code/100-kata/93-three-dp-problems)
- do one DP problem only
- force:
  - brute force first
  - state definition
  - transition
  - base case
  - complexity

PM:
- `code-defense`
- 20 min DP verbal recap without code
- write one “DP template in words” note

### Day 7: Thursday, April 16, 2026

AM:
- Mixed mock:
  - 35 min coding verbal + implementation start
  - 25 min behavioral / role-fit
- coding prompt should be medium difficulty and unseen if possible

PM:
- Light review only
- reread notes from Days 1-6
- make a short “Week 1 misses” list:
  - requirement misses
  - wrong pattern choices
  - explanation issues
  - role-fit communication misses
- write 2 short bullet outlines for:
  - one production debugging story
  - one performance story

## Week 2

### Day 8: Friday, April 17, 2026

AM:
- Full coding interview round:
  - graph / BFS / DFS medium
  - preferably one not already practiced
- focus:
  - choose the right representation quickly
  - be decisive

PM:
- `code-defense`
- 20 min approach-selection drill
- 20 min graph pattern recap

### Day 9: Saturday, April 18, 2026

AM:
- System design: CRM / billing integration service or notifications
- focus:
  - correctness
  - retries
  - idempotency
  - audit logs
  - data consistency across systems
  - if notifications:
    - delivery model
    - unread state
    - fanout tradeoffs
    - online vs offline users

PM:
- Behavioral prep:
  - collaboration
  - emergent leadership
  - influence without title
- 15 min spoken design summary
- 15 min “write and review detailed technical design documents” rehearsal:
  - explain a design in doc style, not just whiteboard style
- 15 min technical experience drill:
  - design doc you wrote or could credibly discuss

### Day 10: Sunday, April 19, 2026

AM:
- Coding round:
  - intervals or heap problem
  - use [single_track](/home/nickthorpe71/code/100-kata/code-defense/worlds/single_track/tests.cpp) or [priority_bakery](/home/nickthorpe71/code/100-kata/code-defense/worlds/priority_bakery/tests.cpp) for selection ideas
- focus:
  - sorting-based reasoning
  - heap tradeoffs

PM:
- Light day
- 30 min `code-defense`
- 30 min technical experience drill:
  - cross-stack debugging or reliability improvement

### Day 11: Monday, April 20, 2026

AM:
- Verbal-only coding drill
- 3 fast prompts
- no coding
- goal:
  - best approach faster
  - cleaner tradeoff language

PM:
- Review only:
  - revisit all prior coding notes
  - write a one-page “coding interview checklist”
  - write a one-page “system design / design doc checklist”

### Day 12: Tuesday, April 21, 2026

AM:
- System design final primary rep:
  - cloud support tooling again, from memory, cleaner than Day 2
- goal:
  - simpler
  - calmer
  - better tradeoff ordering
  - stronger stakeholder framing

PM:
- Behavioral final primary rep
- pick best 5 stories and tighten each to `2-3 min`
- include:
  - conflict
  - ambiguity
  - leadership
  - failure
  - impact
  - customer advocacy
  - prioritization
- 20 min technical experience drill:
  - logs / metrics / traces story

### Day 13: Wednesday, April 22, 2026

AM:
- Full coding mock
- 45 min realistic round
- 15 min critique
- use `JS`
- do not switch languages

PM:
- Light review
- no heavy new work
- walk through:
  - one graph pattern
  - one DP pattern
  - one pointer/list pattern
  - one interval/heap pattern

### Day 14: Thursday, April 23, 2026

AM:
- Final pre-interview polish block
- choose one:
  - light coding verbal rep
  - light system design recap
- do not do a draining hard round

PM:
- Resume line review
- 3 behavioral stories out loud
- 2 technical experience stories out loud
- 10 min “tell me about yourself”
- stop early

## Coding Priority Order For This Two-Week Window

If you need to cut something, keep this order:

1. `76-alien-dictionary`
2. one linked-list rep
3. one recursion/tree rep
4. one DP rep
5. one graph rep
6. one interval or heap rep

Do not cut all system design.
Do not cut all behavioral work.
That would be a mistake for Google.

## Behavioral Prompts To Rehearse

Prepare concise `CAR` stories for:

- a time you led without authority
- a disagreement with another strong engineer
- a project with ambiguity or changing requirements
- a failure or missed expectation
- a time you learned something quickly
- a tradeoff between speed and quality
- a time you improved performance or reliability
- a time you worked across functions or stakeholders
- a time customer pain points changed your technical plan

## Role-Specific Talking Points

Be ready to speak clearly about:

- a system you designed that had to scale
- a production issue you diagnosed from weak signals
- how you prioritize engineering work with multiple stakeholders
- how you balance product speed with reliability
- how you test integration-heavy systems
- how you write technical design docs that others can execute from

## Best Technical Experience Stories To Prepare

Have strong, structured stories ready for:

- one production incident
- one performance improvement
- one integration / cross-stack debugging issue
- one design doc / architecture decision
- one customer-pain-driven prioritization decision

## Fast Start Prompt For Tomorrow

Use:

> Use `interview-prep/CODEX_INTERVIEW_DRILL_COACH.md`, `interview-prep/CPP_INTERVIEW_WEEK_PLAN.md`, and `interview-prep/GOOGLE_TWO_WEEK_PLAN.md`. We are preparing for Google, using `JS` as the interview language. Start with Day 1 AM: `76-alien-dictionary` in interview mode.
