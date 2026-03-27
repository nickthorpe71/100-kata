# 95 Phased Task Scheduler

## Format
Evolving requirements with scheduling follow-ups.

## Time Target
45-60 minutes.

## Phase 1
Given tasks represented by capital letters and a cooldown `n`, return the minimum number of time units needed to finish all tasks.

## Phase 2
Return one valid schedule string as well, using `_` for idle slots.

## Phase 3
Now each task type can have its own cooldown.
Explain what parts of your earlier solution still hold and what must change.

## Starting Signature

```cpp
int leastInterval(const vector<char>& tasks, int cooldown);
string buildSchedule(const vector<char>& tasks, int cooldown);
```

## Interview Focus
- frequency counting
- max-heap style scheduling
- adapting when a closed-form shortcut no longer applies
