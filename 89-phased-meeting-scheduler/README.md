# 89 Phased Meeting Scheduler

## Format
Evolving requirements with scope expansion.

## Time Target
45-60 minutes.

## Phase 1
Given two sorted lists of available time slots and a meeting duration, return the earliest slot where both people can meet.

## Phase 2
Now support any number of participants.
Each participant still provides a sorted list of available slots.

## Phase 3
Support `cancelAvailability(personId, start, end)` and discuss how you would avoid recomputing everything from scratch after every update.

## Starting Signature

```cpp
pair<int, int> earliestMeetingSlot(
    const vector<vector<pair<int, int>>>& allAvailability,
    int duration
);
```

## Interview Focus
- two-pointer interval reasoning
- scaling a simple baseline to more participants
- preserving correctness as the API grows
