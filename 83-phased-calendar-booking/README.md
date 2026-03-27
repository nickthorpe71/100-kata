# 83 Phased Calendar Booking

## Format
Evolving requirements with business-rule changes.

## Time Target
45-60 minutes.

## Phase 1
Implement `book(start, end)` that returns `true` if a half-open interval `[start, end)` can be added with no overlap, otherwise `false`.

## Phase 2
Product changes: double booking is allowed, but triple booking is not.
Preserve as much of the working solution as you can.

## Phase 3
Add `earliestAvailable(duration, windowStart, windowEnd)` that returns the earliest start time where a new event of length `duration` can fit without violating the current booking rules.

## Starting Skeleton

```cpp
class CalendarBooking {
public:
    bool book(int start, int end);
    int earliestAvailable(int duration, int windowStart, int windowEnd) const;
};
```

## Interview Focus
- interval reasoning
- extending a correct baseline safely
- speaking clearly about hidden edge cases
