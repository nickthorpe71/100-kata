# 80 Phased Rate Limiter

## Format
Evolving requirements with controlled edits between phases.

## Time Target
45-60 minutes.

## Phase 1
Implement `allow(userId, timestamp)` for a rule of at most `3` requests per user in any rolling `60` second window.

## Phase 2
Add per-user limits with `setLimit(userId, limit)`.
Users without a custom limit still use the default limit of `3`.

## Phase 3
Explain how you would prevent stale request history from growing forever and what changes you would make if this needed to run across multiple machines.

## Starting Skeleton

```cpp
class RateLimiter {
public:
    bool allow(const string& userId, int timestamp);
    void setLimit(const string& userId, int limit);
};
```

## Interview Focus
- queues or deques for rolling windows
- keeping a stable design while requirements change
- operational thinking beyond just code
