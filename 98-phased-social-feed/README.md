# 98 Phased Social Feed

## Format
Evolving requirements with API design.

## Time Target
45-60 minutes.

## Phase 1
Design a `SocialFeed` supporting:
- `post(userId, postId)`
- `follow(followerId, followeeId)`
- `unfollow(followerId, followeeId)`
- `getFeed(userId)` returning the `10` most recent post IDs from the user and followed users

## Phase 2
Add `deletePost(postId)`.

## Phase 3
Now product wants pagination for older posts.
Explain what data you would keep and what API you would expose.

## Starting Skeleton

```cpp
class SocialFeed {
public:
    void post(int userId, int postId);
    void deletePost(int postId);
    void follow(int followerId, int followeeId);
    void unfollow(int followerId, int followeeId);
    vector<int> getFeed(int userId) const;
};
```

## Interview Focus
- design under changing requirements
- heap-based feed merging
- separating correctness from scalability discussion
