#pragma once

class WriteTimer {
public:
    void start(float seconds);
    void update(float dt);
    void pause();
    void resume();
    bool expired() const;
    float remaining() const;
    bool isPaused() const { return paused; }

private:
    float time_remaining = 0.0f;
    bool paused = false;
};
