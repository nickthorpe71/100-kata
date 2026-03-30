#include "timer.h"

void WriteTimer::start(float seconds) {
    time_remaining = seconds;
    paused = false;
}

void WriteTimer::update(float dt) {
    if (!paused && time_remaining > 0.0f) {
        time_remaining -= dt;
        if (time_remaining < 0.0f) time_remaining = 0.0f;
    }
}

void WriteTimer::pause() {
    paused = true;
}

void WriteTimer::resume() {
    paused = false;
}

bool WriteTimer::expired() const {
    return time_remaining <= 0.0f;
}

float WriteTimer::remaining() const {
    return time_remaining;
}
