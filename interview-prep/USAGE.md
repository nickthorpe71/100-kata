# Interview Prep Usage Guide

## Quick Start

### 1. Start a Session
```bash
cd interview-prep
./continue.sh
```

This script will:
- Show your current progress 
- Let you pick next problem (sequential, specific, or random)
- Give you motivational quotes and session goals
- Tell you exactly what to say to Claude Code

### 2. Begin Interview with Claude Code
After running `./continue.sh`, it will tell you to say:
```
Interview mode: [problem-name]
```

For example: `Interview mode: pair-finder`

### 3. Complete the Interview Session
- Solve the problem in 45 minutes
- Follow R* Rust principles
- Think out loud
- Ask Claude for clarification when needed

### 4. Mark Your Progress
```bash
./mark_complete.sh problem-name status time [notes]
```

**Examples:**
```bash
./mark_complete.sh pair-finder solved 35min "Used HashMap, got optimal O(n) solution"
./mark_complete.sh stock-trader attempted 1h5min "Struggled with edge cases"
./mark_complete.sh triple-finder needs_revisit 45min "Need more practice with two-pointer"
```

**Status Options:**
- `solved` - Got working solution ✅
- `attempted` - Tried but didn't complete 🟨  
- `needs_revisit` - Need more practice 🔄

## Workflow

```
./continue.sh → Pick Problem → "Interview mode: X" → Solve → ./mark_complete.sh
```

## Features

### Progress Tracking
- **Session state** - Tracks streaks, total sessions, completed problems
- **README updates** - Automatically updates progress table
- **Milestones** - Celebrates at 10, 20, 30 problems completed

### Problem Selection
1. **Sequential** - Next available problem in order
2. **Specific** - Choose from list of available problems  
3. **Random** - Surprise yourself
4. **Review** - See completed problems
5. **Study areas** - Open README to review growth areas

### Smart Features
- Won't show you completed problems
- Tracks current streak
- Shows motivational quotes
- Creates session history
- Prevents accidental spoilers

## Files Created

### Automatic Files (Don't Edit)
- `.session_state` - Tracks your progress data
- `README.md.bak` - Backup of progress table

### Manual Files (You Can Edit)
- `README.md` - Add notes to "Growth Areas" and "Strengths" sections
- Problem `solution.rs` files - Your actual solutions

## Tips

### Before Each Session
- Clear your mind, focus mode
- Have water and snacks ready  
- Set timer for 45 minutes
- Remember R* principles

### During Session
- Read problem carefully
- Ask clarifying questions
- Start with brute force if needed
- Think out loud
- Don't peek at hints too early

### After Session  
- Mark completion honestly
- Note what you struggled with
- Ask Claude for supplemental problems if needed
- Review your solution for R* compliance

## Troubleshooting

### Script Won't Run
```bash
chmod +x continue.sh mark_complete.sh
```

### Problem Not Found
Check spelling - use exact name from questions/ directory

### Progress Not Updating
Make sure you're in the interview-prep directory when running scripts

## Advanced Usage

### Create Supplemental Problems
After struggling with a concept, ask Claude:
```
"Create supplemental problems for [struggling area] in supplemental/"
```

### Review Patterns
```bash
grep "needs_revisit\|attempted" README.md
```

### Reset Progress (Nuclear Option)
```bash
rm .session_state
# Edit README.md to reset all ✅ back to ⬜
```

---

*"The way of the warrior is to stop trouble before it starts."* - Miyamoto Musashi  
*The way of the coder is to debug before you run.* - Ancient Developer Proverb