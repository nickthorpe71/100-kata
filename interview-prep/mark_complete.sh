#!/bin/bash

# Mark a problem as complete and update progress
# Usage: ./mark_complete.sh problem-name status time [notes]

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SESSION_FILE="$SCRIPT_DIR/.session_state"
README_FILE="$SCRIPT_DIR/README.md"

# Colors
GREEN='\033[0;32m'
RED='\033[0;31m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m'

if [ $# -lt 3 ]; then
    echo -e "${RED}Usage: $0 <problem-name> <status> <time> [notes]${NC}"
    echo -e "${BLUE}Status options: solved, attempted, needs_revisit${NC}"
    echo -e "${BLUE}Time format: 25min, 1h5min, etc.${NC}"
    exit 1
fi

PROBLEM="$1"
STATUS="$2"
TIME="$3"
NOTES="${4:-}"
DATE=$(date +%Y-%m-%d)

# Strip the number prefix (01-, 02-, etc.) from problem name for README
PROBLEM_NAME=$(echo "$PROBLEM" | sed 's/^[0-9]*-//')

# Convert status to emoji
case $STATUS in
    "solved")
        EMOJI="✅"
        ;;
    "attempted")
        EMOJI="🟨"
        ;;
    "needs_revisit")
        EMOJI="🔄"
        ;;
    *)
        echo -e "${RED}Invalid status. Use: solved, attempted, or needs_revisit${NC}"
        exit 1
        ;;
esac

# Update README.md
if [ -f "$README_FILE" ]; then
    # Create backup
    cp "$README_FILE" "$README_FILE.bak"
    
    # Update the specific problem line (use PROBLEM_NAME for README lookup)
    if grep -q "| $PROBLEM_NAME |" "$README_FILE"; then
        # Replace existing line
        sed -i "s/| $PROBLEM_NAME | ⬜ | - | - | - |/| $PROBLEM_NAME | $EMOJI | $DATE | $TIME | $NOTES |/g" "$README_FILE"
        echo -e "${GREEN}✓ Updated $PROBLEM_NAME status to $EMOJI${NC}"
    else
        echo -e "${RED}Problem $PROBLEM_NAME not found in README${NC}"
        exit 1
    fi
else
    echo -e "${RED}README.md not found${NC}"
    exit 1
fi

# Update session state
if [ -f "$SESSION_FILE" ]; then
    source "$SESSION_FILE"
    
    # Update counters based on status
    if [ "$STATUS" = "solved" ]; then
        PROBLEMS_COMPLETED=$((PROBLEMS_COMPLETED + 1))
        CURRENT_STREAK=$((CURRENT_STREAK + 1))
        echo -e "${GREEN}🎉 Problem solved! Streak: $CURRENT_STREAK${NC}"
    elif [ "$STATUS" = "attempted" ]; then
        CURRENT_STREAK=0
        echo -e "${YELLOW}📝 Problem attempted. Keep practicing!${NC}"
    else
        CURRENT_STREAK=0
        echo -e "${BLUE}🔄 Marked for revisit. Good self-awareness!${NC}"
    fi
    
    # Update session file
    echo "PROBLEMS_COMPLETED=$PROBLEMS_COMPLETED" > "$SESSION_FILE"
    echo "LAST_PROBLEM=$PROBLEM" >> "$SESSION_FILE"
    echo "CURRENT_STREAK=$CURRENT_STREAK" >> "$SESSION_FILE"
    echo "TOTAL_SESSIONS=$TOTAL_SESSIONS" >> "$SESSION_FILE"
    echo "AREAS_TO_PRACTICE=$AREAS_TO_PRACTICE" >> "$SESSION_FILE"
fi

echo
echo -e "${BLUE}📊 Progress Update:${NC}"
echo "   Problem: $PROBLEM_NAME (from $PROBLEM)"
echo "   Status: $EMOJI $STATUS"
echo "   Time: $TIME"
echo "   Date: $DATE"
if [ -n "$NOTES" ]; then
    echo "   Notes: $NOTES"
fi
echo "   Problems Completed: $PROBLEMS_COMPLETED/30"
echo "   Current Streak: $CURRENT_STREAK"

echo

# Solution Analysis Section
echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${PURPLE}📊 SOLUTION ANALYSIS${NC}"
echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
echo

# Check if the solution file exists
SOLUTION_FILE="$SCRIPT_DIR/questions/$PROBLEM/src/main.rs"
if [ -f "$SOLUTION_FILE" ] && [ "$STATUS" = "solved" ]; then
    echo -e "${BLUE}Analyzing your solution...${NC}"
    echo
    
    # Create analysis prompt for Claude
    echo -e "${YELLOW}Ask Claude Code for detailed analysis:${NC}"
    echo
    echo "\"Analyze my solution for $PROBLEM_NAME\""
    echo
    echo -e "${BLUE}Claude will provide:${NC}"
    echo "   ✓ Time & Space Complexity of your solution"
    echo "   ✓ Comparison with optimal solution"
    echo "   ✓ Code quality and R* style adherence"
    echo "   ✓ Edge cases you might have missed"
    echo "   ✓ Interview follow-up questions"
    echo
    
    # Automatically prompt for analysis
    echo -e "${GREEN}💡 AUTOMATIC ANALYSIS REQUEST:${NC}"
    echo "──────────────────────────────────────────"
    echo "Copy and paste this to Claude Code:"
    echo
    echo -e "${YELLOW}Analyze my solution for $PROBLEM_NAME in questions/$PROBLEM/src/main.rs${NC}"
    echo -e "${YELLOW}Please provide:${NC}"
    echo -e "${YELLOW}1. Big-O time and space complexity of my solution${NC}"
    echo -e "${YELLOW}2. The most optimal solution with explanation${NC}"
    echo -e "${YELLOW}3. What I did well and what could be improved${NC}"
    echo -e "${YELLOW}4. Common interview follow-ups for this problem${NC}"
    echo -e "${YELLOW}5. Any edge cases I might have missed${NC}"
    echo "──────────────────────────────────────────"
    echo
elif [ "$STATUS" = "attempted" ] || [ "$STATUS" = "needs_revisit" ]; then
    echo -e "${YELLOW}📝 Since you marked this as '$STATUS', consider:${NC}"
    echo "   - Reviewing the optimal solution approach"
    echo "   - Understanding why your solution didn't work"
    echo "   - Asking Claude for hints without seeing the full solution"
    echo
    echo -e "${BLUE}Ask Claude:${NC}"
    echo "\"What's the key insight for $PROBLEM_NAME without showing the solution?\""
    echo
fi

echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
echo

echo -e "${YELLOW}💡 Next steps:${NC}"
echo "   - Run ./continue.sh for next problem"
echo "   - Ask Claude Code to create supplemental problems if you struggled"
echo "   - Review your progress in README.md"

# Celebration for milestones
if [ $PROBLEMS_COMPLETED -eq 10 ]; then
    echo
    echo -e "${GREEN}🎊 MILESTONE: 10 problems completed! You're 1/3 of the way there!${NC}"
elif [ $PROBLEMS_COMPLETED -eq 20 ]; then
    echo
    echo -e "${GREEN}🎊 MILESTONE: 20 problems completed! You're 2/3 of the way there!${NC}"
elif [ $PROBLEMS_COMPLETED -eq 30 ]; then
    echo
    echo -e "${GREEN}🏆 CONGRATULATIONS! All 30 core problems completed!${NC}"
    echo -e "${YELLOW}\"The ultimate aim of martial arts is not having to use them.\" - Miyamoto Musashi${NC}"
fi