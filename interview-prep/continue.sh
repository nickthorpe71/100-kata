#!/bin/bash

# Interview Prep Session Manager
# Run this to continue your interview preparation journey

set -e

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
SESSION_FILE="$SCRIPT_DIR/.session_state"
README_FILE="$SCRIPT_DIR/README.md"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
PURPLE='\033[0;35m'
NC='\033[0m' # No Color

echo -e "${PURPLE}╔════════════════════════════════════════╗${NC}"
echo -e "${PURPLE}║     🥋 INTERVIEW PREP - MUSASHI MODE   ║${NC}"
echo -e "${PURPLE}╚════════════════════════════════════════╝${NC}"
echo

# Check if this is first run
if [ ! -f "$SESSION_FILE" ]; then
    echo -e "${BLUE}🌟 Welcome, student. Beginning your coding warrior journey...${NC}"
    echo
    echo "Creating session state..."
    echo "PROBLEMS_COMPLETED=0" > "$SESSION_FILE"
    echo "LAST_PROBLEM=" >> "$SESSION_FILE"
    echo "CURRENT_STREAK=0" >> "$SESSION_FILE"
    echo "TOTAL_SESSIONS=0" >> "$SESSION_FILE"
    echo "AREAS_TO_PRACTICE=" >> "$SESSION_FILE"
    echo
    echo -e "${GREEN}✓ Session state initialized${NC}"
    echo
fi

# Source session state
source "$SESSION_FILE"

# Display current progress
echo -e "${YELLOW}📊 CURRENT PROGRESS:${NC}"
echo "   Problems Completed: $PROBLEMS_COMPLETED/30"
echo "   Current Streak: $CURRENT_STREAK"
echo "   Total Sessions: $TOTAL_SESSIONS"
if [ -n "$LAST_PROBLEM" ]; then
    echo "   Last Problem: $LAST_PROBLEM"
fi
echo

# Get list of available problems
AVAILABLE_PROBLEMS=()
COMPLETED_PROBLEMS=()

for dir in "$SCRIPT_DIR"/questions/*/; do
    if [ -d "$dir" ]; then
        problem_name=$(basename "$dir")
        # Check if problem is marked as completed in README
        if grep -q "| $problem_name | ✅" "$README_FILE" 2>/dev/null; then
            COMPLETED_PROBLEMS+=("$problem_name")
        else
            AVAILABLE_PROBLEMS+=("$problem_name")
        fi
    fi
done

# Determine next action
if [ ${#AVAILABLE_PROBLEMS[@]} -eq 0 ]; then
    echo -e "${GREEN}🎉 Congratulations! You've completed all 30 core problems!${NC}"
    echo -e "${BLUE}Check the supplemental/ directory for additional practice.${NC}"
    echo
    echo -e "${PURPLE}\"You have become the master. Now teach others the way.\" - Musashi${NC}"
    exit 0
fi

echo -e "${BLUE}🎯 NEXT STEPS:${NC}"
echo "1. Continue with next problem"
echo "2. Choose specific problem"
echo "3. Random problem"
echo "4. Review completed problems"
echo "5. View study areas"
echo "6. Exit"
echo

read -p "Choose option (1-6): " choice

case $choice in
    1)
        # Next sequential problem
        NEXT_PROBLEM=${AVAILABLE_PROBLEMS[0]}
        echo -e "${GREEN}Continuing with: $NEXT_PROBLEM${NC}"
        ;;
    2)
        # Choose specific problem
        echo -e "${YELLOW}Available problems:${NC}"
        for i in "${!AVAILABLE_PROBLEMS[@]}"; do
            echo "  $((i+1)). ${AVAILABLE_PROBLEMS[i]}"
        done
        echo
        read -p "Enter number: " prob_num
        if [[ "$prob_num" =~ ^[0-9]+$ ]] && [ "$prob_num" -ge 1 ] && [ "$prob_num" -le "${#AVAILABLE_PROBLEMS[@]}" ]; then
            NEXT_PROBLEM=${AVAILABLE_PROBLEMS[$((prob_num-1))]}
            echo -e "${GREEN}Selected: $NEXT_PROBLEM${NC}"
        else
            echo -e "${RED}Invalid selection. Exiting.${NC}"
            exit 1
        fi
        ;;
    3)
        # Random problem
        NEXT_PROBLEM=${AVAILABLE_PROBLEMS[$RANDOM % ${#AVAILABLE_PROBLEMS[@]}]}
        echo -e "${GREEN}Random selection: $NEXT_PROBLEM${NC}"
        ;;
    4)
        # Review completed
        echo -e "${GREEN}✅ COMPLETED PROBLEMS:${NC}"
        for problem in "${COMPLETED_PROBLEMS[@]}"; do
            echo "   - $problem"
        done
        echo
        exit 0
        ;;
    5)
        # View study areas
        echo -e "${BLUE}📚 Opening README for study areas review...${NC}"
        if command -v code &> /dev/null; then
            code "$README_FILE"
        elif command -v vim &> /dev/null; then
            vim "$README_FILE"
        else
            cat "$README_FILE"
        fi
        exit 0
        ;;
    6)
        echo -e "${BLUE}Until next time, warrior. 🥋${NC}"
        exit 0
        ;;
    *)
        echo -e "${RED}Invalid choice. Exiting.${NC}"
        exit 1
        ;;
esac

echo
echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
echo -e "${PURPLE}  \"Today I will do what others won't, so tomorrow I can     ${NC}"
echo -e "${PURPLE}   accomplish what others can't.\" - Jerry Rice              ${NC}"
echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
echo

# Update session state
TOTAL_SESSIONS=$((TOTAL_SESSIONS + 1))
echo "PROBLEMS_COMPLETED=$PROBLEMS_COMPLETED" > "$SESSION_FILE"
echo "LAST_PROBLEM=$NEXT_PROBLEM" >> "$SESSION_FILE"
echo "CURRENT_STREAK=$CURRENT_STREAK" >> "$SESSION_FILE"
echo "TOTAL_SESSIONS=$TOTAL_SESSIONS" >> "$SESSION_FILE"
echo "AREAS_TO_PRACTICE=$AREAS_TO_PRACTICE" >> "$SESSION_FILE"

# Navigate to problem directory
PROBLEM_DIR="$SCRIPT_DIR/questions/$NEXT_PROBLEM"

echo -e "${GREEN}🚀 Starting interview session with: $NEXT_PROBLEM${NC}"
echo -e "${BLUE}📁 Problem directory: questions/$NEXT_PROBLEM${NC}"
echo

# Check if Rust project exists
if [ -f "$PROBLEM_DIR/Cargo.toml" ]; then
    echo -e "${GREEN}✓ Rust project detected${NC}"
    echo -e "${YELLOW}🔧 Quick commands:${NC}"
    echo "   cd questions/$NEXT_PROBLEM"
    echo "   cargo run          # View problem statement"
    echo "   cargo test         # Test your solution (will fail until implemented)"
    echo "   code src/main.rs   # Edit in VS Code"
    echo
    
    echo -e "${BLUE}🎯 Workflow:${NC}"
    echo "   1. Read problem: 'cargo run'"
    echo "   2. Tell Claude: 'Interview mode: $NEXT_PROBLEM'"
    echo "   3. Implement solution in src/main.rs"
    echo "   4. Test solution: 'cargo test'"
    echo "   5. Mark complete: './mark_complete.sh $NEXT_PROBLEM solved 35min'"
    echo
    
    # Automatically show problem statement
    echo -e "${PURPLE}═══════════════════ PROBLEM STATEMENT ═══════════════════${NC}"
    cd "$PROBLEM_DIR"
    cargo run 2>/dev/null || echo -e "${YELLOW}Run 'cargo run' in the problem directory to see the full problem statement${NC}"
    cd "$SCRIPT_DIR"
    echo -e "${PURPLE}═══════════════════════════════════════════════════════════${NC}"
    echo
else
    echo -e "${YELLOW}💡 Tell Claude Code: 'Interview mode: $NEXT_PROBLEM'${NC}"
fi

echo -e "${BLUE}🎯 Session Goals:${NC}"
echo "   - Solve in 45 minutes"
echo "   - Follow R* principles"
echo "   - Think aloud"
echo "   - Ask for clarification if needed"
echo
echo -e "${RED}⚠️  Remember: Don't look at hints until stuck!${NC}"
echo

# Create a simple way to mark completion
echo -e "${BLUE}After your session, run: ./mark_complete.sh $NEXT_PROBLEM [status] [time]${NC}"
echo -e "${BLUE}Status options: solved, attempted, needs_revisit${NC}"
echo -e "${BLUE}Time format: 25min, 1h5min, etc.${NC}"