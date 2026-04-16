class VoteCounter {
    #votes = new Map();

    addVote(candidate) {
        this.validateCandidateInput(candidate);
        if (!this.#votes.has(candidate)) {
            this.#votes.set(candidate, 0);
        }
        this.#votes.set(candidate, this.#votes.get(candidate) + 1);
    }

    removeVote(candidate) {
        this.validateCandidateInput(candidate);
        const candidateVotes = this.#votes.get(candidate);
        if (!candidateVotes) return;
        this.#votes.set(candidate, candidateVotes - 1);
    }

    validateCandidateInput(candidate) {
        if (candidate === "" || typeof candidate !== "string") {
            throw new Error("Invalid Candidate");
        }
    }

    topCandidate() {
        let top = null;
        let highest = 0;
        for (const [candidate, votes] of this.#votes) {
            if (votes > highest) {
                highest = votes;
                top = candidate;
            } else if (votes === highest && candidate < top) {
                top = candidate;
            }
        }
        return top;
    }

    topK(k) {
        if (k <= 0) {
            return [];
        }

        const validCandidates = [];
        for (const [name, votes] of this.#votes.entries()) {
            if (votes > 0) {
                validCandidates.push({ name, votes });
            }
        }
        const sortedCandidates = validCandidates.sort((candidateA, candidateB) => {
            if (candidateA.votes !== candidateB.votes) {
                return candidateB.votes - candidateA.votes;
            }
            return candidateA.name.localeCompare(candidateB.name);
        });

        return sortedCandidates.slice(0, k).map(candidate => candidate.name);
    }
}

function main() {
    console.log("Implement the vote-counter functions for kata 77 and add tests.");
}

if (require.main === module) {
    main();
}

module.exports = {
    VoteCounter,
};
