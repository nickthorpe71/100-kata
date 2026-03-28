class VoteCounter {
  addVote(candidate) {
    throw new Error("Not implemented");
  }

  removeVote(candidate) {
    throw new Error("Not implemented");
  }

  topCandidate() {
    throw new Error("Not implemented");
  }

  topK(k) {
    throw new Error("Not implemented");
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
