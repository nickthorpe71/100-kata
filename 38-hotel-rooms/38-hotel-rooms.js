function allocateRooms(customers) {
  // Sort customers based on arrival days
  customers.sort((a, b) => a[0] - b[0]);

  // Priority queue (min-heap) to keep track of rooms by their end times
  const minHeap = new MinHeap();

  // Array to store room allocations
  const roomAllocations = new Array(customers.length);

  // Variable to keep track of room numbers
  let roomNumber = 0;

  // Iterate over the sorted customers
  for (let i = 0; i < customers.length; i++) {
    const [arrival, departure] = customers[i];

    // If the earliest available room is free before the current customer's arrival, reuse that room
    if (!minHeap.isEmpty() && minHeap.peek()[0] < arrival) {
      const [endTime, roomId] = minHeap.poll();
      roomAllocations[i] = roomId;
      minHeap.add([departure, roomId]);
    } else {
      // Otherwise, allocate a new room
      roomNumber++;
      roomAllocations[i] = roomNumber;
      minHeap.add([departure, roomNumber]);
    }
  }

  return roomAllocations;
}

// Min-Heap implementation
class MinHeap {
  constructor() {
    this.heap = [];
  }

  isEmpty() {
    return this.heap.length === 0;
  }

  peek() {
    return this.heap[0];
  }

  add([endTime, roomId]) {
    this.heap.push([endTime, roomId]);
    this._heapifyUp();
  }

  poll() {
    if (this.isEmpty()) return null;

    const min = this.heap[0];
    const end = this.heap.pop();

    if (!this.isEmpty()) {
      this.heap[0] = end;
      this._heapifyDown();
    }

    return min;
  }

  _heapifyUp() {
    let index = this.heap.length - 1;

    while (index > 0) {
      const parentIndex = Math.floor((index - 1) / 2);

      if (this.heap[index][0] >= this.heap[parentIndex][0]) break;

      [this.heap[index], this.heap[parentIndex]] = [
        this.heap[parentIndex],
        this.heap[index],
      ];
      index = parentIndex;
    }
  }

  _heapifyDown() {
    let index = 0;
    const length = this.heap.length;

    while (true) {
      let leftChildIndex = 2 * index + 1;
      let rightChildIndex = 2 * index + 2;
      let smallestIndex = index;

      if (
        leftChildIndex < length &&
        this.heap[leftChildIndex][0] < this.heap[smallestIndex][0]
      ) {
        smallestIndex = leftChildIndex;
      }

      if (
        rightChildIndex < length &&
        this.heap[rightChildIndex][0] < this.heap[smallestIndex][0]
      ) {
        smallestIndex = rightChildIndex;
      }

      if (smallestIndex === index) break;

      [this.heap[index], this.heap[smallestIndex]] = [
        this.heap[smallestIndex],
        this.heap[index],
      ];
      index = smallestIndex;
    }
  }
}

/**
 * @param {[][]} rawCustomer
 * @returns Customer
 */
function formatCustomer(rawCustomer, originalIndex) {
  return {
    arrival: rawCustomer[0],
    departure: rawCustomer[1],
    originalIndex,
  };
}

const tests = [
  {
    customers: [
      [1, 2],
      [2, 4],
      [4, 4],
    ],
    expected: "// [1,2,1] or [2,1,1]",
  },
  {
    customers: [
      [1, 5],
      [2, 4],
      [6, 8],
      [7, 7],
    ],
    expected: "// any of [1,2,1,2], [1,2,2,1], [2,1,2,1], or [2,1,1,2]",
  },
  {
    customers: [
      [15, 22],
      [2, 4],
      [6, 9],
      [3, 33],
      [12, 21],
    ],
    expected: "// [1,2,2,3,2], [2,1,1,3,1], [3,1,3,2,1], etc",
  },

  {
    customers: [
      [1, 10],
      [2, 5],
      [6, 6],
      [3, 7],
      [6, 6],
      [11, 13],
      [9, 15],
      [8, 14],
    ],
    expected:
      "// Solutions include:   [1,2,2,3,4,1,3,2], [1,2,2,3,4,1,2,3], [1,2,4,3,2,1,3,2], [2,3,3,1,4,2,1,3], and others",
  },
];

for (const test of tests) {
  console.log(allocateRooms(test.customers));
  console.log(`Expected: ${test.expected}`);
}
