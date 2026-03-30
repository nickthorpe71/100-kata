function twoSumIndices(nums, target) {
    const cache = new Map();

    for (let i = 0; i < nums.length; i++) {
        const needed = target - nums[i];
        if (cache.has(needed)) {
            return [cache.get(needed), i];
        }
        cache.set(nums[i], i);
    }

    return [];
}

function validNearPalindrome(s) {
    function isValid(left, right, canSkip) {
        while (left < right) {
            if (s[left] === s[right]) {
                left++;
                right--;
                continue;
            }
            if (!canSkip) return false;
            return isValid(left + 1, right, false) || isValid(left, right - 1, false);
        }
        return true;
    }
    return isValid(0, s.length - 1, true);
}

function daysUntilWarmer(temperatures) {
    if (temperatures.length === 0) {
        return [];
    }

    const result = new Array(temperatures.length).fill(0);
    const stack = [];

    for (let i = 0; i < temperatures.length; i++) {
        while (
            stack.length > 0 &&
            temperatures[i] > temperatures[stack[stack.length - 1]]
        ) {
            const popped = stack.pop();
            result[popped] = i - popped;
        }
        stack.push(i);
    }

    return result;
}

const temps = [11, 0, 22, 1, 1, 3, 12];
console.log(daysUntilWarmer(temps));