def next_higher_with_same_ones(n):
    c = n
    c0 = c1 = 0
    
    # Count trailing zeros (c0)
    while ((c & 1) == 0) and (c != 0):
        c0 += 1
        c >>= 1
    
    # Count the ones before the trailing zeros (c1)
    while (c & 1) == 1:
        c1 += 1
        c >>= 1
    
    # Error if n is of the form 111...11000...00
    if c0 + c1 == 31 or c0 + c1 == 0:
        return -1
    
    # Step 2: Flip the rightmost non-trailing zero
    pos = c0 + c1
    n |= (1 << pos)  # Flip the rightmost non-trailing zero
    
    # Step 3: Clear all the bits to the right of pos
    n &= ~((1 << pos) - 1)
    
    # Step 4: Insert (c1-1) ones on the right.
    n |= (1 << (c1 - 1)) - 1
    
    return n

# Test cases
print(next_higher_with_same_ones(129))   # Output: 130
print(next_higher_with_same_ones(127))   # Output: 191
print(next_higher_with_same_ones(1))     # Output: 2
print(next_higher_with_same_ones(323423))# Output: 323439

# --------------------------------
# Simplified but less effecient
# --------------------------------

def next_higher_with_same_ones_sible(n):
    # Count the number of '1' bits in the binary representation of n
    num_ones = bin(n).count('1')
    
    # Find the next higher number with the same number of '1' bits
    m = n + 1
    while bin(m).count('1') != num_ones:
        m += 1
    
    return m

# Test cases
print(next_higher_with_same_ones_sible(129))   # Output: 130
print(next_higher_with_same_ones_sible(127))   # Output: 191
print(next_higher_with_same_ones_sible(1))     # Output: 2
print(next_higher_with_same_ones_sible(323423))# Output: 323439