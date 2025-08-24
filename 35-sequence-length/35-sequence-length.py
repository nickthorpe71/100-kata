from typing import List

def split_digits(n: int) -> List[int]:
    curr = n
    res = []
    while curr > 0: 
        res.append(curr % 10)
        curr //= 10
    return res
    
def sum_of_squares(n: int) -> int:
    return sum([x * x for x in split_digits(n)])

def repeat_sequence_len(n: int) -> int:
    index_dict = {}
    current_res = n
    i = 0

    while True:
        new_res = sum_of_squares(current_res)

        if new_res in index_dict:
            return i - index_dict[new_res]
        else:
            index_dict[new_res] = i;
        
        current_res = new_res
        i += 1

print(repeat_sequence_len(123))