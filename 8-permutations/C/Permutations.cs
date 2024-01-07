using System;
using System.Collections.Generic;

class Program
{
    static void Main(string[] args)
    {
        Console.WriteLine("Enter a string:");
        string input = Console.ReadLine();
        var permutations = Permute(input, 0, input.Length - 1);

        Console.WriteLine("All permutations:");
        foreach (var permutation in permutations)
        {
            Console.WriteLine(permutation);
        }
    }

    static IEnumerable<string> Permute(string str, int l, int r)
    {
        if (l == r)
        {
            return new List<string> { str };
        }
        else
        {
            var result = new List<string>();
            for (int i = l; i <= r; i++)
            {
                str = Swap(str, l, i);
                result.AddRange(Permute(str, l + 1, r));
                str = Swap(str, l, i); // backtrack
            }
            return result;
        }
    }

    static string Swap(string a, int i, int j)
    {
        char temp;
        char[] charArray = a.ToCharArray();
        temp = charArray[i];
        charArray[i] = charArray[j];
        charArray[j] = temp;
        return new string(charArray);
    }
}
