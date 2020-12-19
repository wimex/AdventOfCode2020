using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Reflection;
using Microsoft.FSharp.Collections;

namespace AdventOfCode.Runner
{
    public static class Program
    {
        private static readonly Dictionary<int, Type> Solutions = new()
        {
            {1, typeof(Day01.Puzzles)}, {2, typeof(Day02.Puzzles)}, {3, typeof(Day03.Puzzles)}, {4, typeof(Day04.Puzzles)},
            {5, typeof(Day05.Puzzles)}, {6, typeof(Day06.Puzzles)}, {7, typeof(Day07.Puzzles)}, {8, typeof(Day08.Puzzles)},
            {9, typeof(Day09.Puzzles)}, {10, typeof(Day10.Puzzles)}, {11, typeof(Day11.Puzzles)}, {12, typeof(Day12.Puzzles)},
            {13, typeof(Day13.Puzzles)}, {14, typeof(Day14.Puzzles)}, {15, typeof(Day15.Puzzles)}, {16, typeof(Day16.Puzzles)},
            {17, typeof(Day17.Puzzles)}, {18, typeof(Day18.Puzzles)}, {19, typeof(Day19.Puzzles)}
        };

        public static void Main(string[] args)
        {
            string header = "Advent of Code 2020";
            string separator = new string('=', header.Length);

            Console.WriteLine(header);
            Console.WriteLine(separator);
            Console.WriteLine();

            foreach ((int key, Type solution) in Solutions)
            {
                if (solution == null)
                    continue;

                PropertyInfo describe = solution.GetProperty("describe");
                string name = describe?.GetValue(null) as string;

                Console.WriteLine($"{key.ToString(CultureInfo.InvariantCulture).PadLeft(2, '0')}: {(!string.IsNullOrEmpty(name) ? name : "(no description)")}");
            }

            Console.WriteLine();
            Console.Write("Please select a puzzle or type 0 to exit: ");

            string selection = Console.ReadLine();
            if (!TryParseDay(selection, out int puzzle))
            {
                Console.WriteLine("Your selection is invalid");
                return;
            }

            Console.WriteLine();
            Console.WriteLine(separator);
            Console.WriteLine();
            
            Type target = Solutions[puzzle];
            MethodInfo execute = target.GetMethod("execute");
            if (execute == null)
            {
                Console.WriteLine("Unable to find puzzle entrypoint");
                return;
            }

            DirectoryInfo root = new(Path.Combine(Directory.GetCurrentDirectory(), "App_Data"));
            string pattern = $"day{puzzle.ToString().PadLeft(2, '0')}*.txt";

            FileInfo[] files = root.GetFiles(pattern);
            object[] parameters = { ListModule.OfSeq(files.Select(f => f.FullName)) };
            execute.Invoke(null, parameters);

            Console.WriteLine();
        }

        private static bool TryParseDay(string input, out int result)
        {
            if (!int.TryParse(input, NumberStyles.Any, CultureInfo.InvariantCulture, out result))
                return false;
            if (result <= 0)
                return false;

            return Solutions.ContainsKey(result);
        }
    }
}
