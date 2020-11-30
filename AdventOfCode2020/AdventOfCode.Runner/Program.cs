using System;
using System.Collections.Generic;
using System.Globalization;
using System.IO;
using System.Reflection;

namespace AdventOfCode.Runner
{
    public static class Program
    {
        private static List<Type> Solutions = new List<Type>{};

        public static void Main(string[] args)
        {
            string header = "Advent of Code 2020";
            string separator = new string('=', header.Length);

            Console.WriteLine(header);
            Console.WriteLine(separator);
            Console.WriteLine();

            for (int i = 0; i < Solutions.Count; i++)
            {
                Type solution = Solutions[i];
                MethodInfo describe = solution.GetMethod("describe");
                string name = describe?.Invoke(null, null) as string;

                Console.WriteLine($"{(i + 1).ToString(CultureInfo.InvariantCulture).PadLeft(2, '0')}: {(!string.IsNullOrEmpty(name) ? name : "(no description)")}");
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

            string ident = puzzle.ToString().PadLeft(2, '0');
            string input = $"App_Data{Path.DirectorySeparatorChar}day{ident}.txt";

            Type target = Solutions[puzzle - 1];
            MethodInfo execute = target.GetMethod("execute");
            if (execute == null)
            {
                Console.WriteLine("Unable to find puzzle entrypoint");
                return;
            }

            object[] parameters = { new[] { input } };
            execute.Invoke(null, parameters);

            Console.WriteLine();
        }

        private static bool TryParseDay(string input, out int result)
        {
            if (!int.TryParse(input, NumberStyles.Any, CultureInfo.InvariantCulture, out result))
                return false;
            if (result <= 0)
                return false;

            return result <= Solutions.Count;
        }
    }
}
