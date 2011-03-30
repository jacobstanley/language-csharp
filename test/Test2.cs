namespace Language.CSharp
{
    public class Class
    {
        public void Method(int a, double b)
        {
            // booleans
            bool m, n = false, @void = true;

            // numbers
            float x = 101.2e10, y = 10, z = .1e-6;
            decimal d = 101.6m;
            long l = 60lU;

            // characters
            char c = 'a', c2 = '@', c3 = '*';
            char c5 = '\x123', c6 = '\u1234', c7 = '\U12345678';
            char c10 = '\\', c11 = '\'';

            // some strings
            string simple = "this is a simple string";
            string txt = "this is \r \n \t \b \a \v \" woooo";
            string vb1 = @"abc";
            string vb2 = @"abc ""def"" ghi";
            string vb = @"
    ""a""
         ""verbatim""
                     ""string!""

       \r\n\t\b\a\v\";

            // some lambda
            //var f = x => x + 1;

            //ulong x = 100;
            //x >>= 3;
            //x <<= 6;

            //global::System.Console.WriteLine("f\'r\"e\\d");
        }
#if 0
#  error :(
#endif

        #region SomeHorribleRegion

        public void AnotherMethod(bool b, decimal d, char c)
        {
        }

        #endregion
    }
}
