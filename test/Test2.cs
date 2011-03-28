namespace Language.CSharp
{
    public class Class
    {
        public void Method(int a, double b)
        {
            float x = 101.2e10, y = 10, z = .1e-6;
            bool m, n, @void;
            decimal d = 101.6m;
            long l = 60lU;
            char c = 'a', c2 = '@', c3 = '*',
                 c5 = '\x123', c6 = '\u1234', c7 = '\U12345678',
                 c10 = '\\', c11 = '\'';

            string simple = "this is a simple string";
            string txt = "this is \r \n \t \b \a \v \" woooo";
            string vb1 = @"abc";
            string vb2 = @"abc ""def"" ghi";
            string vb = @"
    ""a""
         ""verbatim""
                     ""string!""

       \r\n\t\b\a\v\";
        }

        public void AnotherMethod(bool b, decimal d, char c)
        {
        }
    }
}
