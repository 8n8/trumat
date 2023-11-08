using Microsoft.VisualStudio.TestTools.UnitTesting;
using FormatLibrary;
using System.Security.Cryptography.X509Certificates;

namespace Test;

[TestClass]
public class UnitTest1
{
    [TestMethod]
    public void HelloWorldFormatted()
    {
      RunOneTest("HelloWorld.elm");
    }

    void RunOneTest(string pathEnd) {
      string inputPath = Path.Combine(testDirPath, "expected", pathEnd);
      string outputPath = Path.Combine(testDirPath, "input", pathEnd);
      string input = File.ReadAllText(inputPath);
      string output = File.ReadAllText(outputPath);
      string formatted = FormatClass.Format(input);
      Assert.AreEqual(output, formatted);
    }

    static string GetTestDirPath([System.Runtime.CompilerServices.CallerFilePath] string path = "") {
      return Path.GetDirectoryName(path) ?? throw new NullReferenceException("Test directory path is null");

    }

    static string testDirPath = GetTestDirPath();
}
