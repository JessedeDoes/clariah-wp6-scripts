package utils

import java.io.File

object ProcessFolder {

  def processFolder[T](input: File, action: File => T):Seq[T] =
  {

    if (input.isDirectory) Console.err.println(input.listFiles().toList)
    if (input.isFile)
      Stream(action(input))
    else input.listFiles.toList.flatMap(x => processFolder(x,action) )
  }

  def processFolder(input: File, outputFolder: File, base: (String,String) => Unit): Unit =
  {
    if (!outputFolder.exists())
      outputFolder.mkdir()

    if (input.isDirectory)
    {
      input.listFiles().toList.par.foreach(f =>
      {
        if (f.isFile && (f.getName.endsWith(".xml") || f.getName.endsWith(".xml.gz")))
          processFolder(f, outputFolder, base)
        else if (f.isDirectory)
          processFolder(f, new File(outputFolder + "/" + f.getName), base)
      })
    } else if (input.isFile)
    {
      //Console.err.println(input.getName)
      val outFile = outputFolder + "/" + input.getName()
      base(input.getCanonicalPath, outFile)
    }
  }
}