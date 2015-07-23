val path = 
  case OS.Process.getEnv "BOOTCLASSPATH" of
    NONE => 
    (print 
  "\nSet BOOTCLASSPATH to the zip or jar file containing system classes\n";
     OS.Process.exit OS.Process.failure)

  | SOME path => path;

val version_id = #version_id Compiler.version;
val newer = hd version_id > 110 orelse hd (tl version_id) > 0;
if newer then CM.set_root "sources2.cm" else ();
OS.FileSys.chDir "src";
CM.make ();
Version.compileTime := Date.toString (Date.fromTimeLocal (Time.now ()));
SMLofNJ.Internals.GC.messages false;

PackageManager.setBootClassPath (Path.pathToList path);

if Make.makeAndFreezeBasis (true, "build.log")
then SMLofNJ.exportFn ("../bin/mlj-jdk1.1.1", TopLevel.main)
else (print "\nFailed to build basis\n"; OS.Process.exit OS.Process.failure);
    
