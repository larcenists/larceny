using System;
using System.IO;
using System.Collections;
using System.Diagnostics;
using Scheme.Rep;

namespace Scheme.RT {

    class NoMoreDescriptorsExn : Exception {
        // pass in the file name we were trying to open.
        public NoMoreDescriptorsExn(string file)
            : base("No more file descriptors available when trying to open "
                   + file) 
        {}
    }

    class StdIOExn : Exception {
        public StdIOExn(string msg) : base(msg) {}
    }
        
    // The standard library code depends heavily upon the behavior
    // of the Unix open(), close(), read(), write(), unlink() system calls.
    // This code aims to replicate that behavior.
    class Unix {
        private static int fake_errno;  // simulates libc's errno
        
        private static int descriptor;

        // open_files : hashtable[int => Stream]
        private static Hashtable open_files;
                
        // Reserve 0,1,2 for standard streams
        private const int STDIN = 0;
        private const int STDOUT = 1;
        private const int STDERR = 2;

        private const int min_descriptor = 3;
        private const int max_descriptor = SFixnum.maxPreAlloc;
                
        static Unix() {
            fake_errno = 0;
            // Reserve 0,1,2 for standard streams
            descriptor = min_descriptor;
            open_files = new Hashtable();
            
            open_files[STDIN] = System.Console.OpenStandardInput();
            open_files[STDOUT] = System.Console.OpenStandardOutput();
            open_files[STDERR] = System.Console.OpenStandardError();
        }

        // 
        private static void check_for_stdio(int fd, string msg) {
            if ((fd >= 0) && (fd <= 2))
                throw new StdIOExn(msg + fd);
        }

        // use an internal counter
        private static void inc_descriptor() {
            // loop around if we get to the max.
            if (descriptor == max_descriptor)
                descriptor = min_descriptor;
            else
                ++descriptor;
        }

        // return the next available descriptor as an int,
        // or throw an exception if no more are available.
        private static int next_descriptor(string file) {
            // iterate until we either find an unused descriptor or
            // we've exhausted all descriptors.
            for (int i = max_descriptor - min_descriptor;
                 open_files.ContainsKey(descriptor) && i > 0;
                 --i) {
                inc_descriptor();
            }
            if (open_files.ContainsKey(descriptor))
                throw new NoMoreDescriptorsExn(file);
            else
                return descriptor;
        }

        private static Stream fd2stream(int fd) {
            object s = open_files[fd];
            if (s is Stream) {
                return (Stream) s;
            } else {
                Exn.internalError("fd " + fd + " is not a stream: " + s);
                return null;
            }
        }

        private static Stream fd2input(int fd) {
            Stream s = fd2stream(fd);
            if (s == null || !s.CanRead) {
                Exn.internalError("file descriptor " + fd + " not open for input");
                return null;
            } else {
                return s;
            }
        }

        private static Stream fd2output(int fd) {
            Stream s = fd2stream(fd);
            if (s == null || !s.CanWrite) {
                Exn.internalError("file descriptor " + fd + " not open for output");
                return null;
            } else {
                return s;
            }
        }

        // Return a int representing a descriptor.  A negative int
        // indicates a generic error which will be handled by the 
        // standard library.
        public static int Open(string file, FileMode fm, FileAccess fa) {
            try {
                Stream s;
                if (file.Equals("CON:")) {
                    if (fa == FileAccess.Read) {
                        s = System.Console.OpenStandardInput();
                    } else {
                        s = System.Console.OpenStandardOutput();
                    }
                } else {
                    s = File.Open(file, fm, fa, (fa == FileAccess.Read) ? FileShare.Read : FileShare.None);
                }
                int fd = next_descriptor(file);
                open_files[fd] = s;
                return fd;
            } catch (IOException) {
                return -1;
            } catch (NoMoreDescriptorsExn) {
                return -1;
            }
        }

        // return 0 on success, -1 on error
        public static int Close(int fd) {
            check_for_stdio(fd, "Tried to close FD: ");

            try {
                fd2stream(fd).Close();
                open_files.Remove(fd);
                return 0;
            } catch (Exception) {
                return -1;
            }
        }

        // return 0 on success, -1 on error
        public static int Unlink(string path) {
            try {
                File.Delete(path);
                return 0;
            } catch (Exception) {
                return -1;
            }
        }

        // return number of bytes actually read on success,
        // 0 on EOF,
        // -1 on error.
        public static int Read(int fd, ref byte[] bytes, int count) {
            try {
                return fd2input(fd).Read(bytes, 0, count);
            } catch (Exception) {
                // either the file descriptor doesn't exist, the file
                // wasn't opened with read access, or an IOException
                return -1;
            }
        }

        // return number of bytes actually written on success,
        // -1 on error
        public static int Write(int fd, byte[] bytes, int count) {
            try {
                // Stream.Write() doesn't return number of chars
                // actually written.  If it's successful, I assume all
                // of them are written.  The MS docs aren't very specific.
                fd2output(fd).Write(bytes, 0, count);
                return count;
            } catch (Exception) {
                return -1;
            }
        }

        // return new stream position,
        // -1 on error
        public static long LSeek(int fd, long offset, int whence_code) {
            SeekOrigin whence = SeekOrigin.Begin;
            if (whence_code == 0)
                whence = SeekOrigin.Begin;
            else if (whence_code == 1)
                whence = SeekOrigin.Current;
            else if (whence_code == 2)
                whence = SeekOrigin.End;
            else Exn.fault(Constants.EX_ASSERT, "bad whence_code for lseek");
            try {
                return fd2stream(fd).Seek(offset, whence);
            } catch (Exception) {
                return -1;
            }
        }

        // simulates libc's errno, just for compatibility with other
        // varieties of Larceny.

        public static int GetErrno() { return fake_errno; }

        public static int SetErrno( int n ) {
            int r = fake_errno;
            fake_errno = n;
            return r;
        }
    }

    // Syscall magic numbers are defined in 
    // <larceny_src>/Lib/Common/syscall-id.sch
    // Generate the C# enum 'Sys' with larceny-csharp/scripts/syscall-enum.ss
    public class Syscall {

        private static
        int system_boot_tick = Environment.TickCount;

        private Syscall() {}

        // type checking is done in the scheme code that implements
        // the standard library.  if we get here, we can assume
        // that the parameters in the registers are all as expected.
		// (starting at register 2)
        public static void dispatch(int num_args, Sys num_proc) {
            switch (num_proc) {
            case Sys.open : open(); break;
            case Sys.unlink : unlink(); break;
            case Sys.close : close(); break;
            case Sys.read : read(); break;
            case Sys.write : write(); break;
            case Sys.get_resource_usage : get_resource_usage(); break;
            // case Sys.dump_heap :
            case Sys.exit : exit(); break;
            case Sys.mtime : mtime(); break;
            case Sys.access : access(); break;
            case Sys.rename : rename(); break;
            case Sys.pollinput : pollinput(); break;
            case Sys.getenv : getenv(); break;
            case Sys.setenv : setenv(); break;
            // case Sys.gc : gc(); break;
            case Sys.flonum_acos:  flonum_acos(); break;
            case Sys.flonum_asin:  flonum_asin(); break;
            case Sys.flonum_atan:  flonum_atan(); break;
            case Sys.flonum_atan2: flonum_atan2(); break;
            case Sys.flonum_cos:   flonum_cos(); break;
            case Sys.flonum_cosh:  flonum_cosh(); break;
            case Sys.flonum_exp:   flonum_exp(); break;
            case Sys.flonum_log:   flonum_log(); break;
            case Sys.flonum_sin:   flonum_sin(); break;
            case Sys.flonum_sinh:  flonum_sinh(); break;
            case Sys.flonum_sqrt:  flonum_sqrt(); break;
            case Sys.flonum_tan:   flonum_tan(); break;

            case Sys.system : system(); break;
            case Sys.c_ffi_dlsym: FFI.ffi_syscall(); break;
            case Sys.sro:         fake_sro(); break;
            case Sys.sys_feature: sys_feature(); break;

            case Sys.segment_code_address : segment_code_address() ; break;
            case Sys.chdir : chdir() ; break;
            case Sys.cwd : cwd() ; break;

            case Sys.errno :    geterrno(); break;
            case Sys.seterrno : seterrno(); break;
//          case Sys.time :     gettime();  break;
            case Sys.lseek :    lseek();    break;

            case Sys.sysglobal:
                SObject g = (SObject) Reg.globals[((SByteVL)Reg.Register2).asString()];
                if (g == null) {
                    Reg.Result = Factory.Undefined;
                } else {
                    Reg.Result = g;
                }
                break;
        
            default: Exn.internalError("unsupported syscall: " + num_proc); break;
            }
        }

        // Magic numbers are defined in <larceny_src>/Lib/Common/sys-unix.sch
        private static void open() {
            string file_name = ((SByteVL)Reg.Register2).asString();
            int flags = ((SFixnum)Reg.Register3).intValue();
            int mode  = ((SFixnum)Reg.Register4).intValue();

            FileAccess fa = 0;
            if ((flags & 0x01) != 0) fa |= FileAccess.Read;
            if ((flags & 0x02) != 0) fa |= FileAccess.Write;

            FileMode fm;

            // use append mode (file must already exist)
            if ((flags & 0x04) != 0) 
                fm = FileMode.Append;
            // if it exists, truncate, otherwise create a new file
            else if (((flags & 0x08) != 0) && ((flags & 0x10) != 0)) 
                fm = FileMode.Create;
            // create iff it doesn't exist
            else if ((flags & 0x08) != 0)
                fm = FileMode.CreateNew;
            // truncate iff it already exists
            else if ((flags & 0x10) != 0)
                fm = FileMode.Truncate;
            else {
                if ((fa & FileAccess.Read) != 0) {
                    fm = FileMode.Open;
                } else {
                    fm = FileMode.OpenOrCreate;
                }
            }

            Reg.Result = Factory.makeFixnum(Unix.Open(file_name, fm, fa));
        }
                
        private static void unlink() {
            Reg.Result = Factory.makeFixnum(
                                Unix.Unlink(((SByteVL)Reg.Register2).asString()));
        }
                
        private static void close() {
            Reg.Result = Factory.makeFixnum(
                                Unix.Close(((SFixnum)Reg.Register2).intValue()));
        }

        private static void read() {
            int fd = ((SFixnum)Reg.Register2).intValue();
            byte[] bytes = ((SByteVL)Reg.Register3).elements;
            int count = ((SFixnum)Reg.Register4).intValue();

            Reg.Result = Factory.makeFixnum(Unix.Read(fd, ref bytes, count));
        }

        private static void write() {
            int fd = ((SFixnum)Reg.Register2).intValue();
            byte[] bytes = ((SByteVL)Reg.Register3).elements;
            int count = ((SFixnum)Reg.Register4).intValue();

            Reg.Result = Factory.makeFixnum(Unix.Write(fd, bytes, count));
        }

        // FIXME: limits offset to the size of a fixnum.

        private static void lseek() {
            int fd = ((SFixnum)Reg.Register2).intValue();
            long offset = (long) ((SFixnum)Reg.Register3).intValue();
            int whence_code = ((SFixnum)Reg.Register4).intValue();
            long r = Unix.LSeek(fd, offset, whence_code);
            Reg.Result = Factory.makeFixnum((int) r);
        }

        private static void get_resource_usage() {
            SObject zero = Factory.makeFixnum (0);
            SObject[] stats = ((SVL)Reg.Register2).elements;

            int ticks = unchecked (Environment.TickCount - system_boot_tick);
            stats[28 /*RTIME*/] = Factory.makeNumber (ticks);
            //stats[29 /*STIME*/] = Factory.makeNumber (ticks);
            stats[30 /*UTIME*/] = Factory.makeNumber (ticks);

            // amount of physical memory mapped to the process context.
            long working_set_bytes = Environment.WorkingSet;
            long working_set_words = working_set_bytes / 4;
            stats[19 /*WORDS_HEAP*/] = Factory.makeNumber(working_set_words);
            Reg.Result = Reg.Register2;
        }

        private static void exit() {
            int retval = ((SFixnum)Reg.Register2).intValue();
            Call.exit(retval);
        }

        private static void geterrno() {
            Reg.Result = Factory.makeFixnum(Unix.GetErrno());
        }

        private static void seterrno() {
            int n = ((SFixnum) Reg.Register2).intValue();
            Reg.Result = Factory.makeFixnum(Unix.SetErrno(n));
        }

        // file modification time
        private static void mtime() {
            string file = ((SByteVL)Reg.Register2).asString();
            SVL v = (SVL)Reg.Register3;

            try {
                DateTime time = File.GetLastWriteTime(file);
                v.setElementAt(0, Factory.makeFixnum(time.Year));
                v.setElementAt(1, Factory.makeFixnum(time.Month));
                v.setElementAt(2, Factory.makeFixnum(time.Day));
                v.setElementAt(3, Factory.makeFixnum(time.Hour));
                v.setElementAt(4, Factory.makeFixnum(time.Minute));
                v.setElementAt(5, Factory.makeFixnum(time.Second));
                                
                Reg.Result = Factory.makeFixnum(0);
            }
            // file doesn't exist, or bad path name...
            catch (Exception) {
                Reg.Result = Factory.makeFixnum (-1);
            }
        }

        // file exists?
        private static void access() {
            string file = ((SByteVL)Reg.Register2).asString();
            int operation = ((SFixnum)Reg.Register3).value;
            int result = 2; // WHY?
            if (operation == 0x01) { // FILE EXISTS?
                if (File.Exists(file) || Directory.Exists(file)) {
                    result = 0;
                } else {
                    result = -1;
                }
            } else {
                Exn.internalError("access: read/write/execute checking not supported");
                return;
            }
            Reg.Result = Factory.makeNumber (result);
        }
                
        // rename file
        private static void rename() {
            string from = ((SByteVL)Reg.Register2).asString();
            string to = ((SByteVL)Reg.Register3).asString();
            try {
                File.Move(from, to);
                Reg.Result = Factory.makeFixnum(0);
            } catch (Exception) {
                Reg.Result = Factory.makeFixnum (-1);
            }
        }
                
        private static void pollinput() {
            Reg.Result = Factory.makeFixnum(-1);
        }
                
        private static void getenv() {
            // FIXME: #f seems to be right answer... but maybe not
            string result = Environment.GetEnvironmentVariable (((SByteVL)Reg.Register2).asString());

            Reg.Result = (result == null)
              ? (SObject) Factory.False
              : (SObject) Factory.makeAsciiByteVector (result);
        }

        private static void setenv() {
#if HAS_SETENV_SUPPORT
            try {
              String arg1 = (((SByteVL)Reg.Register2).asString());
              String arg2 = (((SByteVL)Reg.Register3).asString());
              Environment.SetEnvironmentVariable( arg1, arg2 );
              Reg.Result = (SObject) Factory.True;
            } catch (ArgumentException) {
              Reg.Result = (SObject) Factory.False;
            }
#else
            Reg.Result = (SObject) Factory.False;
#endif
	}

        private static void flonum_acos()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Acos (arg));
          Reg.Result = dst;
        }

        private static void flonum_asin()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Asin (arg));
          Reg.Result = dst;
        }

        private static void flonum_atan()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Atan (arg));
          Reg.Result = dst;
        }

        private static void flonum_atan2()
        {
          double arg1 = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          double arg2 = ((SByteVL) Reg.Register3).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register4;
          dst.unsafeSetDouble (0, Math.Atan2 (arg1, arg2));
          Reg.Result = dst;
        }

        private static void flonum_cos()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Cos (arg));
          Reg.Result = dst;
        }

        private static void flonum_cosh()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Cosh (arg));
          Reg.Result = dst;
        }

        private static void flonum_exp()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Exp (arg));
          Reg.Result = dst;
        }

        private static void flonum_log()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Log (arg));
          Reg.Result = dst;
        }

        private static void flonum_sin()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Sin (arg));
          Reg.Result = dst;
        }

        private static void flonum_sinh()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Sinh (arg));
          Reg.Result = dst;
        }

        private static void flonum_sqrt()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Sqrt (arg));
          Reg.Result = dst;
        }

        private static void flonum_tan()
        {
          double arg = ((SByteVL) Reg.Register2).unsafeAsDouble (0);
          SByteVL dst = (SByteVL) Reg.Register3;
          dst.unsafeSetDouble (0, Math.Tan (arg));
          Reg.Result = dst;
        }

        private static void system()
        {
          string command = ((SByteVL) Reg.Register2).asString();
          ProcessStartInfo pi = new ProcessStartInfo();
#if USING_UNIX
          pi.FileName = "sh";
          pi.Arguments = "-c \"" + command +"\"";
#else
          pi.FileName = "cmd";
          pi.Arguments = "/c " + command;
#endif
          // This is set to false because we are manually invoking
          // the shell to run the command.
          pi.UseShellExecute = false;

          // We want to run the process in the same window
          // so we set this to false. This seems like it should
          // be true because we _don't_ want to create a window.
          // Setting it to true starts the process outside of
          // the CommonLarceny window, and waits for it to stop,
          // but this can never happen if it is spawned without 
          // a window in which user interaction needs to occur.
          pi.CreateNoWindow = false;

          Process p = Process.Start (pi);
          p.WaitForExit();
          Reg.Result = Factory.makeNumber (p.ExitCode);
          p.Dispose();
        }

        // returns an empty vector

        private static void fake_sro() {
            Reg.Result = Factory.makeVector(0, Factory.makeFixnum(0));
        }

        // These numbers are supposed to stay in sync 
        // with those in Lib/Common/system-interface.sch
        private static void sys_feature()
        {
          SObject[] v = ((SVL) Reg.Register2).elements;
          int request = ((SFixnum)v[0]).value;
          switch (request) {
          case 0: // larceny-major
              v[0] = Factory.makeFixnum (0);
              break;
          case 1: // larceny-minor
                v[0] = Factory.makeFixnum (97);  // FIXME
//              v[0] = Factory.makeFixnum (Environment.AssemblyVersion.Minor);
              break;
#if HAS_OSVERSION
          case 2: // os-major
              v[0] = Factory.makeNumber (Environment.OSVersion.Version.Major);
              break;
          case 3: // os-minor
              v[0] = Factory.makeNumber (Environment.OSVersion.Version.Minor);
              break;
#else
          case 2: // os-major
              v[0] = Factory.makeNumber (-1);
              break;
          case 3: // os-minor
              v[0] = Factory.makeNumber (-1);
              break;
#endif
          case 4: // gc-info
              v[0] = Factory.False;
              v[1] = Factory.makeFixnum (0);
              break;
          case 5: // gen-info
              v[0] = Factory.False;
              break;
          case 6: // arch-name
              v[0] = Factory.makeFixnum (4);
              break;
          case 7: // os-name
              v[0] = Factory.makeFixnum (3); // win32 // FIXME
              break;
          case 8: // endianness
#if BIG_ENDIAN
              v[0] = Factory.makeFixnum (0); // big
#else
              v[0] = Factory.makeFixnum (1); // little
#endif
              break;
          case 9: // stats-generations
              v[0] = Factory.makeFixnum (0);
              break;
          case 10: // stats-remsets
              v[0] = Factory.makeFixnum (0);
              break;
          case 11: // codevec (not used by Common Larceny)
              v[0] = Factory.makeFixnum (0);  // bytevector
              break;

          // FIXME: The rest of these aren't implemented yet.

          case 12: // foldcase
              v[0] = Factory.makeFixnum (0);
              break;
          case 13: // execmode
              v[0] = Factory.makeFixnum (0); // r5rs is default
              break;
          case 14: // pedantic
              v[0] = Factory.makeFixnum (0);
              break;
          case 15: // r6path
              v[0] = Factory.makeUString ("");
              break;
          case 16: // r6program
              v[0] = Factory.makeUString ("");
              break;
          case 17: // unsafe
              v[0] = Factory.makeFixnum (0);
              break;
          case 18: // transcoder
              v[0] = Factory.makeFixnum (0);
              break;
          }
          Reg.Result = Factory.Unspecified;
        }

        private static void segment_code_address()
        {
          // file, ns, id, number
          string file = ((SByteVL) Reg.Register2).asString();
          string ns = ((SByteVL) Reg.Register3).asString();
          int id = ((SFixnum)Reg.Register4).value;
          int number = ((SFixnum)Reg.Register5).value;

          Reg.Result = Load.findCode(file, ns, id, number);
        }

      private static void chdir()
      {
        string dir = ((SByteVL) Reg.Register2).asString();
        try {
            Directory.SetCurrentDirectory(dir);
            Reg.Result = Factory.makeFixnum (0);
            } catch {
            Reg.Result = Factory.makeFixnum (-1);
            }
      }

      private static void cwd()
      {
        Reg.Result = Factory.makeAsciiByteVector (Directory.GetCurrentDirectory());
      }
    }
}
