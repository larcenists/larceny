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
        private static int descriptor;
        private static Hashtable open_files;
                
        // Reserve 0,1,2 for standard streams
        private static readonly int STDIN = 0;
        private static readonly int STDOUT = 1;
        //private static readonly int STDERR = 2;

        private static readonly int min_descriptor = 3;
        private static readonly int max_descriptor = SFixnum.maxPreAlloc;
                
        static Unix() {
            // Reserve 0,1,2 for standard streams
            descriptor = min_descriptor;
            open_files = new Hashtable();
            
            open_files[STDIN] = System.Console.OpenStandardInput();
            open_files[STDOUT] = System.Console.OpenStandardOutput();
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
                    s = File.Open(file, fm, fa);
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
    }

    // Syscall magic numbers are defined in 
    // <larceny_src>/Lib/Common/syscall-id.sch
    // Generate the C# enum 'Sys' with larceny-csharp/scripts/syscall-enum.ss
    public class Syscall {

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
            // case Sys.get_resource_usage :
            // case Sys.dump_heap :
            case Sys.exit : exit(); break;
            case Sys.mtime : mtime(); break;
            case Sys.access : access(); break;
            case Sys.rename : rename(); break;
            case Sys.pollinput : pollinput(); break;
            case Sys.getenv : getenv(); break;
            // case Sys.gc : gc(); break;
			case Sys.flonum_log : flonum_log(); break;
			case Sys.flonum_exp : flonum_exp(); break;
            case Sys.flonum_sin : flonum_sin(); break;
            case Sys.flonum_cos : flonum_cos(); break;
            case Sys.flonum_tan : flonum_tan(); break;
            case Sys.flonum_asin : flonum_asin(); break;
            case Sys.flonum_acos : flonum_acos(); break;
            case Sys.flonum_atan : flonum_atan(); break;
            case Sys.flonum_atan2 : flonum_atan2(); break;
            case Sys.flonum_sqrt : flonum_sqrt(); break;
			case Sys.flonum_sinh : flonum_sinh(); break;
			case Sys.flonum_cosh : flonum_cosh(); break;
			
			case Sys.system : system(); break;
			// case Sys.sys_feature : sys_feature(); break;
			
			case Sys.segment_code_address : segment_code_address() ; break;
			case Sys.chdir : chdir() ; break;
			case Sys.cwd : cwd() ; break;

            default: Exn.internalError("unsupported syscall: " + num_proc); break;
            }
        }
                
        // Magic numbers are defined in <larceny_src>/Lib/Common/sys-unix.sch
        private static void open() {
            string file_name = ((SByteVL)Reg.register2).asString();
            int flags = ((SFixnum)Reg.register3).intValue();
            int mode  = ((SFixnum)Reg.register4).intValue();

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
				Unix.Unlink(((SByteVL)Reg.register2).asString()));
        }
                
        private static void close() {
            Reg.Result = Factory.makeFixnum(
				Unix.Close(((SFixnum)Reg.register2).intValue()));
        }
                
        private static void read() {
            int fd = ((SFixnum)Reg.register2).intValue();
            byte[] bytes = ((SByteVL)Reg.register3).elements;
            int count = ((SFixnum)Reg.register4).intValue();
                        
            Reg.Result = Factory.makeFixnum(Unix.Read(fd, ref bytes, count));
        }

        private static void write() {
            int fd = ((SFixnum)Reg.register2).intValue();
            byte[] bytes = ((SByteVL)Reg.register3).elements;
            int count = ((SFixnum)Reg.register4).intValue();
                        
            Reg.Result = Factory.makeFixnum(Unix.Write(fd, bytes, count));
        }

        private static void exit() {
            int retval = ((SFixnum)Reg.register2).intValue();
            Call.exit(retval);
        }

        // file modification time
        private static void mtime() {
            string file = ((SByteVL)Reg.register2).asString();
            SVL v = (SVL)Reg.register3;
                        
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
                Reg.Result = Factory.wrap(-1);
            }
        }
                
        // file exists?
        private static void access() {
            string file = ((SByteVL)Reg.register2).asString();
    	    int result = 2;
            if (File.Exists(file)) {
                result = 0;
            } else {
                result = -1;
            }
    	    Reg.Result = Factory.wrap(result);
        }
                
        // rename file
        private static void rename() {
            string from = ((SByteVL)Reg.register2).asString();
            string to = ((SByteVL)Reg.register3).asString();
            try {
                File.Move(from, to);
                Reg.Result = Factory.makeFixnum(0);
            } catch (Exception) {
                Reg.Result = Factory.wrap(-1);
            }
        }
                
        private static void pollinput() {
            Reg.Result = Factory.makeFixnum(-1);
        }
                
        private static void getenv() {
            // FIXME: #f seems to be right answer... but maybe not
            Reg.Result = SObject.False;
        }

		private static void flonum_log() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Log(arg));
            Reg.Result = dst;
        }
		private static void flonum_exp() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Exp(arg));
            Reg.Result = dst;
		}
		private static void flonum_sin() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Sin(arg));
            Reg.Result = dst;
		}
		private static void flonum_cos() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Cos(arg));
            Reg.Result = dst;
		}
		private static void flonum_tan() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Tan(arg));
            Reg.Result = dst;
		}
		private static void flonum_asin() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Asin(arg));
            Reg.Result = dst;
		}
		private static void flonum_acos() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Acos(arg));
            Reg.Result = dst;
		}
		private static void flonum_atan() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Atan(arg));
            Reg.Result = dst;
		}
		private static void flonum_atan2() {
			double arg1 = ((SByteVL)Reg.register2).unsafeAsDouble(0);
			double arg2 = ((SByteVL)Reg.register3).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register4;
            dst.unsafeSetDouble(Math.Atan2(arg1, arg2));
            Reg.Result = dst;
		}
		private static void flonum_sqrt() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Sqrt(arg));
            Reg.Result = dst;
		}
		private static void flonum_sinh() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Sinh(arg));
            Reg.Result = dst;
		}
		private static void flonum_cosh() {
			double arg = ((SByteVL)Reg.register2).unsafeAsDouble(0);
            SByteVL dst = (SByteVL)Reg.register3;
            dst.unsafeSetDouble(Math.Cosh(arg));
            Reg.Result = dst;
		}
		
		private static void system() {
		    string command = ((SByteVL)Reg.register2).asString();
		    ProcessStartInfo pi = new ProcessStartInfo();
		    pi.FileName = "cmd";
		    pi.Arguments = "/c " + command;
		    pi.UseShellExecute = true;
			
			// The CreateNoWindow field isn't defined in
			// ProcessStartInfo on Rotor. :(
			#if !USING_ROTOR
            pi.CreateNoWindow = true;
			#endif
            
			Process p = Process.Start(pi);
		    p.WaitForExit();
		    Reg.Result = Factory.wrap(p.ExitCode);
		    p.Dispose();
		}
		
//		private static void sys_feature() {
//		    
//		}
		
		private static void segment_code_address() {
		    // file, ns, id, number
		    string file = ((SByteVL)Reg.register2).asString();
		    string ns = ((SByteVL)Reg.register3).asString();
		    int id = ((SFixnum)Reg.register4).value;
		    int number = ((SFixnum)Reg.register5).value;
		    
		    Reg.Result = Load.findCode(file, ns, id, number);
		}
		
		private static void chdir() {
		    string dir = ((SByteVL)Reg.register2).asString();
		    try {
		        Directory.SetCurrentDirectory(dir);
		        Reg.Result = Factory.wrap(0);
		    } catch {
		        Reg.Result = Factory.wrap(-1);
		    }
		}
		
		private static void cwd() {
		    Reg.Result = Factory.wrap(Directory.GetCurrentDirectory());
		}
    }
}
