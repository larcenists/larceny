using System;
using System.Reflection;
using Scheme.RT;
using Scheme.Rep;

/* DynLoad
 * Program which dynamically loads all programs given as command-line arguments
 * reporting Result and timing for each individually as well as all together.
 */
namespace Scheme.RT {
    public class DynLoad {
        public static void Main(string[] args) {
            bool keepRunning = true;
            foreach (string arg in args) {
                if (keepRunning) {
                    keepRunning = Load.handleAssembly(Load.findAssembly(arg));
                }
            }
            Exn.msg.WriteLine("Finished loading all modules");
            if (keepRunning) Load.handleGo(new string[0]);
        }
    }
}
