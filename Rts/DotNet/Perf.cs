using System;
using System.Diagnostics;
namespace Perf {

  class Perf {

  static void Main () {
    // Delete existing counters
    if (PerformanceCounterCategory.Exists("Scheme"))
        PerformanceCounterCategory.Delete("Scheme");

    CounterCreationDataCollection CCDC = new CounterCreationDataCollection();
    CCDC.Add (new CounterCreationData ("Apply Setup", "Calls to applySetup per second", PerformanceCounterType.RateOfCountsPerSecond32));
    CCDC.Add (new CounterCreationData ("Trampoline Bounces", "Trampoline bounces per second", PerformanceCounterType.RateOfCountsPerSecond32));
    CCDC.Add (new CounterCreationData ("Scheme Call Exceptions", "Scheme Call Exceptions per second", PerformanceCounterType.RateOfCountsPerSecond32));
    CCDC.Add (new CounterCreationData ("Millicode Support Calls", "Millicode Support Calls per second", PerformanceCounterType.RateOfCountsPerSecond32));
    CCDC.Add (new CounterCreationData ("Stack Flushes", "Stack Flushes per second", PerformanceCounterType.RateOfCountsPerSecond32));
    CCDC.Add (new CounterCreationData ("Stack Reloads", "Stack Reloads per second", PerformanceCounterType.RateOfCountsPerSecond32));

    PerformanceCounterCategory.Create ("Scheme", "Runtime", CCDC);
    Console.WriteLine("Created Scheme performance counters.");
    }
  }

}
