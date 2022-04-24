/*@internal*/
/** Performance measurements for the compiler. */
namespace ts.performance {
  let perfHooks: PerformanceHooks | undefined;
  // when set, indicates the implementation of `Performance` to use for user timing.
  // when unset, indicates user timing is unavailable or disabled.
  let performanceImpl: Performance | undefined;

  export interface Timer {
    enter(): void;
    exit(): void;
  }

  export function createTimerIf(
    condition: boolean,
    measureName: string,
    startMarkName: string,
    endMarkName: string,
  ) {
    return condition
      ? createTimer(measureName, startMarkName, endMarkName)
      : nullTimer;
  }

  export function createTimer(
    measureName: string,
    startMarkName: string,
    endMarkName: string,
  ): Timer {
    let enterCount = 0;
    return {
      enter,
      exit,
    };

    function enter() {
      if (++enterCount === 1) {
        mark(startMarkName);
      }
    }

    function exit() {
      if (--enterCount === 0) {
        mark(endMarkName);
        measure(measureName, startMarkName, endMarkName);
      } else if (enterCount < 0) {
        Debug.fail("enter/exit count does not match.");
      }
    }
  }

  export const nullTimer: Timer = { enter: noop, exit: noop };

  let enabled = false;
  let timeorigin = timestamp();
  const marks = new Map<string, number>();
  const counts = new Map<string, number>();
  const durations = new Map<string, number>();

  /**
   * Marks a performance event.
   *
   * @param markName The name of the mark.
   */
  export function mark(markName: string) {
    if (enabled) {
      const count = counts.get(markName) ?? 0;
      counts.set(markName, count + 1);
      marks.set(markName, timestamp());
      performanceImpl?.mark(markName);
    }
  }

  /**
   * Adds a performance measurement with the specified name.
   *
   * @param measureName The name of the performance measurement.
   * @param startMarkName The name of the starting mark. If not supplied, the point at which the
   *      profiler was enabled is used.
   * @param endMarkName The name of the ending mark. If not supplied, the current timestamp is
   *      used.
   */
  export function measure(
    measureName: string,
    startMarkName?: string,
    endMarkName?: string,
  ) {
    if (enabled) {
      const end = (endMarkName !== undefined ? marks.get(endMarkName) : undefined)
        ?? timestamp();
      const start = (startMarkName !== undefined ? marks.get(startMarkName) : undefined)
        ?? timeorigin;
      const previousDuration = durations.get(measureName) || 0;
      durations.set(measureName, previousDuration + (end - start));
      performanceImpl?.measure(measureName, startMarkName, endMarkName);
    }
  }

  /**
   * Gets the number of times a marker was encountered.
   *
   * @param markName The name of the mark.
   */
  export function getCount(markName: string) {
    return counts.get(markName) || 0;
  }

  /**
   * Gets the total duration of all measurements with the supplied name.
   *
   * @param measureName The name of the measure whose durations should be accumulated.
   */
  export function getDuration(measureName: string) {
    return durations.get(measureName) || 0;
  }

  /**
   * Iterate over each measure, performing some action
   *
   * @param cb The action to perform for each measure
   */
  export function forEachMeasure(
    cb: (measureName: string, duration: number) => void,
  ) {
    durations.forEach((duration, measureName) => cb(measureName, duration));
  }

  /**
   * Indicates whether the performance API is enabled.
   */
  export function isEnabled() {
    return enabled;
  }

  /** Enables (and resets) performance measurements for the compiler. */
  export function enable(system: System = sys) {
    if (!enabled) {
      enabled = true;
      perfHooks ||= tryGetNativePerformanceHooks();
      if (perfHooks) {
        timeorigin = perfHooks.performance.timeOrigin;
        // NodeJS's Web Performance API is currently slower than expected, but we'd still like
        // to be able to leverage native trace events when node is run with either `--cpu-prof`
        // or `--prof`, if we're running with our own `--generateCpuProfile` flag, or when
        // running in debug mode (since its possible to generate a cpu profile while debugging).
        if (
          perfHooks.shouldWriteNativeEvents
          || system?.cpuProfilingEnabled?.()
          || system?.debugMode
        ) {
          performanceImpl = perfHooks.performance;
        }
      }
    }
    return true;
  }

  /** Disables performance measurements for the compiler. */
  export function disable() {
    if (enabled) {
      marks.clear();
      counts.clear();
      durations.clear();
      performanceImpl = undefined;
      enabled = false;
    }
  }
}
