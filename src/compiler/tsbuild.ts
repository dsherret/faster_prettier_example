/*@internal*/
namespace ts {
  export enum UpToDateStatusType {
    Unbuildable,
    UpToDate,
    /**
     * The project appears out of date because its upstream inputs are newer than its outputs,
     * but all of its outputs are actually newer than the previous identical outputs of its (.d.ts) inputs.
     * This means we can Pseudo-build (just touch timestamps), as if we had actually built this project.
     */
    UpToDateWithUpstreamTypes,
    /**
     * The project appears out of date because its upstream inputs are newer than its outputs,
     * but all of its outputs are actually newer than the previous identical outputs of its (.d.ts) inputs.
     * This means we can Pseudo-build (just manipulate outputs), as if we had actually built this project.
     */
    OutOfDateWithPrepend,
    OutputMissing,
    OutOfDateWithSelf,
    OutOfDateWithUpstream,
    UpstreamOutOfDate,
    UpstreamBlocked,
    ComputingUpstream,
    TsVersionOutputOfDate,

    /**
     * Projects with no outputs (i.e. "solution" files)
     */
    ContainerOnly,
  }

  export type UpToDateStatus =
    | Status.Unbuildable
    | Status.UpToDate
    | Status.OutOfDateWithPrepend
    | Status.OutputMissing
    | Status.OutOfDateWithSelf
    | Status.OutOfDateWithUpstream
    | Status.UpstreamOutOfDate
    | Status.UpstreamBlocked
    | Status.ComputingUpstream
    | Status.TsVersionOutOfDate
    | Status.ContainerOnly;

  export namespace Status {
    /**
     * The project can't be built at all in its current state. For example,
     * its config file cannot be parsed, or it has a syntax error or missing file
     */
    export interface Unbuildable {
      type: UpToDateStatusType.Unbuildable;
      reason: string;
    }

    /**
     * This project doesn't have any outputs, so "is it up to date" is a meaningless question.
     */
    export interface ContainerOnly {
      type: UpToDateStatusType.ContainerOnly;
    }

    /**
     * The project is up to date with respect to its inputs.
     * We track what the newest input file is.
     */
    export interface UpToDate {
      type:
        | UpToDateStatusType.UpToDate
        | UpToDateStatusType.UpToDateWithUpstreamTypes;
      newestInputFileTime?: Date;
      newestInputFileName?: string;
      newestDeclarationFileContentChangedTime?: Date;
      newestOutputFileTime?: Date;
      newestOutputFileName?: string;
      oldestOutputFileName: string;
    }

    /**
     * The project is up to date with respect to its inputs except for prepend output changed (no declaration file change in prepend).
     */
    export interface OutOfDateWithPrepend {
      type: UpToDateStatusType.OutOfDateWithPrepend;
      outOfDateOutputFileName: string;
      newerProjectName: string;
    }

    /**
     * One or more of the outputs of the project does not exist.
     */
    export interface OutputMissing {
      type: UpToDateStatusType.OutputMissing;
      /**
       * The name of the first output file that didn't exist
       */
      missingOutputFileName: string;
    }

    /**
     * One or more of the project's outputs is older than its newest input.
     */
    export interface OutOfDateWithSelf {
      type: UpToDateStatusType.OutOfDateWithSelf;
      outOfDateOutputFileName: string;
      newerInputFileName: string;
    }

    /**
     * This project depends on an out-of-date project, so shouldn't be built yet
     */
    export interface UpstreamOutOfDate {
      type: UpToDateStatusType.UpstreamOutOfDate;
      upstreamProjectName: string;
    }

    /**
     * This project depends an upstream project with build errors
     */
    export interface UpstreamBlocked {
      type: UpToDateStatusType.UpstreamBlocked;
      upstreamProjectName: string;
      upstreamProjectBlocked: boolean;
    }

    /**
     *  Computing status of upstream projects referenced
     */
    export interface ComputingUpstream {
      type: UpToDateStatusType.ComputingUpstream;
    }

    export interface TsVersionOutOfDate {
      type: UpToDateStatusType.TsVersionOutputOfDate;
      version: string;
    }

    /**
     * One or more of the project's outputs is older than the newest output of
     * an upstream project.
     */
    export interface OutOfDateWithUpstream {
      type: UpToDateStatusType.OutOfDateWithUpstream;
      outOfDateOutputFileName: string;
      newerProjectName: string;
    }
  }

  export function resolveConfigFileProjectName(
    project: string,
  ): ResolvedConfigFileName {
    if (fileExtensionIs(project, Extension.Json)) {
      return project as ResolvedConfigFileName;
    }

    return combinePaths(project, "tsconfig.json") as ResolvedConfigFileName;
  }
}
