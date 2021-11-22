export interface IDocument {
    key: string;
    eTag: string;
    listItemId: number;
    driveItemId: string;
    name: string;
    extension?: string;
    iconName?: string;
    iconNameUrl?: string;
    fileType: ResponseItemType;
    remark?: string;
    versionId?: string;
    simulationType?: SimulationType;
    scenarioCount?: number;
    filterCount?: number;
    studyCount?: number;
    dateModified: Date;
    specControlStatus?: SpecControlStatus;
    convergedStatus?: ConvergedStatus;
    tags?: ITag[];
    versionNumber?: number;
    isVersion?: boolean;
    isAnalyzed?: boolean;
    isAnalyzedLoading?: boolean;
    downloadURL?: string;
    odataType: string;
    editUrl: string;
    createdBy?: IUser;
    isExample: boolean;
    isExam: boolean;
    examId: number;
    hideFromDashboard: boolean;
    createdFromFilterId: string;
    createdFromFilterVersionId: string;
    parentPath?: string;
    shared: boolean;
}

export interface ISelectedFolder {
    id: string;
    driveId: string;
    webUrl: string;
    displayName: string;
    parentDriveItemId: string;
}
export  enum ResponseItemType {
    Folder = "folder",
    File = "file"
}
export  enum SimulationType {
    SteadyState = "steady-state",
    Dynamic = "dynamic"
}
export  enum SpecControlStatus {
    Valid = "valid",
    NotValid = "not-valid",
    NotTested = "not-tested"
}
export  enum ConvergedStatus {
    Converged = "converged",
    NotConverged = "not-converged",
    NotTested = "not-tested"
}
export interface ITag {
    key: string;
    name: string;
    isNewItem: boolean;
}
export interface IUser {
    id?: string;
    email?: string;
    displayName?: string;
}
