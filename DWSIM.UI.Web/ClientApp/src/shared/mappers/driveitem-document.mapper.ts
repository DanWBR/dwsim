import { IDocument, ITag, IUser, ResponseItemType } from "../../interfaces/documents/document.interfaces";
import { getFileExtension } from "../utilities/get-file-extension";

export function MapDriveItemToDocument(item: any): IDocument {
    let isFolder: boolean = item.folder ? true : false;

    let document = {} as IDocument;
    document.key = item.id;
    document.eTag = item.eTag;
    document.name = item.name;
    document.iconName = isFolder ? "FabricFolder" : undefined;
    document.extension = isFolder ? "folder" : getFileExtension(item.name);

    document.remark = item.listItem.fields.Remark;
    document.dateModified = item.listItem.fields.Modified ? new Date(item.listItem.fields.Modified) : new Date(item.lastModifiedDateTime);
    //document.iconNameUrl = !isFolder ? require("../../assets/filetype-icons/cc7.png") : null;
    document.remark = item.listItem.fields.Remark;
    //document.versionId = item.UIVersion;
    document.versionNumber = item.listItem.fields._UIVersionString;
    document.simulationType = item.listItem.fields.Simulation_x0020_Type;
    document.specControlStatus = item.listItem.fields.SpecControl;
    document.convergedStatus = item.listItem.fields.Convergance;
    document.isExample = item.listItem.fields.ExampleFile ? item.listItem.fields.ExampleFile : false;
    document.hideFromDashboard = item.listItem.fields.HideFromDashboard ? item.listItem.fields.HideFromDashboard : false;
    document.isExam = item.listItem.fields.IsExam ? item.listItem.fields.IsExam : false;
    document.examId= item.listItem.fields.ExamId;
    document.createdFromFilterId = item.listItem.fields.CreatedFromFilterId;
    document.createdFromFilterVersionId = item.listItem.fields.CreatedFromFilterVersionId;
    document.fileType = isFolder ? ResponseItemType.Folder : ResponseItemType.File;
    document.listItemId = item.listItem.id;
    document.driveItemId = item.id;
    document.downloadURL = item["@microsoft.graph.downloadUrl"];
    document.tags = item.listItem.fields.Tags ? item.listItem.fields.Tags.map((value:any) => {
        return { key: value.LookupId, name: value.LookupValue } as ITag;
    }) : null;
    document.createdBy = (item.createdBy && item.createdBy.user) ? (item.createdBy.user as IUser) : undefined;
    document.parentPath = item.parentReference.path;
    document.shared = item.shared ? true : false;
    return document;
}