
import { IDocument, ISelectedFolder, ResponseItemType } from "../interfaces/documents/document.interfaces";
import { MapDriveItemToDocument } from "../shared/mappers/driveitem-document.mapper";
import { msGraphClient } from "../shared/ms-graph/ms-graph-factory";
import { _copyAndSort } from "../shared/utilities/copy-sort";
declare const chrome: any;

export async function getFlowsheetListItemsAsync(selectedFolder: ISelectedFolder, siteId: string, flowsheetsListId: string, filterFileTypes: string[]) {
    try {
        const selectedFolderPath = selectedFolder.webUrl.split('/').slice(1).reduce((prev, curr) => prev + "/" + curr, "");

        let apidrivePath = `/sites/${siteId}/lists/${flowsheetsListId}/drive/root:/${selectedFolderPath}:/children`;


        console.log("get drive items apiPath:", apidrivePath, msGraphClient);
        let driveItems = [];

        let driveItemsResp = await msGraphClient.api(apidrivePath).expand("listItem($expand=fields)").get();
        driveItems.push(...driveItemsResp.value);
        while (driveItemsResp["@odata.nextLink"]) {
            driveItemsResp = await msGraphClient.api(driveItemsResp["@odata.nextLink"]).get();
            driveItems.push(...driveItemsResp.value);
        }
        console.log("Drive items:", driveItems);

        let documents: IDocument[] = [];

        documents = driveItems.map(driveItem => MapDriveItemToDocument(driveItem));
        //remove hide from dashboard files
        documents = documents.filter(document => document.hideFromDashboard === false);

        const files = documents.filter((document) => document.fileType === ResponseItemType.File && !!document.extension
            && filterFileTypes.findIndex(x => x == document.extension) > -1);
        const folders = documents.filter((document) => document.fileType === ResponseItemType.Folder);

        let sortedFolders = _copyAndSort<IDocument>(folders, "name", false);
        let sortedFiles = _copyAndSort<IDocument>(files, "name", false);

        const filesAndFolders = { files: sortedFiles, folders: sortedFolders };
        console.log("getSharedFlowsheetsItems results:", filesAndFolders);
        return filesAndFolders;

    } catch (error) {
        console.log("Error while getting List items:", error);
    }

}

export async function OpenDwsimFile(driveItemId: string, driveId: string, filePath: string) {
    console.log("filePath", filePath);
    if (chrome?.webview?.hostObjects?.filePickerService) {

        const filePickerService = chrome.webview.hostObjects.filePickerService;
        await filePickerService.openFile(driveItemId, driveId, filePath);
    } else {
        throw "filePickerService not initialized.";
    }

}

export async function SaveDwsimFile(filename: string, flowsheetsDriveId: string, parentDriveId: string, filePath: string) {
    console.log("filePath", filePath);
    if (chrome?.webview?.hostObjects?.filePickerService) {

        const filePickerService = chrome.webview.hostObjects.filePickerService;
        await filePickerService.SaveFile(filename, flowsheetsDriveId, parentDriveId, filePath);
    } else {
        throw "filePickerService not initialized.";
    }

}

export async function CreateFolder(folderName: string, flowsheetsDriveId: string, parentDriveId: string) {

    if (chrome?.webview?.hostObjects?.filePickerService) {

        const filePickerService = chrome.webview.hostObjects.filePickerService;
        await filePickerService.CreateFolder(folderName, flowsheetsDriveId, parentDriveId);
    } else {
        throw "filePickerService not initialized.";
    }
}
