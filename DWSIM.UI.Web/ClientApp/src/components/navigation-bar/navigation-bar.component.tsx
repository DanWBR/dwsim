import * as React from 'react'
import { RouteComponentProps, useLocation } from 'react-router-dom';
import { useState } from 'react';
import * as History from "history";
import { ISelectedFolder } from "../../interfaces/documents/document.interfaces";
import { IBreadcrumbItem, IconButton } from '@fluentui/react';
import { msGraphClient } from "../../shared/ms-graph/ms-graph-factory"

export interface INavigationBarRouteProps {

}

export interface INavigationBarProps {
    selectedFolder: ISelectedFolder;
    baseFolder: ISelectedFolder;
    siteId: string;
    flowsheetsListId: string;
    onSelectedFolderChanged(selectedFolder: ISelectedFolder): void;
}


export interface IBreadcrumbItemWrapper {
    breadcrumb: IBreadcrumbItem;
    path: string;
}

class NavigationBar extends React.Component<INavigationBarProps, {}> {



    async getSelectedFolder(selectedFolderPath: string) {

         if(selectedFolderPath===this.props.baseFolder.webUrl){
             return {...this.props.baseFolder};
         }

        console.log("CurrentFolder path:", selectedFolderPath);
        const folderPath = selectedFolderPath.split('/').slice(1).reduce((prev, curr) => prev + "/" + curr, "");
        console.log("getSelectedFolder path:", folderPath);

        let apiPath = `/sites/${this.props.siteId}/lists/${this.props.flowsheetsListId}/drive`;
        if (folderPath && folderPath !== "" && folderPath !== "/") {
            apiPath = `/sites/${this.props.siteId}/lists/${this.props.flowsheetsListId}/drive/root:${folderPath}`;
        }


        var folderInfo = await msGraphClient.api(apiPath).get();
        let selectedFolder = {
            driveId: folderInfo.id,
            displayName: folderInfo.name,
            webUrl: selectedFolderPath
        } as ISelectedFolder;
        return selectedFolder;

    }

    navigateToURL = async (webUrl: string) => {
        try {
            const baseFolder = this.props.baseFolder.webUrl.split('/').slice(1).reduce((prev, curr) => prev + "/" + curr, "");
            if (webUrl === "/") {
                webUrl = baseFolder;
            } 
            console.log("navigateToURL item path:", webUrl);

            const newSelectedFolder = await this.getSelectedFolder(webUrl);
            this.props.onSelectedFolderChanged(newSelectedFolder);

        }
        catch (error) {
            console.log("An error ocurred while navigating to url (breadcrumb component)", error);
        }
    }
    onNavigateUpClick() {
    
        if (this.props.selectedFolder.driveId !== this.props.baseFolder.driveId) {

            let webUrlArray = this.props.selectedFolder.webUrl.split('/').filter(x=>x!=="" && x!=="/");
            webUrlArray.pop();
            
            console.log("onNavigateUpClick",webUrlArray);
            const webUrl = webUrlArray.reduce((prev, curr) => prev + "/" + curr, "");
            this.navigateToURL(webUrl);


        }

    }




    render() {

        return <div style={{ display: "flex", alignItems: "center" }}>        <IconButton iconProps={{ iconName: 'MoveToFolder' }} onClick={this.onNavigateUpClick.bind(this)} />
            <strong>{this.props.selectedFolder.displayName}</strong></div>
    }
}







export default NavigationBar