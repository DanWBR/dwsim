import * as React from 'react'
import { RouteComponentProps, useLocation } from 'react-router-dom';
import { useState } from 'react';
import * as History from "history";
import { ISelectedFolder } from "../../interfaces/documents/document.interfaces";
import { Breadcrumb, IBreadcrumbItem, Icon, IconButton } from '@fluentui/react';
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
export interface INavigationBarState {

}


export interface IBreadcrumbItemWrapper {
    breadcrumb: IBreadcrumbItem;
    path: string;
}

class NavigationBar extends React.Component<INavigationBarProps, INavigationBarState> {


    /**
     *
     */
    constructor(props: INavigationBarProps) {
        super(props);
        this.state = {};
    }
    componentDidUpdate(prevProps: INavigationBarProps, prevState: INavigationBarState) {
        //navigate down the folder structure

    }




    // addSelectedFolder(folder: ISelectedFolder) {
    //     console.log("addSelectedFolder",folder);
    //     this.setState(s => ({ folders: [...s.folders, folder] }));
    // }

    getBreadcrumbItems() {
        console.log("getBreadcrumbItems", this.props);
        const items: IBreadcrumbItem[] = [
        ];
        let dashboardItem = {
            text: "Dashboard", key: "0", onClick: async () => await this._onBreadcrumbItemClicked.call(this, 0), isCurrentItem: false
        };

        const baseUrl = this.props.baseFolder.webUrl;
        const splited = this.props.selectedFolder.webUrl.split(baseUrl).filter(x => x);
        const subdirectoryUrl = splited?.[0];
        if (subdirectoryUrl) {
            items.push(dashboardItem);
            const directories = subdirectoryUrl.split("/").filter(x => x);
            console.log("directories", directories);
            directories.forEach((directory, index) => {
                const dirItem = {
                    text: decodeURIComponent(directory),
                    key: (index + 1).toString(),
                    onClick: async () => await this._onBreadcrumbItemClicked.call(this, index + 1),
                    isCurrentItem: (index + 1) == directories.length
                } as IBreadcrumbItem;
                items.push(dirItem);
            });

        } else {
            dashboardItem.isCurrentItem = true;
            items.push(dashboardItem);
        }



        return items;
    }

    async _onBreadcrumbItemClicked(index: number) {
        console.log("_onBreadcrumbItemClicked index", index);
        const { selectedFolder, baseFolder } = this.props;
        if (index == 0) {
            this.props.onSelectedFolderChanged(baseFolder);
        } else {
            const baseUrl = this.props.baseFolder.webUrl;
            const splited = this.props.selectedFolder.webUrl.split(baseUrl).filter(x => x);
            const subdirectoryUrl = splited?.[0];
            let directories = subdirectoryUrl.split('/').filter(x => x);
            directories = directories.slice(0, index);
            console.log("_onBreadcrumbItemClicked directories sliced", directories);
            const selectedFolderPath = this.props.baseFolder.webUrl + "/" + directories.join("/");
            console.log("_onBreadcrumbItemClicked selectedFolderPath", selectedFolderPath);
            const selectedFolder = await this.getSelectedFolder(selectedFolderPath);
            this.props.onSelectedFolderChanged(selectedFolder);
        }

    }

    async onNavigateUpClick() {
        const { selectedFolder, baseFolder } = this.props;
        const baseUrl = baseFolder.webUrl;
        const splited = selectedFolder.webUrl.split(baseUrl).filter(x => x);
        const subdirectoryUrl = splited?.[0];
        let directories = subdirectoryUrl.split('/').filter(x => x);
        directories = directories.slice(0, -1);
        if (directories && directories.length != 0) {
            const selectedFolderPath = this.props.baseFolder.webUrl + "/" + directories.join("/");
            const selectedFolder = await this.getSelectedFolder(selectedFolderPath);
            this.props.onSelectedFolderChanged(selectedFolder);
        } else {
            this.props.onSelectedFolderChanged(baseFolder);
        }

    }

    async getSelectedFolder(selectedFolderPath: string) {

        if (selectedFolderPath === this.props.baseFolder.webUrl) {
            return { ...this.props.baseFolder };
        }

        console.log("CurrentFolder path:", selectedFolderPath);
        const folderPath = selectedFolderPath.split('/').slice(1).reduce((prev, curr) => prev + "/" + curr, "");
        console.log("getSelectedFolder path:", folderPath);

        let apiPath = `/sites/${this.props.siteId}/lists/${this.props.flowsheetsListId}/drive`;
        if (folderPath && folderPath !== "" && folderPath !== "/") {
            apiPath = `/sites/${this.props.siteId}/lists/${this.props.flowsheetsListId}/drive/root:${folderPath}`;
        }


        try {
            var folderInfo = await msGraphClient.api(apiPath).get();
            let selectedFolder = {
                driveId: folderInfo.id,
                displayName: folderInfo.name,
                webUrl: selectedFolderPath
            } as ISelectedFolder;
            return selectedFolder;
        } catch (error) {
            console.log("An error occurred while loading selected Folder with path", selectedFolderPath);
            return this.props.baseFolder;
        }

    }

    render() {
        const items = this.getBreadcrumbItems();

        //     return  <Breadcrumb           
        //     items={items}
        //     maxDisplayedItems={3}
        // />
        const lastItem = items.findIndex(x => x.isCurrentItem);
        const showIcon = lastItem !== 0;

        return <div style={{ display: "flex", alignItems: "center" }}>
            {showIcon && <IconButton iconProps={{ iconName: 'Up' }} styles={{ root: { margin: "11px 0px 1px" } }} onClick={this.onNavigateUpClick.bind(this)} />}

            <div style={{ minWidth: "100%" }}>
                <Breadcrumb
                    items={items}
                    maxDisplayedItems={3}
                />
            </div>
        </div>
    }
}







export default NavigationBar