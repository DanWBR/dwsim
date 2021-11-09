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
    folders: ISelectedFolder[];
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
        this.state = { folders: [props.baseFolder] };
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


        var folderInfo = await msGraphClient.api(apiPath).get();
        let selectedFolder = {
            driveId: folderInfo.id,
            displayName: folderInfo.name,
            webUrl: selectedFolderPath
        } as ISelectedFolder;
        return selectedFolder;

    }


    addSelectedFolder(folder: ISelectedFolder) {
        console.log("addSelectedFolder,folder");
        this.setState(s => ({ folders: [...s.folders, folder] }));
    }

    getBreadcrumbItems() {
        const items: IBreadcrumbItem[] = this.state.folders.map((folder, index) => {
            let isCurrent = (this.state.folders.length - 1) == index;

            return { text: folder.displayName, key: index.toString(), onClick: () => this._onBreadcrumbItemClicked.call(this, index), isCurrentItem: isCurrent };

        });

        return items;
    }

    _onBreadcrumbItemClicked(index: number) {
        let { folders } = this.state;
        folders.splice(index + 1);
        this.setState({ folders: folders });
        this.props.onSelectedFolderChanged(folders[folders.length - 1]);
    }

    onNavigateUpClick() {
        let { folders } = this.state;
        if (folders.length > 1) {
            folders.pop();

            this.setState({ folders: folders });
            this.props.onSelectedFolderChanged(folders[folders.length - 1]);
        }



    }

    render() {
        const items = this.getBreadcrumbItems();

    //     return  <Breadcrumb           
    //     items={items}
    //     maxDisplayedItems={3}
    // />
            const lastItem= items.findIndex(x=>x.isCurrentItem);
            const showIcon= this.state.folders[lastItem].driveId!==this.props.baseFolder.driveId;     

        return <div style={{ display: "flex", alignItems: "center" }}>
          {showIcon &&  <IconButton iconProps={{ iconName: 'Up' }} styles={{ root: { margin: "11px 0px 1px" } }} onClick={this.onNavigateUpClick.bind(this)} />}

           <div style={{minWidth:"100%"}}>
            <Breadcrumb
                items={items}
                maxDisplayedItems={3}
            />
            </div>
        </div>
    }
}







export default NavigationBar