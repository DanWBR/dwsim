import * as React from "react";
import { RouteComponentProps } from "react-router-dom";
import {
    DetailsListLayoutMode, SelectionMode, ShimmeredDetailsList, Selection, mergeStyleSets,
    IColumn, CheckboxVisibility, ISelectionOptions, ConstrainMode, IDetailsListStyles,
    IRenderFunction, IDetailsHeaderProps, IStyle, StickyPositionType, Sticky,
    ScrollablePane
} from "@fluentui/react";
import moment from "moment";
import { IDocument, ISelectedFolder, ResponseItemType } from "../../interfaces/documents/document.interfaces";
import { getFlowsheetListItemsAsync } from "../../api/documents.api";
import { FileTypeIcon, IFileTypeIconProps } from "../file-type-icon/file-type-icon.component";
import { getFileTypeIconPropsCustom } from "../file-type-icon/file-type-icon.helpers";
import NavigationBar from "../navigation-bar/navigation-bar.component";
import { Client } from "@microsoft/microsoft-graph-client";

interface IS365FilePickerProps extends RouteComponentProps<IS365FilePickerRouteProps> {
    baseFolder: ISelectedFolder;
    siteId: string;
    flowsheetsDriveId: string;
    flowsheetsListId: string;
    defaultFileType?: string;
    filterFileTypes?: string[];
    graphClient: Client;
    onSelectedFileChanged?(selectedDocument: IDocument): void;
    onSelectedFolderChanged?(folder: ISelectedFolder): void;


}
interface IS365FilePickerRouteProps {
    extension?: string;
}

interface IS365FilePickerState {
    files: IDocument[];
    folders: IDocument[];
    selectedFolder: ISelectedFolder;
    isDataLoaded: boolean;
    selectedFileType?: string;
    showCreateFolderModal: boolean;
}

const gridStyles: Partial<IDetailsListStyles> = {
    root: {
        selectors: {
            '.ms-DetailsRow': {
                minWidth: "calc(100vw - 50px) !important"
            } as IStyle
        },
    }
};

const classNames = mergeStyleSets({
    header: {
        margin: 0,
        height: "50px"
    },
    row: {
        flex: '0 0 auto',
    },
    wrapper: {
        height: '40vh',
        position: 'relative',
        backgroundColor: 'white',
    },
    fileIconHeaderIcon: {
        padding: 0,
        fontSize: '16px',
    },
    fileIconCell: {
        display: "flex !important",
        alignItems: "center",
        textAlign: 'center',
        selectors: {
            '&:before': {
                content: '.',
                display: 'inline-block',
                verticalAlign: 'middle',
                height: '100%',
                width: '0px',
                visibility: 'hidden',
            },
        },
    },
    fileIconImg: {
        verticalAlign: 'middle',
        maxHeight: '20px',
        maxWidth: '20px',
    },
    controlWrapper: {
        display: 'flex',
        flexWrap: 'wrap',
    },
    exampleToggle: {
        display: 'inline-block',
        marginBottom: '10px',
        marginRight: '30px',
    },
    selectionDetails: {
        marginBottom: '20px',
    },
    commandbarInfoIcon: {
        border: "none"
    },
    remarkCell: {
        whiteSpace: "pre-line !important",
        selectors: {
            'span': {
                overflow: "hidden !important",
                textOverflow: "ellipsis !important",
                display: "-webkit-box !important",
                ...({ '-webkit-line-clamp': '2 !important' } as any),
                ...({ '-webkit-box-orient': 'vertical !important' } as any)
            }
        }
    },
    dateModifiedColumn: {
        textAlign: "center",
        justifyContent: "center",
        selectors: {
            '.ms-DetailsHeader-cellTitle': {
                justifyContent: "center",
                paddingRight: "32px"
            }
        }
    },
    column: {
        display: "flex !important",
        textAlign: "center",
        justifyContent: "center",
        alignItems: "center",
        selectors: {
            '.ms-DetailsHeader-cellTitle': {
                justifyContent: "center"
            }
        }

    },
    sharedColumn: {
        textAlign: "center",
        justifyContent: "center",
        alignItems: "center",
        fontSize: "18px",
        selectors: {
            '.ms-DetailsHeader-cellTitle': {
                justifyContent: "center"
            }
        }
    },
    breadcrumbsticky: {
        height: "100px"
    },
    tableStyle: {
        selectors: {
            '.ms-DetailsRow-cell': {
                ...({ 'display': 'flex !important' } as any),
                alignItems: "center"
            }
        }
    }
});

class S365FilePicker extends React.Component<IS365FilePickerProps, IS365FilePickerState>{

    private _selection: Selection;


    constructor(props: IS365FilePickerProps) {
        super(props);
        this.state = {
            files: [],
            folders: [],
            selectedFolder: props.baseFolder,
            isDataLoaded: false,
            selectedFileType: props.defaultFileType,
            showCreateFolderModal: false
        };

        this._selection = new Selection({ onSelectionChanged: this.selectedRowChanged.bind(this) } as ISelectionOptions);

    }
    componentDidMount() {
        this.getFilesAndFolders();
    }



    async getFilesAndFolders() {
        const { selectedFolder, selectedFileType } = this.state;
        const { filterFileTypes } = this.props;
        const { siteId, flowsheetsListId } = this.props;
        try {
            this.setState({ isDataLoaded: false });

            const filesAndFolders = await getFlowsheetListItemsAsync(selectedFolder, siteId, flowsheetsListId, filterFileTypes ?? []);
            console.log("Files and folders", filesAndFolders);
            this.setState({ files: filesAndFolders!.files ?? [], folders: filesAndFolders!.folders ?? [] });
        } catch (error) {
            console.log("An error occurred while loading dashboard files", error);
        }
        finally {
            this.setState({ isDataLoaded: true });
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
            var folderInfo = await this.props.graphClient.api(apiPath).get();
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


    private selectedRowChanged(): void {
        const selection = this._selection.getSelection();

        const { selectedFolder } = this.state;
        if (selection && selection.length > 0) {
            const item = selection[0] as IDocument;
            if (item.fileType == ResponseItemType.Folder) {
                let folderPath = `/${encodeURIComponent(item.name)}`;

                folderPath = this.state.selectedFolder.webUrl + folderPath;
                let newSelectedFolder = {
                    driveId: item.driveItemId,
                    displayName: item.name,
                    webUrl: folderPath,
                    parentDriveItemId: selectedFolder.driveId
                } as ISelectedFolder;
                console.log("Setting selected folder", newSelectedFolder);


                this.setState({ selectedFolder: newSelectedFolder }, () => {
                    if (this.props.onSelectedFolderChanged) {
                        this.props.onSelectedFolderChanged(newSelectedFolder);
                    }
                    this.getFilesAndFolders();
                });
            } else {
                if (this.props.onSelectedFileChanged) { this.props.onSelectedFileChanged(item); }
            }

        }
    }

    private getColumns = (): IColumn[] => {



        const nameContainerStyle = { display: "flex", flexDirection: "row", flex: 1, alignItems: "center" } as React.CSSProperties;
        const documentNameStyle = { overflow: "hidden" } as React.CSSProperties;


        let columns: IColumn[] = [
            {
                key: 'file-type',
                name: 'File Type',
                className: classNames.fileIconCell,
                iconClassName: classNames.fileIconHeaderIcon,
                ariaLabel: 'Column operations for File type, Press to sort on File type',
                iconName: 'Page',
                isIconOnly: true,
                fieldName: 'icon',
                minWidth: 18,
                maxWidth: 20,
                onRender: (item: IDocument) => {
                    const isFolder = item.fileType == ResponseItemType.Folder;
                    return <FileTypeIcon {...getFileTypeIconPropsCustom(
                        {
                            extension: item.extension,
                            size: 20,
                            width: 20,
                            height: 20

                        }) as IFileTypeIconProps} />
                },
            },
            {
                key: 'name',
                name: 'Name',
                fieldName: 'name',
                minWidth: 150,

                maxWidth: 400,
                isRowHeader: true,
                isResizable: true,
                className: classNames.column,
                data: 'string',
                onRender: (item: IDocument, i) => {

                    const isFolder = item.fileType == ResponseItemType.Folder;

                    return (<div className="document-name-container" style={nameContainerStyle}>
                        <div className="document-name" style={documentNameStyle}>
                            {item.name ? <span className="kt-font-bold">{item.name}</span> : null}

                        </div>
                    </div>
                    );

                },
                // isPadded: true,
            },


            {
                key: 'date',
                name: 'Modified At',
                fieldName: 'dateModified',
                minWidth: 135,

                maxWidth: 200,
                isResizable: true,
                isCollapsible: true,
                isMultiline: true,
                headerClassName: classNames.dateModifiedColumn,
                className: classNames.dateModifiedColumn,

                data: 'string',
                onRender: (item: IDocument) => {
                    return <span>{moment(item.dateModified).format("ddd DD MMM YYYY")}
                        <br />{moment(item.dateModified).format("HH:mm:ss")}  </span>;
                },
                isPadded: true,
            },


            {
                key: 'tags',
                name: 'Tags',
                fieldName: 'tags',
                minWidth: 180,
                maxWidth: 250,
                isResizable: true,
                isCollapsible: true,
                data: 'string',
                onRender: (item: IDocument) => {
                    if (item.tags && item.tags.length > 0) {
                        const tags = item.tags.reduce((prev, curr, currindex, []) => { return { key: (1).toString(), name: prev.name + ", " + curr.name, isNewItem: false } });
                        return (<span style={{ whiteSpace: "initial" }}>{tags.name}</span>);
                    }
                    return (<span></span>);
                },
                isPadded: true,
            },
            {
                key: 'remark',
                name: 'Remark',
                fieldName: 'remark',
                minWidth: 80,
                maxWidth: 350,
                isResizable: true,
                isCollapsible: true,
                isMultiline: true,
                className: classNames.remarkCell,
                data: 'string',

                onRender: (item: IDocument) => {
                    return <span>{item.remark}</span>;
                },
            }
        ];
        return columns;
    }

    onRenderDetailsHeader: IRenderFunction<IDetailsHeaderProps> = (props, defaultRender) => {

        const { siteId, baseFolder, flowsheetsListId } = this.props;
        return (

            <Sticky stickyPosition={StickyPositionType.Header} key="header_sticky" stickyBackgroundColor={"white"} >
                <NavigationBar
                    siteId={siteId}
                    flowsheetsListId={flowsheetsListId}
                    baseFolder={baseFolder}
                    selectedFolder={this.state.selectedFolder}
                    onSelectedFolderChanged={(selectedFolder) => {

                        this.setState({ selectedFolder: selectedFolder }, () => {
                            if (this.props.onSelectedFolderChanged) {
                                this.props.onSelectedFolderChanged(selectedFolder);
                            }
                            this.getFilesAndFolders();
                        });
                    }} />
                {defaultRender!(props)}
            </Sticky>
        )
    };



    render() {
        const { folders, files, isDataLoaded } = this.state;

        const columns = [...this.getColumns()];
        const items = [...folders, ...files];
        console.log("isDataLoaded", isDataLoaded);


        return <div style={{ height: '100%', display: 'flex', flexDirection: 'column' }} >


            <div style={{ flex: 1, display: 'flex', flexDirection: 'row', minHeight: "300px" }}>
                {/* We need this div because scrollable pane has position absolute, to make it full height */}
                <div style={{ position: 'relative', flex: 1 }}>
                    <ScrollablePane styles={{ root: { marginLeft: "30px" } }}>
                        <ShimmeredDetailsList
                            enableShimmer={!isDataLoaded}
                            items={items}
                            columns={columns}
                            selection={this._selection}
                            layoutMode={DetailsListLayoutMode.fixedColumns}
                            constrainMode={ConstrainMode.unconstrained}
                            onRenderDetailsHeader={this.onRenderDetailsHeader.bind(this)}
                            selectionMode={SelectionMode.single}
                            checkboxVisibility={CheckboxVisibility.hidden}
                            onShouldVirtualize={() => false}
                            setKey="none"
                            detailsListStyles={gridStyles}
                            isHeaderVisible={true}
                        // onItemInvoked={this._onItemInvoked.bind(this)}

                        />
                    </ScrollablePane>
                </div>
            </div>

        </div>;



    }
}


export default S365FilePicker;