import * as React from "react";
import { RouteComponentProps } from "react-router-dom";
import { IDocument, ISelectedFolder, ResponseItemType } from "../interfaces/documents/document.interfaces";
import { getFlowsheetListItemsAsync } from "../api/documents.api";
import { DetailsListLayoutMode, SelectionMode, ShimmeredDetailsList, mergeStyleSets, IColumn, CheckboxVisibility } from "@fluentui/react";
import moment from "moment";
import { FileTypeIcon, IFileTypeIconProps } from "../components/file-type-icon/file-type-icon.component";
import { getFileTypeIconPropsCustom } from "../components/file-type-icon/file-type-icon.helpers";


interface IOpenDashboardFilePageProps extends RouteComponentProps<IOpenDashboardFilePageRouteProps> {

}
interface IOpenDashboardFilePageRouteProps {

}

interface IOpenDashboardFilePageState {
    files: IDocument[];
    folders: IDocument[];
    selectedFolder: ISelectedFolder;
    siteId: string;
    flowsheetsDriveId: string;
    flowsheetsListId: string;
    isDataLoaded: boolean;
}

const classNames = mergeStyleSets({
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

class OpenDashboardFilePage extends React.Component<IOpenDashboardFilePageProps, IOpenDashboardFilePageState>{
    // private _selection: Selection;


    constructor(props: IOpenDashboardFilePageProps) {
        super(props);
        this.state = {
            files: [],
            folders: [],
            siteId: "a5cec843-dfd2-4f2a-a121-0e349c6c39b2",
            flowsheetsDriveId: "b!Q8jOpdLfKk-hIQ40nGw5siUXS3kQCQVIt706knRzFAhsplPztq-WSpcwmKq6KD5I",
            flowsheetsListId: "f353a66c-afb6-4a96-9730-98aaba283e48",
            selectedFolder: {
                // webUrl: folder.flowsheets.parentName + "/" + folder.flowsheets.folderName,
                displayName: "Dashboard",
                driveId: "01RM334CTN4RCO4OXLURFYT6GRMFHFJC7F",
                id: "EE44E46D-EB3A-4BA4-89F8-D1614E548BE5",
                webUrl: "/bc2d8a86-b1f4-4df3-8c7d-b8e49d91650f",
            } as ISelectedFolder,
            isDataLoaded: false
        };

        //    this._selection = new Selection({ onSelectionChanged: this.selectedRowChanged.bind(this) } as ISelectionOptions);

    }
     componentDidMount() {
        this.getFilesAndFolders();

    }
    async getFilesAndFolders(){
        const { selectedFolder, siteId, flowsheetsListId } = this.state;
        try {
            this.setState({isDataLoaded:false});
            const filesAndFolders = await getFlowsheetListItemsAsync(selectedFolder, siteId, flowsheetsListId);
            console.log("Files and folders", filesAndFolders);
            this.setState({ files: filesAndFolders!.files ?? [], folders: filesAndFolders!.folders ?? [] });
        } catch (error) {
            console.log("An error occurred while loading dashboard files", error);
        }
        finally {
            this.setState({ isDataLoaded: true });
        }
    }

    private _getKey(item: any, index?: number): string {
        if (item) {
            return item.key;
        } else {
            console.log("Item null", this.state);
            return "";
        }
    }
    private _onItemInvoked(item: IDocument): void {
        console.log("On item invoked called!", item);
        const { selectedFolder } = this.state;
        if (item.fileType == ResponseItemType.Folder) {
            let folderPath = `/${encodeURIComponent(item.name)}`;
            if (this.props.history.location.pathname !== "/") {
                folderPath = this.props.history.location.pathname + folderPath;
                let newSelectedFolder = {
                    driveId: item.driveItemId,
                    displayName: item.name,
                    webUrl: folderPath,
                    parentDriveItemId: selectedFolder.driveId
                } as ISelectedFolder;
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
                // onColumnResize: new Async().debounce((width) => this._onColumnResize("tags", width), 500),               




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

    render() {
        const { folders, files, isDataLoaded } = this.state;
        const columns = [...this.getColumns()];
        const items = [...folders, ...files];
        console.log("isDataLoaded",isDataLoaded);
        return <>    <ShimmeredDetailsList
            enableShimmer={!isDataLoaded}
            items={items}
            columns={columns}
            //selection={this._selection}
            selectionPreservedOnEmptyClick
            selectionMode={SelectionMode.single}
            checkboxVisibility={CheckboxVisibility.hidden}
            onShouldVirtualize={() => false}
            
            setKey="none"
            layoutMode={DetailsListLayoutMode.fixedColumns}
            isHeaderVisible={true}
            onItemInvoked={this._onItemInvoked.bind(this)}

        /></>;
    }
}


export default OpenDashboardFilePage;