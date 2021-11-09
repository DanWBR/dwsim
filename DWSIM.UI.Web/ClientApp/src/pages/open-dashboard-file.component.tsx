import * as React from "react";
import { RouteComponentProps } from "react-router-dom";
import { IDocument, ISelectedFolder, ResponseItemType } from "../interfaces/documents/document.interfaces";
import { getFlowsheetListItemsAsync, OpenDwsimFile, SaveDwsimFile } from "../api/documents.api";
import { DetailsListLayoutMode, SelectionMode, ShimmeredDetailsList, mergeStyleSets, IColumn, CheckboxVisibility, TextField, PrimaryButton, Dropdown, DefaultButton } from "@fluentui/react";
import moment from "moment";
import { FileTypeIcon, IFileTypeIconProps } from "../components/file-type-icon/file-type-icon.component";
import { getFileTypeIconPropsCustom } from "../components/file-type-icon/file-type-icon.helpers";
import NavigationBar from "../components/navigation-bar/navigation-bar.component";
import CreateFolderModal from "../components/create-folder-modal/create-folder-modal.component";


interface IOpenDashboardFilePageProps extends RouteComponentProps<IOpenDashboardFilePageRouteProps> {
    baseFolder: ISelectedFolder;
    siteId: string;
    flowsheetsDriveId: string;
    flowsheetsListId: string;
    isSaveDialog: boolean;

}
interface IOpenDashboardFilePageRouteProps {

}

interface IOpenDashboardFilePageState {
    files: IDocument[];
    folders: IDocument[];
    selectedFolder: ISelectedFolder;
    isDataLoaded: boolean;
    filename?: string;
    filetype?: string;
    showCreateFolderModal: boolean;
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
    private _navigationBarRef: React.RefObject<NavigationBar> | undefined;
    // private _selection: Selection;


    constructor(props: IOpenDashboardFilePageProps) {
        super(props);
        this.state = {
            files: [],
            folders: [],
            selectedFolder: props.baseFolder,
            isDataLoaded: false,
            //   isSaveDialog: false,
            filename: '',
            filetype: "dwxmz",
            showCreateFolderModal: false
        };
        this._navigationBarRef = React.createRef<NavigationBar>();
        //    this._selection = new Selection({ onSelectionChanged: this.selectedRowChanged.bind(this) } as ISelectionOptions);

    }
    componentDidMount() {
        // const queryParams = QueryString.parse(this.props.location.search);
        // if (queryParams && queryParams.save) {
        //     this.setState({ isSaveDialog: true });
        //     console.log("Save dialog");
        // }

        this.getFilesAndFolders();

    }
    async getFilesAndFolders() {
        const { selectedFolder } = this.state;
        const { siteId, flowsheetsListId } = this.props;
        try {
            this.setState({ isDataLoaded: false });
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


    private _onItemInvoked(item: IDocument): void {
        console.log("On item invoked called!", item);
        const { selectedFolder } = this.state;
        const { isSaveDialog } = this.props;
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
            this._navigationBarRef?.current?.addSelectedFolder(newSelectedFolder);

            this.setState({ selectedFolder: newSelectedFolder }, () => {
                this.getFilesAndFolders();
            });
        } else {
            if (!isSaveDialog) {
                OpenDwsimFile(item.driveItemId, this.props.flowsheetsDriveId).then(() => { }, (error) => { alert(error); });
            } else {
                let nameArray = item.name.split('.');
                if (nameArray.length > 1)
                    nameArray.pop();
                const fileName = nameArray.reduce((prev, curr) => prev + curr, "");
                this.setState({ filename: fileName });
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

    onSaveFileClick() {
        console.log("Save clicked", this.state);
        const { filename, filetype, selectedFolder } = this.state;
        const { flowsheetsDriveId } = this.props;
        if (filename && filetype)
            SaveDwsimFile(filename, filetype, flowsheetsDriveId, selectedFolder.driveId);
    }

    render() {
        const { folders, files, isDataLoaded, selectedFolder, filename, filetype, showCreateFolderModal } = this.state;
        const { isSaveDialog } = this.props;
        const { siteId, flowsheetsListId, baseFolder, } = this.props;
        const columns = [...this.getColumns()];
        const items = [...folders, ...files];
        console.log("isDataLoaded", isDataLoaded);


        const dropdownControlledExampleOptions = [
            { key: 'dwxmz', text: 'Simulation File (Compressed XML) (*.dwxmz)' }
        ];


        return <div style={{ marginLeft: "30px" }}>
            {isSaveDialog && <><div style={{ display: "flex", marginBottom: "5px", marginTop: "5px" }}>
                <div style={{ flexBasis: "80%" }} >
                    <TextField placeholder="Enter file name here" value={filename} onChange={(ev, newValue) => this.setState({ filename: newValue })} />
                </div>
                <div style={{ flexBasis: "20%" }}>
                    <DefaultButton text="New Folder" styles={{ root: { marginLeft: "10px" } }} onClick={()=>this.setState({showCreateFolderModal:true})}/>
                    </div>

            </div>
                <div style={{ display: "flex", marginBottom: "5px", marginTop: "5px" }} >
                    <div style={{ flexBasis: "80%" }}>
                        <Dropdown
                            selectedKey={filetype}
                            placeholder="Select an option"
                            options={dropdownControlledExampleOptions}

                        />
                    </div>
                    <div style={{ flexBasis: "20%" }}>
                        <PrimaryButton text="Save" styles={{ root: { marginLeft: "10px" } }} onClick={this.onSaveFileClick.bind(this)} />
                    </div>
                </div>
               

                {showCreateFolderModal &&    <CreateFolderModal 
                onFolderCreated={()=>{this.setState({showCreateFolderModal:false}); this.getFilesAndFolders();}}
                selectedFolder={selectedFolder}  
                flowsheetsDriveId={this.props.flowsheetsDriveId}
                onHide={()=>this.setState({showCreateFolderModal:false})} />}
                </>
            }


            <div className="ms-Grid-row">
                <div className="ms-Grid-col ms-sm12">
                    <NavigationBar
                        ref={this._navigationBarRef}
                        siteId={siteId}
                        flowsheetsListId={flowsheetsListId}
                        baseFolder={baseFolder}
                        selectedFolder={selectedFolder}
                        onSelectedFolderChanged={(selectedFolder) => {
                            this.setState({ selectedFolder: selectedFolder }, () => {
                                this.getFilesAndFolders();
                            });
                        }} /></div>

            </div>
            <div className="ms-Grid-row">
                <div className="ms-Grid-col ms-sm12">
                    <ShimmeredDetailsList
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

                    />
                </div>
            </div>
        </div>;



    }
}


export default OpenDashboardFilePage;