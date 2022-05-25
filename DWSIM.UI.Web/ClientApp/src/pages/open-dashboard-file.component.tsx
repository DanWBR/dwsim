import * as React from "react";
import { RouteComponentProps } from "react-router-dom";
import { IDocument, ISelectedFolder, ResponseItemType } from "../interfaces/documents/document.interfaces";
import { getFlowsheetListItemsAsync, OpenDwsimFile, SaveDwsimFile } from "../api/documents.api";
import { DetailsListLayoutMode, SelectionMode, ShimmeredDetailsList, Selection, mergeStyleSets, IColumn, CheckboxVisibility, TextField, PrimaryButton, Dropdown, DefaultButton, ISelectionOptions, ConstrainMode, IDetailsListStyles, IRenderFunction, IDetailsHeaderProps, TooltipHost, IDetailsColumnRenderTooltipProps, IDetailsFooterProps, DetailsRow, IStyle, StickyPositionType, Sticky, ScrollablePane, IScrollablePaneStyles, IDropdownOption } from "@fluentui/react";
import moment from "moment";
import { FileTypeIcon, IFileTypeIconProps } from "../components/file-type-icon/file-type-icon.component";
import { getFileTypeIconPropsCustom } from "../components/file-type-icon/file-type-icon.helpers";
import NavigationBar from "../components/navigation-bar/navigation-bar.component";
import CreateFolderModal from "../components/create-folder-modal/create-folder-modal.component";
import { withInitializeDashboard } from "../components/with-initialize-dashboard.hoc";
import { msGraphClient } from "../shared/ms-graph/ms-graph-factory";
import S365FilePicker from "../components/s365-file-picker/s365-file-picker.component";

interface IOpenDashboardFilePageProps extends RouteComponentProps<IOpenDashboardFilePageRouteProps> {
    baseFolder: ISelectedFolder;
    siteId: string;
    flowsheetsDriveId: string;
    flowsheetsListId: string;
    isSaveDialog: boolean;

}
interface IOpenDashboardFilePageRouteProps {
    extension?: string;
}

interface IOpenDashboardFilePageState {
    files: IDocument[];
    folders: IDocument[];
    selectedFolder: ISelectedFolder;
    isDataLoaded: boolean;
    filename?: string;
    selectedFileType?: string;
    filterFileTypes: string[];
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

class OpenDashboardFilePage extends React.Component<IOpenDashboardFilePageProps, IOpenDashboardFilePageState>{


    private FilePickerRef: React.RefObject<S365FilePicker>;

    constructor(props: IOpenDashboardFilePageProps) {
        super(props);

        this.FilePickerRef = React.createRef<S365FilePicker>();

        this.state = {
            files: [],
            folders: [],
            selectedFolder: props.baseFolder,
            isDataLoaded: false,
            //   isSaveDialog: false,
            filename: '',
            selectedFileType: "dwxmz",
            filterFileTypes: ["dwxml", "dwxmz", "xml", "dwcsd", "dwcsd2", "dwrsd", "dwrsd2", "dwruf"],
            showCreateFolderModal: false
        };


    }
    componentDidMount() {
        this.changeFileTypeFilter();
    }

    async changeFileTypeFilter() {
        console.log("props", this.props);
        let params: { [key: string]: string } = {};
        const paramsString = this.props.location.search?.toString().replace("?", "");
        if (paramsString) {
            for (let param of paramsString.split("&")) {
                if (param) {
                    const paramSplit = param.split("=");
                    if (paramSplit.length == 2) {
                        params[paramSplit[0]] = paramSplit[1];
                    }
                }
            }
        }
        console.log("params", params);
        let { selectedFolder, filterFileTypes } = this.state;
        if (params.directory) {
            if (this.state.selectedFolder?.webUrl !== params.directory) {
                let path = decodeURIComponent(params.directory.replaceAll('+', ' '));
                console.log("Decoded path", params.directory, path);
                if (path[0] != "/") {
                    path = `/${path}`;
                }
                selectedFolder = await this.getSelectedFolder(path);
            }
        }
        let fileTypeExtensions: string[] = filterFileTypes;
        if (params.extensions) {
            fileTypeExtensions = params.extensions.split('_');
        }
        let filename = "";
        if (params.filename) {
            filename = decodeURIComponent(params.filename.replaceAll('+', ' '));
        }

        this.setState({ selectedFileType: fileTypeExtensions[0], filterFileTypes: fileTypeExtensions, selectedFolder: selectedFolder, filename: filename },
            () => { this.FilePickerRef.current?.getFilesAndFolders(); });
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


    private selectedFileChanged(slectedDocument: IDocument): void {


        const { selectedFolder } = this.state;
        const { isSaveDialog } = this.props;
        if (slectedDocument) {

            if (slectedDocument.fileType == ResponseItemType.Folder) {
                let folderPath = `/${encodeURIComponent(slectedDocument.name)}`;

                folderPath = this.state.selectedFolder.webUrl + folderPath;
                let newSelectedFolder = {
                    driveId: slectedDocument.driveItemId,
                    displayName: slectedDocument.name,
                    webUrl: folderPath,
                    parentDriveItemId: selectedFolder.driveId
                } as ISelectedFolder;
                console.log("Setting selected folder", newSelectedFolder);


                this.setState({ selectedFolder: newSelectedFolder });
            } else {
                if (!isSaveDialog) {
                    const url = selectedFolder.webUrl.split('/').slice(2).reduce((prev, curr) => prev + "/" + decodeURIComponent(curr), "");
                    const filePath = url && url.length > 0 ? `//Simulate 365 Dashboard${url}/${slectedDocument.name}`
                        : `//Simulate 365 Dashboard/${slectedDocument.name}`;

                    OpenDwsimFile(slectedDocument.driveItemId, this.props.flowsheetsDriveId, filePath).then(() => { }, (error) => { alert(error); });
                } else {
                    let nameArray = slectedDocument.name.split('.');
                    if (nameArray.length > 1)
                        nameArray.pop();
                    const fileName = nameArray.reduce((prev, curr) => prev + curr, "");
                    this.setState({ filename: fileName });
                }
            }
        }
    }




    onSaveFileClick() {
        console.log("Save clicked", this.state);
        const { filename, selectedFileType, selectedFolder } = this.state;
        const { flowsheetsDriveId } = this.props;



        if (filename && selectedFileType) {

            let fileNameWithExtension = filename;
            if (fileNameWithExtension.toUpperCase().indexOf(selectedFileType.toUpperCase()) == -1) {
                fileNameWithExtension = `${filename}.${selectedFileType}`
            }

            const url = selectedFolder.webUrl.split('/').slice(2).reduce((prev, curr) => prev + "/" + decodeURIComponent(curr), "");
            const filePath = url && url.length > 0 ? `//Simulate 365 Dashboard${url}/${fileNameWithExtension}`
                : `//Simulate 365 Dashboard/${fileNameWithExtension}`;
            SaveDwsimFile(fileNameWithExtension, flowsheetsDriveId, selectedFolder.driveId, filePath);
        }
    }

    render() {
        const { selectedFolder, filename, selectedFileType, filterFileTypes, showCreateFolderModal } = this.state;
        const { isSaveDialog } = this.props;




        const dropdownControlledExampleOptions = getDropDownOptions(filterFileTypes);


        return <div style={{ height: '100%', display: 'flex', flexDirection: 'column' }} >
            {isSaveDialog && <div style={{ marginLeft: "30px" }}>
                <div style={{ display: "flex", marginBottom: "5px", marginTop: "5px" }}>
                    <div style={{ flexBasis: "80%" }} >
                        <TextField placeholder="Enter file name here" value={filename} onChange={(ev, newValue) => this.setState({ filename: newValue })} />
                    </div>
                    <div style={{ flexBasis: "20%" }}>
                        <DefaultButton text="New Folder" styles={{ root: { marginLeft: "10px" } }} onClick={() => this.setState({ showCreateFolderModal: true })} />
                    </div>

                </div>
                <div style={{ display: "flex", marginBottom: "5px", marginTop: "5px" }} >
                    <div style={{ flexBasis: "80%" }}>
                        <Dropdown
                            selectedKey={selectedFileType}
                            placeholder="Select an option"
                            options={dropdownControlledExampleOptions}
                            onChange={(e, option) => { this.setState({ selectedFileType: option?.key.toString() }, () => { this.FilePickerRef.current?.getFilesAndFolders(); }); }}

                        />
                    </div>
                    <div style={{ flexBasis: "20%" }}>
                        <PrimaryButton text="Save" styles={{ root: { marginLeft: "10px" } }} onClick={this.onSaveFileClick.bind(this)} />
                    </div>
                </div>


                {showCreateFolderModal && <CreateFolderModal
                    onFolderCreated={() => { this.setState({ showCreateFolderModal: false }); this.FilePickerRef.current?.getFilesAndFolders(); }}
                    selectedFolder={selectedFolder}
                    flowsheetsDriveId={this.props.flowsheetsDriveId}
                    onHide={() => this.setState({ showCreateFolderModal: false })} />}
            </div>
            }

            <S365FilePicker
                {...this.props}
                ref={this.FilePickerRef}
                graphClient={msGraphClient}
                defaultFileType={selectedFileType}
                filterFileTypes={filterFileTypes}
                onSelectedFileChanged={(selectedDocument: IDocument) => { console.log("selectedFile changed", selectedDocument); this.selectedFileChanged(selectedDocument); }}
                onSelectedFolderChanged={(selectedFolder) => { console.log("selectedFolder changed", selectedFolder); this.setState({ selectedFolder: selectedFolder }); }}
            />

        </div>;



    }
}

function getDropDownOptions(fileExtensions: string[]): IDropdownOption[] {

    let options: IDropdownOption[] = [];

    fileExtensions.forEach(extension => {
        const option = getFileTypeDropdownOption(extension);
        if (option) {
            options.push(option);
        }
    });

    return options;

}

function getFileTypeDropdownOption(extension?: string): IDropdownOption | undefined {


    switch (extension) {
        //dwsim default
        case 'dwxmz': return { key: 'dwxmz', text: 'Compressed XML Simulation File (*.dwxmz)' };
        case 'dwxml': return { key: 'dwxml', text: 'XML Simulation File (*.dwxml)' };
        case 'xml': return { key: 'xml', text: 'Mobile XML Simulation File (*.xml)' };
        case 'dwcsd': return { key: 'dwcsd', text: 'Compound Creator Study (*.dwcsd)' };
        case 'dwcsd2': return { key: 'dwcsd2', text: 'Compound Creator Study (*.dwcsd2)' };
        case 'dwrsd': return { key: 'dwrsd', text: 'Data Regression Study (*.dwrsd)' };
        case 'dwrsd2': return { key: 'dwrsd2', text: 'Data Regression Study (*.dwrsd2)' };
        case 'dwruf': return { key: 'dwruf', text: 'UNIFAC Regression Study File (*.dwruf)' };
        case 'dwund': return { key: 'dwund', text: '"DWSIM System of Units File (*.dwund)' };
        case 'dwrxm': return { key: 'dwrxm', text: '"DWSIM Reactions File (*.dwrxm)' };
        case 'dwrxs': return { key: 'dwrxs', text: '"DWSIM Reactions File (*.dwrxs)' };
        //end of dwsim default
        case 'xlsx': return { key: 'xlsx', text: 'Excel Workbook (*.xlsx)' };
        case 'odt': return { key: 'odt', text: 'OpenOffice Writer Document (*.odt)' };
        case 'pdf': return { key: 'pdf', text: 'PDF Files (*.pdf)' };
        case 'ods': return { key: 'ods', text: 'OpenOffice Calc SpreadSheet (*.ods)' };
        case 'json': return { key: 'json', text: 'JSON file (*.json)' };
        case 'bmp': return { key: 'bmp', text: 'BMP file (*.bmp)' };
        case 'jpg': return { key: 'jpg', text: 'JPG file (*.jpg)' };
        case 'png': return { key: 'json', text: 'PNG file (*.png)' };
        case 'gif': return { key: 'gif', text: 'GIF file (*.gif)' };
        case 'mov': return { key: 'mov', text: 'MOV file (*.mov)' };
        case 'mp4': return { key: 'mp4', text: 'MP4 file (*.mp4)' };
        case 'mp3': return { key: 'mp3', text: 'MP3 file (*.mp3)' };
        case 'txt': return { key: 'txt', text: 'Text file (*.txt)' };
        case 'py': return { key: 'py', text: 'Python file (*.py)' };
        case 'html': return { key: 'html', text: 'HTML file (*.html)' };
        default:
            return undefined;
    }

}


export default withInitializeDashboard(OpenDashboardFilePage);