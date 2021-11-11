import * as React from "react";
import { Spinner } from "@fluentui/react";
import { ISelectedFolder } from "../interfaces/documents/document.interfaces";
declare const chrome: any;



export interface IInitializeDashboardProps {
    baseFolder?: ISelectedFolder;
    isLoaded: boolean;
    siteId: string;
    flowsheetsListId: string;
    flowsheetsDriveId: string;
}

export const user_token = window.localStorage.getItem("s365_local_user_token");
const DashboardServiceUrl = process.env.REACT_APP_DASHBOARD_SERVICE_URL;


export function withInitializeDashboard(WrappedComponent: any) {

    return class extends React.Component<{}, IInitializeDashboardProps>{

        /**
         *
         */
        constructor(props: IInitializeDashboardProps) {
            super(props);
            this.state = {
                isLoaded: false,
                siteId: "",
                flowsheetsDriveId: "",
                flowsheetsListId: ""
            };
        }

        async componentDidMount() {
            await this.getBaseFolder();
        }
        async getBaseFolder() {
            console.log("chrome.webview?.hostObjects?.authService", chrome.webview?.hostObjects?.authService);
            let token = !chrome.webview?.hostObjects?.authService ? user_token : undefined;
            if (chrome.webview?.hostObjects?.authService) {
                this.setState({ isLoaded: false });

                const authService = chrome.webview.hostObjects.authService;
                try {
                    token = await authService.getUserToken();
                } catch (error) {

                    token = "";
                }
            }
            console.log("App.tsx token", token, !!token);
            try {
                if (!!token && token.length > 0) {
                    const data = {
                        userAccessToken: token,
                        siteId: "not required",
                        flowsheetsListId: "not required",
                        tagsListId: "not required"
                    };

                    const resp = await fetch(`${DashboardServiceUrl}/api/dashboard/initialize`, {
                        method: 'POST', // *GET, POST, PUT, DELETE, etc.
                        mode: 'cors', // no-cors, *cors, same-origin
                        cache: 'no-cache', // *default, no-cache, reload, force-cache, only-if-cached
                        // credentials: 'same-origin', // include, *same-origin, omit
                        headers: {
                            'Content-Type': 'application/json'
                            // 'Content-Type': 'application/x-www-form-urlencoded',
                        },
                        // redirect: 'follow', // manual, *follow, error
                        referrerPolicy: 'no-referrer', // no-referrer, *no-referrer-when-downgrade, origin, origin-when-cross-origin, same-origin, strict-origin, strict-origin-when-cross-origin, unsafe-url
                        body: JSON.stringify(data) // body data type must match "Content-Type" header
                    });
                    const folder = await resp.json();
                    console.log("Initialize resp", folder);
                    const baseFolder = {
                        // webUrl: folder.flowsheets.parentName + "/" + folder.flowsheets.folderName,
                        webUrl: "/" + folder.flowsheets.folderName,
                        id: folder.flowsheets.folderListId,
                        driveId: folder.flowsheets.folderDriveId,
                        displayName: "Dashboard"
                    } as ISelectedFolder;

                    this.setState({
                        baseFolder: baseFolder,
                        siteId: folder.siteId,
                        flowsheetsDriveId: folder.flowsheetsDriveId,
                        flowsheetsListId: folder.flowsheetsListId
                    });
                } else {
                    this.setState({ isLoaded: true });
                }
            }
            catch (error) {
                console.log("An error occurred while initializing dashboard.", error);
            } finally {
                this.setState({ isLoaded: true });
            }

        }


        render() {
            const { isLoaded } = this.state;

            if (!isLoaded) {
                return (<div style={{
                    display: "flex",
                    flexDirection: "column",
                    alignItems: "center",
                    justifyContent: "center",
                    height: "100%"
                }}>

                    <Spinner
                        className="main-spinner"
                        styles={{ circle: { width: "100px", height: "100px" } }}
                        label="Loading Dashboard..."
                        labelPosition="bottom" />

                </div>);

            }
            else

                return <WrappedComponent {...this.props} {...this.state} />
        }
    }

}


