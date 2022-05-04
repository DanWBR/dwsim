import * as React from "react";
import { Spinner } from "@fluentui/react";
import { ISelectedFolder } from "../interfaces/documents/document.interfaces";
import { toast } from "react-toastify";
import { RouteComponentProps, useHistory } from "react-router-dom";
declare const chrome: any;


export interface IInitializeDashboardProps extends RouteComponentProps<any> {

}
export interface IInitializeDashboardState {
    baseFolder?: ISelectedFolder;
    isLoaded: boolean;
    isLoggedIn: boolean;
    isDashboardInitialized: boolean;
    siteId: string;
    flowsheetsListId: string;
    flowsheetsDriveId: string;
    isError: boolean;
}

export const user_token = window.localStorage.getItem("s365_local_user_token");
const DashboardServiceUrl = process.env.REACT_APP_DASHBOARD_SERVICE_URL;


export function withInitializeDashboard(WrappedComponent: any) {


    return class extends React.Component<IInitializeDashboardProps, IInitializeDashboardState>{

        /**
         *
         */
        constructor(props: IInitializeDashboardProps) {
            super(props);
            this.state = {
                isLoaded: false,
                isDashboardInitialized: false,
                isLoggedIn: true,
                isError: false,
                siteId: "",
                flowsheetsDriveId: "",
                flowsheetsListId: ""
            };
        }

        async componentDidMount() {
            const token = await this.getUserToken();
            if (token) {
                this.setState({ isLoggedIn: true, isLoaded: true });
                await this.getBaseFolder(token);
            } else {
                this.setState({ isLoggedIn: false, isLoaded: true });
            }

        }

        async getUserToken() {
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
            return token;
        }
        async getBaseFolder(token: string) {


            try {
                if (!!token && token.length > 0) {
                    this.setState({ isLoaded: false });
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
                    if (!folder || !folder.flowsheets.folderDriveId) {
                        throw "An error occurred while initializing Simulate 365 Dashboard.";
                    }
                    const baseFolder = {
                        // webUrl: folder.flowsheets.parentName + "/" + folder.flowsheets.folderName,
                        webUrl: "/" + folder.flowsheets.folderName,
                        id: folder.flowsheets.folderListId,
                        driveId: folder.flowsheets.folderDriveId,
                        displayName: "Dashboard"
                    } as ISelectedFolder;

                    this.setState({
                        isDashboardInitialized: true,
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
                this.setState({ isError: true });
                console.log("An error occurred while initializing dashboard.", error);

                //  toast.error("An error occurred while initializing dashboard.");
            } finally {
                this.setState({ isLoaded: true });
            }

        }


        render() {
            const { isLoaded, isError, isLoggedIn, isDashboardInitialized } = this.state;


            if (isError) {
                return (<div style={{
                    display: "flex",
                    flexDirection: "column",
                    alignItems: "center",
                    justifyContent: "center",
                    height: "100%"
                }}>

                    <span className="text-danger">An error occurred while initializing Simulate 365 Dashboard.</span>

                </div>)
            }

            if (!isLoggedIn) {
                return (<div style={{
                    display: "flex",
                    flexDirection: "column",
                    alignItems: "center",
                    justifyContent: "center",
                    height: "100%"
                }}>

                    <div className="text-danger" style={{textAlign:"center"}}>
                       To open, save or upload files to the program, log in with <b>Simulate 365 account</b>.<br/>
                       The login connects the program to <b>dashboard.simulate365.com</b>, your personal file management system.<br/>
                        You will be able to access and manage DASHBOARD files directly in your simulator.<br/>
                       To benefit from this feature, first <b>sync files and flowsheets</b> on your local machine with DASHBOARD.  </div>

                </div>)
            }

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
            else if (isDashboardInitialized) {
                return <WrappedComponent {...this.props} {...this.state} />;
            }
            return null;
        }
    }

}


