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

export const user_token = "eyJ0eXAiOiJKV1QiLCJub25jZSI6IjNoV1N5VW1BZ180U0o1MGFQdmxxeVFLLVlpVElEMkZYMnVhNGl6MDNWZGMiLCJhbGciOiJSUzI1NiIsIng1dCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCIsImtpZCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCJ9.eyJhdWQiOiJodHRwczovL2dyYXBoLm1pY3Jvc29mdC5jb20iLCJpc3MiOiJodHRwczovL3N0cy53aW5kb3dzLm5ldC9lYjI1NDJiOC01YTVkLTRmNjEtYTliNS02Y2U3ZGJjNGViZmQvIiwiaWF0IjoxNjM2NjE3MzE4LCJuYmYiOjE2MzY2MTczMTgsImV4cCI6MTYzNjYyMTUwMywiYWNjdCI6MCwiYWNyIjoiMSIsImFpbyI6IkUyWmdZTmpacGVVWmZ2WHdsLzMvTWlQdkszWXZkODF4WmZ0NHRZcGw5YUdBVXlLdkJOc0EiLCJhbXIiOlsicHdkIl0sImFwcF9kaXNwbGF5bmFtZSI6IlNoYXJlUG9pbnQgT25saW5lIENsaWVudCBFeHRlbnNpYmlsaXR5IFdlYiBBcHBsaWNhdGlvbiBQcmluY2lwYWwiLCJhcHBpZCI6IjdiNzhlODY5LWQ4NTctNGNlMS1iYzdhLTcwZTNkZDhiM2U0YyIsImFwcGlkYWNyIjoiMCIsImZhbWlseV9uYW1lIjoiTXVsYW9zbWFub3ZpxIciLCJnaXZlbl9uYW1lIjoiU2VuYWQiLCJoYXN3aWRzIjoidHJ1ZSIsImlkdHlwIjoidXNlciIsImlwYWRkciI6Ijc3Ljc3LjIxNS4xOCIsIm5hbWUiOiJTZW5hZCBNdWxhb3NtYW5vdmnEhyIsIm9pZCI6ImJjMmQ4YTg2LWIxZjQtNGRmMy04YzdkLWI4ZTQ5ZDkxNjUwZiIsInBsYXRmIjoiMyIsInB1aWQiOiIxMDAzMjAwMEJDNTg4OTlDIiwicmgiOiIwLkFWMEF1RUlsNjExYVlVLXB0V3puMjhUcl9Xbm9lSHRYMk9GTXZIcHc0OTJMUGt4ZEFEOC4iLCJzY3AiOiJBcHBsaWNhdGlvbi5SZWFkV3JpdGUuQWxsIERpcmVjdG9yeS5BY2Nlc3NBc1VzZXIuQWxsIERpcmVjdG9yeS5SZWFkLkFsbCBGaWxlcy5SZWFkIEZpbGVzLlJlYWQuQWxsIEZpbGVzLlJlYWRXcml0ZSBGaWxlcy5SZWFkV3JpdGUuQWxsIEdyb3VwLlJlYWQuQWxsIEdyb3VwTWVtYmVyLlJlYWQuQWxsIFBlb3BsZS5SZWFkIFBlb3BsZS5SZWFkLkFsbCBTaXRlcy5SZWFkLkFsbCBTaXRlcy5SZWFkV3JpdGUuQWxsIFVzZXIuUmVhZC5BbGwgcHJvZmlsZSBvcGVuaWQgZW1haWwiLCJzaWduaW5fc3RhdGUiOlsia21zaSJdLCJzdWIiOiJfSFlDMGRyTnBneGFoVjFOWmhvVDI0ckdjNXJkNkNLc0tQNGJJMkJDMzlNIiwidGVuYW50X3JlZ2lvbl9zY29wZSI6IkVVIiwidGlkIjoiZWIyNTQyYjgtNWE1ZC00ZjYxLWE5YjUtNmNlN2RiYzRlYmZkIiwidW5pcXVlX25hbWUiOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1cG4iOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1dGkiOiIwejJjLUpYZkVVZUMxcnczX2Njb0FBIiwidmVyIjoiMS4wIiwieG1zX3N0Ijp7InN1YiI6Ik9tb1Q0OWxmRHpMT2hBZFZhNmswU3g3ZFJrTEk4aFJUendoaklLMkdhUjAifSwieG1zX3RjZHQiOjE1ODg2MDY5NTV9.Y0Vq1JQjn7AuthakmSOi5ajle7wn2t-voUi9U83H1kO0kT3NItPIG70tSnaRK4jCul2bnAg3MgSsP8DnOx8dMM5f4dTVUgTG7LfvI2fF_Ay55tQ5l-wy9a5Xi4sf7q5BMbDdWHjGY0DdWty0loumxrMPfNQP2QkMHljHQqtF939vST3AbirE7dooNmY51YVoWqlvq5VQ85KD8ZUu9ZFOthyw698-q33aRNHuDEBfdiA3MLWstlKQ-EP0nQFGVfTtR5CQrLGhkqd8C7r5Bs7UyC9i8kHMtgR-24pKGN7lc-SHqAgXDHvy2zw_ImqcZRhoB5DiLXNiAJglsjbh5xn2CA";
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

                return <WrappedComponent {...this.state} />
        }
    }

}


