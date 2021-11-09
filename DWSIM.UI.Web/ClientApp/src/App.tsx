import React from 'react';
import './App.css';
import { Route } from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";
import { ISelectedFolder } from './interfaces/documents/document.interfaces';
import { Spinner } from '@fluentui/react';
import HomePage from "./pages/home";
declare const chrome: any;

interface IAppState {
  baseFolder?: ISelectedFolder;
  isLoaded: boolean;
}

const SITE_ID = process.env.REACT_APP_DASHBOARD_SITE_ID;
const FLOWSHEETS_DRIVE_ID = process.env.REACT_APP_DASHBOARD_FLOWSHEETS_DRIVE_ID;
const FLOWSHEETS_LIST_ID = process.env.REACT_APP_DASHBOARD_FLOWSHEETS_LIST_ID;
const DashboardServiceUrl = process.env.REACT_APP_DASHBOARD_SERVICE_URL;
const TAGS_LIST_ID = process.env.REACT_APP_TAGS_LIST_ID;
//used only when opening in browser while developing
export const user_token = "eyJ0eXAiOiJKV1QiLCJub25jZSI6IkgtNlpxTmxQc05TNXE5cnM1YU9PM0JUN1hESElOWkpxQnh5RVc4TTRfNzAiLCJhbGciOiJSUzI1NiIsIng1dCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCIsImtpZCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCJ9.eyJhdWQiOiJodHRwczovL2dyYXBoLm1pY3Jvc29mdC5jb20iLCJpc3MiOiJodHRwczovL3N0cy53aW5kb3dzLm5ldC9lYjI1NDJiOC01YTVkLTRmNjEtYTliNS02Y2U3ZGJjNGViZmQvIiwiaWF0IjoxNjM2NDU4ODczLCJuYmYiOjE2MzY0NTg4NzMsImV4cCI6MTYzNjQ2Mjc3MywiYWNjdCI6MCwiYWNyIjoiMSIsImFpbyI6IkUyWmdZREJxMkp2U0ZEMjF2bXpKVzU2Z2RISHU4dks5bVFvbjVaMWJYbTJYYXhiOXNRNEEiLCJhbXIiOlsicHdkIl0sImFwcF9kaXNwbGF5bmFtZSI6IlNoYXJlUG9pbnQgT25saW5lIENsaWVudCBFeHRlbnNpYmlsaXR5IFdlYiBBcHBsaWNhdGlvbiBQcmluY2lwYWwiLCJhcHBpZCI6IjdiNzhlODY5LWQ4NTctNGNlMS1iYzdhLTcwZTNkZDhiM2U0YyIsImFwcGlkYWNyIjoiMCIsImZhbWlseV9uYW1lIjoiTXVsYW9zbWFub3ZpxIciLCJnaXZlbl9uYW1lIjoiU2VuYWQiLCJoYXN3aWRzIjoidHJ1ZSIsImlkdHlwIjoidXNlciIsImlwYWRkciI6Ijc3Ljc3LjIxNS4xOCIsIm5hbWUiOiJTZW5hZCBNdWxhb3NtYW5vdmnEhyIsIm9pZCI6ImJjMmQ4YTg2LWIxZjQtNGRmMy04YzdkLWI4ZTQ5ZDkxNjUwZiIsInBsYXRmIjoiMyIsInB1aWQiOiIxMDAzMjAwMEJDNTg4OTlDIiwicmgiOiIwLkFWMEF1RUlsNjExYVlVLXB0V3puMjhUcl9Xbm9lSHRYMk9GTXZIcHc0OTJMUGt4ZEFEOC4iLCJzY3AiOiJBcHBsaWNhdGlvbi5SZWFkV3JpdGUuQWxsIERpcmVjdG9yeS5BY2Nlc3NBc1VzZXIuQWxsIERpcmVjdG9yeS5SZWFkLkFsbCBGaWxlcy5SZWFkIEZpbGVzLlJlYWQuQWxsIEZpbGVzLlJlYWRXcml0ZSBGaWxlcy5SZWFkV3JpdGUuQWxsIEdyb3VwLlJlYWQuQWxsIEdyb3VwTWVtYmVyLlJlYWQuQWxsIFBlb3BsZS5SZWFkIFBlb3BsZS5SZWFkLkFsbCBTaXRlcy5SZWFkLkFsbCBTaXRlcy5SZWFkV3JpdGUuQWxsIFVzZXIuUmVhZC5BbGwgcHJvZmlsZSBvcGVuaWQgZW1haWwiLCJzaWduaW5fc3RhdGUiOlsia21zaSJdLCJzdWIiOiJfSFlDMGRyTnBneGFoVjFOWmhvVDI0ckdjNXJkNkNLc0tQNGJJMkJDMzlNIiwidGVuYW50X3JlZ2lvbl9zY29wZSI6IkVVIiwidGlkIjoiZWIyNTQyYjgtNWE1ZC00ZjYxLWE5YjUtNmNlN2RiYzRlYmZkIiwidW5pcXVlX25hbWUiOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1cG4iOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1dGkiOiJ2S0VUTW83ay1raXpSVC1Ia3hJbEFBIiwidmVyIjoiMS4wIiwieG1zX3N0Ijp7InN1YiI6Ik9tb1Q0OWxmRHpMT2hBZFZhNmswU3g3ZFJrTEk4aFJUendoaklLMkdhUjAifSwieG1zX3RjZHQiOjE1ODg2MDY5NTV9.hFU9TLxh066ZuPfvTfnNpREYRltzGvtsMg6Wkw4GCyFFww4Zq5TwGQNW5ALspMpelZUOlz5MPzCco-DqQK4iISFJgt3u7K1vtaQ3IDi3dafrZWmTEylwOtDvoDIaT00_X6hECxH4DCduITxdJw2zXe5Rl7sC9m8pgwbqsJhTZf8pNf0f-HilRrrUIujjcv1WeOddC12K2ylHzBZACyCAUMGXseUFlRgf8OKk4eLOOSNVnkbvS-4TSIj_yYQ8B55Nepy8fWHRpgZ522k_UHPCkXxq2Jiyxsb_omXHqS2Pmo--iSFy3f1cp4llf9Zi4xPin-tGIVTweAcocrxWfFFFUw";

class App extends React.Component<{}, IAppState> {
  /**
   *
   */
  constructor(props: any) {
    super(props);
    this.state = { baseFolder: undefined, isLoaded: false };
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
          siteId: SITE_ID,
          flowsheetsListId: FLOWSHEETS_LIST_ID,
          tagsListId: TAGS_LIST_ID
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

        this.setState({ baseFolder: baseFolder });
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
    const { baseFolder, isLoaded } = this.state;

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

      return <div className="App">

        <Route path="/open" exact render={(props: any) => <OpenDashboardFilePage
          baseFolder={baseFolder}
          siteId={SITE_ID}
          flowsheetsDriveId={FLOWSHEETS_DRIVE_ID}
          flowsheetsListId={FLOWSHEETS_LIST_ID}
          {...props} />} />
        <Route path="/save" exact render={(props: any) => <OpenDashboardFilePage
          isSaveDialog={true}
          baseFolder={baseFolder}
          siteId={SITE_ID}
          flowsheetsDriveId={FLOWSHEETS_DRIVE_ID}
          flowsheetsListId={FLOWSHEETS_LIST_ID}
          {...props} />} />

        <Route path="/" exact component={HomePage} />

      </div>;
  }
}

export default App;
