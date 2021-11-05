import React from 'react';
import './App.css';
import { Route} from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";
import { ISelectedFolder } from './interfaces/documents/document.interfaces';
import { Spinner } from '@fluentui/react';
declare const chrome: any;

interface IAppState{
  baseFolder?:ISelectedFolder;
  isLoaded:boolean;
}

const SITE_ID=process.env.REACT_APP_DASHBOARD_SITE_ID;
const FLOWSHEETS_DRIVE_ID=process.env.REACT_APP_DASHBOARD_FLOWSHEETS_DRIVE_ID;
const FLOWSHEETS_LIST_ID=process.env.REACT_APP_DASHBOARD_FLOWSHEETS_LIST_ID;
const DashboardServiceUrl=process.env.REACT_APP_DASHBOARD_SERVICE_URL;
const TAGS_LIST_ID=process.env.REACT_APP_TAGS_LIST_ID;
//used only when opening in browser while developing
const user_token="eyJ0eXAiOiJKV1QiLCJub25jZSI6InZSakV5N1hsbzlYMllwTzFoS1NOdVVXbjJjUDAtM0FoSTJzSXVJWDMwUlUiLCJhbGciOiJSUzI1NiIsIng1dCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCIsImtpZCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCJ9.eyJhdWQiOiJodHRwczovL2dyYXBoLm1pY3Jvc29mdC5jb20iLCJpc3MiOiJodHRwczovL3N0cy53aW5kb3dzLm5ldC9lYjI1NDJiOC01YTVkLTRmNjEtYTliNS02Y2U3ZGJjNGViZmQvIiwiaWF0IjoxNjM2MTE2NzEwLCJuYmYiOjE2MzYxMTY3MTAsImV4cCI6MTYzNjEyMTkzNywiYWNjdCI6MCwiYWNyIjoiMSIsImFpbyI6IkFTUUEyLzhUQUFBQVJGM0x3VnlyejFLaUhFeHNCZDUwM3JreEROV2NQMmNpZy95WnVVVDZ6THM9IiwiYW1yIjpbInB3ZCJdLCJhcHBfZGlzcGxheW5hbWUiOiJTaGFyZVBvaW50IE9ubGluZSBDbGllbnQgRXh0ZW5zaWJpbGl0eSBXZWIgQXBwbGljYXRpb24gUHJpbmNpcGFsIiwiYXBwaWQiOiI3Yjc4ZTg2OS1kODU3LTRjZTEtYmM3YS03MGUzZGQ4YjNlNGMiLCJhcHBpZGFjciI6IjAiLCJmYW1pbHlfbmFtZSI6Ik11bGFvc21hbm92acSHIiwiZ2l2ZW5fbmFtZSI6IlNlbmFkIiwiaGFzd2lkcyI6InRydWUiLCJpZHR5cCI6InVzZXIiLCJpcGFkZHIiOiI0Ni4yMzkuNi41MyIsIm5hbWUiOiJTZW5hZCBNdWxhb3NtYW5vdmnEhyIsIm9pZCI6ImJjMmQ4YTg2LWIxZjQtNGRmMy04YzdkLWI4ZTQ5ZDkxNjUwZiIsInBsYXRmIjoiMyIsInB1aWQiOiIxMDAzMjAwMEJDNTg4OTlDIiwicmgiOiIwLkFWMEF1RUlsNjExYVlVLXB0V3puMjhUcl9Xbm9lSHRYMk9GTXZIcHc0OTJMUGt4ZEFEOC4iLCJzY3AiOiJBcHBsaWNhdGlvbi5SZWFkV3JpdGUuQWxsIERpcmVjdG9yeS5BY2Nlc3NBc1VzZXIuQWxsIERpcmVjdG9yeS5SZWFkLkFsbCBGaWxlcy5SZWFkIEZpbGVzLlJlYWQuQWxsIEZpbGVzLlJlYWRXcml0ZSBGaWxlcy5SZWFkV3JpdGUuQWxsIEdyb3VwLlJlYWQuQWxsIEdyb3VwTWVtYmVyLlJlYWQuQWxsIFBlb3BsZS5SZWFkIFBlb3BsZS5SZWFkLkFsbCBTaXRlcy5SZWFkLkFsbCBTaXRlcy5SZWFkV3JpdGUuQWxsIFVzZXIuUmVhZC5BbGwgcHJvZmlsZSBvcGVuaWQgZW1haWwiLCJzaWduaW5fc3RhdGUiOlsia21zaSJdLCJzdWIiOiJfSFlDMGRyTnBneGFoVjFOWmhvVDI0ckdjNXJkNkNLc0tQNGJJMkJDMzlNIiwidGVuYW50X3JlZ2lvbl9zY29wZSI6IkVVIiwidGlkIjoiZWIyNTQyYjgtNWE1ZC00ZjYxLWE5YjUtNmNlN2RiYzRlYmZkIiwidW5pcXVlX25hbWUiOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1cG4iOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1dGkiOiJZdzNyTmxYZlUwS2FHRHUyZ0tzcUFBIiwidmVyIjoiMS4wIiwieG1zX3N0Ijp7InN1YiI6Ik9tb1Q0OWxmRHpMT2hBZFZhNmswU3g3ZFJrTEk4aFJUendoaklLMkdhUjAifSwieG1zX3RjZHQiOjE1ODg2MDY5NTV9.V7-hO9gaybtHxG39-vQ8L56Fw-HLbT0Im8p7wJoa8V4FqWF4C6Jw_LFOCW3YA8aNqWOiz3h8fzJ60fvpi37zM_nY9iJm6MrKHCPbMVS8hhrY40rDxFzvZjptKLJfdlajyLtbpKr8fnY-XC_L2tkpoUnsJ14EGx1KA3ACr-x6TCPCLVjcxGUhXXEK-saYERw0CaOQByNiZPKQaUB8diyBI18LCuBfpcExJc0nPGiPgssZ1XoO-2nwOBUduL5Fs6-Hcwaig_7USwuvnsHitjqhZX42Dc8OLoKAaDVQF2Bqv54aahdz-FnEDXjXt2syOwgWbraivHxHX0XK6YPRNsmp5Q";

class  App extends React.Component<{},IAppState> {
/**
 *
 */
constructor(props:any) {
  super(props);
 this.state={baseFolder:undefined, isLoaded:false};
}
async componentDidMount(){
await this.getBaseFolder();
}
async getBaseFolder(){
  try {

    this.setState({isLoaded:false});

    let token=user_token;
  if(chrome.webview?.hostObjects?.authService){
  const authService = chrome.webview.hostObjects.authService;
   token= await authService.getUserToken();
  }
const data={
  userAccessToken:token,
  siteId:SITE_ID,
  flowsheetsListId:FLOWSHEETS_LIST_ID,
  tagsListId:TAGS_LIST_ID
};

const resp= await  fetch(`${DashboardServiceUrl}/api/dashboard/initialize`,{
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
const folder= await resp.json();
console.log("Initialize resp",folder);
const baseFolder = {
  // webUrl: folder.flowsheets.parentName + "/" + folder.flowsheets.folderName,
  webUrl: "/" + folder.flowsheets.folderName,
  id: folder.flowsheets.folderListId,
  driveId: folder.flowsheets.folderDriveId,
  displayName: "Dashboard"
} as ISelectedFolder;

this.setState({baseFolder:baseFolder});
  } catch (error) {
    console.log("An error occurred while initializing dashboard.",error);
  }finally{
 this.setState({isLoaded:true});
  }
  
}


render(){
  const {baseFolder, isLoaded}=this.state;

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

  return !!baseFolder ?  <div className="App">
     
        <Route path="/" render={(props:any)=><OpenDashboardFilePage
         baseFolder={baseFolder}
         siteId={SITE_ID}
         flowsheetsDriveId={FLOWSHEETS_DRIVE_ID}
         flowsheetsListId={FLOWSHEETS_LIST_ID}
          {...props}/>} />
       
     
    </div>: null;
}
}

export default App;
