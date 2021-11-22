import { AuthProvider, AuthProviderCallback, Client, Options } from "@microsoft/microsoft-graph-client";
import {user_token} from "../../components/with-initialize-dashboard.hoc";
declare const chrome: any;

// Some callback function
const authProvider: AuthProvider = async (callback: AuthProviderCallback) => {
    // Your logic for getting and refreshing accessToken
    console.log("GetToken called", chrome.webview);
    if (chrome.webview) {
        const authService = chrome.webview.hostObjects.authService;
        if (!authService) {
            callback("Auth Service not Found.", null); return;
        }

        try {
            const token = await authService.getUserToken();
            callback(null, token);
        } catch (error) {
            console.log("An error occurred while getting token.",error);
            const errorModel = { message: "An error occurred while getting token.", error: error };
            callback(errorModel, null);
        }


    } else {
        console.log("Auth Service not found, using hardcoded token");
        //for test cases using browser
        const token = user_token;
        callback(null, token);

    }


};
let options: Options = {
    authProvider,
};
export const msGraphClient = Client.init(options);