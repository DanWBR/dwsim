import { AuthProvider, AuthProviderCallback, Client, Options } from "@microsoft/microsoft-graph-client";

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
        const token = "eyJ0eXAiOiJKV1QiLCJub25jZSI6InZSakV5N1hsbzlYMllwTzFoS1NOdVVXbjJjUDAtM0FoSTJzSXVJWDMwUlUiLCJhbGciOiJSUzI1NiIsIng1dCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCIsImtpZCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCJ9.eyJhdWQiOiJodHRwczovL2dyYXBoLm1pY3Jvc29mdC5jb20iLCJpc3MiOiJodHRwczovL3N0cy53aW5kb3dzLm5ldC9lYjI1NDJiOC01YTVkLTRmNjEtYTliNS02Y2U3ZGJjNGViZmQvIiwiaWF0IjoxNjM2MTE2NzEwLCJuYmYiOjE2MzYxMTY3MTAsImV4cCI6MTYzNjEyMTkzNywiYWNjdCI6MCwiYWNyIjoiMSIsImFpbyI6IkFTUUEyLzhUQUFBQVJGM0x3VnlyejFLaUhFeHNCZDUwM3JreEROV2NQMmNpZy95WnVVVDZ6THM9IiwiYW1yIjpbInB3ZCJdLCJhcHBfZGlzcGxheW5hbWUiOiJTaGFyZVBvaW50IE9ubGluZSBDbGllbnQgRXh0ZW5zaWJpbGl0eSBXZWIgQXBwbGljYXRpb24gUHJpbmNpcGFsIiwiYXBwaWQiOiI3Yjc4ZTg2OS1kODU3LTRjZTEtYmM3YS03MGUzZGQ4YjNlNGMiLCJhcHBpZGFjciI6IjAiLCJmYW1pbHlfbmFtZSI6Ik11bGFvc21hbm92acSHIiwiZ2l2ZW5fbmFtZSI6IlNlbmFkIiwiaGFzd2lkcyI6InRydWUiLCJpZHR5cCI6InVzZXIiLCJpcGFkZHIiOiI0Ni4yMzkuNi41MyIsIm5hbWUiOiJTZW5hZCBNdWxhb3NtYW5vdmnEhyIsIm9pZCI6ImJjMmQ4YTg2LWIxZjQtNGRmMy04YzdkLWI4ZTQ5ZDkxNjUwZiIsInBsYXRmIjoiMyIsInB1aWQiOiIxMDAzMjAwMEJDNTg4OTlDIiwicmgiOiIwLkFWMEF1RUlsNjExYVlVLXB0V3puMjhUcl9Xbm9lSHRYMk9GTXZIcHc0OTJMUGt4ZEFEOC4iLCJzY3AiOiJBcHBsaWNhdGlvbi5SZWFkV3JpdGUuQWxsIERpcmVjdG9yeS5BY2Nlc3NBc1VzZXIuQWxsIERpcmVjdG9yeS5SZWFkLkFsbCBGaWxlcy5SZWFkIEZpbGVzLlJlYWQuQWxsIEZpbGVzLlJlYWRXcml0ZSBGaWxlcy5SZWFkV3JpdGUuQWxsIEdyb3VwLlJlYWQuQWxsIEdyb3VwTWVtYmVyLlJlYWQuQWxsIFBlb3BsZS5SZWFkIFBlb3BsZS5SZWFkLkFsbCBTaXRlcy5SZWFkLkFsbCBTaXRlcy5SZWFkV3JpdGUuQWxsIFVzZXIuUmVhZC5BbGwgcHJvZmlsZSBvcGVuaWQgZW1haWwiLCJzaWduaW5fc3RhdGUiOlsia21zaSJdLCJzdWIiOiJfSFlDMGRyTnBneGFoVjFOWmhvVDI0ckdjNXJkNkNLc0tQNGJJMkJDMzlNIiwidGVuYW50X3JlZ2lvbl9zY29wZSI6IkVVIiwidGlkIjoiZWIyNTQyYjgtNWE1ZC00ZjYxLWE5YjUtNmNlN2RiYzRlYmZkIiwidW5pcXVlX25hbWUiOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1cG4iOiJzbUBzaW11bGF0ZTM2NS5vbm1pY3Jvc29mdC5jb20iLCJ1dGkiOiJZdzNyTmxYZlUwS2FHRHUyZ0tzcUFBIiwidmVyIjoiMS4wIiwieG1zX3N0Ijp7InN1YiI6Ik9tb1Q0OWxmRHpMT2hBZFZhNmswU3g3ZFJrTEk4aFJUendoaklLMkdhUjAifSwieG1zX3RjZHQiOjE1ODg2MDY5NTV9.V7-hO9gaybtHxG39-vQ8L56Fw-HLbT0Im8p7wJoa8V4FqWF4C6Jw_LFOCW3YA8aNqWOiz3h8fzJ60fvpi37zM_nY9iJm6MrKHCPbMVS8hhrY40rDxFzvZjptKLJfdlajyLtbpKr8fnY-XC_L2tkpoUnsJ14EGx1KA3ACr-x6TCPCLVjcxGUhXXEK-saYERw0CaOQByNiZPKQaUB8diyBI18LCuBfpcExJc0nPGiPgssZ1XoO-2nwOBUduL5Fs6-Hcwaig_7USwuvnsHitjqhZX42Dc8OLoKAaDVQF2Bqv54aahdz-FnEDXjXt2syOwgWbraivHxHX0XK6YPRNsmp5Q";
        callback(null, token);

    }


};
let options: Options = {
    authProvider,
};
export const msGraphClient = Client.init(options);