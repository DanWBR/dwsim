import { AuthProvider, AuthProviderCallback, Client, Options } from "@microsoft/microsoft-graph-client";
import { IUsersService } from "../../interfaces/services/users-service.interface";

declare const chrome: any;

// Some callback function
const authProvider: AuthProvider = async (callback: AuthProviderCallback) => {
    // Your logic for getting and refreshing accessToken
    if (chrome.webview) {
        const usersService = chrome.webview.hostObjects.usersService as IUsersService;
        if (!usersService)
            callback("User Service not Found.", null);
        const token = await usersService.getUserToken();
        callback(null, token);
        try {

        } catch (error) {
            const errorModel = { message: "An error occurred while getting token.", error: error };
            callback(errorModel, null);
        }

    }
    //for test cases using browser
    const token="eyJ0eXAiOiJKV1QiLCJub25jZSI6Ikg0QmdFVmREYnIzRzl0a2VWM1VMbDVuWU50VHdJQU56VWZ0ZE1aSTBDdzQiLCJhbGciOiJSUzI1NiIsIng1dCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCIsImtpZCI6Imwzc1EtNTBjQ0g0eEJWWkxIVEd3blNSNzY4MCJ9.eyJhdWQiOiJodHRwczovL2dyYXBoLm1pY3Jvc29mdC5jb20iLCJpc3MiOiJodHRwczovL3N0cy53aW5kb3dzLm5ldC9lYjI1NDJiOC01YTVkLTRmNjEtYTliNS02Y2U3ZGJjNGViZmQvIiwiaWF0IjoxNjM2MDMzMzQ0LCJuYmYiOjE2MzYwMzMzNDQsImV4cCI6MTYzNjAzNzI0NCwiYWNjdCI6MCwiYWNyIjoiMSIsImFpbyI6IkFTUUEyLzhUQUFBQVRVcGx3WkFwb29SSEZsaXhNVEpSN3V4ZG1hSHdqUmlVSThKazR0aStHUDQ9IiwiYW1yIjpbInB3ZCJdLCJhcHBfZGlzcGxheW5hbWUiOiJTaGFyZVBvaW50IE9ubGluZSBDbGllbnQgRXh0ZW5zaWJpbGl0eSBXZWIgQXBwbGljYXRpb24gUHJpbmNpcGFsIiwiYXBwaWQiOiI3Yjc4ZTg2OS1kODU3LTRjZTEtYmM3YS03MGUzZGQ4YjNlNGMiLCJhcHBpZGFjciI6IjAiLCJmYW1pbHlfbmFtZSI6Ik11bGFvc21hbm92acSHIiwiZ2l2ZW5fbmFtZSI6IlNlbmFkIiwiaGFzd2lkcyI6InRydWUiLCJpZHR5cCI6InVzZXIiLCJpcGFkZHIiOiI3Ny43Ny4yMTUuMTgiLCJuYW1lIjoiU2VuYWQgTXVsYW9zbWFub3ZpxIciLCJvaWQiOiJiYzJkOGE4Ni1iMWY0LTRkZjMtOGM3ZC1iOGU0OWQ5MTY1MGYiLCJwbGF0ZiI6IjMiLCJwdWlkIjoiMTAwMzIwMDBCQzU4ODk5QyIsInJoIjoiMC5BVjBBdUVJbDYxMWFZVS1wdFd6bjI4VHJfV25vZUh0WDJPRk12SHB3NDkyTFBreGRBRDguIiwic2NwIjoiQXBwbGljYXRpb24uUmVhZFdyaXRlLkFsbCBEaXJlY3RvcnkuQWNjZXNzQXNVc2VyLkFsbCBEaXJlY3RvcnkuUmVhZC5BbGwgRmlsZXMuUmVhZCBGaWxlcy5SZWFkLkFsbCBGaWxlcy5SZWFkV3JpdGUgRmlsZXMuUmVhZFdyaXRlLkFsbCBHcm91cC5SZWFkLkFsbCBHcm91cE1lbWJlci5SZWFkLkFsbCBQZW9wbGUuUmVhZCBQZW9wbGUuUmVhZC5BbGwgU2l0ZXMuUmVhZC5BbGwgU2l0ZXMuUmVhZFdyaXRlLkFsbCBVc2VyLlJlYWQuQWxsIHByb2ZpbGUgb3BlbmlkIGVtYWlsIiwic2lnbmluX3N0YXRlIjpbImttc2kiXSwic3ViIjoiX0hZQzBkck5wZ3hhaFYxTlpob1QyNHJHYzVyZDZDS3NLUDRiSTJCQzM5TSIsInRlbmFudF9yZWdpb25fc2NvcGUiOiJFVSIsInRpZCI6ImViMjU0MmI4LTVhNWQtNGY2MS1hOWI1LTZjZTdkYmM0ZWJmZCIsInVuaXF1ZV9uYW1lIjoic21Ac2ltdWxhdGUzNjUub25taWNyb3NvZnQuY29tIiwidXBuIjoic21Ac2ltdWxhdGUzNjUub25taWNyb3NvZnQuY29tIiwidXRpIjoiWnA1RTFoUGE2VVM0R1V2TXplWVBBQSIsInZlciI6IjEuMCIsInhtc19zdCI6eyJzdWIiOiJPbW9UNDlsZkR6TE9oQWRWYTZrMFN4N2RSa0xJOGhSVHp3aGpJSzJHYVIwIn0sInhtc190Y2R0IjoxNTg4NjA2OTU1fQ.E6DsaNAqpwjFqIAo8PBJyn5nWU4f2e9VIzKZXZ71y7bSvPH4U3X9UDi_W7OlCE7CmUU56TH5QLuXnp1RWs1M19jWFAyMa0h5Kzw-Lf6MyAD1JPPCf9wYJDtCJ1Wv14oEVBNjWxHXkIgV6vSa1tdIhUtK-CP-H_MV0nU8KP3pSi_FkEDAFtyGg32fyWxvZYK1rj8a61qMlXNbTbkd98eoR23sVpxz05VKmgVyG9HXrkBQrxOzJ3z3ufDT_Hzu8gwb1iy6oAfI80G9OU3rMmDKcLKmBtY7IcpL4LPhoFveGk9IQVMbrrpwr2lVo_NyM6Gh_Ep9oSbIwe7cpJqAtw7tuQ";
    callback(null, token);


};
let options: Options = {
    authProvider,
};
export const client = Client.init(options);