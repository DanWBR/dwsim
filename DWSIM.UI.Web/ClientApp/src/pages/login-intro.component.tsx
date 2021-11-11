import { DefaultButton, PrimaryButton } from "@fluentui/react";
import * as React from "react";
declare const chrome: any;


interface IHomePageProps {

}

const LoginIntroPage: React.FC<IHomePageProps> = () => {

    const onRegisterClick = async () => {
        try {
            const systemService = chrome?.webview?.hostObjects?.systemService;
            systemService.openUrlInLocalBrowser("https://simulate365.com/registration-dwsim-pro/");
        } 
        catch (error) {
            console.log("An error occurred while navigating to register page.", error);
        }

    }

    const onLoginClick = async () => {
        try {
            if (chrome?.webview?.hostObjects?.authService) {
                const authService = chrome.webview.hostObjects.authService;
                await authService.navigateToLoginPage();
            } else {
                alert("auth Service not initialized.");
            }
        } catch (error) {
            console.log("An error occurred while navigating to login page.", error);
        }
    }

    const OpenAnchorInSystemBrowser = (e: React.MouseEvent<HTMLAnchorElement, MouseEvent>) => {
        e.preventDefault();

        let nativeEvent = e.nativeEvent as PointerEvent;

        for (let element of (e.nativeEvent as any).path) {
            if (element.tagName == 'A') {
                const systemService = chrome?.webview?.hostObjects?.systemService;
                if (!systemService) {
                    alert("SystemService not available.");
                }
                else {
                    let url = element.href;
                    systemService.openUrlInLocalBrowser(url);
                }

                return false; // Element found, stop execution
            }
        }

        alert("OpenAnchorInSystemBrowser() supports only 'a' element. 'a' element not found in invocation path.");

        return false;
    }

    return <div style={{ height: "100%", display: "flex", flexDirection: "column", justifyContent: 'center', padding: "0px 20px" }}>
        <div style={{ marginLeft: 'auto', marginRight: 'auto', marginBottom: '16px' }}>
            <a href="https://simulate365.com" onClick={OpenAnchorInSystemBrowser}><img src="/s365-full-logo.png" style={{ width: '250px' }} /></a>
        </div>
        <p>
            Access <b><a href="https://simulate365.com/shops/simulate-365-suite/" onClick={OpenAnchorInSystemBrowser}>Simulate 365 DASHBOARD</a></b>, the "all in one place" simulation platform for:
            <ul>
                <li>Intelligent file management</li>
                <li>Syncing simulation files from DASHBOARD to DWSIM</li>
                <li>Easier team collaboration</li>
            </ul>
        </p>
        <div style={{ marginLeft: 'auto', marginRight: 'auto', marginBottom: '16px' }}>
            <PrimaryButton text="Register" onClick={() => onRegisterClick()} style={{ marginRight: "10px" }} />
            <DefaultButton text="Login" onClick={() => onLoginClick()} />
        </div>
        <p>
            Discover <b><a href="https://simulate365.com/downloads/dwsim-pro/" onClick={OpenAnchorInSystemBrowser}>DWSIM Pro</a></b> and other Simulate 365 tools:
            <ul>
                <li>PPBDesigner for Population Balance Modeling</li>
                <li>Multivariate Senstivity Study (MSS)</li>
                <li>Design of Experiments (DoE)</li>
                <li>Take-Home Exercises/Exams (THEE)</li>
                <li>And more!</li>
            </ul>
        </p>
    </div>;
}


export default LoginIntroPage;