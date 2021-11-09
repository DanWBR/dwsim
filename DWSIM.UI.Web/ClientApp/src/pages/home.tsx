import { DefaultButton, PrimaryButton } from "@fluentui/react";
import * as React from "react";
declare const chrome: any;


interface IHomePageProps {

}

const HomePage: React.FC<IHomePageProps> = () => {

const onRegisterClick= async ()=>{
    try {
        if (chrome?.webview?.hostObjects?.authService) {
            const authService= chrome.webview.hostObjects.authService;
            await authService.openRegisterLinkInLocalBrowser();
     }else{
         alert("auth Service not initialized.");
     }  
    } catch (error) {
        console.log("An error occurred while navigating to register page.", error);
    }
    
}
const onLoginClick= async ()=>{
    try {
        if (chrome?.webview?.hostObjects?.authService) {
            const authService= chrome.webview.hostObjects.authService;
            await authService.navigateToLoginPage();
     }else{
         alert("auth Service not initialized.");
     }  
    } catch (error) {
        console.log("An error occurred while navigating to login page.", error);
    }
    
}

    return <div style={{display:"flex",justifyContent:"center", alignItems:"center" , height:"100%", flexDirection:"column", padding:"0px 20px"}}>
        <span>  Access Simulate 365 DASHBOARD, the “all in one place” simulation platform for:</span>
        <ul>
            <li>Intelligent file management</li>
            <li>Easier team collaboration</li>
            <li>Access to advanced engineering tools </li>
        </ul>
        <div>
            <PrimaryButton text="Register" onClick={()=>onRegisterClick()} style={{marginRight:"10px"}}/> 
             <DefaultButton text="Login"  onClick={()=>onLoginClick()}/>
        </div>
    </div>;
}


export default HomePage