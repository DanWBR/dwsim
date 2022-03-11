import React from 'react';
import './App.css';
import { Route } from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";
import LoginIntroPage from "./pages/login-intro.component";
import { ToastContainer } from 'react-toastify';
import 'react-toastify/dist/ReactToastify.css';

//used only when opening in browser while developing

const  App =()=> {
 

    return <>  <ToastContainer
    position="top-right"
    autoClose={5000}
    hideProgressBar={true}
    newestOnTop={false}
    closeOnClick
    rtl={false}
    pauseOnFocusLoss
    draggable
    pauseOnHover
  /> <div className="App">

      <Route path="/filepicker/open/:extension?" exact component={OpenDashboardFilePage} />
      <Route path="/filepicker/save/:extension?" exact render={(props)=><OpenDashboardFilePage {...{isSaveDialog:true}} {...props} />} />

      <Route path="/login/intro" exact component={LoginIntroPage} />

    </div> </>;
  
}

export default App;
