import React from 'react';
import './App.css';
import { Route } from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";
import LoginIntroPage from "./pages/login-intro.component";


//used only when opening in browser while developing

const  App =()=> {
 

    return <div className="App">

      <Route path="/filepicker/open" exact component={OpenDashboardFilePage} />
      <Route path="/filepicker/save/:extension?" exact render={(props)=><OpenDashboardFilePage {...{isSaveDialog:true}} {...props} />} />

      <Route path="/login/intro" exact component={LoginIntroPage} />

    </div>;
  
}

export default App;
