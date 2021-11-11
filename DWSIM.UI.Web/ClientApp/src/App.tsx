import React from 'react';
import './App.css';
import { Route } from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";
import HomePage from "./pages/home";


//used only when opening in browser while developing

const  App =()=> {
 

    return <div className="App">

      <Route path="/open" exact component={OpenDashboardFilePage} />
      <Route path="/save" exact component={OpenDashboardFilePage} />

      <Route path="/" exact component={HomePage} />

    </div>;
  
}

export default App;
