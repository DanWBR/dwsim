import React from 'react';
import './App.css';
import { Route} from 'react-router-dom';
import OpenDashboardFilePage from "./pages/open-dashboard-file.component";

function App() {
  return (
    <div className="App">
     
        <Route path="/" component={OpenDashboardFilePage} />
       
     
    </div>
  );
}

export default App;
