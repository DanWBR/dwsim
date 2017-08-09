//This file is part of DWSIM

function LoadMenue() 
  {
  
     //This script is to be used for every content page.
     //DWSIM directely calls a topic page.  
     //On loading a content page this script checks if menu, header and footer are already existing.
     //If not, they will be reloaded. Actual topic is passed to frame.htm to reload this page again.
 

     if (top.frames.length == 0)
	{
	  filename = location.href;
	  ps=filename.split("/").length;
	  filename = filename.split("/")[ps-1]
	  top.location.href = "frame.htm?"+filename;
	}
  
}