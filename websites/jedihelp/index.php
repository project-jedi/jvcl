<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";

  $tpl = new HTML_Template_IT("./"); 

  $tpl->loadTemplatefile("index.tpl.html", false, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonFooter($tpl); 
  
  $projects = GetProjectsInfos();

  if (is_array($projects))
  {
    foreach ($projects as $project)
    {
      // Assign data to the search option block 
      $tpl->setCurrentBlock("search_section");
      $tpl->setCurrentBlock("search_project_name");
      $tpl->setVariable("PROJECT_ID", $project["Id"]);
      $tpl->setVariable("PROJECT_NAME", $project["Name"]);
      $tpl->parseCurrentBlock("search_project_name");
      $tpl->parseCurrentBlock("search_section");
  
      // Assign data to the browse block 
      $tpl->setCurrentBlock("browse_section");
      $tpl->setCurrentBlock("browse_project");
      $tpl->setVariable("PROJECT_ID", $project["Id"]);
      $tpl->setVariable("PROJECT_NAME", $project["Name"]);
      $tpl->setVariable("PROJECT_DESCRIPTION", $project["Description"]);
      $tpl->parseCurrentBlock("browse_project");
      $tpl->parseCurrentBlock("browse_section");
    }
  }
  else
  {
      $tpl->setCurrentBlock("search_no_projects");
      $tpl->setVariable("NO_PROJECTS_MESSAGE", "There are no projects in the system, the search function is disabled.");
      $tpl->parseCurrentBlock("search_no_projects");
      $tpl->setCurrentBlock("browse_no_projects");
      $tpl->setVariable("NO_PROJECTS_MESSAGE", "There are no projects in the system, the browse function is disabled.");
      $tpl->parseCurrentBlock("browse_no_projects");
  }
  
  // if user is not logged in, show section to ask him to do so.
  // if user is logged in, show section that allows him to change his details
  if (!IsLogged())
  {
    $tpl->setCurrentBlock("not_logged");
    $tpl->touchBlock("not_logged");
    $tpl->parseCurrentBlock("not_logged");
  }
  else
  {
    $tpl->setCurrentBlock("logged");
    $tpl->touchBlock("logged");
    $tpl->parseCurrentBlock("logged");
  }

  // print the output
  $tpl->show(); 

?>
