<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "page_blocks.php";

  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("advanced_search.tpl.html", false, true);
  
  SetCommonLoginStatus($tpl);
  SetCommonToolbar($tpl);
  SetCommonFooter($tpl); 
  
  $projects = GetProjectsInfos();
  
  $projectsCount = count($projects)+1;
  if ($projectsCount > 6)
    $projectsCount = 6;

  $tpl->setVariable("PROJECT_COUNT", $projectsCount);
  
  $selFields = array(
    array("", "All"),
    array("Description", "Description"),
    array("Summary", "Summary"),
    array("Name", "Name"),
    array("ReturnValue", "Return Value"),
    array("Parameters", "Parameters"),
    array("Author", "Author"),
    array("Extras", "Extras")
    );
  
  foreach ($selFields as $selField)
  {
    // Assign data to the field list block 
    $tpl->setCurrentBlock("field_list");
    $tpl->setVariable("FIELD_NAME", $selField[0]);
    $tpl->setVariable("FIELD_DISPLAY_NAME", $selField[1]);
    $tpl->parseCurrentBlock("field_list");
  }

  foreach ($projects as $project)
  {
    // Assign data to the project list block 
    $tpl->setCurrentBlock("project_list");
    $tpl->setVariable("PROJECT_NAME", $project["Name"]);
    if ($project == $projects[0])
      $tpl->setVariable("PROJECT_ID", $project["Id"].'" selected class="');
    else
    $tpl->setVariable("PROJECT_ID", $project["Id"]);
      
    $tpl->parseCurrentBlock("project_list");
  }

  // print the output
  $tpl->show(); 
?>