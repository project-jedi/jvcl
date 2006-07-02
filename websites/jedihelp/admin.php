<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security_utils.php";
  require_once "page_blocks.php";
  require_once "security.php";
  require_once "not_a_power_user.php";

  if (HasNoSpecificRights())
    die(GetNotAPowerUserPage());
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl); 
  
  $projects = GetProjectsInfos();
  
  $AtLeastOneSection = false;

  // if user has rights to add files, then we show the section
  if (CanUpload())
  {
    $AtLeastOneSection = true;
  	$tpl->setCurrentBlock("add_file_section");
	  if (is_array($projects))
	  {
	    $tpl->setCurrentBlock("add_file_project_list_section");
	    foreach ($projects as $project)
	    {
	      // Assign data to the search option block 
	      $tpl->setCurrentBlock("add_file_project_name_section");
	      $tpl->setVariable("PROJECT_ID", $project["Id"]);
	      $tpl->setVariable("PROJECT_NAME", $project["Name"]);
	      $tpl->parseCurrentBlock("add_file_project_name_section");
	    }
	    $tpl->parseCurrentBlock("add_file_project_list_section");
	  }
	  else
	  {
	    $tpl->setCurrentBlock("add_file_no_projects_section");
	    $tpl->setVariable("NO_PROJECTS_MESSAGE", "There are no projects in the system. You must have at least one project to be able to add files.");
	    $tpl->parseCurrentBlock("add_file_no_projects_section");
	  }
  	$tpl->parseCurrentBlock("add_file_section");
  }
  
  if (CanManageSI())
  {
  	$AtLeastOneSection = true;
  	$tpl->setCurrentBlock("manage_si_section");
  	$tpl->touchBlock("manage_si_section");
  	$tpl->parseCurrentBlock("manage_si_section");
  }
  
  if (CanManageSeeAlso())
  {
  	$AtLeastOneSection = true;
  	$tpl->setCurrentBlock("manage_see_also_section");
  	$tpl->touchBlock("manage_see_also_section");
  	$tpl->parseCurrentBlock("manage_see_also_section");
  }
  
  if (CanManageProjects())
  {
  	$AtLeastOneSection = true;
  	$tpl->setCurrentBlock("manage_projects_section");
  	$tpl->touchBlock("manage_projects_section");
  	$tpl->parseCurrentBlock("manage_projects_section");
  }
  
  if (CanManageUsers())
  {
  	$AtLeastOneSection = true;
  	$tpl->setCurrentBlock("manage_users_section");
  	$tpl->touchBlock("manage_users_section");
  	$tpl->parseCurrentBlock("manage_users_section");
  }
  
  if (!$AtLeastOneSection)
  {
  	$tpl->setCurrentBlock("nothing_allowed_section");
  	$tpl->touchBlock("nothing_allowed_section");
  	$tpl->parseCurrentBlock("nothing_allowed_section");
  }
  
  $tpl->show();
?>