<?php

  require_once "pear/HTML/Template/IT.php";
  require_once "data_access.php";
  require_once "security.php";
  require_once "utils.php";
  require_once "page_blocks.php";
  require_once "not_an_admin.php";
  
  define("ITEMS_PER_ITERATION", 15);
  define("SEE_ALSO_COOKIE_NAME", "seeAlsoFile");

  if (!IsAdmin())
    die(GetNotAnAdminPage());
  
  $msg = "";
  if (array_key_exists("action", $_POST))
    $action = $_POST["action"];
  else if (array_key_exists("action", $_GET))
    $action = $_GET["action"];
  else
    $action = "";

  $itemsLeft = false;

  switch ($action)
  {
    case "add":
      $result = AddExternalItem($_POST["name"], $_POST["description"]);
      if ($result == "")
        $msg = "Add successful";
      else
        $msg = "Error while adding: ".$result;
      break;
    case "delete":
      if (is_array($_POST["externalItems"]) && $_POST["externalItems"][0] != -1)
      {
        StartAccessToDB();
        foreach ($_POST["externalItems"] as $externalItem)
        {
          $error = DeleteExternalItem($externalItem);
          if ($error != "")
          {
            if ($msg != "")
              $msg .= "<br>";
            $msg .= $error;
          }
        }
        if ($msg == "")
          $msg = "Delete successful";
        else
          $msg = "Error(s) while deleting: ".$msg; 
        EndAccessToDB();
      }
      else
        $msg = "Error: Please select an external item to delete";
      break;
    case "gen":
      $startTimeStr = microtime();  
      StartAccessToDB();
      
      $msg = "";
      
      // Get the items that have undefined SeeAlso sections
      if (array_key_exists(SEE_ALSO_COOKIE_NAME, $_COOKIE))
      {
        $IDs = file($_COOKIE[SEE_ALSO_COOKIE_NAME]);
        unlink($_COOKIE[SEE_ALSO_COOKIE_NAME]);
        //$undefSAItems = GetItemsByIDs($IDs);
        foreach ($IDs as $ID)
          $undefSAItemsIDs[] = trim($ID);
          
        $IDs = ""; // free memory
      }
      else
      {
        $undefSAItemsIDs = GetUndefinedSAItemsIDs();
        if (is_string($undefSAItemsIDs))
        {
          $msg = $undefSAItemsIDs;
          EndAccessToDB();
          break;
        }
      }
        
      // And the list of all items sorted by name
      $itemsByName = GetItemsOrderedByName();
      if (is_string($itemsByName))
      {
        $msg = $itemsByName;
        EndAccessToDB();
        break;
      }
        
      $externalItems = GetExternalItemsOrderedByName();
      if (is_string($externalItems))
      {
        $msg = $externalItems;
        EndAccessToDB();
        break;
      }
      
      $NumItems=0;
      // Now process all the items that have undefined See Alsos
      if (is_array($undefSAItemsIDs))
      {
        while (count($undefSAItemsIDs) > 0 && $NumItems < ITEMS_PER_ITERATION)
        {
          $undefSAItem = GetItemInfos($undefSAItemsIDs[0]);
          
          if (is_string($undefSAItem))
          {
            $msg = $undefSAItem;
            EndAccessToDB();
            break;
          }
          
          $undefs = "";
          $internals = "";
          $externals = "";
          
          $baseclass = trim(substr($undefSAItem["Name"], 0, strpos($undefSAItem["Name"],".")));
  
          $SeeAlsos = explode(",", $undefSAItem["SeeAlsoUndefined"]);
          foreach($SeeAlsos as $SeeAlso)
          {
            // try to find the see also in the external list
            $item = BinarySearchColumnCompare($externalItems, trim($SeeAlso), "Name");
            if (is_array($item))
            {
              $externals .= ",".$item["Id"];
            }
            else
            {
              // try to find the undefined see also in the items list
              $item = BinarySearchColumnCompare($itemsByName, trim($SeeAlso), "Name");
              if (is_array($item))
              {              
                $internals .= ",".$item["Id"];
              }
              else
              {
                // try it again by adding the base class if there is one
                if ($baseclass != "")
                {
                  $item = BinarySearchColumnCompare($itemsByName, $baseclass.".".trim($SeeAlso), "Name");
                  if (is_array($item))
                  {
                    $internals .= ",".$item["Id"];
                  }              
                  else
                    $undefs .= ",".$SeeAlso;
                }
                else
                  $undefs .= ",".$SeeAlso;
              }
            }
          }
          
          // remove extra comma in front:
          $internals = substr($internals, 1);
          $externals = substr($externals, 1);
          $undefs = substr($undefs, 1);
          
          // now update the item in the DB
          $insertResult = UpdateItemSeeAlsos($undefSAItem["Id"], $internals, $externals, $undefs);
          if ($insertResult != "")
          {
            $msg = $insertResult;
            break; // get out of the foreach
          }
          $NumItems++;
          array_shift($undefSAItemsIDs);
        }
      }
      else
        $msg = "No items with unlinked See Also sections";      
      
      EndAccessToDB();
      
      // if items array is not empty, then it is because we reached the limit.
      // as such, we put the list into a file which we set the cookie to
      if (count($undefSAItemsIDs) > 0)
      {
        $filename = tempnam("", "jh_");
        $file = fopen($filename, "w");
        foreach($undefSAItemsIDs as $item)
          fwrite($file, $item."\n");
        fclose($file);
        setcookie(SEE_ALSO_COOKIE_NAME, $filename);
        $itemsLeft = true;
      }
      else
      {
        // no more items, no more cookie
        setcookie (SEE_ALSO_COOKIE_NAME, "", time() - 3600);
      }
      
      $endTimeStr = microtime();
      $duration = MicrotimeDifference($startTimeStr, $endTimeStr);
      if ($msg == "") 
      { 
        $msg = "Generation successful: Processed $NumItems items in ".round($duration,4)." seconds.";
        if ($itemsLeft)
        {
          $timeLeft = ($duration+2) * count($undefSAItemsIDs) / ITEMS_PER_ITERATION;
          $msg .= "<br>".count($undefSAItemsIDs)." items left, please wait for automatic ".
                  "progress or click <a href='admin_seealso.php?action=gen'>here</a> ".
                  "to continue now.<br>".
                  "Estimated time left: ".
                  gmstrftime("%T", $timeLeft)."";
        }
      }
      break;
  }
  
  $tpl = new HTML_Template_IT("./"); 
  $tpl->loadTemplatefile("admin_seealso.tpl.html", true, true);
  
  SetCommonLoginStatus($tpl);
  SetAdminToolbar($tpl);
  SetCommonFooter($tpl);
  
  if ($itemsLeft)
  {
    $tpl->setCurrentBlock("auto_refresher");
    $tpl->touchBlock("auto_refresher");
    $tpl->parseCurrentBlock("auto_refresher");
  }
   
  $tpl->setVariable("MESSAGE", $msg);

  $externalItems = GetExternalItemsInfos();

  if (is_array($externalItems))
  {
    $tpl->setCurrentBlock("external_items_sections");
    foreach ($externalItems as $externalItem)
    {
      // Assign data to the external items deletion list 
      $tpl->setCurrentBlock("delete_external_items");
      $tpl->setVariable("EXTERNAL_ITEM_ID", $externalItem["Id"]);
      $tpl->setVariable("EXTERNAL_ITEM_NAME", $externalItem["Name"]);
      $tpl->setVariable("EXTERNAL_ITEM_DESCRIPTION", EncodeString($externalItem["Description"]));
      $tpl->parseCurrentBlock("delete_external_items");
    }
    $tpl->setCurrentBlock("external_items_sections");
  }
  else
  {
    $tpl->setCurrentBlock("no_external_items");
    $tpl->setVariable("NO_EXTERNAL_ITEMS_MESSAGE", "There are no external items in the system, use the form below to add one.");
    $tpl->parseCurrentBlock("no_external_items");
  }
  
  $tpl->show();
  

?>