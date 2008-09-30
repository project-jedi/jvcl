<!--
****************************************************************************
   JVCL daily zips index page

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License 
for the specific language governing rights and limitations under the License.

The Original Code is: index.php, released on 2005-05-26.

The Initial Developer of the Original Code is Olivier Sannier
Portions created by Peter Thörnqvist are Copyright (C) 2005 Olivier Sannier.
All Rights Reserved.

Contributor(s): none

You may retrieve the latest version of this file at the Project JEDI's JVCL 
home page, located at http://jvcl.sourceforge.net

Description:
  This is a PHP index page that looks for JVCL's daily snapshots and presents
  them in a table. It is to be placed where the daily cron job creates the
  daily zips, usually in the daily subdirectory of the htdocs directory for
  the jvcl group.

****************************************************************************
-->
<!-- $Id$ -->
<?php 

// Never forget the trailing slash
$mirror_url_root = "http://obones.free.fr/jvcl_daily/"; 

function GetDisplayFileDate($filename)
{
  if (file_exists($filename))
    return date("Y-m-d H:i:s T", filemtime($filename));
  else
    return " - ";
}

function GetShortFileDate($filename)
{
  if (file_exists($filename))
    return date("Y-m-d", filemtime($filename));
  else
    return " - ";
}

function GetDisplayFileSize($filename)
{
  if (!file_exists($filename))
    return " - ";
  $size = filesize($filename);
  
  if ($size > 1024*1024)
    return round($size / (1024*1024), 2)." M";
  else if ($size > 1024)
    return round($size / 1024, 2)." k";
  else
    return $size;
}

function GetLatestFileName($name_start)
{
  $dir = opendir(".");
  $result = "";
  $max_time = 0;
  while (($file = readdir($dir)) !== false) 
  {
      if (substr($file, 0, $name_start) == $name_start)
      {
          $file_time = filemtime($file);
          if ($file_time > $max_time)
          {
              $max_time = $file_time;
              $result = $file;
          }
      }      
  }
  closedir($dir);
  
  return $result; 
}

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1"
 http-equiv="content-type">
  <title>Daily packages</title>
  <link rel="STYLESHEET" type="text/css" href="styles/default.css">
</head>
<body>
<h1>JVCL Daily packages
</h1>
<hr style="width: 100%; height: 2px;">
<br>
Welcome to the daily packages website for the JVCL.<br>
On this page, you will find archive files containing a snapshot of the
JVCL done automatically every night.<br>
They contain a copy of the development repository and as such contain
all the latest bug fixes and improvements provided by the JVCL
developers.<br>
As a result, from time to time, the content of those files may not
compile. We will do everything we can to prevent that but we can't
avoid all errors. As an additional information, you may want to have a
look at the <a href="Build%20status.html">Build Status</a> page. <br>
<br>
<font color="red">Note: If you use one of the archives below, you SHOULD use a daily version of
the JCL as well. You can get it here: <a href="http://jcl.sf.net/daily/">http://jcl.sf.net/daily/</a></font><br>
<br>
The latest version is available for download below<br>
<br>
<table
 style="width: 75%; text-align: left; margin-left: auto; margin-right: auto;"
 cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; font-weight: bold;">File<br>
      </td>
      <td style="vertical-align: top; font-weight: bold;">Date and time<br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Size</span><br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Description</span><br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; white-space: nowrap;">Latest full: <a href="<?php echo GetLatestFileName("JVCL3-2"); ?>">7z</a>
      (Mirror: <a href="<?php print $mirror_url_root.GetLatestFileName("JVCL3-2"); ?>" >7z</a>)</td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileDate(GetLatestFileName("JVCL3-2")); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileSize(GetLatestFileName("JVCL3-2"));?> 
      </td>
      <td style="vertical-align: top;">The complete set of files,
including examples and installer.<br>
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; white-space: nowrap;">Latest sources: <a href="<?php echo GetLatestFileName("JVCL3-Source"); ?>">7z</a>
      (Mirror: <a href="<?php print $mirror_url_root.GetLatestFileName("JVCL3-Source"); ?>" >7z</a>)</td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileDate(GetLatestFileName("JVCL3-Source")); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;"><?php print GetDisplayFileSize(GetLatestFileName("JVCL3-Source"));?> 
      </td>
      <td style="vertical-align: top;">Only the source files, no
examples and no installer<br>
      </td>
    </tr>
  </tbody>
</table>
<br>
<br>
or you can also grab one of the previous complete or source packages.<br>
<br>
<table
 style="width: 75%; text-align: left; margin-left: auto; margin-right: auto;"
 cellspacing="2" cellpadding="2">
  <tbody>
    <tr>
      <td style="vertical-align: top; font-weight: bold;">File<br>
      </td>
      <td style="vertical-align: top; font-weight: bold;">Date and time<br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Size</span><br>
      </td>
      <td style="vertical-align: top;"><span style="font-weight: bold;">Description</span><br>
      </td>
    </tr>
      <?php
      
      $dh = opendir("./");
      $filenames = array();
      while (($filename = readdir($dh)) !== false)
      {
        if (!is_dir($filename) && 
            (substr($filename, 0, 14) == "JVCL3-Source-2") &&
            (substr($filename, -2) == "7z"))
        {
          $filenames[] = $filename;
        }
      }
        
      rsort($filenames);
      
      foreach($filenames as $filename)
      {
        $filename_full = str_replace("JVCL3-Source-2", "JVCL3-2", $filename);
        $file_date = substr($filename_full, 6, 10);
        
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.$file_date.' full: <a href="'.$filename_full.'">7z</a>'."\n";
        echo '  (Mirror: <a href="'.$mirror_url_root.$filename_full.'">7z</a>)'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename_full)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename_full)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top;">The complete set of files, including examples and installer.<br>'."\n";
        echo '  </td>'."\n";
        echo '</tr>'."\n";
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.$file_date.' sources: <a href="'.$filename.'">7z</a>'."\n";
        echo '  (Mirror: <a href="'.$mirror_url_root.$filename.'">7z</a>)'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top;">Only the source files, no examples and no installer<br>'."\n";
        echo '  </td>'."\n";
        echo '</tr>'."\n";
      }
      ?>
  </tbody>
</table>
<br>
The dates are presented according to the ISO standard (YYYY-MM-DD) and the hours are those of the web server (US Pacific time).<br>
Notes:<br>
<ul>
  <li>The generation is started at 01:30 Us Pacific Time.</li> 
  <li>The mirror generation is started at 10:30 Paris Time.</li> 
  <li>The mirror is located in France.</li>
  <li>It takes up to two hours for the <a href="http://www.7-zip.org/">7zip</a> files to be generated.</li> 
  <li>The "Latest" link is created once every file is available.</li>
</ul> 
<br>
Should you have any problems with those files, please do not hesitate
to contact us on our newsgroup here:<br>
<br>
<a href="news://forums.talkto.net/jedi.vcl">news://forums.talkto.net/jedi.vcl</a><br>
<br>
Thank you for considering the JVCL.<br>
<br>
</body>
</html>
