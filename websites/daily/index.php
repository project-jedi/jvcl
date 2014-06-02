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
Portions created by Olivier Sannier are Copyright (C) 2005 Olivier Sannier.
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

function GetLatestFileName($name_exp)
{
  $dir = opendir(".");
  $result = "";
  $max_time = 0;
  while (($file = readdir($dir)) !== false) 
  {
      if (preg_match($name_exp, $file) == 1)
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

$LatestFullFileName7z = GetLatestFileName("/JVCL3-2.+\.7z/");
$LatestFullFileNameZip = GetLatestFileName("/JVCL3-2.+\.zip/");
$LatestSourceFileName7z = GetLatestFileName("/JVCL3-Source.+\.7z/");
$LatestSourceFileNameZip = GetLatestFileName("/JVCL3-Source.+\.zip/");

?>
<!DOCTYPE html PUBLIC "-//W3C//DTD HTML 4.01 Transitional//EN">
<html>
<head>
  <meta content="text/html; charset=ISO-8859-1"
 http-equiv="content-type">
  <title>Daily packages</title>
  <link rel="STYLESHEET" type="text/css" href="styles/default.css">
  <link rel="shortcut icon" href="styles/JEDI.ico" />
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
look at the <a href="http://sourceforge.net/p/projectjedi/mailman/projectjedi-automatic-builds/">Build machine</a> archive. <br>
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
      <td style="vertical-align: top; white-space: nowrap;">
        Latest full: 
        <a href="<?php echo $LatestFullFileNameZip; ?>">zip</a>,
        <a href="<?php echo $LatestFullFileName7z; ?>">7z</a>
        (Mirror: 
         <a href="<?php print $mirror_url_root.$LatestFullFileNameZip; ?>" >zip</a>,
         <a href="<?php print $mirror_url_root.$LatestFullFileName7z; ?>" >7z</a>
        )
      </td>
      <td style="vertical-align: top; white-space: nowrap;">
        <?php print GetDisplayFileDate($LatestFullFileName7z); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;">
        <?php print GetDisplayFileSize($LatestFullFileName7z);?> 
      </td>
      <td style="vertical-align: top;">
        The complete set of files, including examples and installer.
      </td>
    </tr>
    <tr>
      <td style="vertical-align: top; white-space: nowrap;">
        Latest sources: 
        <a href="<?php echo $LatestSourceFileNameZip; ?>">zip</a>,
        <a href="<?php echo $LatestSourceFileName7z; ?>">7z</a>
        (Mirror: 
         <a href="<?php print $mirror_url_root.$LatestSourceFileNameZip; ?>" >zip</a>,
         <a href="<?php print $mirror_url_root.$LatestSourceFileName7z; ?>" >7z</a>
        )
      </td>
      <td style="vertical-align: top; white-space: nowrap;">
        <?php print GetDisplayFileDate($LatestSourceFileName7z); ?> 
      </td>
      <td style="vertical-align: top; white-space: nowrap;">
        <?php print GetDisplayFileSize($LatestSourceFileName7z);?> 
      </td>
      <td style="vertical-align: top;">Only the source files, no examples and no installer<br>
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
      
      foreach($filenames as $filename7z)
      {
        $filename7z_full = str_replace("JVCL3-Source-2", "JVCL3-2", $filename7z);
        $file_date = substr($filename7z_full, 6, 10);

        $filenamezip = str_replace(".7z", ".zip", $filename7z);
        $filenamezip_full = str_replace(".7z", ".zip", $filename7z_full);
        
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'."\n";
        echo '    '.$file_date.' full: '."\n";
        echo '    <a href="'.$filenamezip_full.'">zip</a>,'."\n";
        echo '    <a href="'.$filename7z_full.'">7z</a>'."\n";
        echo '    (Mirror:'."\n";
        echo '     <a href="'.$mirror_url_root.$filenamezip_full.'">zip</a>,'."\n";
        echo '     <a href="'.$mirror_url_root.$filename7z_full.'">7z</a>'."\n";
        echo '    )'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename7z_full)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename7z_full)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top;">The complete set of files, including examples and installer.<br>'."\n";
        echo '  </td>'."\n";
        echo '</tr>'."\n";
        echo '<tr>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'."\n";
        echo '    '.$file_date.' sources:'."\n";
        echo '    <a href="'.$filenamezip.'">zip</a>,'."\n";
        echo '    <a href="'.$filename7z.'">7z</a>'."\n";
        echo '    (Mirror:'."\n";
        echo '     <a href="'.$mirror_url_root.$filenamezip.'">zip</a>,'."\n";
        echo '     <a href="'.$mirror_url_root.$filename7z.'">7z</a>'."\n";
        echo '    )'."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileDate($filename7z)."\n";
        echo '  </td>'."\n";
        echo '  <td style="vertical-align: top; white-space: nowrap;">'.GetDisplayFileSize($filename7z)."\n";
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
  <li>The generation is started at 10:30 Paris Time.</li> 
  <li>The mirror is located in France.</li>
  <li>It takes up to two hours for the process to complete</li> 
</ul> 
<br>
Should you have any problems with those files, please do not hesitate
to contact us on our newsgroup here:<br>
<br>
<a href="news://news.delphi-jedi.org/jedi.vcl">news://news.delphi-jedi.org/jedi.vcl</a><br>
<br>
Thank you for considering the JVCL.<br>
<br>
</body>
<a href="http://sourceforge.net/projects/jvcl"><img src="http://sflogo.sourceforge.net/sflogo.php?group_id=45786&amp;type=15" width="150" height="40" alt="Get JEDI Visual Component Library at SourceForge.net. Fast, secure and Free Open Source software downloads" /></a></body>
</html>
