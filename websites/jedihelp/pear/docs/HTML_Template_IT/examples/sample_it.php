<?php
require_once "HTML/Template/IT.php";

$data = array (
    "0" => array("Stig", "Bakken"),
    "1" => array("Martin", "Jansen"),
    "2" => array("Alexander", "Merz")
);

$tpl = new HTML_Template_IT("./templates");

$tpl->loadTemplatefile("main.tpl.htm", true, true);

foreach($data as $name) {
    foreach($name as $cell) {
        // Assign data to the inner block
        $tpl->setCurrentBlock("cell") ;
        $tpl->setVariable("DATA", $cell) ;
        $tpl->parseCurrentBlock("cell") ;
    }
    // Assign data and the inner block to the
    // outer block
    $tpl->setCurrentBlock("row") ;
    $tpl->parseCurrentBlock("row") ;
}
// print the output
$tpl->show();
?>
