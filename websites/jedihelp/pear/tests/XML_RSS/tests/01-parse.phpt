--TEST--
XML_RSS::getItems
--FILE--
<?php
require "../RSS.php";

$r =& new XML_RSS("test.rss");
$r->parse();

foreach ($r->getItems() as $value) {
    echo $value['title'] . ": " . $value['link'] . "\n";
}

?>
--EXPECT--
PHP homepage: http://php.net/
PEAR homepage: http://pear.php.net/
PHP-GTK homepage: http://gtk.php.net/
PHP QA homepage: http://qa.php.net/
