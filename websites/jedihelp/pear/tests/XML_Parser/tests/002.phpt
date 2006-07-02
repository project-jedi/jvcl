--TEST--
XML Parser: parse from file
--SKIPIF--
<?php if (!extension_loaded("xml")) echo 'skip'; ?>
--FILE--
<?php // -*- C++ -*-
//
// Test for: XML/Parser.php
// Parts tested: - parser creation
//               - some handlers
//               - parse from file
//
chdir (dirname(__FILE__));

require_once "XML/Parser.php";

class __TestParser2 extends XML_Parser {
    function __TestParser2() {
        $this->XML_Parser();
    }
    function startHandler($xp, $element, $attribs) {
        print "<$element";
        reset($attribs);
        while (list($key, $val) = each($attribs)) {
            $enc = htmlentities($val);
            print " $key=\"$enc\"";
        }
        print ">";
    }
    function endHandler($xp, $element) {
        print "</$element>\n";
    }
    function cdataHandler($xp, $cdata) {
        print "<![CDATA[$cdata]]>";
    }
    function defaultHandler($xp, $cdata) {

    }
}
print "new __TestParser2 ";
var_dump(get_class($o = new __TestParser2()));
print "setInputFile ";
var_dump($o->setInputFile("test2.xml"));
print "parse ";
var_dump($o->parse());

?>
--EXPECT--
new __TestParser2 string(13) "__testparser2"
setInputFile resource(4) of type (stream)
parse <ROOT><![CDATA[foo]]></ROOT>
bool(true)
