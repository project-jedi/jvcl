<?php
/* vim: set expandtab tabstop=4 softtabstop=4 shiftwidth=4: */ 
// +----------------------------------------------------------------------+ 
// | PHP version 4                                                        | 
// +----------------------------------------------------------------------+ 
// | Copyright (c) 1997-2002 The PHP Group                                | 
// +----------------------------------------------------------------------+ 
// | This source file is subject to version 2.0 of the PHP license,       | 
// | that is bundled with this package in the file LICENSE, and is        | 
// | available at through the world-wide-web at                           | 
// | http://www.php.net/license/2_02.txt.                                 | 
// | If you did not receive a copy of the PHP license and are unable to   | 
// | obtain it through the world-wide-web, please send a note to          | 
// | license@php.net so we can mail you a copy immediately.               | 
// +----------------------------------------------------------------------+ 
// | Authors: Paul M. Jones <pjones@ciaweb.net>                           | 
// +----------------------------------------------------------------------+ 
// 
// $Id: Contact_Vcard_Parse.php,v 1.6 2003/04/08 16:01:34 ciaweb Exp $ 

/**
* 
* Parser for vCards.
*
* This class parses vCard 2.1 and 3.0 sources from file or text into a
* structured array.
* 
* Usage:
* 
* <code>
*     // include this class file
*     require_once 'Contact_Vcard_Parse.php';
*     
*     // instantiate a parser object
*     $parse = new Contact_Vcard_Parse();
*     
*     // parse a vCard file and store the data
*     // in $cardinfo
*     $cardinfo = $parse->fromFile('sample.vcf');
*     
*     // view the card info array
*     echo '<pre>';
*     print_r($cardinfo);
*     echo '</pre>';
* </code>
* 
*
* @author Paul M. Jones <pjones@ciaweb.net>
* @package Contact_Vcard_Parse
* @version $Revision: 1.6 $
* 
*/

require_once 'PEAR.php';

class Contact_Vcard_Parse extends PEAR {
    
    /**
    *
    * Constructor.
    *
    * @access public
    * @return void
    *
    */
    
    function Contact_Vcard_Parse()
    {
        $this->PEAR();
    }
    
    
    /**
    * 
    * Reads a file for parsing, then sends it to $this->fromText()
    * and returns the results.
    * 
    * @access public
    * 
    * @param array $filename The filename to read for vCard information.
    * 
    * @return array An array of of vCard information extracted from the
    * file.
    * 
    * @see Contact_Vcard_Parse::_fromText()
    * @see Contact_Vcard_Parse::_fromArray()
    * 
    */
    
    function fromFile($filename, $decode_qp = true)
    {
        // get the file data
        $text = implode('', file($filename));
        
        // dump to, and get return from, the fromText() method.
        return $this->fromText($text, $decode_qp);
    }
    
    
    /**
    * 
    * Prepares a block of text for parsing, then sends it through and
    * returns the results from $this->fromArray().
    * 
    * @access public
    * 
    * @param array $text A block of text to read for vCard information.
    * 
    * @return array An array of vCard information extracted from the
    * source text.
    * 
    * @see Contact_Vcard_Parse::_fromArray()
    * 
    */
    
    function fromText($text, $decode_qp = true)
    {
        // convert all kinds of line endings to Unix-standard and get
        // rid of double blank lines.
        $this->convertLineEndings($text);
        
        // unfold lines.  concat two lines where line 1 ends in \n and
        // line 2 starts with any amount of whitespace.  only removes
        // the first whitespace character, leaves others in place.
        $fold_regex = '(\n)([ |\t])';
        $text = preg_replace("/$fold_regex/i", "", $text);
        
        // convert the resulting text to an array of lines
        $lines = explode("\n", $text);
        
        // parse the array of lines and return vCard info
        return $this->_fromArray($lines, $decode_qp);
    }
    
    
    /**
    * 
    * Converts line endings in text.
    * 
    * Takes any text block and converts all line endings to UNIX
    * standard. DOS line endings are \r\n, Mac are \r, and UNIX is \n.
    * As a side-effect, all double-newlines (\n\n) are converted to a
    * single-newline.
    *
    * NOTE: Acts on the text block in-place; does not return a value.
    * 
    * @access public
    * 
    * @param string $text The string on which to convert line endings.
    * 
    * @return void
    * 
    */
    
    function convertLineEndings(&$text)
    {
        // first, replace \r with \n to fix up from DOS and Mac
        $text = str_replace("\r", "\n", $text);
        
        // now eliminate all instances of double-newlines that result
        // from having converted \r\n to \n\n (from DOS).  note that
        // this removes newlines in general, not only if they resulted
        // from the earlier conversion of \r.
        $text = str_replace("\n\n", "\n", $text);
    }
    
    
    /**
    * 
    * Splits a string into an array at semicolons.  Honors backslash-
    * escaped semicolons (i.e., splits at ';' not '\;').
    * 
    * @access public
    * 
    * @param string $text The string to split into an array.
    * 
    * @param bool $convertSingle If splitting the string results in a
    * single array element, return a string instead of a one-element
    * array.
    * 
    * @return mixed An array of values, or a single string.
    * 
    */
    
    function splitBySemi($text, $convertSingle = false)
    {
        // we use these double-backs (\\) because they get get converted
        // to single-backs (\) by preg_split.  the quad-backs (\\\\) end
        // up as as double-backs (\\), which is what preg_split requires
        // to indicate a single backslash (\). what a mess.
        $regex = '(?<!\\\\)(\;)';
        $tmp = preg_split("/$regex/i", $text);
        
        // if there is only one array-element and $convertSingle is
        // true, then return only the value of that one array element
        // (instead of returning the array).
        if ($convertSingle && count($tmp) == 1) {
            return $tmp[0];
        } else {
            return $tmp;
        }
    }
    
    
    /**
    * 
    * Splits a string into an array at commas.  Honors backslash-
    * escaped commas (i.e., splits at ',' not '\,').
    * 
    * @access public
    * 
    * @param string $text The string to split into an array.
    * 
    * @param bool $convertSingle If splitting the string results in a
    * single array element, return a string instead of a one-element
    * array.
    * 
    * @return mixed An array of values, or a single string.
    * 
    */
    
    function splitByComma($text, $convertSingle = false)
    {
        // we use these double-backs (\\) because they get get converted
        // to single-backs (\) by preg_split.  the quad-backs (\\\\) end
        // up as as double-backs (\\), which is what preg_split requires
        // to indicate a single backslash (\). ye gods, how ugly.
        $regex = '(?<!\\\\)(\,)';
        $tmp = preg_split("/$regex/i", $text);
        
        // if there is only one array-element and $convertSingle is
        // true, then return only the value of that one array element
        // (instead of returning the array).
        if ($convertSingle && count($tmp) == 1) {
            return $tmp[0];
        } else {
            return $tmp;
        }
    }
    
    
    /**
    * 
    * Used to make string human-readable after being a vCard value.
    * 
    * Converts...
    *     \; => ;
    *     \, => ,
    *     literal \n => newline
    * 
    * @access public
    * 
    * @param mixed $text The text to unescape.
    * 
    * @return void
    * 
    */
    
    function unescape(&$text)
    {
        if (is_array($text)) {
            foreach ($text as $key => $val) {
                $this->unescape($val);
                $text[$key] = $val;
            }
        } else {
            $text = str_replace('\;', ';', $text);
            $text = str_replace('\,', ',', $text);
            $text = str_replace('\n', "\n", $text);
        }
    }
    
    
    /**
    *
    * Emulated destructor.
    *
    * @access private
    * @return boolean true
    *
    */
    
    function _Contact_Vcard_Parse()
    {
        return true;
    }
    
    
    /**
    * 
    * Parses an array of source lines and returns an array of vCards.
    * Each element of the array is itself an array expressing the types,
    * parameters, and values of each part of the vCard. Processes both
    * 2.1 and 3.0 vCard sources.
    *
    * @access private
    * 
    * @param array $source An array of lines to be read for vCard
    * information.
    * 
    * @return array An array of of vCard information extracted from the
    * source array.
    * 
    */
    
    function _fromArray($source, $decode_qp = true)
    {
        // the info array will hold all resulting vCard information.
        $info = array();
        
        // tells us whether the source text indicates the beginning of a
        // new vCard with a BEGIN:VCARD tag.
        $begin = false;
        
        // holds information about the current vCard being read from the
        // source text.
        $card = array();
        
        // loop through each line in the source array
        foreach ($source as $line) {
            
            // if the line is blank, skip it.
            if (trim($line) == '') {
                continue;
            }
            
            // find the first instance of ':' on the line.  The part
            // to the left of the colon is the type and parameters;
            // the part to the right of the colon is the value data.
            $pos = strpos($line, ':');
            
            // if there is no colon, skip the line.
            if ($pos === false) {
                continue;
            }
            
            // get the left and right portions
            $left = trim(substr($line, 0, $pos));
            $right = trim(substr($line, $pos+1, strlen($line)));
            
            // have we started yet?
            if (! $begin) {
                
                // nope.  does this line indicate the beginning of
                // a new vCard?
                if (strtoupper($left) == 'BEGIN' &&
                    strtoupper($right) == 'VCARD') {
                    
                    // tell the loop that we've begun a new card
                    $begin = true;
                }
                
                // regardless, loop to the next line of source. if begin
                // is still false, the next loop will check the line. if
                // begin has now been set to true, the loop will start
                // collecting card info.
                continue;
                
            } else {
                
                // yep, we've started, but we don't know how far along
                // we are in the card. is this the ending line of the
                // current vCard?
                if (strtoupper($left) == 'END' &&
                    strtoupper($right) == 'VCARD') {
                    
                    // yep, we're done. keep the info from the current
                    // card...
                    $info[] = $card;
                    
                    // ...and reset to grab a new card if one exists in
                    // the source array.
                    $begin = false;
                    $card = array();
                    
                } else {
                    
                    // we're not on an ending line, so collect info from
                    // this line into the current card. split the
                    // left-portion of the line into a type-definition
                    // (the kind of information) and parameters for the
                    // type.
                    $typedef = $this->_getTypeDef($left);
                    $params = $this->_getParams($left);
                    
                    // if we are decoding quoted-printable, do so now.
                    // QUOTED-PRINTABLE is not allowed in version 3.0,
                    // but we don't check for versioning, so we do it
                    // regardless.  ;-)
                    $this->_decode_qp($params, $right);
                    
                    // now get the value-data from the line, based on
                    // the typedef
                    switch ($typedef) {
                        
                    case 'N':
                        // structured name of the person
                        $value = $this->_parseN($right);
                        break;
                        
                    case 'ADR':
                        // structured address of the person
                        $value = $this->_parseADR($right);
                        break;
                        
                    case 'NICKNAME':
                        // nicknames
                        $value = $this->_parseNICKNAME($right);
                        break;
                        
                    case 'ORG':
                        // organizations the person belongs to
                        $value = $this->_parseORG($right);
                        break;
                        
                    case 'CATEGORIES':
                        // categories to which this card is assigned
                        $value = $this->_parseCATEGORIES($right);
                        break;
                        
                    case 'GEO':
                        // geographic coordinates
                        $value = $this->_parseGEO($right);
                        break;
                        
                    default:
                        // by default, just grab the plain value. keep
                        // as an array to make sure *all* values are
                        // arrays.  for consistency. ;-)
                        $value = array(array($right));
                        break;
                    }
                    
                    // add the type, parameters, and value to the
                    // current card array.  note that we allow multiple
                    // instances of the same type, which might be dumb
                    // in some cases (e.g., N).
                    $card[$typedef][] = array(
                        'param' => $params,
                        'value' => $value
                    );
                }
            }
        }
        
        $this->unescape($info);
        return $info;
    }
    
    
    /**
    *
    * Takes a vCard line and extracts the Type-Definition for the line.
    * 
    * @access private
    *
    * @param string $text A left-part (before-the-colon part) from a
    * vCard line.
    * 
    * @return string The type definition for the line.
    * 
    */
    
    function _getTypeDef($text)
    {
        // split the text by semicolons
        $split = $this->splitBySemi($text);
        
        // only return first element (the typedef)
        return $split[0];
    }
    
    
    /**
    *
    * Finds the Type-Definition parameters for a vCard line.
    * 
    * @access private
    * 
    * @param string $text A left-part (before-the-colon part) from a
    * vCard line.
    * 
    * @return mixed An array of parameters.
    * 
    */
    
    function _getParams($text)
    {
        // split the text by semicolons into an array
        $split = $this->splitBySemi($text);
        
        // drop the first element of the array (the type-definition)
        array_shift($split);
        
        // set up an array to retain the parameters, if any
        $params = array();
        
        // loop through each parameter.  the params may be in the format...
        // "TYPE=type1,type2,type3"
        //    ...or...
        // "TYPE=type1;TYPE=type2;TYPE=type3"
        foreach ($split as $full) {
            
            // split the full parameter at the equal sign so we can tell
            // the parameter name from the parameter value
            $tmp = explode("=", $full);
            
            // the key is the left portion of the parameter (before
            // '='). if in 2.1 format, the key may in fact be the
            // parameter value, not the parameter name.
            $key = strtoupper(trim($tmp[0]));
            
            // get the parameter name by checking to see if it's in
            // vCard 2.1 or 3.0 format.
            $name = $this->_getParamName($key);
            
            // list of all parameter values
            $listall = trim($tmp[1]);
            
            // if there is a value-list for this parameter, they are
            // separated by commas, so split them out too.
            $list = $this->splitByComma($listall);
            
            // now loop through each value in the parameter and retain
            // it.  if the value is blank, that means it's a 2.1-style
            // param, and the key itself is the value.
            foreach ($list as $val) {
                if (trim($val) != '') {
                    // 3.0 formatted parameter
                    $params[$name][] = trim($val);
                } else {
                    // 2.1 formatted parameter
                    $params[$name][] = $key;
                }
            }
            
            // if, after all this, there are no parameter values for the
            // parameter name, retain no info about the parameter (saves
            // ram and checking-time later).
            if (count($params[$name]) == 0) {
                unset($params[$name]);
            }
        }
        
        // return the parameters array.
        return $params;
    }
    
    
    /**
    *
    * Looks at the parameters of a vCard line; if one of them is
    * ENCODING[] => QUOTED-PRINTABLE then decode the text in-place.
    * 
    * @access private
    * 
    * @param array $params A parameter array from a vCard line.
    * 
    * @param string $text A right-part (after-the-colon part) from a
    * vCard line.
    *
    * @return void
    * 
    */
    
    function _decode_qp(&$params, &$text)
    {
        // loop through each parameter
        foreach ($params as $param_key => $param_val) {
            
            // check to see if it's an encoding param
            if (trim(strtoupper($param_key)) == 'ENCODING') {
            
                // loop through each encoding param value
                foreach ($param_val as $enc_key => $enc_val) {
                
                    // if any of the values are QP, decode the text
                    // in-place and return
                    if (trim(strtoupper($enc_val)) == 'QUOTED-PRINTABLE') {
                        $text = quoted_printable_decode($text);
                        return;
                    }
                }
            }
        }
    }
    
    
    /**
    * 
    * Returns parameter names from 2.1-formatted vCards.
    *
    * The vCard 2.1 specification allows parameter values without a
    * name. The parameter name is then determined from the unique
    * parameter value.
    * 
    * Shamelessly lifted from Frank Hellwig <frank@hellwig.org> and his
    * vCard PHP project <http://vcardphp.sourceforge.net>.
    * 
    * @access private
    * 
    * @param string $value The first element in a parameter name-value
    * pair.
    * 
    * @return string The proper parameter name (TYPE, ENCODING, or
    * VALUE).
    * 
    */
     
    function _getParamName($value)
    {
        static $types = array (
            'DOM', 'INTL', 'POSTAL', 'PARCEL','HOME', 'WORK',
            'PREF', 'VOICE', 'FAX', 'MSG', 'CELL', 'PAGER',
            'BBS', 'MODEM', 'CAR', 'ISDN', 'VIDEO',
            'AOL', 'APPLELINK', 'ATTMAIL', 'CIS', 'EWORLD',
            'INTERNET', 'IBMMAIL', 'MCIMAIL',
            'POWERSHARE', 'PRODIGY', 'TLX', 'X400',
            'GIF', 'CGM', 'WMF', 'BMP', 'MET', 'PMB', 'DIB',
            'PICT', 'TIFF', 'PDF', 'PS', 'JPEG', 'QTIME',
            'MPEG', 'MPEG2', 'AVI',
            'WAVE', 'AIFF', 'PCM',
            'X509', 'PGP'
        );
        
        // CONTENT-ID added by pmj
        static $values = array (
            'INLINE', 'URL', 'CID', 'CONTENT-ID'
        );
        
        // 8BIT added by pmj
        static $encodings = array (
            '7BIT', '8BIT', 'QUOTED-PRINTABLE', 'BASE64'
        );
        
        // changed by pmj to the following so that the name defaults to
        // whatever the original value was.  Frank Hellwig's original
        // code was "$name = 'UNKNOWN'".
        $name = $value;
        
        if (in_array($value, $types)) {
            $name = 'TYPE';
        } elseif (in_array($value, $values)) {
            $name = 'VALUE';
        } elseif (in_array($value, $encodings)) {
            $name = 'ENCODING';
        }
        
        return $name;
    }
    
    
    /**
    *
    * Parses a vCard line value identified as being of the "N"
    * (structured name) type-defintion.
    *
    * @access private
    *
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return array An array of key-value pairs where the key is the
    * portion-name and the value is the portion-value.  The value itself
    * may be an array as well if multiple comma-separated values were
    * indicated in the vCard source.
    *
    */
    
    function _parseN($text)
    {
        $tmp = $this->splitBySemi($text);
        return array(
            $this->splitByComma($tmp[0]), // family (last)
            $this->splitByComma($tmp[1]), // given (first)
            $this->splitByComma($tmp[2]), // addl (middle)
            $this->splitByComma($tmp[3]), // prefix
            $this->splitByComma($tmp[4])  // suffix
        );
    }
    
    
    /**
    *
    * Parses a vCard line value identified as being of the "ADR"
    * (structured address) type-defintion.
    *
    * @access private
    *
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return array An array of key-value pairs where the key is the
    * portion-name and the value is the portion-value.  The value itself
    * may be an array as well if multiple comma-separated values were
    * indicated in the vCard source.
    *
    */
    
    function _parseADR($text)
    {
        $tmp = $this->splitBySemi($text);
        return array(
            $this->splitByComma($tmp[0]), // pob
            $this->splitByComma($tmp[1]), // extend
            $this->splitByComma($tmp[2]), // street
            $this->splitByComma($tmp[3]), // locality (city)
            $this->splitByComma($tmp[4]), // region (state)
            $this->splitByComma($tmp[5]), // postcode (ZIP)
            $this->splitByComma($tmp[6])  // country
        );
    }
    
    
    /**
    * 
    * Parses a vCard line value identified as being of the "NICKNAME"
    * (informal or descriptive name) type-defintion.
    *
    * @access private
    * 
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return array An array of nicknames.
    *
    */
    
    function _parseNICKNAME($text)
    {
        return array($this->splitByComma($text));
    }
    
    
    /**
    * 
    * Parses a vCard line value identified as being of the "ORG"
    * (organizational info) type-defintion.
    *
    * @access private
    *
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return array An array of organizations; each element of the array
    * is itself an array, which indicates primary organization and
    * sub-organizations.
    *
    */
    
    function _parseORG($text)
    {
        $tmp = $this->splitbySemi($text);
        $list = array();
        foreach ($tmp as $val) {
            $list[] = array($val);
        }
        
        return $list;
    }
    
    
    /**
    * 
    * Parses a vCard line value identified as being of the "CATEGORIES"
    * (card-category) type-defintion.
    *
    * @access private
    * 
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return mixed An array of categories.
    *
    */
    
    function _parseCATEGORIES($text)
    {
        return array($this->splitByComma($text));
    }
    
    
    /**
    * 
    * Parses a vCard line value identified as being of the "GEO"
    * (geographic coordinate) type-defintion.
    *
    * @access private
    *
    * @param string $text The right-part (after-the-colon part) of a
    * vCard line.
    * 
    * @return mixed An array of lat-lon geocoords.
    *
    */
    
    function _parseGEO($text)
    {
        $tmp = $this->splitBySemi($text);
        return array(
            array($tmp[0]), // lat
            array($tmp[1])  // lon
        );
    }
}

?>