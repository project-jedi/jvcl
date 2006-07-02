<?php
/* vim: set expandtab tabstop=4 shiftwidth=4: */
// +----------------------------------------------------------------------+
// | PHP Version 4                                                        |
// +----------------------------------------------------------------------+
// | Copyright (c) 1997, 1998, 1999, 2000, 2001 The PHP Group             |
// +----------------------------------------------------------------------+
// | This source file is subject to version 2.0 of the PHP license,       |
// | that is bundled with this package in the file LICENSE, and is        |
// | available at through the world-wide-web at                           |
// | http://www.php.net/license/2_02.txt.                                 |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
// | Authors: Adam Daniel <adaniel1@eesus.jnj.com>                        |
// |          Bertrand Mansion <bmansion@mamasam.com>                     |
// +----------------------------------------------------------------------+
//
// $Id: Table.php,v 1.12 2003/05/20 10:48:03 mansion Exp $

require_once "PEAR.php";
require_once "HTML/Common.php";

/**
 * Builds an HTML table
 * @author        Adam Daniel <adaniel1@eesus.jnj.com>
 * @author        Bertrand Mansion <bmansion@mamasam.com>
 * @version       1.7
 * @since         PHP 4.0.3pl1
 */
class HTML_Table extends HTML_Common {

    /**
     * Automatically adds a new row or column if a given row or column index does not exist
     * @var    bool
     * @access private
     */
    var $_autoGrow = true;

    /**
     * Value to insert into empty cells
     * @var    string
     * @access private
     */
    var $_autoFill = "&nbsp;";

    /**
     * Array containing the table structure
     * @var     array
     * @access  private
     */
    var $_structure = array();

    /**
     * Number of rows composing in the table
     * @var     int
     * @access  private
     */
    var $_rows = 0;

    /**
     * Number of column composing the table
     * @var     int
     * @access  private
     */
    var $_cols = 0;

    /**
     * Tracks the level of nested tables
     * @var    int
     * @access private
     */
    var $_nestLevel = 0;

    /**
     * Class constructor
     * @param    array    $attributes        Associative array of table tag attributes
     * @param    int      $tabOffset
     * @access   public
     */
    function HTML_Table($attributes = null, $tabOffset = 0)
    {
        $commonVersion = 1.7;
        if (HTML_Common::apiVersion() < $commonVersion) {
            return PEAR::raiseError("HTML_Table version " . $this->apiVersion() . " requires " .
                "HTML_Common version $commonVersion or greater.", 0, PEAR_ERROR_TRIGGER);
        }
        HTML_Common::HTML_Common($attributes, $tabOffset);
    } // end constructor

    /**
     * Returns the API version
     * @access  public
     * @return  double
     */
    function apiVersion()
    {
        return 1.7;
    } // end func apiVersion

    /**
     * Sets the table caption
     * @param   string    $caption
     * @param   mixed     $attributes        Associative array or string of table row attributes
     * @access  public
     */
    function setCaption($caption, $attributes = null)
    {
        $attributes = $this->_parseAttributes($attributes);
        $this->_structure["caption"] = array("attr" => $attributes, "contents" => $caption);
    } // end func setCaption

    /**
     * Sets the autoFill value
     * @param   mixed   $fill
     * @access  public
     */
    function setAutoFill($fill)
    {
        $this->_autoFill = $fill;
    } // end func setAutoFill

    /**
     * Returns the autoFill value
     * @access   public
     * @return   mixed
     */
    function getAutoFill()
    {
        return $this->_autoFill;
    } // end func getAutoFill

    /**
     * Sets the autoGrow value
     * @param    bool   $fill
     * @access   public
     */
    function setAutoGrow($grow)
    {
        $this->_autoGrow = $grow;
    } // end func setAutoGrow

    /**
     * Returns the autoGrow value
     * @access   public
     * @return   mixed
     */
    function getAutoGrow()
    {
        return $this->_autoGrow;
    } // end func getAutoGrow

    /**
     * Sets the number of rows in the table
     * @param    int     $rows
     * @access   public
     */
    function setRowCount($rows)
    {
        $this->_rows = $rows;
    } // end func setRowCount

    /**
     * Sets the number of columns in the table
     * @param    int     $cols
     * @access   public
     */
    function setColCount($cols)
    {
        $this->_cols = $cols;
    } // end func setColCount

    /**
     * Returns the number of rows in the table
     * @access   public
     * @return   int
     */
    function getRowCount()
    {
        return $this->_rows;
    } // end func getRowCount

    /**
     * Sets the number of columns in the table
     * @access   public
     * @return   int
     */
    function getColCount()
    {
        return $this->_cols;
    } // end func getColCount

    /**
     * Sets a rows type 'TH' or 'TD'
     * @param    int         $row    Row index
     * @param    string      $type   'TH' or 'TD'
     * @access   public
     */

    function setRowType($row, $type)
    {
        for ($counter = 0; $counter < $this->_cols; $counter++) {
            $this->_structure[$row][$counter]["type"] = $type;
        }
    } // end func setRowType

    /**
     * Sets a columns type 'TH' or 'TD'
     * @param    int         $col    Column index
     * @param    string      $type   'TH' or 'TD'
     * @access   public
     */
    function setColType($col, $type)
    {
        for ($counter = 0; $counter < $this->_rows; $counter++) {
            $this->_structure[$counter][$col]["type"] = $type;
        }
    } // end func setColType

    /**
     * Sets the cell attributes for an existing cell.
     *
     * If the given indices do not exist and autoGrow is true then the given
     * row and/or col is automatically added.  If autoGrow is false then an
     * error is returned.
     * @param    int        $row         Row index
     * @param    int        $col         Column index
     * @param    mixed      $attributes  Associative array or string of table row attributes
     * @access   public
     * @throws   PEAR_Error
     */
    function setCellAttributes($row, $col, $attributes)
    {
        if (isset($this->_structure[$row][$col]) && $this->_structure[$row][$col] == "__SPANNED__") return;
        $attributes = $this->_parseAttributes($attributes);
        $err = $this->_adjustEnds($row, $col, 'setCellAttributes', $attributes);
        if (PEAR::isError($err)) {
            return $err;
        }
        $this->_structure[$row][$col]["attr"] = $attributes;
        $this->_updateSpanGrid($row, $col);
    } // end func setCellAttributes

    /**
     * Updates the cell attributes passed but leaves other existing attributes in tact
     * @param    int     $row         Row index
     * @param    int     $col         Column index
     * @param    mixed   $attributes  Associative array or string of table row attributes
     * @access   public
     */
    function updateCellAttributes($row, $col, $attributes)
    {
        if (isset($this->_structure[$row][$col]) && $this->_structure[$row][$col] == "__SPANNED__") return;
        $attributes = $this->_parseAttributes($attributes);
        $err = $this->_adjustEnds($row, $col, 'updateCellAttributes', $attributes);
        if (PEAR::isError($err)) {
            return $err;
        }
        $this->_updateAttrArray($this->_structure[$row][$col]["attr"], $attributes);
        $this->_updateSpanGrid($row, $col);
    } // end func updateCellAttributes

    /**
     * Returns the attributes for a given cell
     * @param    int     $row         Row index
     * @param    int     $col         Column index
     * @return   array
     * @access   public
     */
    function getCellAttributes($row, $col)
    {
        if (isset($this->_structure[$row][$col]) && $this->_structure[$row][$col] != '__SPANNED__') {
            return $this->_structure[$row][$col]['attr'];
        } elseif (!isset($this->_structure[$row][$col])) {
            return PEAR::raiseError('Invalid table cell reference[' .
                $row . '][' . $col . '] in HTML_Table::getCellAttributes');
        }
        return;
    } // end func getCellAttributes

    /**
     * Sets the cell contents for an existing cell
     *
     * If the given indices do not exist and autoGrow is true then the given
     * row and/or col is automatically added.  If autoGrow is false then an
     * error is returned.
     * @param    int      $row        Row index
     * @param    int      $col        Column index
     * @param    mixed    $contents   May contain html or any object with a toHTML method
     * @param    string   $type       (optional) Cell type either 'TH' or 'TD'
     * @access   public
     * @throws   PEAR_Error
     */
    function setCellContents($row, $col, $contents, $type = 'TD')
    {
        if(isset($this->_structure[$row][$col]) && $this->_structure[$row][$col] == "__SPANNED__") return;
        $err = $this->_adjustEnds($row, $col, 'setCellContents');
        if (PEAR::isError($err)) {
            return $err;
        }
        $this->_structure[$row][$col]['contents'] = $contents;
        $this->_structure[$row][$col]['type'] = $type;
    } // end func setCellContents

    /**
     * Returns the cell contents for an existing cell
     * @param    int        $row    Row index
     * @param    int        $col    Column index
     * @access   public
     * @return   mixed
     */
    function getCellContents($row, $col)
    {
        if (isset($this->_structure[$row][$col]) && $this->_structure[$row][$col] == "__SPANNED__") return;
        return $this->_structure[$row][$col]["contents"];
    } // end func getCellContents

    /**
     * Sets the contents of a header cell
     * @param    int     $row
     * @param    int     $col
     * @param    mixed   $contents
     * @access   public
     */
    function setHeaderContents($row, $col, $contents)
    {
        $this->setCellContents($row, $col, $contents, 'TH');
    } // end func setHeaderContents

    /**
     * Adds a table row and returns the row identifier
     * @param    array    $contents   (optional) Must be a indexed array of valid cell contents
     * @param    mixed    $attributes (optional) Associative array or string of table row attributes
     *                                This can also be an array of attributes, in which case the attributes
     *                                will be repeated in a loop.
     * @param    string   $type       (optional) Cell type either 'TH' or 'TD'
     * @param    bool     $inTR           false if attributes are to be applied in TD tags
     *                                    true if attributes are to be applied in TR tag
     * @return   int
     * @access   public
     */
    function addRow($contents = null, $attributes = null, $type = 'TD', $inTR = false)
    {
        if (isset($contents) && !is_array($contents)) {
            return PEAR::raiseError("First parameter to HTML_Table::addRow must be an array");
        }
        $row = $this->_rows++;
        for ($counter = 0; $counter < count($contents); $counter++) {
            if ($type == 'TD') {
                $this->setCellContents($row, $counter, $contents[$counter]);
            } elseif ($type == 'TH') {
                $this->setHeaderContents($row, $counter, $contents[$counter]);
            }
        }
        $this->setRowAttributes($row, $attributes, $inTR);
        return $row;
    } // end func addRow

    /**
     * Sets the row attributes for an existing row
     * @param    int      $row            Row index
     * @param    mixed    $attributes     Associative array or string of table row attributes
     *                                    This can also be an array of attributes, in which case the attributes
     *                                    will be repeated in a loop.
     * @param    bool     $inTR           false if attributes are to be applied in TD tags
     *                                    true if attributes are to be applied in TR tag
     * @access   public
     * @throws   PEAR_Error
     */
    function setRowAttributes($row, $attributes, $inTR = false)
    {
        $multiAttr = $this->_isAttributesArray($attributes);
        if (!$inTR) {
            for ($i = 0; $i < $this->_cols; $i++) {
                if ($multiAttr) {
                    $this->setCellAttributes($row, $i,
                        $attributes[$i - ((ceil(($i+1) / count($attributes)))-1) * count($attributes)]);
                } else {
                    $this->setCellAttributes($row, $i, $attributes);
                }
            }
        } else {
            $attributes = $this->_parseAttributes($attributes);
            $err = $this->_adjustEnds($row, 1, 'setRowAttributes', $attributes);
            if (PEAR::isError($err)) {
                return $err;
            }
            $this->_structure[$row]['attr'] = $attributes;
        }
    } // end func setRowAttributes

    /**
     * Updates the row attributes for an existing row
     * @param    int      $row            Row index
     * @param    mixed    $attributes     Associative array or string of table row attributes
     * @param    bool     $inTR           false if attributes are to be applied in TD tags
     *                                    true if attributes are to be applied in TR tag
     * @access   public
     * @throws   PEAR_Error
     */
    function updateRowAttributes($row, $attributes = null, $inTR = false)
    {
        $multiAttr = $this->_isAttributesArray($attributes);
        if (!$inTR) {
            for ($i = 0; $i < $this->_cols; $i++) {
                if ($multiAttr) {
                    $this->updateCellAttributes($row, $i,
                        $attributes[$i - ((ceil(($i+1) / count($attributes)))-1) * count($attributes)]);
                } else {
                    $this->updateCellAttributes($row, $i, $attributes);
                }
            }
        } else {
            $attributes = $this->_parseAttributes($attributes);
            $err = $this->_adjustEnds($row, 1, 'updateRowAttributes', $attributes);
            if (PEAR::isError($err)) {
                return $err;
            }
            $this->_updateAttrArray($this->_structure[$row]['attr'], $attributes);
        }
    } // end func updateRowAttributes

    /**
     * Returns the attributes for a given row as contained in the TR tag
     * @param    int     $row         Row index
     * @return   array
     * @access   public
     */
    function getRowAttributes($row)
    {
        if (isset($this->_structure[$row]['attr'])) {
            return $this->_structure[$row]['attr'];
        }
        return;
    } // end func getRowAttributes

    /**
     * Alternates the row attributes starting at $start
     * @param    int      $start          Row index of row in which alternating begins
     * @param    mixed    $attributes1    Associative array or string of table row attributes
     * @param    mixed    $attributes2    Associative array or string of table row attributes
     * @param    bool     $inTR           false if attributes are to be applied in TD tags
     *                                    true if attributes are to be applied in TR tag
     * @access   public
     */
    function altRowAttributes($start, $attributes1, $attributes2, $inTR = false)
    {
        for ($row = $start ; $row < $this->_rows ; $row++) {
            $attributes = ( ($row+$start)%2 == 0 ) ? $attributes1 : $attributes2;
            $this->updateRowAttributes($row, $attributes, $inTR);
        }
    } // end func altRowAttributes

    /**
     * Adds a table column and returns the column identifier
     * @param    array    $contents   (optional) Must be a indexed array of valid cell contents
     * @param    mixed    $attributes (optional) Associative array or string of table row attributes
     * @param    string   $type       (optional) Cell type either 'TH' or 'TD'
     * @return   int
     * @access   public
     */
    function addCol($contents = null, $attributes = null, $type = 'TD')
    {
        if (isset($contents) && !is_array($contents)) {
            return PEAR::raiseError("First parameter to HTML_Table::addCol must be an array");
        }
        $col = $this->_cols++;
        for ($counter = 0; $counter < count($contents); $counter++) {
            $this->setCellContents($counter, $col, $contents[$counter], $type);
        }
        $this->setColAttributes($col, $attributes);
        return $col;
    } // end func addCol

    /**
     * Sets the column attributes for an existing column
     * @param    int      $col            Column index
     * @param    mixed    $attributes     (optional) Associative array or string of table row attributes
     * @access   public
     */
    function setColAttributes($col, $attributes = null)
    {
        $multiAttr = $this->_isAttributesArray($attributes);
        for ($i = 0; $i < $this->_rows; $i++) {
            if ($multiAttr) {
                $this->setCellAttributes($i, $col,
                    $attributes[$i - ((ceil(($i+1) / count($attributes)))-1) * count($attributes)]);
            } else {
                $this->setCellAttributes($i, $col, $attributes);
            }
        }
    } // end func setColAttributes

    /**
     * Updates the column attributes for an existing column
     * @param    int      $col            Column index
     * @param    mixed    $attributes     (optional) Associative array or string of table row attributes
     * @access   public
     */
    function updateColAttributes($col, $attributes = null)
    {
        $multiAttr = $this->_isAttributesArray($attributes);
        for ($i = 0; $i < $this->_rows; $i++) {
            if ($multiAttr) {
                $this->updateCellAttributes($i, $col,
                    $attributes[$i - ((ceil(($i+1) / count($attributes)))-1) * count($attributes)]);
            } else {
                $this->updateCellAttributes($i, $col, $attributes);
            }
        }
    } // end func updateColAttributes

    /**
     * Sets the attributes for all cells
     * @param    mixed    $attributes        (optional) Associative array or string of table row attributes
     * @access   public
     */
    function setAllAttributes($attributes = null)
    {
        for ($i = 0; $i < $this->_rows; $i++) {
            $this->setRowAttributes($i, $attributes);
        }
    } // end func setAllAttributes

    /**
     * Updates the attributes for all cells
     * @param    mixed    $attributes        (optional) Associative array or string of table row attributes
     * @access   public
     */
    function updateAllAttributes($attributes = null)
    {
        for ($i = 0; $i < $this->_rows; $i++) {
            $this->updateRowAttributes($i, $attributes);
        }
    } // end func updateAllAttributes

    /**
     * Returns the table structure as HTML
     * @access  public
     * @return  string
     */
    function toHtml()
    {
        $strHtml = '';
        $tabs = $this->_getTabs();
        $tab = $this->_getTab();
        $lnEnd = $this->_getLineEnd();
        if ($this->_comment) {
            $strHtml .= $tabs . "<!-- $this->_comment -->" . $lnEnd;
        }
        $strHtml .=
            $tabs . "<table" . $this->_getAttrString($this->_attributes) . ">" . $lnEnd;
        if (!empty($this->_structure["caption"])) {
            $attr = $this->_structure["caption"]["attr"];
            $contents = $this->_structure["caption"]["contents"];
            $strHtml .= $tabs . $tab . "<caption" . $this->_getAttrString($attr) . ">";
            if (is_array($contents)) $contents = implode(", ", $contents);
            $strHtml .= $contents;
            $strHtml .= "</caption>" . $lnEnd;
        }
        for ($i = 0 ; $i < $this->_rows ; $i++) {
            if (isset($this->_structure[$i]['attr'])) {
                $strHtml .= $tabs . $tab . "<tr".$this->_getAttrString($this->_structure[$i]['attr']).">" . $lnEnd;
            } else {
                $strHtml .= $tabs .$tab . "<tr>" . $lnEnd;
            }
            for ($j = 0 ; $j < $this->_cols ; $j++) {
                if (isset($this -> _structure[$i][$j]) && $this->_structure[$i][$j] == "__SPANNED__") {
                    $strHtml .= $tabs . $tab . $tab ."<!-- span -->" . $lnEnd;
                    continue;
                }
                if (isset($this->_structure[$i][$j]["type"])) {
                    $type = (strtoupper($this->_structure[$i][$j]["type"]) == "TH" ? "th" : "td");
                } else {
                    $type = "td";
                }
                if (isset($this->_structure[$i][$j]["attr"])) {
                    $attr = $this->_structure[$i][$j]["attr"];
                } else {
                    $attr = "";
                }
                if (isset($this->_structure[$i][$j]["contents"])) {
                    $contents = $this->_structure[$i][$j]["contents"];
                } else {
                    $contents = "";
                }
                $strHtml .= $tabs . $tab . $tab . "<$type" . $this->_getAttrString($attr) . ">";
                if (is_object($contents)) {
                    // changes indent and line end settings on nested tables
                    if (is_subclass_of($contents, "html_common")) {
                        $contents->setTab($tab);
                        $contents->setTabOffset($this->_tabOffset + 3);
                        $contents->_nestLevel = $this->_nestLevel + 1;
                        $contents->setLineEnd($this->_getLineEnd());
                    }
                    if (method_exists($contents, "toHtml")) {
                        $contents = $contents->toHtml();
                    } elseif (method_exists($contents, "toString")) {
                        $contents = $contents->toString();
                    }
                }
                if (is_array($contents)) {
                    $contents = implode(", ",$contents);
                }
                if (isset($this->_autoFill) && $contents == "") {
                    $contents = $this->_autoFill;
                }
                $strHtml .= $contents;
                $strHtml .= "</$type>" . $lnEnd;
            }
            $strHtml .= $tabs . $tab . "</tr>" . $lnEnd;
        }
        $strHtml .= $tabs . "</table>" . $lnEnd;
        return $strHtml;
    } // end func toHtml

    /**
     * Checks if rows or columns are spanned
     * @param    int        $row            Row index
     * @param    int        $col            Column index
     * @access   private
     */
    function _updateSpanGrid($row, $col)
    {
        if (isset($this->_structure[$row][$col]["attr"]["colspan"])) {
            $colspan = $this->_structure[$row][$col]["attr"]["colspan"];
        }
        if (isset($this->_structure[$row][$col]["attr"]["rowspan"])) {
            $rowspan = $this->_structure[$row][$col]["attr"]["rowspan"];
        }
        if (isset($colspan)) {
            for ($j = $col+1; (($j < $this->_cols) && ($j <= ($col + $colspan - 1))); $j++) {
                $this->_structure[$row][$j] = "__SPANNED__";
            }
        }
        if (isset($rowspan)) {
            for ($i = $row+1; (($i < $this->_rows) && ($i <= ($row + $rowspan - 1))); $i++) {
                $this->_structure[$i][$col] = "__SPANNED__";
            }
        }
        if (isset($colspan) && isset($rowspan)) {
            for ($i = $row+1; (($i < $this->_rows) && ($i <= ($row + $rowspan - 1))); $i++) {
                for ($j = $col+1; (($j <= $this->_cols) && ($j <= ($col + $colspan - 1))); $j++) {
                    $this->_structure[$i][$j] = "__SPANNED__";
                }
            }
        }
    } // end func _updateSpanGrid

    /**
    * Adjusts ends (total number of rows and columns)
    * @param    int     $row        Row index
    * @param    int     $col        Column index
    * @param    string  $method     Method name of caller
    *                               Used to populate PEAR_Error if thrown.
    * @param    array   $attributes Assoc array of attributes
    *                               Default is an empty array.
    * @access   private
    * @throws   PEAR_Error
    */
    function _adjustEnds($row, $col, $method, $attributes = array())
    {
        $colspan = isset($attributes['colspan']) ? $attributes['colspan'] : 1;
        $rowspan = isset($attributes['rowspan']) ? $attributes['rowspan'] : 1;
        if (($row + $rowspan - 1) >= $this->_rows) {
            if ($this->_autoGrow) {
                $this->_rows = $row + $rowspan;
            } else {
                return PEAR::raiseError('Invalid table row reference[' .
                    $row . '] in HTML_Table::' . $method);
            }
        }
        if (($col + $colspan - 1) >= $this->_cols) {
            if ($this->_autoGrow) {
                $this->_cols = $col + $colspan;
            } else {
                return PEAR::raiseError('Invalid table column reference[' .
                    $col . '] in HTML_Table::' . $method);
            }
        }
    } // end func _adjustEnds

    /**
    * Tells if the parameter is an array of attribute arrays/strings
    * @param    mixed   $attributes Variable to test
    * @access   private
    * @return   bool
    */
    function _isAttributesArray($attributes)
    {
        if (is_array($attributes) && isset($attributes[0])) {
            if (is_array($attributes[0]) || (is_string($attributes[0]) && count($attributes) > 1)) {
                return true;
            }
        }
        return false;
    } // end func _isAttributesArray
} // end class HTML_Table
?>