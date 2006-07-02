<?php
//
// +----------------------------------------------------------------------+
// | PEAR :: Benchmark                                                    |
// +----------------------------------------------------------------------+
// | Copyright (c) 2002-2003 Matthias Englert <Matthias.Englert@gmx.de>.       |
// +----------------------------------------------------------------------+
// | This source file is subject to version 3.00 of the PHP License,      |
// | that is available at http://www.php.net/license/3_0.txt.             |
// | If you did not receive a copy of the PHP license and are unable to   |
// | obtain it through the world-wide-web, please send a note to          |
// | license@php.net so we can mail you a copy immediately.               |
// +----------------------------------------------------------------------+
//
// $Id: Profiler.php,v 1.6 2003/04/24 15:33:53 matthias Exp $
//

/**
 * Benchmark::Profiler
 *
 * Purpose:
 *
 *     Timing Script Execution, Generating Profiling Information
 *
 * Example with automatic profiling start, stop, and output:
 *
 *     $profiler =& new Benchmark_Profiler(true);
 *     function myFunction() {
 *         $profiler->enterSection('myFunction');
 *         //do something
 *         $profiler->leaveSection('myFunction');
 *         return;
 *     }
 *     //do something
 *     myFunction();
 *     //do more
 *
 *
 * Example without automatic profiling:
 *
 *     $profiler =& new Benchmark_Profiler();
 *     function myFunction() {
 *         $profiler->enterSection('myFunction');
 *         //do something
 *         $profiler->leaveSection('myFunction');
 *         return;
 *     }
 *     $profiler->start();
 *     //do something
 *     myFunction();
 *     //do more
 *     $profiler->stop();
 *     $profiler->display();
 *
 * @author   Matthias Englert <Matthias.Englert@gmx.de>
 * @version  $Revision: 1.6 $
 * @access   public
 */

require_once 'PEAR.php';

class Benchmark_Profiler extends PEAR {

   /**
     * Contains the total ex. time of each section
     *
     * @var    array
     * @access private
     */
    var $_sections = array();

    /**
     * Calling stack
     *
     * @var    array
     * @access private
     */
    var $_stack = array();

    /**
     * Notes how often a section was entered
     *
     * @var    array
     * @access private
     */
    var $_num_calls = array();

    /**
     * Notes for each section how much time is spend in sub-sections
     *
     * @var    array
     * @access private
     */
    var $_sub_sections_time = array();

    /**
     * Notes for each section how often it calls which section
     *
     * @var    array
     * @access private
     */
    var $_calls = array();

    /**
     * Notes for each section how often it was called by which section
     *
     * @var    array
     * @access private
     */
    var $_callers = array();

    /**
     * Auto-starts and stops profiler
     *
     * @var    boolean
     * @access private
     */
    var $_auto = false;

    /**
     * Max marker name length for non-html output
     *
     * @var    integer
     * @access private
     */
    var $_strlen_max = 0;

    /**
     * Constructor, starts profiling recording
     *
     * @access public
     */
    function Benchmark_Profiler($auto = false) {
        $this->PEAR();
        if ($auto) {
            $this->auto = $auto;
            $this->start();
        }
    }

    /**
     * Destructor, stops profiling recording
     *
     * @access private
     */
    function _Benchmark_Profiler() {
        if (isset($this->auto)) {
            $this->stop();
            $this->display();
        }
    }

    /**
     * Returns profiling informations for a given section.
     *
     * @access public
     */
    function getSectionInformations($section = 'Global') {
        if (isset($this->_sections[$section])) {

            $calls = array();
            if (isset($this->_calls[$section])) {
                $calls = $this->_calls[$section];
            }        
            
            $callers = array();
            if (isset($this->_callers[$section])) {
                $callers = $this->_callers[$section];
            }
    
            $informations = array();
            $informations['time'] = $this->_sections[$section];
            $informations['percentage'] = number_format(100 * $this->_sections[$section] / $this->_sections['Global'], 2, '.', '');
            $informations['calls'] = $calls;
            $informations['num_calls'] = $this->_num_calls[$section];
            $informations['callers'] = $callers;
	    if (isset($this->_sub_sections_time[$section])) {
                $informations['netto_time'] = $this->_sections[$section] - $this->_sub_sections_time[$section];
	    } else {
                $informations['netto_time'] = $this->_sections[$section];
	    }

            return $informations;
        } else {
            $this->raiseError("The section '$section' does not exists.\n", null, PEAR_ERROR_TRIGGER, E_USER_WARNING);        
        }
    }    

    /**
     * Returns profiling informations for all sections.
     *
     * @access public
     */
    function getAllSectionsInformations() {
        $informations = array();
        foreach($this->_sections as $section => $time) {
            $informations[$section] = $this->getSectionInformations($section);
        }

        return $informations;
    }    
    
    /**
     * Returns formatted profiling information.
     *
     * @see    display()
     * @access private
     */
    function _getOutput() {
        if (function_exists('version_compare') &&
            version_compare(phpversion(), '4.1', 'ge')) {
            $http = isset($_SERVER['SERVER_PROTOCOL']);
        } else {
            global $HTTP_SERVER_VARS;
            $http = isset($HTTP_SERVER_VARS['SERVER_PROTOCOL']);
        }
        if ($http) {
            $out = "<table border=1>\n";
            $out .=
                '<tr><td>&nbsp;</td><td align="center"><b>total ex. time</b></td>'.
                '<td align="center"><b>netto ex. time</b></td>'.
                '<td align="center"><b>#calls</b></td><td align="center"><b>%</b></td>'.
                '<td align="center"><b>calls</b></td><td align="center"><b>callers</b></td></tr>'.
                "\n";
        } else {
            $dashes = $out =
                str_pad("\n", ($this->_strlen_max + 52), '-',
                        STR_PAD_LEFT);
            $out .= str_pad('section', $this->_strlen_max);
            $out .= str_pad("total ex time", 22);
            $out .= str_pad("netto ex time", 22);
            $out .= str_pad("#calls", 22);
            $out .= "perct\n";
            $out .= $dashes;
        }
        
        $informations = $this->getAllSectionsInformations();        
        foreach($informations as $name => $values) {
            $percentage = $values['percentage'];
            $calls_str = "";
            foreach($values['calls'] as $key => $val) {
                if ($calls_str) {
                    $calls_str .= ", ";
                }
                $calls_str .= "$key ($val)";
            }
            $callers_str = "";
            foreach($values['callers'] as $key => $val) {
                if ($callers_str) {
                    $callers_str .= ", ";
    		    }
                $callers_str .= "$key ($val)";
            }
            if ($http) {
                $out .=
                    "<tr><td><b>$name</b></td><td>{$values['time']}</td><td>{$values['netto_time']}</td><td>{$values['num_calls']}</td>".
                    "<td align=\"right\">{$values['percentage']}%</td>\n";
                $out .= "<td>$calls_str</td><td>$callers_str</td></tr>";
            } else {
                $out .= str_pad($name, $this->_strlen_max, ' ');
                $out .= str_pad($values['time'], 22);
                $out .= str_pad($values['netto_time'], 22);
                $out .= str_pad($values['num_calls'], 22);
                $out .=
                str_pad($values['percentage']."%\n", 8, ' ',
                            STR_PAD_LEFT);
            }
        }
        $out .= "</table>";
        return $out;        
    }    

    /**
     * Returns formatted profiling information.
     *
     * @access public
     */
    function display() {
        echo $this->_getOutput();
    }

    /**
     * Enters "Global" section.
     *
     * @see    enterSection(), stop()
     * @access public
     */
    function start() {
        $this->enterSection('Global');
    }

    /**
     * Leaves "Global" section.
     *
     * @see    leaveSection(), start()
     * @access public
     */
    function stop() {
        $this->leaveSection('Global');
    }

    /**
     * Enters code section.
     *
     * @param  string  name of the code section
     * @see    start(), leaveSection()
     * @access public
     */
    function enterSection($name) {
        if (count($this->_stack)) {
            if (isset($this->_callers[$name][$this->_stack[count($this->_stack) - 1]["name"]])) {
                $this->_callers[$name][$this->_stack[count($this->_stack) - 1]["name"]]++;
            } else {
                $this->_callers[$name][$this->_stack[count($this->_stack) - 1]["name"]] = 1;
            }
        
            if (isset($this->_calls[$this->_stack[count($this->_stack) - 1]["name"]][$name])) {
                $this->_calls[$this->_stack[count($this->_stack) - 1]["name"]][$name]++;
            } else {
                $this->_calls[$this->_stack[count($this->_stack) - 1]["name"]][$name] = 1;
            }
        }
        
        if (isset($this->_num_calls[$name])) {
            $this->_num_calls[$name]++;
        } else {
            $this->_num_calls[$name] = 1;
        }       

        $microtime = explode(" ", microtime());
        $microtime = $microtime[1].substr($microtime[0], 1);
        array_push($this->_stack,
                   array("name" => $name, "time" => $microtime));
    }

    /**
     * Leaves code section.
     *
     * @param  string  name of the marker to be set
     * @see     stop(), enterSection()
     * @access public
     */
    function leaveSection($name) {
        $microtime = explode(" ", microtime());
        $microtime = $microtime[1].substr($microtime[0], 1);
        $x = array_pop($this->_stack);
        if ($x["name"] != $name) {
            $this->raiseError("reached end of section $name but expecting end of ".
                               $x["name"]."\n",null,PEAR_ERROR_DIE);
        }

        if (isset($this->_sections[$name])) {
            $this->_sections[$name] += $microtime - $x["time"];
        } else {
            $this->_sections[$name] = $microtime - $x["time"];
        }
	
	$parent = array_pop($this->_stack);
	if (isset($parent)) {
            if (isset($this->_sub_sections_time[$parent['name']])) {
                $this->_sub_sections_time[$parent['name']] += $microtime - $x['time'];
            } else {
                $this->_sub_sections_time[$parent['name']] = $microtime - $x['time'];
            }
	    array_push($this->_stack, $parent);
	}
    }

}

?>