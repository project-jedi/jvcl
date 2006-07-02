<?php
//
// +------------------------------------------------------------------------+
// | PEAR :: Benchmark                                                      |
// +------------------------------------------------------------------------+
// | Copyright (c) 2001-2003 Sebastian Bergmann <sb@sebastian-bergmann.de>. |
// +------------------------------------------------------------------------+
// | This source file is subject to version 3.00 of the PHP License,        |
// | that is available at http://www.php.net/license/3_0.txt.               |
// | If you did not receive a copy of the PHP license and are unable to     |
// | obtain it through the world-wide-web, please send a note to            |
// | license@php.net so we can mail you a copy immediately.                 |
// +------------------------------------------------------------------------+
//
// $Id: Timer.php,v 1.7 2002/12/11 17:14:17 sebastian Exp $
//

/**
 * Benchmark::Timer
 *
 * Purpose:
 *
 *     Timing Script Execution, Generating Profiling Information
 *
 * Example with automatic profiling start, stop, and output:
 *
 *     $timer =& new Benchmark_Timer(true);
 *     $timer->setMarker('Marker 1');
 *
 * Example without automatic profiling:
 *
 *     $timer =& new Benchmark_Timer();
 *
 *     $timer->start();
 *     $timer->setMarker('Marker 1');
 *     $timer->stop();
 *
 *     $profiling = $timer->getProfiling();
 *     echo $profiling->getOutput(); // or $timer->display()
 *
 * Contributors:
 * - Ludovico Magnocavallo <ludo@sumatrasolutions.com>
 *   auto profiling and get_output() method
 *
 * @author   Sebastian Bergmann <sb@sebastian-bergmann.de>
 * @version  $Revision: 1.7 $
 * @access   public
 */

require_once 'PEAR.php';

class Benchmark_Timer extends PEAR
{
    /**
     * Contains the markers
     *
     * @var    array
     * @access private
     */
    var $markers = array();

    /**
     * Auto-start and stop timer
     *
     * @var    boolean
     * @access private
     */
    var $auto   = false;

    /**
     * Max marker name length for non-html output
     *
     * @var    integer
     * @access private
     */
    var $strlen_max = 0;

    /**
     * Constructor, starts profiling recording
     *
     * @access public
     */
    function Benchmark_Timer($auto = false) {
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
    function _Benchmark_Timer() {
        if ($this->auto) {
            $this->stop();
            $this->display();
        }
    }

    /**
     * Return formatted profiling information.
     *
     * @see    getProfiling()
     * @access public
     */
    function getOutput()
    {
        if (function_exists('version_compare') &&
            version_compare(phpversion(), '4.1', 'ge'))
        {
            $http = isset($_SERVER['SERVER_PROTOCOL']);
        } else {
            global $HTTP_SERVER_VARS;
            $http = isset($HTTP_SERVER_VARS['SERVER_PROTOCOL']);
        }
        $total = $this->TimeElapsed();
        $result = $this->getProfiling();
        $dashes = "";
        if ($http) {
            $out = "<table border=1>\n";
            $out .= '<tr><td>&nbsp;</td><td align="center"><b>time index</b></td><td align="center"><b>ex time</b></td><td align="center"><b>%</b></td></tr>'."\n";
        } else {
            $dashes = $out = str_pad("\n", ($this->strlen_max + 52), '-', STR_PAD_LEFT);
            $out .= str_pad('marker', $this->strlen_max);
            $out .= str_pad("time index", 22);
            $out .= str_pad("ex time", 22);
            $out .= "perct\n";
            $out .= $dashes;
        }
        foreach ($result as $k => $v) {
            $perc = (($v['diff'] * 100) / $total);
            if ($http) {
                $out .= "<tr><td><b>" . $v['name'] . "</b></td><td>" . $v['time'] . "</td><td>" . $v['diff'] . "</td><td align=\"right\">" . number_format($perc, 2, '.', '') . "%</td></tr>\n";
            } else {
                $out .= str_pad($v['name'], $this->strlen_max, ' ');
                $out .= str_pad($v['time'], 22);
                $out .= str_pad($v['diff'], 22);
                $out .= str_pad(number_format($perc, 2, '.', '') . "%\n", 8, ' ', STR_PAD_LEFT);
            }
            $out .= $dashes;
        }
        if ($http) {
            $out .= "<tr style='background: silver;'><td><b>total</b></td><td>-</td><td>${total}</td><td>100.00%</td></tr>\n";
            $out .= "</table>\n";
        } else {
            $out .= str_pad('total', $this->strlen_max);
            $out .= str_pad('-', 22);
            $out .= str_pad($total, 22);
            $out .= "100.00%\n";
            $out .= $dashes;
        }
        return $out;
    }

    /**
    * Prints the information returned by getOutput
    */
    function display()
    {
        print $this->getOutput();
    }

    /**
     * Set "Start" marker.
     *
     * @see    setMarker(), stop()
     * @access public
     */
    function start() {
        $this->setMarker('Start');
    }

    /**
     * Set "Stop" marker.
     *
     * @see    setMarker(), start()
     * @access public
     */
    function stop() {
        $this->setMarker('Stop');
    }

    /**
     * Set marker.
     *
     * @param  string  name of the marker to be set
     * @see    start(), stop()
     * @access public
     */
    function setMarker($name) {
        $microtime = explode(' ', microtime());
        $this->markers[$name] = $microtime[1] . substr($microtime[0], 1);
    }

    /**
     * Returns the time elapsed betweens two markers.
     *
     * @param  string  $start        start marker, defaults to "Start"
     * @param  string  $end          end marker, defaults to "Stop"
     * @return double  $time_elapsed time elapsed between $start and $end
     * @access public
     */
    function timeElapsed($start = 'Start', $end = 'Stop') {
        if (extension_loaded('bcmath')) {
            return bcsub($this->markers[$end], $this->markers[$start], 6);
        } else {
            return $this->markers[$end] - $this->markers[$start];
        }
    }

    /**
     * Returns profiling information.
     *
     * $profiling[x]['name']  = name of marker x
     * $profiling[x]['time']  = time index of marker x
     * $profiling[x]['diff']  = execution time from marker x-1 to this marker x
     * $profiling[x]['total'] = total execution time up to marker x
     *
     * @return array $profiling
     * @access public
     */
    function getProfiling() {
        $i = $total = $temp = 0;
        $result = array();

        foreach ($this->markers as $marker => $time) {
            if (extension_loaded('bcmath')) {
                $diff  = bcsub($time, $temp, 6);
                $total = bcadd($total, $diff, 6);
            } else {
                $diff  = $time - $temp;
                $total = $total + $diff;
            }

            $result[$i]['name']  = $marker;
            $result[$i]['time']  = $time;
            $result[$i]['diff']  = $diff;
            $result[$i]['total'] = $total;

            $this->strlen_max = (strlen($marker) > $this->strlen_max ? strlen($marker) + 1 : $this->strlen_max);

            $temp = $time;
            $i++;
        }
        $result[0]['diff'] = '-';
        $this->strlen_max = (strlen('total') > $this->strlen_max ? strlen('total') : $this->strlen_max);
        $this->strlen_max += 4;
        return $result;
    }
}
?>
