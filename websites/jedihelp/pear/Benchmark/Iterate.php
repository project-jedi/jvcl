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
// $Id: Iterate.php,v 1.6 2003/01/01 17:57:52 sterling Exp $
//

require_once 'Benchmark/Timer.php';

/**
* Benchmark::Benchmark_Iterate
* 
* Purpose:
* 
*     Benchmarking
* 
* Example:
*   a) 
*     require_once 'Benchmark/Iterate.php';
*     $benchmark = new Benchmark_Iterate;
* 
*     function foo($string) {
*         print $string . '<br>';
*     }
* 
*     $benchmark->run(100, 'foo', 'test');
*     $result = $benchmark->get();
*
*   b)
*     require_once 'Benchmark/Iterate.php';
*     $benchmark = new Benchmark_Iterate;
* 
*     class myclass{
*
*       function foo($string) {
*             print $string . '<br>';
*       }
*     }
* 
*     $benchmark->run(100, 'myclass::foo', 'test');
*     $result = $benchmark->get();
*
*   c)
*     require_once 'Benchmark/Iterate.php';
*     $benchmark = new Benchmark_Iterate;
* 
*     class myclass{
*
*       function foo($string) {
*             print $string . '<br>';
*       }
*     }
*
*     $myobj = new myclass();
* 
*     $benchmark->run(100, 'myobj->foo', 'test');
*     $result = $benchmark->get();
*
* @author   Sebastian Bergmann <sb@sebastian-bergmann.de>
* @version  $Revision: 1.6 $
* @access   public
*/

class Benchmark_Iterate extends Benchmark_Timer {
    /**
    * Benchmarks a function.
    *
    * @access public
    */

    function run() {
        $arguments     = func_get_args();

        $iterations    = array_shift($arguments);
        $function_name = array_shift($arguments);
    
        if (strstr($function_name, '::')) {
          $function_name = explode('::', $function_name);
          $objectmethod = $function_name[1];
        }

        // If we're calling a method on an object use call_user_func
        if (strstr($function_name, '->')) {
            $function_name = explode('->', $function_name);
            $objectname = $function_name[0];

            global ${$objectname};
            $objectmethod = $function_name[1];

            for ($i = 1; $i <= $iterations; $i++) {
                $this->setMarker('start_' . $i);
                call_user_func_array(array(${$objectname}, $function_name[1]), $arguments);
                $this->setMarker('end_' . $i);
            }
            return(0);
        }

        for ($i = 1; $i <= $iterations; $i++) {
            $this->setMarker('start_' . $i);
            call_user_func_array($function_name, $arguments);
            $this->setMarker('end_' . $i);
        }
    }

    /**
    * Returns benchmark result.
    *
    * $result[x           ] = execution time of iteration x
    * $result['mean'      ] = mean execution time
    * $result['iterations'] = number of iterations
    *
    * @return array $result
    * @access public
    */

    function get() {
        $result = array();
        $total  = 0;

        $iterations = count($this->markers)/2;

        for ($i = 1; $i <= $iterations; $i++) {
            $time = $this->timeElapsed('start_'.$i , 'end_'.$i);

            if (extension_loaded('bcmath')) {
                $total = bcadd($total, $time, 6);
            } else {
                $total = $total + $time;
            }
            
            $result[$i] = $time;
        }

        if (extension_loaded('bcmath')) {
            $result['mean'] = bcdiv($total, $iterations, 6);
        } else {
            $result['mean'] = $total / $iterations;
        }

        $result['iterations'] = $iterations;

        return $result;
    }
}
?>
