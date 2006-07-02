--TEST--
DB_driver::prepare/execute test
--SKIPIF--
<?php chdir(dirname(__FILE__)); include("skipif.inc"); ?>
--FILE--
<?php
include("mktable.inc");
include("../prepexe.inc");
?>
--EXPECT--
------------1------------
sth1,sth2,sth3 created
sth1 executed
sth2 executed
sth3 executed
results:
|72 - a -  - |
|72 - bing -  - |
|72 - gazonk - opaque
placeholder
test - |

------------2------------
results:
|72 - set1 - opaque
placeholder
test - |
|72 - set2 - opaque
placeholder
test - |
|72 - set3 - opaque
placeholder
test - |

------------3------------
TRUE
FALSE

------------4------------
|72 - set1 - opaque
placeholder
test - |
|72 - set2 - opaque
placeholder
test - |
|72 - set3 - opaque
placeholder
test - |
~~
~~
|72 - set1 - opaque
placeholder
test - |
~~
|72 - set1 - opaque
placeholder
test - |
|72 - set2 - opaque
placeholder
test - |
|72 - set3 - opaque
placeholder
test - |
~~
