<?php
require '../Timer.php';

function wait($amount) {
    for ($i=0; $i < $amount; $i++) {
        for ($i=0; $i < 100; $i++);
    }
}
// Pass the param "true" to constructor to automatically display the results
$timer = new Benchmark_Timer();
$timer->start();
wait(10);
$timer->setMarker('Mark1');
echo "Elapsed time between Start and Mark1: " .
      $timer->timeElapsed('Start', 'Mark1') . "\n";
wait(50);
$timer->stop();
$timer->display();
?>
