#!/usr/bin/env php
<?php
$words = file('/usr/share/dict/words');
$output = array();
while (count($output) < 3) {
    foreach (array_rand($words, 3) as $key) {
        if (preg_match('/^[A-Za-z]+$/S', $words[$key])) {
            $output[] = trim($words[$key]);
        }
    }
}
$chosen_words = array_slice($output, 0, 3);
$string = join(' ', $chosen_words);
$random = '';
$characters = (
    '!*+-'
    . join('', array_map('chr', range(48, 57)))
    . join('', array_map('chr', range(65, 90)))
    . join('', array_map('chr', range(97, 122)))
);
foreach (range(0,8) as $i) {
    $random .= $characters[rand(0, strlen($characters) - 1)];
}
echo $string;
echo "\n";
echo $random;
echo "\n";
