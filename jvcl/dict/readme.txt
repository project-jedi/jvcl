Dictionary file format:
The words are compressed.
Each word is prefixed with a digit letter.
The digit gives the number of chars the word shares with the previous uncompressed word.
The digit letters range from '0' to '9'. Longer shared stems are not handled.
The words are sorted ascending according to their uncompressed text per TStringList.Sort.
The lineends are CR only to save space (TStringList.LoadFromFile reads that without problems).
