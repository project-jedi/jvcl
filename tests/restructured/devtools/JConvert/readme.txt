JConvert is a command-line tool to convert dfm files between text and binary format. 

It differs from Borland's tool in two ways: 

1. It doesn't create a new file unless the dfm was actually converted from one format to the other. 
	This is essential when working with VCS systems like CVS because you only have to commit the files that were actually translated. If you convert a lot of dfm's this can save you some bandwidth and the need to commit files that appears to have changed but in reality hasn't

2. You can use the -c switch to check how many files would have been converted
	This is great when you want to know if all your dfm's are in the right format without actually change them

