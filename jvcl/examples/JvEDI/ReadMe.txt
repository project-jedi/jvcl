The components are used for building a program that can build and 
update an EDI database used as a buffer for EDI data accessable via SQL.

Running the Demo:
  - Run "EDIDBBuffering.exe" in the programs folder.
  - Select database to use for demo.
  - Click the "Profile" button
  - Browse for the "RGE2_SpecVer2.sef" file in the "SampleSpecs" folder.
  - "Profile Done" message box will appear click "Ok".
  - Click "Update Database Buffer" button
  - The log box on the right will fill up indicating the operations 
    performed on the database.
  - "DB Update Buffer Done" message box will appear click "Ok".

Notes:
Requires JCL 1.90, Interbase 6.0.2.0 & IBX 7.08, Advantage Local Database 7.0 (free at www.advantagedatabase.com) 

  For the Advantage database, actual database restructure is performed.
  For the Interbase database, only SQL scripts are created in the log box.