{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is by Warren Postma.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

2003-07-29 Warren Postma - New features (Sorting, Indexing, UserData)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net
ii

Description:
  TJvCsvDataSet in-memory-dataset component usable by any
    VCL Data Aware Controls.
              TJvCsvDataSet appears in the 'Jv Data Access' tab of the
    Component Pallette.

    USAGE:
      Drop this component onto a form, connect it to
      a standard VCL DataSource, then connect any
      data aware control to that datasource, using
      the standard method you would use if you were
      using any other data aware components.

    KEY PROPERTIES:
      You must set the filename to a valid CSV FileName
      such as "MyCsvFile.csv", and you must define the
      CSV Fields, using the CSVFieldDef property.
      If you don't set those properties, the component
      won't work. It is also *recommended* but not
      required to right-click on the component and
      let the Delphi IDE define the field objects
      so that you can access them in your program.

    MORE HELP, DOCUMENTATION:
      This object works just like the VCL BDE TTable,
      so consult
      the Delphi help file about TTable if you want
      more information.

Known Issues and Updates:
  Feb 10, 2003 - Merged local JvCsvData-1.20a.pas changes.
                 New just-in-time-csv-header parsing fixes long standing
                 bug for tables which are generated from TStrings already
                 in memory instead of ones loaded from files on disk.

  Nov 17, 2003 - Now implements TDataSet.Locate!!! (needs more testing)
  Sept 26, 2003 - Obones made C++Builder fixes.
  Sept 24, 2003 -
  MERGE ALERT: This version is merged with Peter's version, minus
  his case changes, since I think they make the code less readable,
  and since the case changes are the only changes of his I could find,
  this is essentially a one-side merge, where I dropped all his changes
  None appear to cause any functional change in the program. If I missed
  any real changes, I apologize.
  CRITICAL FIX: Length 1 character field bug fixed.
  NEW IMPORT AND APPEND NEW FIELDS:
  New Handy Dandy Import-and-Upgrade feature: If you add fields to your
  dataset definition, you can still load your old file (that is missing
  those columns) and it will add them the next time you save the file.
  New columns are always appended to the physical last position (end of
   existing lines) in the data file.

  NEW WORKING-DIRECTORY-CHANGE FIX:
  If your program uses the File Open Dialog it can sometimes change your
  app's current working directory. If your CsvDataSets have filenames
  without a full path name (C:\MyFolder\MyFile.csv is absoluete,
  MyFile.csv is relative), then you could have problems. This component
  fixes these problems like this: It gets and stores the current working
  directory at startup, and for all filenames where the absolute path
  is not stored, the local startup directory is used.  This prevents
  the problem where complex apps could load a CSV from one directory
  and save it to another, and then next time the app runs, the CSV
  file is the old version, since the new version was stored in a
  different directory.
  -----
  May 26, 2003 - Fixed errors handling null date values.
               - Fixed improper memory access for ftBoolean.
                 Booleans are stored internaly as a 16bit WordBool, inside
                 DataSets and the component was reading/writing a 32 bit value,
                 which could caused all kinds of squirrelly things to happen
                 when the boolean (ftBoolean, csv type '!') was encountered.
                 Search for the WordBool to see the changes.
-----------------------------------------------------------------------------}
// $Id$

//------------------------------------------------------------------------
//
// TJvCSVDataSet
//
// An in-memory TDataSet component similar to TTable but with optional
// saving to CSV file, and which, unlike using TTable in CSV mode, does not
// utilize the BDE, or any external database access layers to do its work.
//
// Since this component inherits from TDataSource, you can use it with any
// standard VCL data aware components.  Remember to link to a DataSource,
// before you can link this to any data aware controls!
//
//
// TJvCustomCsvDataSet
//
// Internally, we first define a TJvCustomCsvDataSet a base class.
// Nothing published.  This exists so you can easily inherit from it
// and define your own version of the component, and publish whatever
// properties and methods you wish to publish, and you can hide or
// override any other elements you don't wish to publish.
//
// How To Use:
// You *must* first set up the important Property
// called CsvFieldDef which describes the expected fields and their types
// since the CSV file itself contains insufficient information to guess the
// field types.
//
//
// Example CsvFieldDef string:
//   ABC:$80,DEFG:$140,HIJKLMN:%,OPQRST:@
//
//   $ = string (ftString) - also used if no character is given.
//   % = whole integer value (ftInteger)
//   & = floating point value (ftFloat)
//   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
//   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
//   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
//   ! = Boolean Field (0 in csv file=false, not 0 = true, blank = NULL)
//
// NOTE: YOU SHOULD PROBABLY JUST USE THE BUILT-IN PROPERTY EDITOR (CLICK ...)
// INSTEAD OF MEMORIZING ALL THIS FIELD TYPE STUFF.
//
// Originally written by Warren Postma
// Contact: warren.postma@sympatico.ca or warrenpstma@hotmail.com
//
// Donated to the Delphi Jedi Project.
// All Copyrights and Ownership donated to the Delphi Jedi Project.
//------------------------------------------------------------------------

{$I jvcl.inc}

unit JvCsvData;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF VisualCLX}
  QWindows,
  {$ENDIF VisualCLX}
  SysUtils, Classes,
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  DB;

const
  MaxCalcDataOffset = 256; // 128 bytes per record for Calculated Field Data.
  // JvCsvSep = ','; // converted to property Separator
  MAXCOLUMNS = 80;
  DEFAULT_CSV_STR_FIELD = 80;
  MAXLINELENGTH = 2048;
  COLUMN_ENDMARKER = $FFFF;
  ON_BOF_CRACK = -1;
  ON_EOF_CRACK = -2;

  { return values from CompareBookmarks: }
  Bookmark_Less = -1; // b1 < b2
  Bookmark_Gtr = 1; // b1 > b2
  Bookmark_Eql = 0; // b1 = b2

type
  PInteger = ^Integer;
  PDouble = ^Double;
  PBoolean = ^Boolean;
  {$IFNDEF COMPILER6_UP}
  PWordBool = ^WordBool;
  {$ENDIF COMPILER6_UP}
  EJvCsvDataSetError = class(EDatabaseError);
  // Subclass DB.EDatabaseError so we can work nicely with existing Delphi apps.

  EJvCsvKeyError = class(EDatabaseError); // Key Uniqueness or Key Problem

  { Special Event Types }
  TJvCsvOnSpecialData = procedure(Sender: TObject; Index: Integer; NonCsvData: string) of object;

  TJvCsvOnGetFieldData = procedure(Sender: TObject; UserTag: Integer; UserData: Pointer; FieldName: string; var Value:
    string) of object;
  TJvCsvOnSetFieldData = procedure(Sender: TObject; UserTag: Integer; UserData: Pointer; FieldName: string; Value:
    string) of object;

  { SPECIAL TYPES OF  DATABASE COLUMNS FOR THIS COMPONENT }
  { Columns are numeric, text, or one of two kinds of Specially Encoded date/time formats: }
  TJvCsvColumnFlag = (jcsvNull, jcsvString, jcsvNumeric, jcsvAsciiDateTime, jcsvGMTDateTime, jcsvTZDateTime);

  { pointer to special CSV COLUMN }
  PCsvColumn = ^TJvCsvColumn;
  // PFieldDef = ^TFieldDef;

  TJvCsvColumn = record
    FFlag: TJvCsvColumnFlag; // Column CSV Format Flags
    FKeyFlag: Boolean; // This column is part of the primary key! (new May 2003-WP)
    FPhysical: Integer; // Physical Column Ordering
    FFieldDef: TFieldDef; // Associated FieldDef
  end;

  { CSV COLUMNS are stored in a TList-Collection }
  TJvCsvColumns = class(TList)
  public
    procedure AddColumn(Item: PCsvColumn);
    function FindByFieldNo(FieldNo: Integer): PCsvColumn;
    procedure Clear; override;
    function FindByName(const FieldName: string): PCsvColumn;
  end;

  TJvCsvBookmark = record
    Flag: TBookmarkFlag;
    Data: Integer;
  end;

  { CSV Data File Row is not very dynamic in this version: }
  PtrToPtrToCsvRow = ^PCsvRow; // bookmark Data = double pointer indirection! Fun fun fun!
  PCsvRow = ^TJvCsvRow; // a pointer to a record
  TJvCsvRow = record { this MUST be a record, not a class, and must be a flag Data record type }
    IsDirty: Boolean; // record is dirty (needs to be written to disk)
    Columns: Integer;
    Index: Integer; // FData Index (-1 means not in FData)
    WordField: array [0..MAXCOLUMNS + 1] of Word;
    // lookup field beginning, Column Data (column dirty bit+column length) }
    Text: array [0..MAXLINELENGTH] of Char; // lookup actual character Data.
    // bookmark
    Bookmark: TJvCsvBookmark;
    // filter flag;
    Filtered: Boolean; // row is hidden from view right now.
    RecursionFlag: Boolean; // helps us fix endless recursion bug in GetFieldData callbacks.
  end;

  { Row collection }
  TJvCsvRows = class(TList)
  protected
    FEnquoteBackslash: Boolean;
    // Optional user Data (only allocated if used, how efficient is that, eh.)
    FUserData: array of Pointer;
    FUserTag: array of Integer;
    FUserLength: Integer;
    function GetUserTag(Index: Integer): Integer;
    procedure SetUserTag(Index, Value: Integer);
    function GetUserData(Index: Integer): Pointer;
    procedure SetUserData(Index: Integer; Value: Pointer);
    // Get internal value, return as Variant.
  public
    procedure AddRow(Item: PCsvRow);
    procedure InsertRow(const Position: Integer;  Item: PCsvRow);
    procedure AddRowStr(const Item: string; Separator: Char); // convert String->TJvCsvRow
    function GetRowPtr(const RowIndex: Integer): PCsvRow;
    function GetRowStr(const RowIndex: Integer): string;
    procedure SetRowStr(const RowIndex: Integer; Value: string; Separator: Char);
    procedure DeleteRow(const RowIndex: Integer);
    procedure SetARowItem(const RowIndex, ColumnIndex: Integer; Value: string);
    function GetARowItem(const RowIndex, ColumnIndex: Integer): string;
    procedure Clear; override;
    property EnquoteBackslash: Boolean read FEnquoteBackslash write FEnquoteBackslash;
    property UserTag[Index: Integer]: Integer read GetUserTag write SetUserTag;
    property UserData[Index: Integer]: Pointer read GetUserData write SetUserData;
  end;

  TArrayOfPCsvColumn = array of PCsvColumn;

  { TJvCustomCsvDataSetFilterFunction: Defines callback function to be passed to CustomFilter routine }
  TJvCustomCsvDataSetFilterFunction = function(RecNo: Integer): Boolean of object;

  // Easily Customizeable Dataset descendant our CSV handler and
  // any other variants we create:
  TJvCustomCsvDataSet = class(TDataSet)
  private
    FSeparator: Char;
    FOpenFileName: string; // This is the Fully Qualified path and filename expanded from the FTableName property when InternalOpen was last called.
    FValidateHeaderRow: Boolean;
    FExtendedHeaderInfo: Boolean;
    procedure SetSeparator(const Value: Char);
    procedure InternalQuickSort(SortList: PPointerList; L, R: Integer;
      SortColumns: TArrayOfPCsvColumn; ACount: Integer; Ascending: Boolean);
    procedure QuickSort(AList: TList; SortColumns: TArrayOfPCsvColumn; ACount: Integer; Ascending: Boolean);
  protected
    // (rom) inacceptable names. Probably most of this should be private.
    FTempBuffer: PChar;
    FInitialWorkingDirectory: string; // Current working dir may change in a delphi app, causing us trouble.
    FStoreDefs: Boolean;
    FEnquoteBackslash: Boolean; // causes _Enquote to use Backslashes. NOT the default behaviour.
    FTimeZoneCorrection: Integer; // defaults to 0 (none)
    FFileDirty: Boolean; // file needs to be written back to disk?

    FCsvFieldDef: string; // Our own "Csv Field Definition String"
    FCsvKeyDef: string; // CSV Key Definition String. Required if FCsvUniqueKeys is True
    FCsvKeyCount: Integer; // Set by parsing FCsvKeyDef
    FCsvKeyFields: TArrayOfPCsvColumn;

    FCsvUniqueKeys: Boolean;
    // CSV Key Uniqueness option.  Also requires that all fields that are part of the Unique Key be Non Null.
    FCsvCaseInsensitiveComparison: Boolean;
    // CSV Key Uniqueness and Key Comparisons - case insensitive mode if True, else case sensitive.

    FIsFiltered: Boolean; // Filter conditions have been set.

    FEmptyRowStr: string; // A string of just separators (used to add a new empty row)
    FHeaderRow: string; // first row of CSV file.
    FPendingCsvHeaderParse: Boolean; // NEW FEB 2004 WP.
    FTableName: string; // CSV File Name
    FAppendedFieldCount: Integer; // Number of fields not in the file on disk, appended to file as NULLs during import.
    FRecordPos: Integer;
    FRecordSize: Integer;
    FBufferSize: Integer;
    FCursorOpen: Boolean;
    FFilterBuffer: PChar; // used when we implement filtering (later)
    FReadOnly: Boolean;
    FLoadsFromFile: Boolean;
    FHasHeaderRow: Boolean;
    FSavesChanges: Boolean;
    FAutoBackupCount: Integer; // Keep Last N Copies the Old Csv File, updated before each save?
    FInsertBlocked: Boolean; // internal way to block new records but allows editing of existing ones!
    FPostBlocked: Boolean; // internal way to block posting of changes, but allows inserting of new ones!

    { Data record holder }
    FCsvColumns: TJvCsvColumns; // Column information
    FData: TJvCsvRows; // Rows are a Collection of Data pointers.

    { temporary holding space only, for a tstringlist of the file contents }
    FCsvFileAsStrings: TStringList;

    {  event pointers }
    FOnSpecialData: TJvCsvOnSpecialData;
    FOnGetFieldData: TJvCsvOnGetFieldData;
      // Helps to allow you to update the contents of your CSV Data from some other object in memory.
    FOnSetFieldData: TJvCsvOnSetFieldData;
      // Helps to keep some other thing in sync with the contents of a changing CSV file.

    //  Internal Use Only Protected Methods
    // function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer: PChar; virtual;
    procedure CsvRowInit(RowPtr: PCsvRow);

    //NEW and very handy dandy!
    function GetFieldValueAsVariant(CsvColumnData: PCsvColumn; Field: TField; RecordIndex: Integer): Variant;

    // New filtering on cursor (GetRecord advances the cursor past
    // any hidden rows using InternalSkipForward).
    function InternalSkipFiltered(DefaultResult: TGetResult; ForwardBackwardMode: Boolean): TGetResult;

    procedure InternalClearFileStrings;
    function InternalLoadFileStrings: Boolean;
    // Internal methods used by sorting:
    function InternalFieldCompare(Column: PCsvColumn; Left, Right: PCsvRow): Integer;
    function InternalCompare(SortColumns: TArrayOfPCsvColumn; SortColumnCount: Integer;
      Left, Right: PCsvRow; Ascending: Boolean): Integer;

    // key uniqueness needs this:
    function InternalFindByKey(Row: PCsvRow): Integer;

    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data: Pointer' stored in treeviews, etc)
    function GetRowUserData: Pointer;
    procedure SetRowUserData(UserData: Pointer);

    function GetRowTag: Integer;
    procedure SetRowTag(TagValue: Integer);

    // protected TDataSet base METHODS:
    procedure SetTableName(const Value: string); virtual;
    function FieldDefsStored: Boolean; virtual;
    function GetCanModify: Boolean; override; //already virtual!

    // internal calls:
    procedure AppendPlaceHolderCommasToAllRows(Strings: TStrings); // Add placeholders to end of a csv file.
    procedure ProcessCsvHeaderRow;
    procedure ProcessCsvDataRow(const DataRow: string; Index: Integer);
    procedure SetCsvFieldDef(const Value: string);

    { Mandatory VCL TDataSet Overrides - Pure Virtual Methods of Base Class }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;

    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ClearCalcFields(Buffer: PChar); override;

    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PChar; Data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override; // on Insertion???
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; Data: Pointer); override;

    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
    { procedure InternalInsert; override; }{not needed.}

    // Misc methods:
    procedure InternalClose; override;
    // procedure DestroyFields; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;

    function GetFileName: string; // used by InternalOpen, and Flush.

    function IsCursorOpen: Boolean; override;
    { Optional overrides }
    function GetRecordCount: Integer; override;
    function GetRecNo: Integer; override;
    procedure SetRecNo(Value: Integer); override;

    { dataset designer calls these }
    procedure DefChanged(Sender: TObject); override;

    // handling functions for enquoting,dequoting string fields in csv files.
    // handles using the default Excel method which is to double the quotes inside
    // quotes.

    // (rom) inacceptable names
    function _Enquote(const StrVal: string): string; virtual;
    // puts whole string in quotes, escapes embedded separators and quote characters!
    function _Dequote(const StrVal: string): string; virtual; // removes quotes

    property Separator: Char read FSeparator write SetSeparator default ',';
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    // Autoincrement feature: Get next available auto-incremented value for numbered/indexed autoincrementing fields.
    function GetAutoincrement(const FieldName: string): Integer;

    // NEW: COPY FROM ANOTHER TDATASET (TTable, TADOTable, TQuery, or whatever)
    function CopyFromDataset(DataSet: TDataset): Integer;

    // SELECT * FROM TABLE WHERE <fieldname> LIKE <pattern>:
    procedure SetFilter(const FieldName: string; Pattern: string); // Make Rows Visible Only if they match filterString

    // SELECT * FROM TABLE WHERE <fieldname> IS <NULL|NOT NULL>:
    procedure SetFilterOnNull(const FieldName: string; NullFlag: Boolean);


    procedure ClearFilter; // Clear all previous SetFilters, shows All Rows. Refresh screen.
    // (rom) inacceptable name
    procedure _ClearFilter; // Clear Previous Filtering. DOES NOT REFRESH SCREEN.


    procedure CustomFilter(FilterCallback: TJvCustomCsvDataSetFilterFunction); {NEW:APRIL 2004-WP}

    // ----------- THIS IS A DUMMY FUNCTION, DON'T USE IT!:
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;

    //------------

    /// procedure FilteredDeletion(Inverted: Boolean); /// XXX TODO?
    /// procedure DeleteRowsMatchingFilter; /// XXX TODO?
    /// procedure DeleteRowsNotMatchingFilter; /// XXX TODO?

    // this is necessary to make bookmarks work as well:
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    // Additional procedures
    procedure EmptyTable;

      // Tells controls to redraw.
    procedure Refresh;

    // Clone current row/record from one CsvDataSet to another (primitive synchronization/copying ability).
    procedure CloneRow(DataSet: TJvCustomCsvDataSet);

    // TODO: Implement row/record copy from ANY dataset.
    
    // A fast row lookup function specific to this CSV table object.
    function FindByCsvKey(const Key: string): Boolean;

    // Sort the table:
    procedure Sort(const SortFields: string; Ascending: Boolean);

    // All rows have a UserData and UserTag property, these
    // next two functions quickly set all the userdata and usertag
    // values for all rows, which is a good way to set defaults
    // without having to iterate through the dataset.
    procedure SetAllUserData(Data: Pointer);
    procedure SetAllUserTags(TagValue: Integer);

    // The UserData/UserTag properties apply to the row that the
    // cursor is sitting on. Without visibly moving the cursor,
    // its handy to get/set the usertag and Data values.
    function GetUserTag(RecNo: Integer): Integer;
    procedure SetUserTag(RecNo, NewValue: Integer);

    function GetUserData(RecNo: Integer): Pointer;
    procedure SetUserData(RecNo: Integer; NewValue: Pointer);

    function GetCsvHeader: string; // NEW FEB 2004 WP

    {  Additional Public methods }
    procedure OpenWith(Strings: TStrings); virtual;

    procedure AppendWith(Strings: TStrings); virtual;

    { Special declarations }
    // as long as the field names and positions have not changed.
    procedure AssignFromStrings(const Strings: TStrings); virtual; // update String Data directly.
    procedure AssignToStrings(Strings: TStrings); virtual;

    procedure DeleteRows(FromRow, ToRow: Integer); // NEW: Quickly zap a bunch of rows:
    procedure ExportRows(const FileName: string; FromRow, ToRow: Integer); // NEW: Quickly save a bunch of rows:

    procedure ExportCsvFile(const FileName: string); virtual;
      // save out to a file. does NOT keep backups! If file exists, it will be
        // overwritten, and NO backups are made!

    procedure Flush; virtual; // Save CSV file to disk if file has changed and SavesChanges is True.
    // Note: FLUSH will make backup copies if FAutoBackupCount>0!!!

    function GetAsString(const Row, Column: Integer): string; virtual;

    { Row Access as String }
    function GetRowAsString(const Index: Integer): string; virtual;

    function CurrentRowAsString: string; virtual; // Return any row by index, special: -1 means last row NEW.

    // Return any row by index, special: -1 means last row
    function GetColumnsAsString: string; virtual;
    { Row Append one String }
    procedure AppendRowString(const RowAsString: string);
    // Along with GetRowAsString, easy way to copy a dataset to another dataset!

    function IsKeyUnique: Boolean; // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  protected
    property InternalData: TJvCsvRows read FData write FData;
    property AppendedFieldCount: Integer read FAppendedFieldCount;
      // Number of fields not in the file on disk, appended to file as NULLs during import.
      // Per-Record user-Data fields:
      //    Each record can have a pointer (for associating each row with an object)
    property UserData: Pointer read GetRowUserData write SetRowUserData;
      //    Each record can have a tag (Integer) (for help in marking rows as Selected/Unselected or some other
      //    end user task)
    property UserTag: Integer read GetRowTag write SetRowTag;

    property FileName: string read FTableName write SetTableName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Changed: Boolean read FFileDirty write FFileDirty;
    // property DataFileSize: Integer read GetDataFileSize;

     // CSV Table definition properties:
    property CsvFieldDef: string read FCsvFieldDef write SetCsvFieldDef; // Our own "Csv Field Definition String"
    property CsvKeyDef: string read FCsvKeyDef write FCsvKeyDef; // Primary key definition.
    property CsvUniqueKeys: Boolean read FCsvUniqueKeys write FCsvUniqueKeys; // Rows must be unique on the primary key.
    // if HasHeaderRow is True, calidate that it conforms to CvsFieldDef
    property ValidateHeaderRow: Boolean read FValidateHeaderRow write FValidateHeaderRow default True;
    property ExtendedHeaderInfo: Boolean read FExtendedHeaderInfo write FExtendedHeaderInfo;

    property CaseInsensitive: Boolean read FCsvCaseInsensitiveComparison write FCsvCaseInsensitiveComparison;

    // Properties for Automatically Loading/Saving CSV file when Active property is set True/False:
    property LoadsFromFile: Boolean read FLoadsFromFile write FLoadsFromFile default True;
    property AutoBackupCount: Integer read FAutoBackupCount write FAutoBackupCount;
    // >0 means Keep Last N Copies the Old Csv File, updated before each save?

    // Do field definitions "persist"?
    // Ie: do they get stored in DFM Form file along with the component
    property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;

    { value in seconds : to do GMT to EST (ie GMT-5) use value of (-3600*5)
      This is only useful if you use the Hex encoded date-time fields.
    }
    property TimeZoneCorrection: Integer read FTimeZoneCorrection write FTimeZoneCorrection default 0;
    { If False (default) we use the more normal CSV rendering of quotes, which is to double them in
      the csv file, but if this property is True, we use backslash-quote to render quotes in the file,
      which has the side-effect of also requiring all backslashes to themself be escaped by a backslash.
      So filenames would have to be in the form "c:\\directory\\names\\like\\c\\programmers\\do\\it".
      Not recommended behaviour, except when absolutely necessary! }
    property EnquoteBackslash: Boolean read FEnquoteBackslash write FEnquoteBackslash default False;


    { Additional Events }
    property OnSpecialData: TJvCsvOnSpecialData read FOnSpecialData write FOnSpecialData;
    property OnGetFieldData: TJvCsvOnGetFieldData read FOnGetFieldData write FOnGetFieldData;
    property OnSetFieldData: TJvCsvOnSetFieldData read FOnSetFieldData write FOnSetFieldData;

   public
    { these MUST be available at runtime even when the object is of the Custom base class type
      This enables interoperability at design time between non-visual helper components
      and user-derived CsvDataSet descendants }
    property OpenFileName: string read FOpenFileName; // Set in InternalOpen, used elsewhere.
    property FieldDefs stored FieldDefsStored;
    property TableName: string read FTableName; // Another name, albeit read only, for the FileName property!
    property HasHeaderRow: Boolean read FHasHeaderRow write FHasHeaderRow default True;
    property HeaderRow: string read FHeaderRow; // first row of CSV file.
    property SavesChanges: Boolean read FSavesChanges write FSavesChanges default True;
  end;

  // TJvCsvDataSet is just a TJvCustomCsvDataSet with all properties and events exposed:
  TJvCsvDataSet = class(TJvCustomCsvDataSet)
  public
    property TableName;
    property UserData;
    property UserTag;
  published
    property FieldDefs;
    property Active;
    property BufferCount;
    property FileName;
    property ReadOnly;
    property BeforeOpen;
    property AfterOpen;
    property BeforeClose;
    property AfterClose;
    property BeforeInsert;
    property AfterInsert;
    property BeforeEdit;
    property AfterEdit;
    property BeforePost;
    property AfterPost;
    property BeforeCancel;
    property AfterCancel;
    property BeforeDelete;
    property AfterDelete;
    property BeforeScroll;
    property AfterScroll;
    property OnDeleteError;
    property OnEditError;
    property OnCalcFields;
    property AutoCalcFields; // TDataSet property!
    //property MasterFields;
    //property MasterSource;
    property Changed;
    property CsvFieldDef;
    property CsvKeyDef;
    property CsvUniqueKeys;
    property HasHeaderRow;
    property ValidateHeaderRow;
    property ExtendedHeaderInfo;
    property CaseInsensitive;
    property Separator;
    property LoadsFromFile;
    property SavesChanges;
    property AutoBackupCount;
    property StoreDefs;
    property OnSpecialData;
    property OnGetFieldData;
    property OnSetFieldData;
    property TimeZoneCorrection;
    property EnquoteBackslash;
    property HeaderRow;
  end;

{ CSV String Processing Functions }
procedure CsvRowToString(RowItem: PCsvRow; var RowString: string);

{ modified! }
procedure StringToCsvRow(const RowString: string; Separator: Char;
  RowItem: PCsvRow; PermitEscapeSequences, EnquoteBackslash: Boolean);

function CsvRowItemCopy(Source, Dest: PCsvRow; FieldIndex, FieldSize: Integer): Boolean;
procedure SetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: string);
function GetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer): string;
procedure CsvRowSetDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
procedure CsvRowClearDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
function CsvRowGetDirtyBit(Row: PCsvRow; ColumnIndex: Integer): Boolean;
procedure CsvRowSetColumnMarker(Row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
function CsvRowGetColumnMarker(Row: PCsvRow; ColumnIndex: Integer): Integer;

{ Date/Time String decoding functions }
function TimeTHexToDateTime(const HexStr: string; TimeZoneCorrection: Integer): TDateTime;
function TimeTAsciiToDateTime(const AsciiDateStr: string): TDateTime;

{ Date/Time string encoding functions }
function DateTimeToTimeToIsoAscii(ADateTime: TDateTime): string;
function DateTimeToTimeTHex(ADateTime: TDateTime; TimeZoneCorrection: Integer): string;

{ Routine to keep backup copies of old Data files around }
function JvCsvBackupPreviousFiles(const FileName: string; MaxFiles: Integer): Boolean;

//JvCsvWildcardMatch:
// Recursive wildcard (%=AnyString, ?=SingleChar) matching function with
// Boolean sub expressions (|=or, &=and).
function JvCsvWildcardMatch(Data, Pattern: string): Boolean;

implementation

uses
  Forms, Controls,
  {$IFNDEF COMPILER6_UP}
  JvJVCLUtils,
  {$ENDIF COMPILER6_UP}
  JvJCLUtils, JvCsvParse, JvConsts, JvResources;

const
  // These characters cannot be used for separator for various reasons:
  // Either they are used as field type specifiers, break lines or are used to
  // delimit field content
  cInvalidSeparators = [#0, Backspace, Lf, #12, Cr, #39, '"', '\',
    '$', '%', '&', '@', '#', '^', '!', '-'];

var
  // (rom) disabled unused
  // CallCount: Integer = 0;
  AsciiTime_MinValue: array [1..6] of Integer = (1900, 1, 1, 0, 0, 0);
  AsciiTime_MaxValue: array [1..6] of Integer = (3999, 12, 31, 23, 59, 59);
  AsciiTime_ExpectLengths: array [1..6] of Integer = (4, 2, 2, 2, 2, 2);

procedure JvCsvDatabaseError(const TableName, Msg: string);
begin
  // (rom) no OutputDebugString in production code
  {$IFDEF DEBUGINFO_ON}
  OutputDebugString(PChar('JvCsvDatabaseError in ' + TableName + ': ' + Msg));
  {$ENDIF DEBUGINFO_ON}
  raise EJvCsvDataSetError.CreateResFmt(@RsECsvErrFormat, [TableName, Msg]);
end;

// note that file is not being locked!

constructor TJvCustomCsvDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSeparator := ',';

  FInitialWorkingDirectory := GetCurrentDir; // from SysUtils;

  FTempBuffer := AllocMem(MAXLINELENGTH + 1); // AllocMem fills with zeros

  // FRecordSize = size of a csv text buffer and the indexes pointing
  //               into that buffer:

  FRecordSize := SizeOf(TJvCsvRow) - SizeOf(TJvCsvBookmark);

  // FBuffer size includes CSV Text buffer, and the bookmark Data, followed
  // by space for storing the binary form of a calculated-field:

  // initial FBufferSize size: My theory is that we should pick a conservative
  // estimate plus a margin for error:

  FBufferSize := SizeOf(TJvCsvRow) + MaxCalcDataOffset; //;128; {CalcFieldsSize}
  //; // our regular record + calculated field Data.

  FReadOnly := False;
  FCursorOpen := False;
  FRecordPos := ON_BOF_CRACK;
  FLoadsFromFile := True;
  FSavesChanges := True;
  FHasHeaderRow := True;
  FValidateHeaderRow := True;

  { Additional initialization }
  FCsvColumns := TJvCsvColumns.Create;
  FData := TJvCsvRows.Create;
  FData.EnquoteBackslash := FEnquoteBackslash;
end;

destructor TJvCustomCsvDataSet.Destroy;
begin
  InternalClearFileStrings; // delete file strings
  FreeMem(FTempBuffer); // Free the memory we allocated.
  FTempBuffer := nil;

  try
    if FCursorOpen then
      InternalClose;
  except
  end;
  if Assigned(FCsvColumns) then
  begin
    FCsvColumns.Clear;
    FCsvColumns.Free;
  end;
  if Assigned(FData) then
  begin
    FData.Clear;
    FData.Free;
  end;
  inherited Destroy;
end;

// Each ROW Record has an internal Data pointer (similar to the
// user-accessible 'Data: Pointer' stored in treeviews, etc)

function TJvCustomCsvDataSet.GetRowUserData: Pointer;
var
  RecNo: Integer;
begin
  RecNo := GetRecNo;
  Result := FData.GetUserData(RecNo);
end;

procedure TJvCustomCsvDataSet.SetRowUserData(UserData: Pointer);
var
  RecNo: Integer;
begin
  RecNo := GetRecNo;
  FData.SetUserData(RecNo, UserData);
end;

function TJvCustomCsvDataSet.GetRowTag: Integer;
var
  RecNo: Integer;
begin
  RecNo := GetRecNo;
  Result := FData.GetUserTag(RecNo);
end;

procedure TJvCustomCsvDataSet.SetRowTag(TagValue: Integer);
var
  RecNo: Integer;
begin
  RecNo := GetRecNo;
  FData.SetUserTag(RecNo, TagValue);
end;

function _WildcardsMatchBoolOp(const Data, Pattern: string; BoolOp: Char): Boolean;
var
  SubPattern: array [0..20] of string;
  I, Count: Integer;
begin
  Count := StrSplit(Pattern, BoolOp, {Chr(0)=No Quoting} Chr(0), SubPattern, 20);
  if Count > 0 then
  begin
    for I := 0 to Count - 1 do
    begin
      Result := JvCsvWildcardMatch(Data, SubPattern[I]);
      // If ANY OR True return True;
      // if ANY AND False return False;
      if (BoolOp = '|') = Result then
        Exit;
    end;
  end
  else
  begin // split failed...
    Result := False;
    Exit;
  end;
  // if we get here, no short circuit was possible.
  if BoolOp = '|' then
    Result := False // NONE of the OR conditions were met!
  else
    Result := True; // ALL of the AND condition were met!
end;

procedure TJvCustomCsvDataSet.SetAllUserTags(TagValue: Integer);
var
  I: Integer;
begin
  FData.SetUserTag(FData.Count - 1, TagValue);
  for I := 0 to FData.Count - 2 do
    FData.SetUserTag(I, TagValue);
end;

procedure TJvCustomCsvDataSet.SetAllUserData(Data: Pointer);
var
  I: Integer;
begin
  FData.SetUserData(FData.Count - 1, Data); // Optimization. Ensures we only call SetLength ONCE!
  for I := 0 to FData.Count - 2 do
    FData.SetUserData(I, Data);
end;

function TJvCustomCsvDataSet.GetUserTag(RecNo: Integer): Integer;
begin
  Result := FData.GetUserTag(RecNo);
end;

procedure TJvCustomCsvDataSet.SetUserTag(RecNo, NewValue: Integer);
begin
  FData.SetUserTag(RecNo, NewValue);
end;

function TJvCustomCsvDataSet.GetUserData(RecNo: Integer): Pointer;
begin
  Result := FData.GetUserData(RecNo);
end;

procedure TJvCustomCsvDataSet.SetUserData(RecNo: Integer; NewValue: Pointer);
begin
  FData.SetUserData(RecNo, NewValue);
end;

// Recursive wildcard matching function

function JvCsvWildcardMatch(Data, Pattern: string): Boolean;
var
  I: Integer;
  FirstWildcard: Integer;
  DataLength, PatternLength, DataPosition, PatternPosition: Integer;
  FirstBoolCondition: Integer;
begin
  Result := True;
  PatternLength := Length(Pattern);
  if PatternLength = 0 then
    Exit;
  // no Data?
  DataLength := Length(Data);
  if DataLength = 0 then
  begin
    Result := (Pattern = '%') or (Pattern = '');
    Exit; // definitely no match.
  end;
  // replace all '%%' -> '%' (don't put duplicate wildcards in)
  I := 1;
  while I < PatternLength do
    if (Pattern[I] = '%') and (Pattern[I + 1] = '%') then
    begin
      Pattern := Copy(Pattern, 1, I) + Copy(Pattern, I + 2, PatternLength);
      PatternLength := Length(Pattern);
    end
    else
      Inc(I);
  // find any | and split into two or more strings, and run ORs on them
  FirstBoolCondition := Pos('&', Pattern);
  if FirstBoolCondition > 0 then
  begin
    Result := _WildcardsMatchBoolOp(Data, Pattern, '&');
    Exit;
  end;
  FirstBoolCondition := Pos('|', Pattern);
  if FirstBoolCondition > 0 then
  begin
    Result := _WildcardsMatchBoolOp(Data, Pattern, '|');
    Exit;
  end;

  FirstWildcard := Pos('%', Pattern); // wildcards?
  if FirstWildcard = 0 then
    FirstWildcard := Pos('?', Pattern); // other wildcard.

  if FirstWildcard <= 0 then
  begin // no wildcard case.
    if Data = Pattern then
      Result := True
    else
      Result := False;
    Exit; // simple match returns immediately.
  end;
  // wildcard tail?
  if (FirstWildcard = PatternLength) and (Pattern[1] <> '?') then
  begin // prefix match
    if Copy(Data, 1, PatternLength - 1) = Copy(Pattern, 1, PatternLength - 1) then
      Result := True
    else
      Result := False;
    Exit; // tail case is easy!
  end;
  // match literal characters until we hit wildcards,
  // then search for a wildcard resync, which continues
  // recursively.
  Result := True;
  DataPosition := 1;
  PatternPosition := 1;
  while (DataPosition <= DataLength) and (PatternPosition <= PatternLength) do
  begin
    // WILDCARD HANDLER
    if Pattern[PatternPosition] = '?' then
    begin // match any one character or nothing.
      Inc(PatternPosition);
      Inc(DataPosition);
    end
    else
    if Pattern[PatternPosition] = '%' then
    begin
      if PatternPosition = PatternLength then
      begin // last byte!
        Result := True;
        Exit;
      end;
       // Resync after %:
      I := Pos(Pattern[PatternPosition + 1], Data);
      while I > 0 do
      begin // possible resync point!
        Result := JvCsvWildcardMatch(Copy(Data, I, Length(Data)),
          Copy(Pattern, PatternPosition + 1, PatternLength));
        if Result then
          Exit; // found a resync, and rest of strings match
        Data := Copy(Data, I + 1, DataLength);
        DataLength := Length(Data);
        // DataPosition := 0;
        if DataLength = 0 then
        begin
          Result := False;
          Exit;
        end;
        I := Pos(Pattern[PatternPosition + 1], Data);
      end;
      // failed to resync
      Result := False;
      Exit;
    end
    else
    begin // NORMAL CHARACTER
      if Data[DataPosition] <> Pattern[PatternPosition] then
      begin
        Result := False; // failed.
        Exit;
      end;
      Inc(DataPosition);
      Inc(PatternPosition);
    end;
  end;
  if (DataPosition <= DataLength) and (PatternPosition <= PatternLength) then
    Result := False; // there is pattern left over, or Data left over.
end;

// NEW: TJvCustomCsvDataSet.SetFilter
//
// XXX Simplest possible filtering routine. Not very flexible.
// XXX Todo: Make this more flexible.
// XXX Users can also subclass and write their own filter.
// XXX Perhaps a OnFilter event should be provided, and SetCustomFilter
// XXX method would allow us to do a row by row filtering scan, and then
// XXX hide rows that the user sets HideRow := True in the event handler.
// XXX

{ New: Custom Filtering }

procedure TJvCustomCsvDataSet.CustomFilter(FilterCallback: TJvCustomCsvDataSetFilterFunction);
var
  I: Integer;
  PRow: PCsvRow;
begin
  Assert(Assigned(FilterCallback));
  // Now check if field value matches given pattern for this row.
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    Assert(Assigned(PRow));
    // if custom function returns False, hide the row.
    PRow^.Filtered  := not FilterCallback(I);
  end;
  FIsFiltered := True;
  if Active then
    First;
end;


procedure TJvCustomCsvDataSet.SetFilterOnNull(const FieldName: string; NullFlag: Boolean);
var
  I: Integer;
  PRow: PCsvRow;
  FieldRec: PCsvColumn;
  FieldIndex: Integer;
  FieldValue: string;
begin
  FieldRec := FCsvColumns.FindByName(FieldName);

  if not Assigned(FieldRec) then
    Exit;
  FieldIndex := FieldRec^.FPhysical;

  // Now filter out if IsNull matches NullFlag
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    if not PRow^.Filtered then
    begin
      FieldValue := FData.GetARowItem(I, FieldIndex);
      if (Length(FieldValue) > 0) = NullFlag then
        PRow^.Filtered := True;
    end;
  end;
  FIsFiltered := True;
  if Active then
    First;
end;

// Make Rows Visible Only if they match filterString

procedure TJvCustomCsvDataSet.SetFilter(const FieldName: string; Pattern: string);
var
  ValueLen, I: Integer;
  PRow: PCsvRow;
  FieldRec: PCsvColumn;
  FieldIndex: Integer;
  FieldValue: string;
  //stillVisible : Integer;
  //m: TBookmark;
begin
  // m := GetBookmark;
  FieldRec := FCsvColumns.FindByName(FieldName);
  // stillVisible := 0;
  if not Assigned(FieldRec) then
    Exit;
  FieldIndex := FieldRec^.FPhysical;
  ValueLen := Length(Pattern); // if valuelen is zero then we are searching for blank or nulls
  Pattern := UpperCase(Pattern); // make value case insensitive.

  // Now check if field value matches given pattern for this row.
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    if not PRow^.Filtered then
    begin
      FieldValue := FData.GetARowItem(I, FieldIndex);
      if (Length(FieldValue) > 0) and (FieldValue[1] = '"') then
        FieldValue := _Dequote(FieldValue); // remove quotes.
      if ValueLen = 0 then
      begin
        if FieldValue <> '' then // if not empty, hide row.
          PRow^.Filtered := True;
      end
      else
      begin
        FieldValue := UpperCase(FieldValue);
        if JvCsvWildcardMatch(FieldValue, Pattern) then // hide row if not same prefix
        begin
          // Inc(stillVisible)   // count the number that are still visible.
        end
        else
          PRow^.Filtered := True
      end;
    end
  end;
  FIsFiltered := True;
  if Active then
    First;
end;

procedure TJvCustomCsvDataSet._ClearFilter; // Clear Previous Filtering.
var
  I: Integer;
  PRow: PCsvRow;
begin
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    if Assigned(PRow) then
      PRow^.Filtered := False; // clear all filter bits.
  end;
  FIsFiltered := False;
end;

procedure TJvCustomCsvDataSet.ClearFilter; // Clear Previous Filtering.
var
  M: TBookmark;
begin
  M := GetBookmark;
  _ClearFilter;
  // Update screen.
  if Active then
    if Assigned(M) then
      GotoBookmark(M)
    else
      First;
end;

function TJvCustomCsvDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Bookmark <> nil) and (PInteger(Bookmark)^ >= 0) and (PInteger(Bookmark)^ < FData.Count);
end;

function TJvCustomCsvDataSet.AllocRecordBuffer: PChar;
var
  RowPtr: PCsvRow;
begin
  RowPtr := AllocMem(FBufferSize); {SizeOf(TJvCsvRow)}
  //  Trace('AllocRecordBuffer Result=$'+IntToHex(Integer(Pointer(RowPtr)),8));
  Result := PChar(RowPtr);
end;

{ calc fields support }

procedure TJvCustomCsvDataSet.ClearCalcFields(Buffer: PChar);
begin
  // Assumes that our buffer is a TJvCsvRow followed by
  // a dynamically resized buffer used for calculated field
  // storage:
  FillChar(Buffer[SizeOf(TJvCsvRow)], CalcFieldsSize, 0);
end;

{ calc fields support and buffer support }

function TJvCustomCsvDataSet.GetActiveRecordBuffer: PChar;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Result := nil
      else
        Result := ActiveBuffer;
    dsCalcFields:
      Result := CalcBuffer;
    dsFilter:
      Result := FFilterBuffer;
    dsEdit, dsInsert:
      Result := ActiveBuffer;
  else
    Result := nil;
  end;
end;

procedure TJvCustomCsvDataSet.SetCsvFieldDef(const Value: string);
begin
  if FCsvFieldDef <> Value then
  begin
    CheckInactive;
    FCsvFieldDef := Value;
    FHeaderRow := '';
    FieldDefs.Clear; // Clear VCL Database field definitions
    FCsvColumns.Clear; // Clear our own CSV related field Data
    FData.Clear; // Clear out Data
  end;
end;

procedure TJvCustomCsvDataSet.FreeRecordBuffer(var Buffer: PChar);
//var
//  RowPtr: PCsvRow;
begin
  //Trace( 'FreeRecordBuffer '+IntToHex(Integer(Buffer),8) );
// try
  if Buffer <> nil then
    FreeMem(Buffer);
// except
     //Trace( 'FreeRecordBuffer - Exception freeing '+IntToHex(Integer(Buffer),8) );
//  end;
//  //Trace('TJvCustomCsvDataSet.FreeRecordBuffer');

end;

{ called after the record is allocated }

procedure TJvCustomCsvDataSet.InternalInitRecord(Buffer: PChar);
var
  RowPtr: PCsvRow;
begin
  //Trace( 'InternalInitRecord '+IntToHex(Integer(Buffer),8) );

  FillChar(Buffer^, FBufferSize, 0);
  RowPtr := PCsvRow(Buffer); // Zero out the buffer.
  CsvRowInit(RowPtr);
end;

// CsvRowInit
//
// Internal handy dandy function to set up a new csv row.
// which is intially full of just commas.
//

procedure TJvCustomCsvDataSet.CsvRowInit(RowPtr: PCsvRow);
var
  I: Integer;
  ColCount: Integer;
begin
  RowPtr^.Index := -1; // Not Yet Indexed
  RowPtr^.IsDirty := False;
  RowPtr^.Bookmark.Flag := bfEOF;
  RowPtr^.Bookmark.Data := ON_BOF_CRACK; // no index into FData yet.
  CsvRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);

  ColCount := FCsvColumns.Count;
  if ColCount <= 0 then
    ColCount := 10;

  for I := 1 to ColCount do
  begin // create an empty line of just commas
    if I < ColCount then
      RowPtr^.Text[I - 1] := FSeparator
    else
      RowPtr^.Text[I - 1] := Chr(0);
    RowPtr^.Text[I] := Chr(0);
    CsvRowSetColumnMarker(RowPtr, {column} I - 1, {marker value} I - 1);
    CsvRowSetColumnMarker(RowPtr, {column} I, {marker value} COLUMN_ENDMARKER);
  end;
end;

function TJvCustomCsvDataSet.IsKeyUnique: Boolean;
  // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
begin
  Result := False; // not yet implemented! XXX
end;


function TJvCustomCsvDataSet.GetFieldValueAsVariant(CsvColumnData: PCsvColumn;
  Field: TField; RecordIndex: Integer): Variant;
var
  RowPtr: PCsvRow;
  {ActiveRowPtr: PCsvRow;}
  TempString: string;
  PhysicalLocation: Integer;
  L: Integer;
begin
  Assert(Assigned(FCsvColumns));

  if not Assigned(CsvColumnData) then
  begin
    JvCsvDatabaseError(FTableName, Format(RsEUnableToLocateCSVFileInfo, [Field.Name]));
    Exit;
  end;

  PhysicalLocation := CsvColumnData^.FPhysical;

  if (PhysicalLocation < 0) and FPendingCsvHeaderParse then
  begin
    FPendingCsvHeaderParse := False;
    ProcessCsvHeaderRow;
    PhysicalLocation := CsvColumnData^.FPhysical;
  end;

  if PhysicalLocation < 0 then
  begin
    JvCsvDatabaseError(FTableName, Format(RsEPhysicalLocationOfCSVField, [Field.FieldName]));
    Exit;
  end;

  RowPtr := FData[RecordIndex];

  TempString := GetCsvRowItem(RowPtr, PhysicalLocation);

  // Strip quotes first!
  if Field.DataType = ftString then
  begin
    L := Length(TempString);
    if L >= 2 then
      if (TempString[1] = '"') and (TempString[L] = '"') then
        TempString := _Dequote(TempString); // quoted string!
  end;

  try
    case Field.DataType of
      ftString:
        Result := TempString;
      ftInteger:
        Result := StrToInt(TempString);
      ftFloat:
        Result := StrToFloatUS(TempString);
      ftBoolean:
        if StrToIntDef(TempString, 0) <> 0 then
          Result := True
        else
          Result := False;
      ftDateTime:
         { one of three different datetime formats}
         if Length(TempString) > 0 then
           case CsvColumnData^.FFlag of
             jcsvAsciiDateTime:
               Result := TimeTAsciiToDateTime(TempString);
             jcsvGMTDateTime:
               Result := TimeTHexToDateTime(TempString,0);
             jcsvTZDateTime:
               Result := TimeTHexToDateTime(TempString, FTimeZoneCorrection);
           end;
    end;
  except
    Result := Unassigned; // No value.
  end;
end;

// Auto-increment

function TJvCustomCsvDataSet.GetAutoincrement(const FieldName: string): Integer;
var
  RecIndex: Integer;
  FieldLookup: TField;
  CsvColumnData: PCsvColumn;
  Max, Value: Integer;
  RowPtr: PCsvRow;
begin
  Result := -1; // failed.
  FieldLookup := FieldByName(FieldName);
  if FieldLookup.DataType <> ftInteger then
      Exit; // failed. Can only auto increment on integer fields!

  if not Assigned(FieldLookup) then
      Exit; //failed.

  CsvColumnData := FCsvColumns.FindByFieldNo(FieldLookup.FieldNo);
  Max := -1;
  for RecIndex := 0 to Self.FData.Count - 1 do
    try
       // skip filtered rows:
       RowPtr := FData[RecIndex];
       Assert(Assigned(RowPtr)); // FData should never contain nils!
       if RowPtr^.Filtered then
            Continue; // skip filtered row!

      Value := GetFieldValueAsVariant(CsvColumnData, FieldLookup, RecIndex);
      if Value > Max then
          Max := Value; // keep maximum.
    except
      on E: EVariantError do
        Exit; // failed.
    end;
  if Max < 0 then
    Result := 0 // autoincrement starts at zero
  else
    Result := Max + 1; // count upwards.
end;


// XXX TODO: REMOVE HARD CODED LIMIT OF 20 FIELDS SEARCHABLE!!!
function TJvCustomCsvDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean; // override;
  // Options is    [loCaseInsensitive]
  //              or [loPartialKey]
  //              or [loPartialKey,loCaseInsensitive]
  //              or [] {none}
var
  KeyFieldArray: array [0..20] of string;
  FieldLookup: array [0..20] of TField;
  CsvColumnData: array [0..20] of PCsvColumn;
  FieldIndex: array [0..20] of Integer;
  RecIndex, I, Lo, Hi, Count, VarCount: Integer;
  Value: Variant;
  MatchCount: Integer;
  StrValueA, StrValueB: string;
begin
  Result := False;
  if not Active then
    Exit;
  if Pos(',', KeyFields) > 0 then
    Count := StrSplit(KeyFields, ',', Chr(0), KeyFieldArray, 20)
  else
    Count := StrSplit(KeyFields, ';', Chr(0), KeyFieldArray, 20);

  if not ((VarType(KeyValues) and VarArray) > 0) then
    Exit;
  Lo := VarArrayLowBound(KeyValues, 1);
  Hi := VarArrayHighBound(KeyValues, 1);
  VarCount := (Hi - Lo) + 1;
  if VarCount <> Count then
    Exit;
  if Count = 0 then
    Exit;
  if KeyFieldArray[0] = '' then
    Exit;
  for I := 0 to 20 do
  begin
    if I < Count then
    begin
      FieldLookup[I] := FieldByName(KeyFieldArray[I]);
      CsvColumnData[I] := FCsvColumns.FindByFieldNo(FieldLookup[I].FieldNo);
      if not Assigned(FieldLookup[I]) then
        Exit;
      FieldIndex[I] := FieldLookup[I].Index;
    end
    else
    begin
      FieldLookup[I] := nil;
      FieldIndex[I] := -1;
    end;
  end;

  // Now search
  // First;
  for RecIndex := 0 to Self.FData.Count - 1 do
  begin
    MatchCount := 0;
    for I := 0 to Count - 1 do
    begin
      Value := GetFieldValueAsVariant(CsvColumnData[I], FieldLookup[I], RecIndex);
      if Value = KeyValues[I + Lo] then
        Inc(MatchCount)
      else
      if Options <> [] then
      begin
        if VarIsStr(Value) then
        begin
          StrValueA := Value;
          StrValueB := KeyValues[I + Lo];
          if loCaseInsensitive in Options then
          begin
            StrValueA := UpperCase(StrValueA);
            StrValueB := UpperCase(StrValueB);
          end;
          if StrValueA = StrValueB then
            Inc(MatchCount)
          else
          begin
            if loPartialKey in Options then
              if Pos(StrValueB, StrValueA) = 1 then
                Inc(MatchCount);
          end;
        end;
      end;
    end;
    if MatchCount = Count then
    begin
      RecNo := RecIndex; // Move cursor position.
      Result := True;
      Exit;
    end;
   // Next;
  end;
end;

function TJvCustomCsvDataSet.InternalSkipFiltered(DefaultResult: TGetResult;
  ForwardBackwardMode: Boolean): TGetResult;
var
  LimitReached: Boolean;
  RowPtr: PCsvRow;
begin
  Result := DefaultResult;
  if FRecordPos < 0 then
    Exit;
  LimitReached := False; // hit BOF or EOF?
  while not LimitReached do
  begin
    { no skippage required }
    RowPtr := PCsvRow(FData.GetRowPtr(FRecordPos));
    if not RowPtr^.Filtered then
      Exit;
    { skippage ensues }
    if ForwardBackwardMode then
    begin // ForwardSkip mode
      Inc(FRecordPos);
      if FRecordPos >= FData.Count then
      begin
        FRecordPos := ON_EOF_CRACK;
        Result := grEOF;
        Exit;
      end;
    end
    else
    begin // BackwardSkip mode
      Dec(FRecordPos);
      if FRecordPos < 0 then
      begin // hit BOF_CRACK
        FRecordPos := ON_BOF_CRACK;
        Result := grBOF;
        Exit;
      end;
    end;
  end;
end;


function TJvCustomCsvDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowPtr: PCsvRow;
begin
  Buffer[0] := Chr(0);
  Result := grEOF;
  if FData.Count < 1 then
  begin
    //Trace(' GetRecord - called when Data buffer empty.');
    Exit;
  end;
  case GetMode of
    gmPrior:
      begin
        //Trace(' GetRecord( Buffer, gmPrior, DoCheck)');
        if FRecordPos = ON_BOF_CRACK then
          Result := grBOF
        else
        if FRecordPos = ON_EOF_CRACK then
        begin
          FRecordPos := FData.Count - 1;

          // NEW FILTERING
          if FIsFiltered then
            Result := InternalSkipFiltered(grOK, False) // skipping backwards.
          else
            Result := grOK;
        end
        else
        if FRecordPos > 0 then
        begin
          Dec(FRecordPos);

          // NEW FILTERING
          if FIsFiltered then
            Result := InternalSkipFiltered(grOK, False) // skipping backwards.
          else
            Result := grOK;
        end
        else
          Result := grBOF;
      end;
    gmCurrent:
      begin
         //Trace(' GetRecord( Buffer, gmCurrent, DoCheck)');
        if FRecordPos < 0 then // BOF Crack or EOF Crack?
          Result := grError
        else
          Result := grOK;

        // NEW FILTERING
        if FIsFiltered then
          Result := InternalSkipFiltered(Result, True); // skipping forwards.
      end;
    gmNext:
      begin
         //Trace(' GetRecord( Buffer, gmNext, DoCheck)');
        if FRecordPos = ON_EOF_CRACK then
          Result := grEOF
        else
        begin
          Inc(FRecordPos);

          if FRecordPos >= FData.Count then
          begin
            FRecordPos := ON_EOF_CRACK;
            Result := grEOF
          end
          else
          begin
            // NEW FILTERING
            if FIsFiltered then
              Result := InternalSkipFiltered(grOK, True) // skipping forwards.
            else
              Result := grOK;
          end;
        end;
      end;
  else
    JvCsvDatabaseError(FTableName, RsEGetMode);
  end;

  if Result = grOK then
  begin
    //Trace( ' GetRecord FRecordPos='+IntToStr(FRecordPos)+'Result=grOk' );
    try
      { get a record into a buffer }
      RowPtr := PCsvRow(Buffer); // Cast to a Row Data Structure to our own type.
      Move(FData.GetRowPtr(FRecordPos)^, RowPtr^, SizeOf(TJvCsvRow));
      RowPtr^.Bookmark.Flag := bfCurrent;
      RowPtr^.Bookmark.Data := FRecordPos;

      // Update calculated fields for this row:
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);
    except
      on E: EJvCsvDataSetError do
        raise; // pass our error through.
      on E: Exception do
        JvCsvDatabaseError(FTableName, Format(RsEProblemReadingRow, [FRecordPos]) +' ' + E.Message);
    end;
  end
  else
  begin
    // fudge: Get bookmark into a record for BOF and EOF records:
    { if RowPtr <> NIL then
        RowPtr^.bookmark.Data := FRecordPos;}

    if (Result = grError) and DoCheck then
      JvCsvDatabaseError(FTableName, RsENoRecord);
  end;

//    if (Result = grError) then
          //Trace(' GetRecord Result = grError');
//    if (Result = grEof) then
          //Trace(' GetRecord Result = grEof');
//     if (Result = grBof) then
          //Trace(' GetRecord Result = grBof');
end;

// puts whole string in quotes, escapes embedded commas and quote characters!
// Can optionally deal with newlines also.

function TJvCustomCsvDataSet._Enquote(const StrVal: string): string;
var
  S: string;
  I, L: Integer;
  Ch: Char;
  LocalEnquoteBackslash: Boolean;
begin
  LocalEnquoteBackslash := FEnquoteBackslash; // can force on, or let it turn on automatically.

  if Pos(StrVal, Cr) > 0 then // we are going to need to enquote the backslashes
    LocalEnquoteBackslash := True; // absolutely need it in just this case.
  if Pos(StrVal, Lf) > 0 then
    LocalEnquoteBackslash := True; // absolutely need it in just this case.

  S := '"';
  L := Length(StrVal);
  for I := 1 to L do
  begin
    Ch := StrVal[I];
    if Ch = Cr then
      // slighlty unstandard csv behavior, hopefully transparently interoperable with other apps that read CSVs
      S := S + '\r'
    else
    if Ch = Lf then // replace linefeed with \n. slightly nonstandard csv behavior.
      S := S + '\n'
    else
    if LocalEnquoteBackslash and (Ch = '\') then
    begin // it would be ambiguous not to escape this in this case!
      S := S + '\\';
      FEnquoteBackslash := True; // XXX This is a lurking bug. Some day we'll get bit by it.
    end
    else
    if Ch = '"' then // always escape quotes by doubling them, since this is standard CSV behaviour
      S := S + '""'
    else
    if Ord(Ch) >= 32 then // strip any other low-ascii-unprintables
      S := S + Ch;
  end;
  S := S + '"'; // end quote.
  Result := S;
end;

function TJvCustomCsvDataSet.GetRecordSize: Word;
begin
 // In create:
 //    FRecordSize := SizeOf(TJvCsvRow) - SizeOf(TJvCsvBookmark);
  Result := FRecordSize;
end;

procedure TJvCustomCsvDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RowPtr: PCsvRow;
  NewVal: string;
  CP, PhysicalLocation: Integer;
  PDestination: PChar;
  CsvColumnData: PCsvColumn;
  DT: TDateTime;
begin
  //Trace( 'SetFieldData '+Field.FieldName );
  PDestination := GetActiveRecordBuffer;
  RowPtr := PCsvRow(PDestination);

  // Dynamic CSV Column Ordering: If we didn't start by
  // assigning column orders when we opened the table,
  // we've now GOT to assume a physical ordering:
  if FHeaderRow = '' then
  begin
    FHeaderRow := GetColumnsAsString;
    ProcessCsvHeaderRow; // process FHeaderRow
  end;

  // If this is a calculated field or lookup field then...
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    if (Field.Offset < 0) or (Field.Offset + Field.DataSize > MaxCalcDataOffset) then
    begin
      // (rom) no OutputDebugString in production code
      {$IFDEF DEBUGINFO_ON}
      OutputDebugString(PChar('JvCsvData.pas: ' + Name + '.SetFieldData(Field=' +
        Field.FieldName + ',...): Invalid field.Offset in Calculated or Lookup field. '));
      {$ENDIF DEBUGINFO_ON}
      Exit;
    end;
    Inc(PDestination, SizeOf(TJvCsvRow) + Field.Offset);
    PDestination[0] := Char(Ord(Buffer <> nil));
    if PDestination[0] <> #0 then
      CopyMemory(@PDestination[1], Buffer, Field.DataSize);
    //Result := True; {there is no return value, oops}
    Exit;
  end;

  // If we get here, we are dealing with a physical record:

  // Set a field Data, taking the physical to logical ordering translation into
  // account:
  CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CsvColumnData) then
    Exit;

  PhysicalLocation := CsvColumnData^.FPhysical;
  // ----- BUG FIX FEB 2004 WP (Location #1 of 2)
  if (PhysicalLocation < 0) and FPendingCsvHeaderParse then
  begin
    FPendingCsvHeaderParse := False; // Just-in-time-CSV-header-parsing fixes a long standing bug.
    ProcessCsvHeaderRow;
    PhysicalLocation := CsvColumnData^.FPhysical;
  end;
  // ----- end

  if PhysicalLocation < 0 then
    Exit;

  if Buffer = nil then
    NewVal := ''
  else
    case Field.DataType of
      ftString:
        begin
            // Copy 0 to Field.Size bytes into NewVal (delphi String)
          if PChar(Buffer)[0] = Chr(0) then
            CP := -1
          else
            for CP := 1 to Field.Size - 1 do
              if PChar(Buffer)[CP] = Chr(0) then
                Break;
          if CP > Field.Size - 1 then
            CP := Field.Size - 1;
          NewVal := Copy(PChar(Buffer), 1, CP + 1);
          //----------------------------------------------------------------------------------------------------
          // NEW RULE: If user displayed value contains a comma, a backslash, or a double quote character
          // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
          // and backslashes preceded by a backslash character.
          //----------------------------------------------------------------------------------------------------
          if (Pos(Separator, NewVal) > 0) or  (Pos(Cr, NewVal) > 0) or
            (Pos(Lf, NewVal) > 0) or (Pos('"', NewVal) > 0) or
            ((Pos('\', NewVal) > 0) and FEnquoteBackslash) then
            NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
        end;
      ftInteger:
        NewVal := IntToStr(PInteger(Buffer)^);
      ftFloat:
        NewVal := FloatToStr(PDouble(Buffer)^);
      ftBoolean:
        NewVal := IntToStr(Ord(PWordBool(Buffer)^)); // bugfix May 26, 2003 - WP
      // There are two ways of handling date and time:
      ftDateTime:
        case CsvColumnData^.FFlag of
          // Localized time in Ascii
          jcsvAsciiDateTime:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
              NewVal := DateTimeToTimeToIsoAscii(DT);
              //OutputDebugString(PChar('date '+NewVal));
            end;
          // GMT Times are stored in HEX
          jcsvGMTDateTime:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
              NewVal := DateTimeToTimeTHex(DT, 0);
            end;
          jcsvTZDateTime: // Move a GMT time into a timezone:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double(Buffer^)));
              NewVal := DateTimeToTimeTHex(DT, FTimeZoneCorrection);
            end;
        else
          JvCsvDatabaseError(FTableName, RsETimeTConvError);
        end;
    else
      JvCsvDatabaseError(FTableName, RsEFieldTypeNotHandled);
    end;

  // Set new Data value (NewVal = String)
  SetCsvRowItem(RowPtr, PhysicalLocation, NewVal);
  if Assigned(FOnSetFieldData) and (RowPtr^.Index >= 0) then
    FOnSetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName, NewVal);

  // Set a dirty bit so we remember to write this later:
  CsvRowSetDirtyBit(PCsvRow(PDestination), PhysicalLocation);

  // Set the file-wide dirty bit:
  FFileDirty := True;

  // Notify controls of a field change:
  DataEvent(deFieldChange, Longint(Field));
end;

// Removes first and last character of the string (assumes they are quotes,
// to be called byGetFieldData only!)

function TJvCustomCsvDataSet._Dequote(const StrVal: string): string;
var
  S: string;
  I, L: Integer;
  Ch: Char;
  SkipFlag: Boolean;
begin
  L := Length(StrVal);
  SkipFlag := False;
  S := '';
  if Length(StrVal) < 2 then
  begin
    Result := S;
    Exit;
  end;

  for I := 2 to L - 1 do
  begin
    Ch := StrVal[I];
    if FEnquoteBackslash then
    begin
      if (not SkipFlag) and (Ch = '\') then
      begin
        SkipFlag := True; // whatever is after the backslash is an escaped literal character.
        Continue;
      end
      else
      begin
        // backslashed escape codes for carriage return, linefeed.
        if SkipFlag then
        begin
          case Ch of
          'r':
            Ch := Cr;
          's':
            Ch := #32;
          't':
            Ch := Tab;
          'n':
            Ch := Lf;
          end;
        end;
        SkipFlag := False;
      end;
    end
    else
    begin
      if (not SkipFlag) and (Ch = '"') and (I < (L - 1)) and (StrVal[I + 1] = '"') then
      begin
        SkipFlag := True;
        Continue; // skip first of the doubled quote characters.
      end
      else
        SkipFlag := False;
    end;
    S := S + Ch;
  end;
  Result := S;
end;

// Tells controls to redraw.

procedure TJvCustomCsvDataSet.Refresh;
var
  M: TBookmark;
begin
  if State <> dsBrowse then
    Exit;
  DisableControls;
  try
    M := GetBookmark; // This appears a bit silly but it works very well.
    First; // Redraws all controls once to relocate to top.
    GotoBookmark(M); // Go back where we were. This could Result in some odd scrolling behaviour but I haven't seen it yet.
  finally
    EnableControls;
  end;
end;

function TJvCustomCsvDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RowPtr: PCsvRow;
  {ActiveRowPtr: PCsvRow;}
  PSource: PChar;
  UserString, TempString: string;
  PhysicalLocation: Integer;
  CsvColumnData: PCsvColumn;
  ADateTime: TDateTime;
  L: Integer;
  ts: TTimeStamp;
begin
  Result := False;
  //Trace( 'GetFieldData '+Field.FieldName );

  if not FCursorOpen then
    Exit;
  if Field = nil then
    Exit;

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if FHeaderRow = '' then
  begin
    FHeaderRow := GetColumnsAsString;
    ProcessCsvHeaderRow; // process FHeaderRow
  end;

  PSource := GetActiveRecordBuffer; // This should not be nil EXCEPT if table is Empty or Closed.
  if PSource = nil then
  begin
    // (rom) no OutputDebugString in production code
    {$IFDEF DEBUGINFO_ON}
    if Self.FData.Count > 0 then
      OutputDebugString( 'TJvCustomCsvDataSet.GetFieldData: GetActiveRecordBuffer is nil but table is not empty. (Internal Fault Condition).');
    {$ENDIF DEBUGINFO_ON}
    Exit;
  end;

  //------------------------------------------------------------------------
  // Calculated and Lookup Field Handling
  //
  // direct memory copy into calculated field or lookup field Data area
  //------------------------------------------------------------------------
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    if (Field.Offset < 0) or (Field.Offset + Field.DataSize > MaxCalcDataOffset) then
    begin
      // (rom) no OutputDebugString in production code
      {$IFDEF DEBUGINFO_ON}
      OutputDebugString('JvCsvData.GetFieldData: Invalid field.Offset in Calculated or Lookup field.');
      {$ENDIF DEBUGINFO_ON}
      Exit;
    end;
    //AutoCalcFields?
    //{$IFDEF DEBUGINFO_ON}
    //OutputDebugString(PChar('GetFieldData ' + Field.FieldName + ' Buffer=' + IntToHex(Integer(Buffer), 8)));
    //{$ENDIF DEBUGINFO_ON}

    Inc(PSource, SizeOf(TJvCsvRow) + Field.Offset);
    if Buffer = nil then
    begin
      // NULL CHECK MEANS THAT SOMEONE IS ASKING IF THIS FIELD HAS A VALUE. RETURN True.
      Result := (Field.DataSize > 0); // Yes, we could read this field if you asked us to!
      Exit;
    end;

    if Field.DataSize <= 0 then
      PChar(Buffer)[0] := Chr(0)
    else // Get the field Data from the buffer:
      CopyMemory(Buffer, @PSource[1], Field.DataSize);

    if (Buffer <> nil) and (Field.DataSize > 0) then
       Result := True;
    Exit;
  end;

  //------------------------------------------------------------------------
  // If we get here we must be dealing with a real column of Data
  // that is part of the CSV file rather than a calculated or lookup
  // field that is just in internal-memory:
  //------------------------------------------------------------------------

  CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CsvColumnData) then
  begin
    JvCsvDatabaseError(FTableName, Format(RsEUnableToLocateCSVFileInfo, [Field.Name]));
    Exit;
  end;
  PhysicalLocation := CsvColumnData^.FPhysical;
  // ----- BUG FIX FEB 2004 WP (Location #2 of 2)
  if (PhysicalLocation < 0) and FPendingCsvHeaderParse then
  begin
    FPendingCsvHeaderParse := False; // Just In Time!
    ProcessCsvHeaderRow;
    PhysicalLocation := CsvColumnData^.FPhysical;
  end;
  // ---

  if PhysicalLocation < 0 then
  begin // does it really exist in the CSV Row?
    JvCsvDatabaseError(FTableName, Format(RsEPhysicalLocationOfCSVField, [Field.FieldName]));
    Exit;
  end;

  //------------------------------------------------------------------------
  // All items in the CSV table are natively stored as strings. Note that
  // an empty string is considered to be a NULL if the field type is anything
  // other than a ftString. There are no NULLs in ftString fields because
  // a CSV file can store an empty string but has no way of indicating a NULL.
  //------------------------------------------------------------------------

  RowPtr := PCsvRow(PSource);
  if Field.Offset + Field.DataSize > MAXLINELENGTH then
  begin
    // SIMPLE WORKAROUND: MAKES FIELDS NON FUNCTIONAL BUT DOES NOT CRASH SYSTEM.
    // (rom) no OutputDebugString in production code
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString('JvCsvData.GetFieldData: Invalid field.Offset in Data field.');
    {$ENDIF DEBUGINFO_ON}
    Exit;
  end;

  TempString := GetCsvRowItem(RowPtr, PhysicalLocation);

  // Strip quotes first!
  if Field.DataType = ftString then
  begin
    L := Length(TempString);
    if L >= 2 then
      if (TempString[1] = '"') and (TempString[L] = '"') then
        // quoted string!
        TempString := _Dequote(TempString);
  end;

  // Custom Get Method allows us to create a "Virtual DataSet" where the Data
  // in the CSV rows is really just a mirror which can be updated when displayed
  // but which we really are fetching from somewhere else.
  if Assigned(FOnGetFieldData) and (RowPtr^.Index >= 0) and (not RowPtr^.RecursionFlag) then
  begin
    RowPtr^.RecursionFlag := True;
    UserString := TempString;
    FOnGetFieldData(Self, FData.GetUserTag(RowPtr^.Index),
      FData.GetUserData(RowPtr^.Index), Field.FieldName, UserString);
    if UserString <> TempString then
    begin
      // Write changed value back to row:
      SetCsvRowItem(RowPtr, PhysicalLocation, UserString);
      TempString := UserString;
      // Notify controls of a field change:
      //DataEvent(deFieldChange, LongInt(Field));
      // XXX Doesn't do what I needed. left here commented out
      // in case I ever go back and try to get something like this
      // working again.
    end;
    RowPtr^.RecursionFlag := False;
  end;

  // NULL:  There are no "Real" NULLS in an ASCII flat file, however for anything
  // other than a string field, we will return "NULL" to indicate there is an
  // empty string in the field.
  if Field.DataType <> ftString then
    if TempString = '' then
      Exit; // NULL field.

  { If buffer is nil, then we are being asked to do a null check only.}
  if Buffer = nil then
  begin
    if TempString = '' then
      Result := False
    else
      Result := True;
    Exit; { cannot actually copy Data into nil buffer, so returns now. }
  end;

  //------------------------------------------------------------------------
  // If we get here Buffer must NOT be nil. Now we handle
  // some csv to TField conversions:
  //------------------------------------------------------------------------
  try
    case Field.DataType of
      // Basic string copy, convert from String to fixed-length
      // buffer, padded with NUL i.e. Chr(0):
      ftString:
        begin
          FillChar(FTempBuffer^, Field.Size + 1, 0);
          StrCopy(FTempBuffer, PChar(TempString)); // we copy in the Data portion
          Move(FTempBuffer^, Buffer^, Field.Size + 1); // Buffer^ is now zero padded.
        end;
      // Standard Integer conversion:
      ftInteger:
        PInteger(Buffer)^ := StrToInt(TempString);
      // Standard Double-precision Float conversion:
      ftFloat:
        PDouble(Buffer)^ := StrToFloatUS(TempString);
      ftBoolean:
        if TempString = '' then
          PInteger(Buffer)^ := 0
        else
        if StrToIntDef(TempString, 0) <> 0 then
          PWordBool(Buffer)^ := True // bugfix May 26, 2003 - WP
        else
          PWordBool(Buffer)^ := False; // bugfix May 26, 2003 - WP
      ftDateTime:
        case CsvColumnData^.FFlag of
          // Ascii Date 1999/03/05 08:23:15
          jcsvAsciiDateTime:
            begin
              ADateTime := TimeTAsciiToDateTime(TempString);
              if ADateTime <= 1.0 then
              begin
                Result := False; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(ADateTime);
              if (ts.Time = 0) and (ts.Date = 0) then
              begin
                // (rom) no OutputDebugString in production code
                {$IFDEF DEBUGINFO_ON}
                OutputDebugString('DateTimeToTimeStamp internal failure.');
                {$ENDIF DEBUGINFO_ON}
                Exit;
              end;
                // XXX Delphi Weirdness Ahead.  Read docs before you try to
                // understand this. We want to store 8 bytes at Buffer^, this
                // is how we do it.
              Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(ADateTime));
            end;
          // GMT Times are Stored in HEX:
          jcsvGMTDateTime:
            Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString, 0)));
          // Move GMT into a Timezone:
          jcsvTZDateTime:
            Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,
              FTimeZoneCorrection)));
        else
          JvCsvDatabaseError(FTableName, RsETimeTConvError);
        end;
    else // not a valid ftXXXX type for this TDataSet descendant!?
      JvCsvDatabaseError(FTableName, RsEFieldTypeNotHandled);
    end
  except
    on E: EConvertError do
    begin
      Result := False; // return a NULL.
      Exit;
    end;
  end;
  // All is Well.
  Result := True;
end;

// Our bookmark Data is a pointer to a PCsvData

procedure TJvCustomCsvDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
begin
// I := PCsvRow(Buffer)^.bookmark.Data;
  PInteger(Data)^ := PCsvRow(Buffer)^.Bookmark.Data;
end;

function TJvCustomCsvDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PCsvRow(Buffer)^.Bookmark.Flag;
end;

// nobody mentioned that I needed this to be overloaded, but I only found
// out when I found that DBGrid and other controls that compare bookmarks
// won't function if you don't provide a non-default implementation of this.

function TJvCustomCsvDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  V1, V2: Integer;
begin
  // (rom) fixed implementation.
  V1 := -MaxInt;
  V2 := -MaxInt;
  if Bookmark1 <> nil then
    V1 := PInteger(Bookmark1)^;
  if Bookmark2 <> nil then
    V2 := PInteger(Bookmark2)^;
  Result := Bookmark_Eql;
  if V1 < V2 then
    Result := Bookmark_Less
  else
  if V1 > V2 then
    Result := Bookmark_Gtr;
end;

procedure TJvCustomCsvDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCsvRow(Buffer)^.Bookmark.Flag := Value;
end;

procedure TJvCustomCsvDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  {Bookmark is just pointer to Integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCustomCsvDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FRecordPos := PCsvRow(Buffer)^.Bookmark.Data; // Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:

procedure TJvCustomCsvDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PCsvRow(Buffer)^.Bookmark.Data := PInteger(Data)^;
end;

procedure TJvCustomCsvDataSet.InternalFirst;
begin
//  Eof := False;
  FRecordPos := ON_BOF_CRACK;
end;

// CsvFieldDef:
//
// A property of our Data Set called CsvFieldDef is treated as
// declaration of the fields in the CSV table.
//
//   <coldef>,<coldef>,...,<coldef>
//   <coldef> = columname:<Data-type-character><size>
//
// See top of file!

procedure TJvCustomCsvDataSet.InternalInitFieldDefs;
var
  CsvFieldRec: TJvCsvRow; //record type.
  CsvFieldOption: string;
  CsvFieldName: string;
  PCsvFieldDef: PCsvColumn;
  I, ColNum, Pos1: Integer;
  // field options:
  FieldTypeChar: Char;
  VclFieldType: TFieldType;
  FieldLen: Integer;
  FieldType: TJvCsvColumnFlag;
  aCsvFieldDef: string;
  CsvKeys: array of string;
//  FDef: TFieldDef;
begin
//  FFieldsInitialized := True;
  FieldType := jcsvString;
  VclFieldType := ftString;

  // create FieldDefs which map to each field in the Data record
  FieldDefs.Clear; // Clear VCL Database field definitions
  FCsvColumns.Clear; // Clear our own CSV related field Data

  aCsvFieldDef := CsvFieldDef;
  if aCsvFieldDef = '' then
  begin
    if FHasHeaderRow and InternalLoadFileStrings then
      aCsvFieldDef := FCsvFileAsStrings[0];
    if ExtendedHeaderInfo then
      CsvFieldDef := aCsvFieldDef;
  end;

  if Length(aCsvFieldDef) > 0 then
  begin
    StringToCsvRow(aCsvFieldDef, ',', @CsvFieldRec, False, False);

    ColNum := 0;
    while CsvRowGetColumnMarker(@CsvFieldRec, ColNum) <> COLUMN_ENDMARKER do
    begin
      FieldLen := 80; // default.
      CsvFieldOption := GetCsvRowItem(@CsvFieldRec, ColNum); // get a string in the format COLUMNAME:Options

       // Look for Colon or Semicolon:
      Pos1 := Pos(':', CsvFieldOption);
      if Pos1 <= 0 then
        Pos1 := Pos(';', CsvFieldOption);

      if Pos1 <= 0 then
      begin
        CsvFieldName := CsvFieldOption;
        CsvFieldOption := '$';
        FieldTypeChar := '$';
      end
      else
      begin
        // extract field name:
        CsvFieldName := Copy(CsvFieldOption, 1, Pos1 - 1);
        // If character after the colon is a symbol character, grab
        // it, otherwise default to '$'.
        if Ord(CsvFieldOption[Pos1 + 1]) < Ord('A') then
        begin
          FieldTypeChar := CsvFieldOption[Pos1 + 1];
          CsvFieldOption := Copy(CsvFieldOption, Pos1 + 2, 80);
        end
        else
        begin
          FieldTypeChar := '$';
          CsvFieldOption := Copy(CsvFieldOption, Pos1 + 1, 80);
        end;
        FieldLen := StrToIntDef(CsvFieldOption, DEFAULT_CSV_STR_FIELD);
      end;
      case FieldTypeChar of
        '$':
          begin // $=string
            VclFieldType := ftString;
            FieldType := jcsvString;
          end;
        '%':
          begin // %=Integervalue
            VclFieldType := ftInteger;
            FieldType := jcsvNumeric;
            FieldLen := 0; // automatic.
          end;
        '&':
          begin // &=Float value
            VclFieldType := ftFloat;
            FieldType := jcsvNumeric;
            FieldLen := 0; // automatic.
          end;
        '@':
          begin // @=Datetime as Ascii YYYY/MM/DD HH:MM:SS
            VclFieldType := ftDateTime;
            FieldType := jcsvAsciiDateTime;
            FieldLen := 0; // automatic.
          end;
        '!':
          begin // != Boolean field True/False
            VclFieldType := ftBoolean; // Boolean field in dataset
            FieldType := jcsvNumeric; // numeric field in file
            FieldLen := 0; // automatic.
          end;
        '#':
          begin // #=Datetime as Seconds since 1970 stored in HEX
            VclFieldType := ftDateTime;
            FieldType := jcsvGMTDateTime;
            FieldLen := 0; // automatic.
          end;
        '-':
          begin // -=Datetime as Seconds since 1970 stored in HEX
            VclFieldType := ftDateTime;
            FieldType := jcsvTZDateTime;
            FieldLen := 0; // automatic.
          end;
      else
        JvCsvDatabaseError(FTableName, Format(RsEInvalidFieldTypeCharacter, [FieldTypeChar]));
      end;

      if CsvFieldName = '' then
      begin
        JvCsvDatabaseError(FTableName, RsEUnexpectedError);
        Break;
      end;

       // sometime later: unpack the rest of the string
       // and declare ftString,ftFloat,ftInteger,ftDateTime, etc.
       // now add the field:
      Inc(ColNum);

      // This may throw an exception. but we'll just allow
      // that as necessary:

      //Was: TFieldDef.Create(FieldDefs, ...., ColNum);
      FieldDefs.Add(CsvFieldName, VclFieldType, FieldLen, False);

      // Now create our internal field Data structure:
      PCsvFieldDef := AllocMem(SizeOf(TJvCsvColumn) {+ 8 BIGFudge});
      PCsvFieldDef^.FFlag := FieldType; {jcsvString}
      PCsvFieldDef^.FFieldDef := FieldDefs.Find(CsvFieldName);

      // Note: field order is established when we open the file (later)
      PCsvFieldDef^.FPhysical := -1; // not yet located in the physical file!
      FCsvColumns.AddColumn(PCsvFieldDef);
    end;

    // if the file doesn't contain this and we haven't
    // generated it yet, generate the header row:
    if (not FHasHeaderRow) and (FHeaderRow = '') then
      FHeaderRow := GetColumnsAsString;

    if Length(FHeaderRow) > 0 then
      ProcessCsvHeaderRow; // process FHeaderRow
  end
  else
    JvCsvDatabaseError(FTableName, RsEFieldDefinitionError);

  if FCsvKeyDef = '' then
    FCsvKeyCount := 0
  else
  begin
    SetLength(CsvKeys, FCsvColumns.Count);
    FCsvKeyCount := StrSplit(FCsvKeyDef, Separator, {Chr(0)=No Quoting} Chr(0), CsvKeys, FCsvColumns.Count);
    SetLength(FCsvKeyFields, FCsvKeyCount);
    if (FCsvKeyCount < 1) or (FCsvKeyCount > FCsvColumns.Count) then
      JvCsvDatabaseError(FTableName, RsEInvalidCsvKeyDef);
    for I := 0 to FCsvKeyCount - 1 do
    begin
      if CsvKeys[I] = '' then
        JvCsvDatabaseError(FTableName, RsEInternalErrorParsingCsvKeyDef);
      PCsvFieldDef := FCsvColumns.FindByName(CsvKeys[I]);
      if not Assigned(PCsvFieldDef) then
        JvCsvDatabaseError(FTableName, Format(RsEContainsField, [CsvKeys[I]]))
      else
      begin
        PCsvFieldDef^.FKeyFlag := True;
        FCsvKeyFields[I] := PCsvFieldDef;
      end;
    end;
  end;
end;

{ set our position onto the EOF Crack }

procedure TJvCustomCsvDataSet.InternalLast;
begin
//  Eof := True;
  FRecordPos := ON_EOF_CRACK; // FData.Count;
end;

// At shutdown or on user-calling this method, check if Data has changed,
// and write changes to the file.

procedure TJvCustomCsvDataSet.Flush;
begin
  if FTableName = '' then
    raise EJvCsvDataSetError.CreateRes(@RsETableNameNotSet);

  if FFileDirty and FSavesChanges and (Length(FTableName) > 0) then
  begin
    // Make backup first, if enabled (>2)
    if FAutoBackupCount > 0 then
    begin
      if FAutoBackupCount < 10 then
        FAutoBackupCount := 10; // can't be between 1 and 9, must be at least 10.
      JvCsvBackupPreviousFiles(FOpenFileName, FAutoBackupCount);
    end;
    // Now write new file.
    ExportCsvFile(FOpenFilename);
    FFileDirty := False;
  end;
end;

{procedure TJvCustomCsvDataSet.DestroyFields;
begin
 inherited DestroyFields;
 // Clear out local TCsvFieldDefs.
 FCsvColumns.Clear;
end;}

procedure TJvCustomCsvDataSet.InternalClose;
begin
  if not FCursorOpen then
  begin
    //OutputDebugString('InternalClose called on already closed dataset');
    Exit;
  end;
  Flush;
  BindFields(False);
  if DefaultFields then
    DestroyFields;
  FData.Clear;
  FCursorOpen := False;
  FRecordPos := ON_BOF_CRACK;
  FOpenFileName := '';
end;

procedure TJvCustomCsvDataSet.InternalHandleException;
begin
  // standard implementation for this method:
  if Application <> nil then
    Application.HandleException(Self);
end;

procedure TJvCustomCsvDataSet.InternalDelete;
begin
  if (FRecordPos >= 0) and (FRecordPos < FData.Count) then
    // FreeMem performed inside DeleteRow!
    FData.DeleteRow(FRecordPos);

  if FRecordPos >= FData.Count then
    FRecordPos := FData.Count - 1;

  FFileDirty := True;
end;

{ returns -1 if not found, else returns record index }

function TJvCustomCsvDataSet.InternalFindByKey(Row: PCsvRow): Integer;
var
  I: Integer;
begin
  Result := -1;
  for I := 0 to FData.Count - 1 do
    if InternalCompare(FCsvKeyFields, FCsvKeyCount, {Left} Row, {Right} FData.Items[I], True) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function TJvCustomCsvDataSet.FindByCsvKey(const Key: string): Boolean;
var
  LogicalRow, PhysicalRow: TJvCsvRow;
  I, RecNo: Integer;
  Str: string;
begin
  Result := False;
  StringToCsvRow(Key + Separator, Separator, @LogicalRow, False, False); // initialize row and put items in their logical order.
  CsvRowInit(@PhysicalRow);
  // Move from Logical (TFieldDef order) to their physical (As found in CSV file) ordering:
  for I := 0 to FCsvKeyCount - 1 do
  begin
    Str := GetCsvRowItem(@LogicalRow, I);
    SetCsvRowItem(@PhysicalRow, FCsvKeyFields[I].FPhysical, Str);
      //if (I <> FCsvKeyFields[I].FPhysical) then
      //    OutputDebugString('FindByCsvKey debug');
  end;
  RecNo := InternalFindByKey(@PhysicalRow);
  if RecNo < 0 then
    Exit;

  FRecordPos := RecNo;
  Resync([]);
  Result := True;
end;

{procedure TJvCustomCsvDataSet.InternalInsert;
//var
//  PAddRec : PCsvRow;
begin
// PAddRec := AllocMem(SizeOf(TJvCsvRow));
// StringToCsvRow( FEmptyRowStr, PAddRec, False ); // initialize row.
// FData.AddRow(PAddRec);
// FCurrentRecord := -1;
// Resync([]);
  FRecordPos :=  ON_EOF_CRACK;
//FCurrentRecord := FData.Count;
end;}

procedure TJvCustomCsvDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
  PAddRec: PCsvRow;
//  KeyIndex: Integer;
begin
  if FInsertBlocked then
  begin
    JvCsvDatabaseError(FTableName, RsEInsertBlocked);
    Exit;
  end;

  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;

  PAddRec := AllocMem(SizeOf(TJvCsvRow));
  if Buffer <> nil then
    Move(PCsvRow(Buffer)^, PAddRec^, SizeOf(TJvCsvRow));

  if PAddRec^.Text = '' then
    StringToCsvRow(FEmptyRowStr, Separator, PAddRec, False, False); // initialize row.

  PAddRec^.IsDirty := True;
  PAddRec^.Index := -1; // Was not loaded from the file!

 {
 if FCsvUniqueKeys then
 begin
   KeyIndex := InternalFindByKey(PAddRec);
   if KeyIndex >= 0 then
   begin
     JvCsvDatabaseError('Key value is not unique. Adding new record failed.');
     Exit; // never get here, since normally JvCsvDatabaseError raises an exception.
   end;
 end;
  }
  FData.EnquoteBackslash := FEnquoteBackslash; // make sure FData is in the right mode.

  FFileDirty := True;
  if Append then
  begin //this is the parameter not a TDataSet method invocation!
    PAddRec^.Index := FData.Count;
    FData.AddRow(PAddRec);
    InternalLast;
  end
  else
  begin
    if (FRecordPos = ON_EOF_CRACK) or (FRecordPos = ON_BOF_CRACK) then
    begin
      PAddRec^.Index := FData.Count;
      FData.AddRow(PAddRec);
      InternalLast;
    end
    else
    begin
      RecPos := FRecordPos;
      PAddRec^.Index := RecPos;
      FData.Insert(RecPos, Pointer(PAddRec));
      // XXX Renumber everything else.
    end;
  end;
end;

{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This creates that TString List. }

function TJvCustomCsvDataSet.GetFileName: string;
begin
   // If FTableName is not set, you can't save or load a file, fire an exception:
   Assert(Length(FTableName) <> 0, 'TJvCustomCsvDataSet.GetFileName - TableName property is not set');

   if (Length(FTableName) > 2) and (FTableName[1] = '.') and
     IsPathDelimiter(FTableName, 2) then // reasonably portable, okay?
     // Design-time local paths that don't move if the current working
     // directory moves.  These paths reference the directory the program
     // starts in.  To use this at design time you have to enter the
     // table name as '.\Subdirectory\FileName.csv' (or './subdir/...' on Kylix)
     Result := IncludeTrailingPathDelimiter(FInitialWorkingDirectory) + FTableName  // SPECIAL CASE.
   else
     Result := ExpandUNCFilename(FTableName); // Expand using current working directory to full path name. DEFAULT BEHAVIOR.
end;

function TJvCustomCsvDataSet.InternalLoadFileStrings: Boolean;
begin
  Result := False;
  // BUGFIX: Problem with tables with LoadsFromFile would not load at all. FEB 2004.
  if FLoadsFromFile and not FileExists(FTableName) then
    Exit; // We can return immediately ONLY if there is no file to load,
          // otherwise this routine is parsing already-loaded Data, and we should NOT
          // return, or we won't get our Data in the table. -WP.

  //if not FLoadsFromFile then
  //  Exit;

  if Assigned(FCsvFileAsStrings) then
  begin
    if FCsvFileAsStrings.Count > 0 then
      Result := True; //loaded already
    Exit; // don't repeat!
  end;

  try // open Data file
    FCsvFileAsStrings := TStringList.Create;

    if FLoadsFromFile then // The IF condition here is NEW!
       FCsvFileAsStrings.LoadFromFile(FOpenFilename);

    if FCsvFileAsStrings.Count > 0 then
      Result := True; // it worked!
  except
    //FTableName := '';
    FCsvFileAsStrings.Free;
    FCsvFileAsStrings := nil;
    raise;
  end;
end;

{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This cleans up that TString List. }

procedure TJvCustomCsvDataSet.InternalClearFileStrings;
begin
  if Assigned(FCsvFileAsStrings) then
  begin
    FCsvFileAsStrings.Free;
    FCsvFileAsStrings := nil;
  end;
end;

// Add 1+ commas to FCsvFileAsStrings[1 .. Count-1]

procedure TJvCustomCsvDataSet.AppendPlaceHolderCommasToAllRows(Strings: TStrings);
var
  Commas: string;
  I: Integer;
begin
  for I := 1 to AppendedFieldCount do
    Commas := Commas + Separator;

  Strings.BeginUpdate;
  try
    for I := 1 to Strings.Count - 1 do
      Strings[I] := Strings[I] + Commas;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvCustomCsvDataSet.InternalOpen;
var
  //  Strings: TStringList;
  TempBuf: array [0..MAXCOLUMNS] of Char;
begin
  if FCursorOpen then
    InternalClose; // close first!

  FOpenFileName := GetFileName; // Always use the same file name to save as you did to load!!! MARCH 2004.WP

  FFileDirty := False;
  if (FTableName = '') and FLoadsFromFile then
    JvCsvDatabaseError(RsENoTableName, RsETableNameRequired);
  //Strings := nil;

  InternalInitFieldDefs; // initialize FieldDef objects

  // Create TField components when no persistent fields have been created
  if DefaultFields then
    CreateFields;
  BindFields(True); // bind FieldDefs to actual Data

  if FCsvColumns.Count > 1 then
  begin
     // Create a null terminated string which is just a bunch of commas:
    FillChar(TempBuf, FCsvColumns.Count - 1, Separator);
    TempBuf[FCsvColumns.Count - 1] := Chr(0);
      // When adding an empty row, we add this string as the ascii equivalent:
    FEmptyRowStr := TempBuf;
  end
  else
    FEmptyRowStr := ''; // nothing.

  FBufferSize := SizeOf(TJvCsvRow) + CalcFieldsSize; // our regular record + calculated field Data.
  //if CalcFieldsSize>0 then
  //    OutputDebugString('Calculated Fields Debug');
  FRecordPos := ON_BOF_CRACK; // initial record pos before BOF
  BookmarkSize := SizeOf(Integer);
  // initialize bookmark size for VCL (Integer uses 4 bytes on 32 bit operating systems)

  //Trace( 'InternalOpen: FBufferSize='+IntToStr(FBufferSize) );
  //Trace( 'InternalOpen: CalcFieldsSize='+IntToStr(CalcFieldsSize) );
  //Trace( 'InternalOpen: FieldDefs.Count='+IntToStr(FieldDefs.Count) );

  if InternalLoadFileStrings then
  begin // may load the strings if they weren't loaded already!
    if FHasHeaderRow then
    begin
      if not ExtendedHeaderInfo then
        FHeaderRow := FCsvFileAsStrings[0]
      else
        FCsvFileAsStrings.Delete(0);
      if Length(FHeaderRow) > 0 then
        ProcessCsvHeaderRow;
      if FAppendedFieldCount > 0 then
      begin
        AppendPlaceHolderCommasToAllRows(FCsvFileAsStrings); // Add 1+ separators to FCsvFileAsStrings[1 .. Count-1]
      end;
    end;
    AssignFromStrings(FCsvFileAsStrings); // load into memory.
  end;

  InternalClearFileStrings; // now unload 'em.

  FCursorOpen := True;
end;

procedure TJvCustomCsvDataSet.InternalPost;
var
  PInsertRec: PCsvRow;
  RecPos: Integer;
  KeyIndex: Integer; // If unique key enforcement is on, this is the key search Result.
begin
  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;

  if FPostBlocked then
  begin
    JvCsvDatabaseError(FTableName, RsEPostingHasBeenBlocked);
    Exit;
  end;

  { Unique Key Enforcement : WARNING This doesn't exactly work correctly right now! - WP APRIL 2004.}
  if FCsvUniqueKeys then
  begin
    KeyIndex := InternalFindByKey(PCsvRow(ActiveBuffer));
    // If posting an update, KeyIndex better be <0 or else equal to FRecordPos!
    // Otherwise, if adding, KeyIndex better be <0.

    if KeyIndex >= 0 then
      if ((State = dsInsert) and {XXX NEW}(FRecordPos < RecordCount)) or
        ((State = dsEdit) and (KeyIndex <> FRecordPos)) then
      begin
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString(PChar('JvCsvDataSet Uniqueness: KeyIndex=' +
          IntToStr(KeyIndex) + ' ' + GetRowAsString(KeyIndex)));
        OutputDebugString(PChar('JvCsvDataSet Uniqueness: recordPos=' +
          IntToStr(FRecordPos) + ' ' + GetRowAsString(FRecordPos)));
        {$ENDIF DEBUGINFO_ON}
        raise EJvCsvKeyError.CreateResFmt(@RsEKeyNotUnique, [FTableName]);
        Exit; // never get here, since normally JvCsvDatabaseError raises an exception.
      end;
  end;

  if State = dsEdit then
  begin
    FFileDirty := True;
    RecPos := FRecordPos;
    Move(PCsvRow(ActiveBuffer)^, FData.GetRowPtr(RecPos)^, SizeOf(TJvCsvRow));
    FData.GetRowPtr(RecPos)^.IsDirty := True;
  end
  else
  if State = dsInsert then
  begin
    if FInsertBlocked then
    begin
      JvCsvDatabaseError(FTableName, RsECannotInsertNewRow);
      Exit;
    end;
    FFileDirty := True;
    PInsertRec := AllocMem(SizeOf(TJvCsvRow));
    Move(PCsvRow(ActiveBuffer)^, PInsertRec^, SizeOf(TJvCsvRow));
    PInsertRec^.IsDirty := True;
    FData.Insert(FRecordPos, Pointer(PInsertRec));
    FRecordPos := FData.IndexOf(Pointer(PInsertRec));
    PInsertRec^.Bookmark.Data := FRecordPos;
  end
  else
    JvCsvDatabaseError(FTableName, RsECannotPost);
end;

function TJvCustomCsvDataSet.IsCursorOpen: Boolean;
begin
  // "Cursor" is open if Data file is open.   File is open if FDataFile's
  // Mode includes the FileMode in which the file was open.
  {  Result := TFileRec(FDataFile).Mode <> 0; }
  Result := FCursorOpen; // bogus value: Valid field definition
end;

function TJvCustomCsvDataSet.GetRecordCount: Integer;
begin
  if FData.Count > 0 then
    Result := FData.Count
  else
    Result := 0;
end;

function TJvCustomCsvDataSet.GetRecNo: Integer; {RecNo := FRecordPos+1}
var
  BufPtr: PChar;
begin
  CheckActive;

 //  UpdateCursorPos; {FUDGE!?}

{
  UpdateCursorPos;
  if (FRecordPos = -1) and (RecordCount > 0) then
    Result := 0 // 1  //FUDGE!?
  else
    Result := FRecordPos + 1;
 }

  if State = dsCalcFields then
    BufPtr := CalcBuffer
  else
    BufPtr := ActiveBuffer;

  Result := (PCsvRow(BufPtr)^.Bookmark.Data); // Record number.
end;

procedure TJvCustomCsvDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= FData.Count - 1) then
  begin
    FRecordPos := Value; {-1 XXXXXX }
    if RecordCount > 0 then
      Resync([]);
  end;
end;

procedure TJvCustomCsvDataSet.SetTableName(const Value: string);
begin
  CheckInactive;   // NOTE: TABLE MUST BE *CLOSED* TO CHANGE THE NAME! WE RAISE EXCEPTION HERE IF OPEN.

  FTableName := Value;
//  if (ExtractFileExt(FTableName) = '') and (FTableName <> '') then
//    FTableName := ChangeFileExt(FTableName,'.csv');

  { update internal filename table }
//  FBmkFileName:= ChangeFileExt(FTableName, '.bmk' ); // bookmark file
end;

(*function TJvCustomCsvDataSet.GetDataFileSize: Integer;
//var
//  File1: TextFile;
begin
//  AssignFile(File1,FTableName);
//  Result := FileSize(File1);
//  CloseFile(File1);
  Result := 8192; // not implemented yet.
end; *)

procedure TJvCustomCsvDataSet.EmptyTable;
begin
  // Erase Rows.
  while (FData.Count > 0) do
    FData.DeleteRow(FData.Count - 1);
  // Refresh controls.
  First;
  if FSavesChanges then
    DeleteFile(FOpenFileName);
end;

// InternalCompare of two records, of a specific field index.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCustomCsvDataSet.InternalFieldCompare(Column: PCsvColumn; Left, Right: PCsvRow): Integer;
var
  StrLeft, StrRight: string;
  NumLeft, NumRight, Diff: Double;
begin

  StrLeft := GetCsvRowItem(Left, Column^.FPhysical);
  StrRight := GetCsvRowItem(Right, Column^.FPhysical);

  (*if (Length(StrLeft)=0) or (length(StrRight)=0) then
  begin
    OutputDebugString('Debugging problem in InternalFieldCompare');
    StrLeft  :=GetCsvRowItem( Left, Column^.FPhysical );
  end;*)

  if FCsvCaseInsensitiveComparison then
  begin
    StrLeft := UpperCase(StrLeft);
    StrRight := UpperCase(StrRight);
  end;

   // everything sorts via string sort (default) or numeric sort
   // (the only special case so far!)
  case Column^.FFlag of
    jcsvNumeric:
      begin
        NumLeft := StrToFloatUSDef(StrLeft, -99999.9);
        NumRight := StrToFloatUSDef(StrRight, -99999.9);
        Diff := NumLeft - NumRight;
        if Diff < -0.02 then
          Result := -1
        else
        if Diff > 0.02 then
          Result := 1
        else
          Result := 0; // For our purposes, .02 difference or less is a match.
        Exit;
      end;
  else
    Result := StrComp(PChar(StrLeft), PChar(StrRight));
  end;
end;

// InternalCompare of multiple fields.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCustomCsvDataSet.InternalCompare(SortColumns: TArrayOfPCsvColumn;
  SortColumnCount: Integer; Left, Right: PCsvRow; Ascending: Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  // null check, raise exception
  if (not Assigned(Left)) or (not Assigned(Right)) then
    JvCsvDatabaseError(FTableName, RsEInternalCompare);
  // now check each field:
  for I := 0 to SortColumnCount - 1 do
  begin
    if not Assigned(SortColumns[I]) then
      JvCsvDatabaseError(FTableName, RsEInternalCompare); // raise exception
    Result := InternalFieldCompare(SortColumns[I], Left, Right);
    if Result <> 0 then
    begin
      if not Ascending then
        Result := -Result;
           // XXX REPEAT Result := InternalFieldCompare( SortColumns[I],Left,Right);
      Exit; // found greater or less than condition
    end;
  end;
  // now we have compared all fields, and if we get here, they were all
  // equal, and Result is already set to 0.
end;

procedure TJvCustomCsvDataSet.InternalQuickSort(SortList: PPointerList;
  L, R: Integer; SortColumns: TArrayOfPCsvColumn; ACount: Integer; Ascending: Boolean);
var
  I, J: Integer;
  P, T: Pointer;
begin
  // TODO: optimization (median of three, insertion sort when Count < 20 etc)
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1];
    repeat
      while InternalCompare(SortColumns, ACount, SortList^[I], P, Ascending) < 0 do
        Inc(I);
      while InternalCompare(SortColumns, ACount, SortList^[J], P, Ascending) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I];
        SortList^[I] := SortList^[J];
        SortList^[J] := T;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      InternalQuickSort(SortList, L, J, SortColumns, ACount, Ascending);
    L := I;
  until I >= R;
end;

procedure TJvCustomCsvDataSet.QuickSort(AList: TList; SortColumns: TArrayOfPCsvColumn;
  ACount: Integer; Ascending: Boolean);
begin
  if (AList <> nil) and (AList.Count > 1) then
    InternalQuickSort(AList.List, 0, AList.Count - 1, SortColumns, ACount, Ascending);
end;

procedure TJvCustomCsvDataSet.Sort(const SortFields: string; Ascending: Boolean);
var
//  Index: array of Pointer;
//  swap: Pointer;
  SortFieldNames: array of string;
  SortColumns: TArrayOfPCsvColumn;
  SortColumnCount: Integer;
//  comparison, I, U, L: Integer;
  I: Integer;
begin
  // Create an indexed list which can be sorted more easily than
  // doing an item swap:
//  L := FData.Count;
//  SetLength(Index, L);
//  for I := 0 to L - 1 do
//  begin
//    Index[I] := FData.Items[I]; // Initial values.
//  end;

  SetLength(SortFieldNames, FCsvColumns.Count);
  SortColumnCount := StrSplit(SortFields, Separator, {Chr(0)=No Quoting} Chr(0), SortFieldNames, FCsvColumns.Count);
  SetLength(SortColumns, SortColumnCount);
  if (SortFields = '') or (SortColumnCount = 0) then
    JvCsvDatabaseError(FTableName, RsESortFailedCommaSeparated);

  // Now check if the fields exist, and find the pointers to the fields
  for I := 0 to SortColumnCount - 1 do
  begin
    if SortFieldNames[I] = '' then
      JvCsvDatabaseError(FTableName, RsESortFailedFieldNames);
    SortColumns[I] := FCsvColumns.FindByName(SortFieldNames[I]);
    if not Assigned(SortColumns[I]) then
      JvCsvDatabaseError(FTableName, Format(RsESortFailedInvalidFieldNameInList, [SortFieldNames[I]]));
  end;
  QuickSort(FData, SortColumns, SortColumnCount, AScending);

  //  bubble sort, compare in the middle,
  //  yes I'm feeling lazy today, yes I know a qsort would be better. - WP
(*
  for I := 0 to L - 2 do
  begin
    for U := I + 1 to L - 1 do
    begin
        // Record comparison between two PCsvRows:
      comparison := InternalCompare(SortColumns,
        SortColumnCount,
        PCsvRow(Index[I]),
        PCsvRow(Index[U])
        );
      if not Ascending then
        comparison := comparison * -1; // flip sign of comparison
      if comparison > 0 then
      begin { bubble sort by swaps }
        swap := Index[I];
        Index[I] := Index[U];
        Index[U] := swap;
      end;
    end;
  end;
  // Now build a new Data elements list:
  for I := 0 to L - 1 do
  begin
    FData.Items[I] := Index[I]; // Rewrite pointers to new order!
  end;
  *)
  FFileDirty := True;
  First; // reposition!
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

procedure TJvCustomCsvDataSet.DefChanged(Sender: TObject);
begin
  FStoreDefs := True;
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

function TJvCustomCsvDataSet.FieldDefsStored: Boolean;
begin
  Result := FStoreDefs and (FieldDefs.Count > 0);
end;

function TJvCustomCsvDataSet.GetCanModify: Boolean;
begin
  Result := not FReadOnly; // You can modify if it's NOT read only.
end;

{ CsvColumns dynamic array of pointers }

procedure TJvCsvColumns.AddColumn(Item: PCsvColumn);
begin
  Add(Pointer(Item));
end;

function TJvCsvColumns.FindByName(const FieldName: string): PCsvColumn;
var
  I: Integer;
begin
  try
    for I := 0 to Count - 1 do
    begin
      Result := PCsvColumn(Get(I));
      if Assigned(Result.FFieldDef) then
        // Case insensitive field name matching:
        if CompareText(Result.FFieldDef.Name, FieldName) = 0 then
          Exit; //return that field was found!
    end;
  except
   // ignore exceptions
  end;
  Result := nil;
end;

function TJvCsvColumns.FindByFieldNo(FieldNo: Integer): PCsvColumn;
var
  I: Integer;
begin
  Result := nil;
  try
    for I := 0 to Count - 1 do
    begin
      Result := PCsvColumn(Get(I));
      if Assigned(Result) then
        if Assigned(Result^.FFieldDef) then
          if Result^.FFieldDef.FieldNo = FieldNo then
            Exit; //return that field was found!
    end;
  except
    // ignore exceptions
    Result := nil;
  end;
end;

procedure TJvCsvColumns.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Self[I]);
  inherited Clear;
end;

{ CsvRows: dynamic array of pointers }

function TJvCsvRows.GetUserTag(Index: Integer): Integer;
begin
  if (Index < 0) or (Index >= FUserLength) then
    Result := 0
  else
    Result := FUserTag[Index];
end;

procedure TJvCsvRows.SetUserTag(Index, Value: Integer);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;

  if Index >= FUserLength then
  begin
    FUserLength := Index + 1;
    SetLength(FUserTag, FUserLength);
    SetLength(FUserData, FUserLength);
  end;
  FUserTag[Index] := Value;
end;

function TJvCsvRows.GetUserData(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FUserLength) then
    Result := nil
  else
    Result := FUserData[Index];
end;

procedure TJvCsvRows.SetUserData(Index: Integer; Value: Pointer);
begin
  if (Index < 0) or (Index >= Count) then
    Exit;
  if Index >= FUserLength then
  begin
    FUserLength := Index + 1;
    SetLength(FUserTag, FUserLength);
    SetLength(FUserData, FUserLength);
  end;
  FUserData[Index] := Value;
end;

procedure TJvCsvRows.AddRow(Item: PCsvRow);
begin
  Add(Pointer(Item));
end;

procedure TJvCsvRows.InsertRow(const Position: Integer;  Item: PCsvRow);
begin
  Insert(Position, Pointer(Item));
end;

procedure TJvCsvRows.AddRowStr(const Item: string; Separator: Char); // convert String->TJvCsvRow
var
  PNewItem: PCsvRow;
begin
  PNewItem := AllocMem(SizeOf(TJvCsvRow));
  StringToCsvRow(Item, Separator, PNewItem, True, FEnquoteBackslash); // decode a csv line that can contain escape sequences
  AddRow(PNewItem);
end;

function TJvCsvRows.GetRowPtr(const RowIndex: Integer): PCsvRow;
begin
 if (RowIndex >= 0) and (RowIndex < Count) then
   Result := PCsvRow(Get(RowIndex)) // return pointer to a row item.
 else
   raise EJvCsvDataSetError.CreateRes(@RsECsvNoRecord); { NO Such Record }
end;

function TJvCsvRows.GetRowStr(const RowIndex: Integer): string;
var
  ResultStr: string;
begin
  CsvRowToString(GetRowPtr(RowIndex), ResultStr);
  Result := ResultStr;
end;

procedure TJvCsvRows.SetRowStr(const RowIndex: Integer; Value: string; Separator: Char);
begin
  StringToCsvRow(Value, Separator, GetRowPtr(RowIndex), True, FEnquoteBackslash);
end;

procedure TJvCsvRows.DeleteRow(const RowIndex: Integer);
var
  P: Pointer;
begin
  if (RowIndex >= 0) and (RowIndex < Count) then
  begin
    P := Self[RowIndex];
    if P <> nil then
      FreeMem(P);
  end;
  Delete(RowIndex);
end;

procedure TJvCsvRows.SetARowItem(const RowIndex, ColumnIndex: Integer; Value: string);
begin
  SetCsvRowItem(GetRowPtr(RowIndex), ColumnIndex, Value);
end;

function TJvCsvRows.GetARowItem(const RowIndex, ColumnIndex: Integer): string;
begin
  Result := GetCsvRowItem(GetRowPtr(RowIndex), ColumnIndex);
end;

procedure TJvCsvRows.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Self[I]);
  inherited Clear;
end;

{ Call this one first, then AssignFromStrings on subsequent updates only.}

procedure TJvCustomCsvDataSet.OpenWith(Strings: TStrings);
begin
  Active := False;
  if FHasHeaderRow then
    FHeaderRow := Strings[0];
  AssignFromStrings(Strings); // parse strings
end;

procedure TJvCustomCsvDataSet.AppendWith(Strings: TStrings);
//var
//  x: Integer;
begin
  //Active := False;
  if FHasHeaderRow then
  begin
    FHeaderRow := Strings[0];
    FPendingCsvHeaderParse := True; // NEW: Just-in-time-CSV-header-parsing.
  end;

  AssignFromStrings(Strings); // parse strings
  // Refresh.
  // DataEvent(deDataSetChange, 0);
  // DataEvent(deRecordChange, 0);
  Resync([]);
  // DataEvent(deUpdateState, 0);
  if Active then
    Last;
end;

{ Additional Custom Methods - internal use }

procedure TJvCustomCsvDataSet.AssignFromStrings(const Strings: TStrings);
var
  // HeaderRowFound: Boolean;
  I: Integer;
  StartIndex, IndexCounter: Integer;
begin
  // CheckInactive; // NO! DON'T DO THIS!
  // if NOT FFieldsInitialized then
  // InternalInitFieldDefs; // must know about field definitions first.
  if Strings = nil then
    Exit;
  FData.EnquoteBackslash := FEnquoteBackslash;

  IndexCounter := 0;
  // Skip first Row:
  if FHasHeaderRow then
    StartIndex := 1
  else
    StartIndex := 0;

  for I := StartIndex to Strings.Count - 1 do
  begin
    // for now ignore any trace or debug Data unless
    // someone has defined an event to handle it.
    if Pos('>>', Strings[I]) = 1 then
    begin
      if Assigned(FOnSpecialData) then
        FOnSpecialData(Self, I, Strings[I]);
      Continue;
    end;
    // Process the row:
    ProcessCsvDataRow(Strings[I], IndexCounter);
    Inc(IndexCounter);
  end;
  if Active then
    First;
end;

procedure TJvCustomCsvDataSet.AssignToStrings(Strings: TStrings);
var
  I: Integer;
  Line: string;
begin
  Strings.BeginUpdate;
  try
    // copy out the current Data set to a TStringList.
    Strings.Clear;

    { Save header row with Data rows? }
    if FHasHeaderRow then
      if ExtendedHeaderInfo then
        Strings.Add(CsvFieldDef)
      else
        Strings.Add(GetColumnsAsString);

    for I := 0 to FData.Count - 1 do
    begin
      CsvRowToString(FData.GetRowPtr(I), Line);
      Strings.Add(Line);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvCustomCsvDataSet.AppendRowString(const RowAsString: string);
begin
  if not Active then
    JvCsvDatabaseError(FTableName, RsEDataSetNotOpen);
  ProcessCsvDataRow(RowAsString, FData.Count);
  FFileDirty := True; // added row, make sure it gets saved!
  Last;
end;

function TJvCustomCsvDataSet.GetAsString(const Row, Column: Integer): string; //virtual;
var
  GetIndex: Integer;
begin
  if Row < 0 then {lastrow}
    GetIndex := FData.Count - 1
  else
    GetIndex := Row; { actual index specified }

  { return string}
  Result := GetCsvRowItem(FData.GetRowPtr(GetIndex), Column);
end;

function TJvCustomCsvDataSet.CurrentRowAsString: string; //virtual;
begin
  Result := GetRowAsString(RecNo);
end;

function TJvCustomCsvDataSet.GetRowAsString(const Index: Integer): string;
var
  GetIndex: Integer;
begin
  if Index < 0 then {lastrow}
    GetIndex := FData.Count - 1
  else
    GetIndex := Index; { actual index specified }

  { return string }
  CsvRowToString(FData.GetRowPtr(GetIndex), Result);
  Result := Result;
end;

// Get names of all the columns as a comma-separated string:

function TJvCustomCsvDataSet.GetColumnsAsString: string;
var
  I: Integer;
begin
  // ColCount:
  if FCsvColumns.Count = 0 then
  begin
    Result := '';
    Exit;
  end;
  // Build a list of column names: <item>, <item>,....,<item>
  Result := FieldDefs[0].Name;
  for I := 1 to FCsvColumns.Count - 1 do
    Result := Result + Separator + FieldDefs[I].Name;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }

procedure TJvCustomCsvDataSet.ProcessCsvHeaderRow;
var
  CsvFieldRec: TJvCsvRow; // CSV Field record type.
  PtrCsvColumn: PCsvColumn;
  CsvFieldName: string;
  ColNum, I: Integer;
begin
  if not ValidateHeaderRow then
  begin
    for I := 0 to FCsvColumns.Count - 1 do
      PCsvColumn(FCsvColumns.Get(I))^.FPhysical := I;
    Exit;
  end;
  FAppendedFieldCount := 0;
  //  Columns Not Yet Found:
  for I := 0 to FCsvColumns.Count - 1 do
    PCsvColumn(FCsvColumns.Get(I))^.FPhysical := -1;

  // Do initial parse.
  StringToCsvRow(FHeaderRow, Separator, @CsvFieldRec, False, False);
  ColNum := 0;
  while (CsvRowGetColumnMarker(@CsvFieldRec, ColNum) <> COLUMN_ENDMARKER) do
  begin
    // Get a string in the format COLUMNAME:Options
    CsvFieldName := StrEatWhiteSpace(GetCsvRowItem(@CsvFieldRec, ColNum));

    if CsvFieldName = '' then
      JvCsvDatabaseError(FTableName, RsEErrorProcessingFirstLine);

    PtrCsvColumn := FCsvColumns.FindByName(CsvFieldName);

    if PtrCsvColumn = nil then
    begin // raise database exception:
      JvCsvDatabaseError(FTableName, Format(RsEFieldInFileButNotInDefinition, [CsvFieldName]));
      Exit;
    end;

    try
      PtrCsvColumn^.FPhysical := ColNum; // numbered from 0.
    except
      JvCsvDatabaseError(FTableName, Format(RsECsvFieldLocationError, [CsvFieldName]));
      Break;
    end;
    Inc(ColNum);
  end; // loop for each column in the physical file's header row.

  // Check that everything was found and physically given a location
  // in the CSV file:
  for I := 0 to FCsvColumns.Count - 1 do
  begin
    PtrCsvColumn := PCsvColumn(FCsvColumns[I]);
    if PtrCsvColumn^.FPhysical < 0 then
    begin
      if not FHasHeaderRow then
      begin
        // If there is no header row we can't cope with fields that aren't in the file
        // because FCsvHeader is not written into the file, so we can't just go expanding
        // what goes into that file.
        JvCsvDatabaseError(FTableName, Format(RsEFieldNotFound, [PtrCsvColumn^.FFieldDef.Name]));
        Exit;
        // Now go fix the CSV file by hand until it matches your new CSVFieldDef.
        // Sounds like not much fun, eh?
      end
      else
      begin
        //-------------------------------------------------------
        // New HANDY DANDY Instant Import Feature
        // APPEND-NEW-FIELDS-to-Existing-CSV file BEHAVIOUR.
        // When Triggered, the end user can tell because we have a
        // new property AppendedFieldCount which will be nonzero.
        // If we continue, we can use this opportunity to
        // import an existing CSV file that can now 'upgraded'
        // in memory (to contain new fields we just added to our
        // field definitions).
        //-------------------------------------------------------
        PtrCsvColumn^.FPhysical := ColNum;
        Inc(ColNum);
        CsvFieldName := PtrCsvColumn^.FFieldDef.Name;
        FHeaderRow := FHeaderRow + Separator + CsvFieldName;
        Inc(FAppendedFieldCount);
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString(PChar('JvCsvData: Field ' + CsvFieldName + ' not found in file ' + Self.FTableName +
          ', inserted new (blank) column during loading.'));
        {$ENDIF DEBUGINFO_ON}
      end;
    end;
  end;
end;

procedure TJvCustomCsvDataSet.ProcessCsvDataRow(const DataRow: string; Index: Integer);
var
  PNewRow: PCsvRow;
begin
  if DataRow = '' then
    Exit;
  if Length(DataRow) >= MAXLINELENGTH - 1 then
    raise EJvCsvDataSetError.CreateResFmt(@RsECsvStringTooLong, [Copy(DataRow, 1, 40)]);
  PNewRow := AllocMem(SizeOf(TJvCsvRow));
  StringToCsvRow(DataRow, Separator, PNewRow, True, FEnquoteBackslash);
  PNewRow^.Index := Index;
  FData.AddRow(PNewRow);
end;

{ This function is handy to save a portion of a csv table that has
grown too large into a file, and then DeleteRows can be called to remove
that section of the file. }

procedure TJvCustomCsvDataSet.ExportRows(const FileName: string; FromRow, ToRow: Integer);
var
  I: Integer;
  StrList: TStringList;
begin
  StrList := TStringList.Create;
  StrList.Add(Self.FHeaderRow);
  try
    for I := FromRow to ToRow do
      StrList.Add(FData.GetRowStr(I));
    StrList.SaveToFile(FileName);
  finally
    StrList.Free;
  end;
end;

procedure TJvCustomCsvDataSet.DeleteRows(FromRow, ToRow: Integer);
var
  Count: Integer;
begin
  Count := (ToRow - FromRow) + 1;
  while Count > 0 do
  begin
    if FromRow < FData.Count then
      FData.DeleteRow(FromRow) // Everything moves down one every time we do this.
    else
      Break;
    Dec(Count);
  end;
  // Force Redraw of Data Controls:
  if FRecordPos >= FData.Count then
  begin
    FRecordPos := FData.Count - 1;
    Last;
  end
  else
    First;
  FFileDirty := True;
end;

procedure TJvCustomCsvDataSet.ExportCsvFile(const FileName: string); // save out to a file.
var
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    AssignToStrings(Strings);
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

function TJvCustomCsvDataSet.GetCsvHeader: string;
var
  F: Text;
  FirstLine: string;
begin
  if (not FLoadsFromFile) or (not FHasHeaderRow) or not (FileExists(FTableName)) then
  begin
    Result := '';
    Exit;
  end;
  { How's this for an ancient Pascal code sequence, AssignFile+Reset is approximately equal to a C fopen() call }
  AssignFile(F, FTableName);
  Reset(F);
  { ReadLn is approximately a gets() call }
  ReadLn(F, FirstLine);
  { And finally, the pascal file close procedure }
  CloseFile(F);
  // return the first line of the file, without the junk
  Result := StrStrip(FirstLine); // in JvCsvParse.pas
end;

{ PROCEDURES: }

// convert CSV Row buffer to a String

procedure CsvRowToString(RowItem: PCsvRow; var RowString: string);
begin
  RowString := RowItem^.Text;
end;

// convert String into a CSV Row buffer

procedure StringToCsvRow(const RowString: string; Separator: Char;
  RowItem: PCsvRow; PermitEscapeSequences, EnquoteBackslash: Boolean);
var
  I, L, Col: Integer;
  QuoteFlag: Boolean;
  SkipFlag: Boolean;
  CharsInColumn: Integer;
begin
  Col := 0;
  RowItem^.WordField[0] := 0; // zero out column marker and dirty bit!
  CharsInColumn := 0;
  QuoteFlag := False;
  SkipFlag := False;
  L := Length(RowString);
  for I := 1 to L do
  begin
    Inc(CharsInColumn);
    if QuoteFlag then
    begin
      // backslash permitted only if specifically enabled by FEnquoteBackslash:
      if PermitEscapeSequences and (not SkipFlag) and (EnquoteBackslash) and (RowString[I] = '\') then
      begin
        SkipFlag := True;
        Continue;
      end;
      // doubled quotes handling:
      if PermitEscapeSequences and (not SkipFlag) and (RowString[I] = '"') and
        (I < L) and (RowString[I + 1] = '"') then
      begin
        SkipFlag := True;
        Continue;
      end;
      // now if either of the above set the SkipFlag True previously, we ALWAYS skip the next character here
      // and turn SkipFlag back off
      if PermitEscapeSequences and SkipFlag then
      begin // skip next character, regardless.
        SkipFlag := False;
        Continue; // by skipping escaped quotes, we don't turn off QuoteFlag in the middle of a string!
      end;
    end;
    // Now we know if we get this far, we are NOT dealing with any escaped characters
    // Any quotes we see here will turn on/off quote mode directly!
    if RowString[I] = '"' then
    begin
      if PermitEscapeSequences then
      begin
        QuoteFlag := not QuoteFlag;
        if QuoteFlag and (CharsInColumn > 1) then
        begin
          // (rom) no OutputDebugString in production code
          {$IFDEF DEBUGINFO_ON}
          OutputDebugString('CsvDataSource.pas: StringToCsvRow - unescaped quote character in middle of string!');
          {$ENDIF DEBUGINFO_ON}
        end;
      end
      else
      begin
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString('CsvDataSource.pas: StringToCsvRow - quote character found where no escape sequences are permitted!');
        {$ENDIF DEBUGINFO_ON}
      end;
    end;

    if (RowString[I] = Separator) and not QuoteFlag then
    begin
      Inc(Col);
      // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
      RowItem.WordField[Col] := (Word(I) and $7FFF); {note that we're going from 1..length }
      CharsInColumn := 0;
    end;
    if (Col >= MAXCOLUMNS) or (I >= MAXLINELENGTH) then
    begin
      raise ERangeError.CreateResFmt(@RsEInternalLimit, [MAXCOLUMNS]);
      Exit;
    end;
  end; // end of string, new flag:
  Inc(Col);
  if QuoteFlag then
  begin
    // (rom) no OutputDebugString in production code
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString('CsvDataSource.pas: StringToCsvRow - Missing end quote character!');
    {$ENDIF DEBUGINFO_ON}
  end;
  // Terminate the column-marker list with a special end-marker:
  {RowItem.WordField[col]   := Word(Length(RowString)+1)AND$7FFF; // length of string
  RowItem.WordField[col+1] := COLUMN_ENDMARKER; // followed by an end marker}
  RowItem.WordField[Col] := COLUMN_ENDMARKER; // last one has no end marker
  StrLCopy(RowItem.Text, PChar(RowString), MAXLINELENGTH);
  RowItem.Columns := Col; // Check this later!
end;

// Copy a single column from one row buffer to another row buffer:

function CsvRowItemCopy(Source, Dest: PCsvRow; FieldIndex, FieldSize: Integer): Boolean;
var
  TempStr: string;
begin
  TempStr := GetCsvRowItem(Source, FieldIndex);
  // length limiting feature:
  if FieldSize > 0 then
    if Length(TempStr) > FieldSize then
      TempStr := Copy(TempStr, 1, FieldSize);
  SetCsvRowItem(Dest, FieldIndex, TempStr);
  Result := True;
end;

// Copy an item into a csv row buffer:

procedure SetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: string);
var
  TempBuf: array [0..MAXLINELENGTH] of Char;
  Copy1, Copy2: Integer;
  Dif, I, Old: Integer;
begin
  Dif := 0;
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
    Exit;

  Copy1 := CsvRowGetColumnMarker(PItem, ColumnIndex);
  if Copy1 = COLUMN_ENDMARKER then
    Exit;
    // Update new rows:  FIX previous fix!
  if (ColumnIndex >= PItem^.Columns) then
      PItem^.Columns := ColumnIndex+1;

  if Copy1 > MAXLINELENGTH then
    Exit;
 // copy initial part of the csv row:
  if Copy1 > 0 then
  begin
    StrLCopy(TempBuf, PItem^.Text, Copy1);
    StrLCat(TempBuf, PChar(NewValue), MAXLINELENGTH);
  end
  else
    StrLCopy(TempBuf, PChar(NewValue), MAXLINELENGTH);

  Copy2 := CsvRowGetColumnMarker(PItem, ColumnIndex + 1);
  if Copy2 <> COLUMN_ENDMARKER then
  begin
    // difference in length:
    Dec(Copy2); // subtract one.
    if Copy2 < 0 then
      Exit;
    if Length(NewValue) = Copy2 - Copy1 then
      Dif := 0
    else
      Dif := Length(NewValue) - (Copy2 - Copy1);
    StrLCat(TempBuf, PItem^.Text + Copy2, MAXLINELENGTH);
  end;

  // Copy over the old memory buffer:
  StrLCopy(PItem^.Text, TempBuf, MAXLINELENGTH);

  // Now that we've copied a new item of a different length into the place of the old one
  // we have to update the positions of the columns after ColumnIndex:
  if Dif <> 0 then
    for I := ColumnIndex + 1 to MAXCOLUMNS do
    begin
      Old := CsvRowGetColumnMarker(PItem, I);
      if Old = COLUMN_ENDMARKER then
        Exit;
      CsvRowSetColumnMarker(PItem, I, Old + Dif);
    end;
end;

// Copy an item out of a csv row buffer:

function GetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer): string;
var
  TempBuf: array [0..MAXLINELENGTH] of Char;
  Copy1, Copy2: Integer;
begin
  Result := '';
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
    Result := RsErrorRowItem
  else
  if ColumnIndex < PItem^.Columns then
  begin
    Copy1 := CsvRowGetColumnMarker(PItem, ColumnIndex);
    Copy2 := CsvRowGetColumnMarker(PItem, ColumnIndex + 1);

    if Copy1 <> COLUMN_ENDMARKER then
    begin
      if Copy2 = COLUMN_ENDMARKER then // copy the rest of the line
        Copy2 := MAXLINELENGTH - Copy1 // All the characters left in the buffer
      else
        Dec(Copy2);

      if (Copy1 <= MAXLINELENGTH) and (Copy2 <= MAXLINELENGTH) then
      begin
       // Copy out just one column from the string:
        StrLCopy(TempBuf, PItem^.Text + Copy1, Copy2 - Copy1);
        PcharEatWhiteChars(@TempBuf[0]);
        Result := string(TempBuf);
      end;
    end;
  end;
end;

{new}

procedure CsvRowSetDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  Row^.IsDirty := True; // triggers search for 'dirty bit' in columns
  Row^.WordField[ColumnIndex] := (Row^.WordField[ColumnIndex] or $8000);
end;

procedure CsvRowClearDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  Row^.WordField[ColumnIndex] := (Row^.WordField[ColumnIndex] and $7FFF);
end;

function CsvRowGetDirtyBit(Row: PCsvRow; ColumnIndex: Integer): Boolean;
begin
  Result := False;
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  if Row^.WordField[ColumnIndex] = COLUMN_ENDMARKER then
    Exit;
  Result := (Row^.WordField[ColumnIndex] and $8000) <> 0;
end;

procedure CsvRowSetColumnMarker(Row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
var
  Old: Word;
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  if ColumnMarker < 0 then
    Exit;

  if ColumnMarker = COLUMN_ENDMARKER then
    Row^.WordField[ColumnIndex] := COLUMN_ENDMARKER
  else
  begin
    Old := Row^.WordField[ColumnIndex];
    if Old = COLUMN_ENDMARKER then
      Row^.WordField[ColumnIndex] := ColumnMarker and $7FFF // auto-clear Dirty bit
    else
      Row^.WordField[ColumnIndex] := (Old and $8000) or // Keep Old Dirty Bit
        (Word(ColumnMarker) and $7FFF); // new value.
  end;
end;

function CsvRowGetColumnMarker(Row: PCsvRow; ColumnIndex: Integer): Integer;
var
  W: Word;
begin
  Result := -1;
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  W := Row^.WordField[ColumnIndex];
  if W = COLUMN_ENDMARKER then
    Result := COLUMN_ENDMARKER
  else
    Result := Integer(W and $7FFF);
end;

{endnew}

 //------------------------------------------------------------------------------
 // TimeTHexToDateTime
 //
 // TDateTime is a whole number representing days since Dec 30, 1899.
 // A standard C library time is seconds since 1970. We compensate by
 // getting the base date (Jan 1, 1970) and adding 1 for every day since
 // then, and a fractional part representing the seconds.
 // By dividing the seconds by the number of seconds in a day (24*24*60=86400)
 // we obtain this Result.
 // The incoming value in hex will roll over in mid-Janary 2038, and
 // hopefully by then this code won't be in use any more! :-)
 //
 // Note: TDateTime is really a Double floating-point and zero is considered
 // an Error code.
 //------------------------------------------------------------------------------

function TimeTHexToDateTime(const HexStr: string; TimeZoneCorrection: Integer): TDateTime;
var
  SecondsSince1970: Double;
  Base: TDateTime;
  { DateTimeAsStr: string; //debug Code.}
begin
  Result := 0.0;
  SecondsSince1970 := StrToIntDef('$' + HexStr, 0) + TimeZoneCorrection;
  if SecondsSince1970 <= 0.0 then
    Exit;
  Base := EncodeDate(1970, 1, 1);
  Base := Base + (SecondsSince1970 / 86400.0);
  { DateTimeAsStr := FormatDateTime('yyyy/mm/dd hh:nn:ss',Base);}
  // Inc(CallCount);
  Result := Base;
end;

function TimeTAsciiToDateTime(const AsciiDateStr: string): TDateTime;
const
  Separators = '// ::'; // separators in yyyy/mm/dd hh:mm:ss
  Separators2 = '-- --'; // separators in yyyy/mm/dd hh:mm:ss
var
  Values: array [1..6] of Integer; //year,month,day,hour,minute,second in that order.
  Ch: Char;
  I, U, Len, Index: Integer;
begin
  Result := 0.0; // default Result.
  Len := Length(AsciiDateStr);

 // validate ranges:
  for I := 1 to 6 do
    Values[I] := 0;

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiDateStr are we looking at?
  for I := 1 to 6 do
  begin
    if (I >= 3) and (Index >= Len) then
      Break; // as long as we at least got the date, we can continue.
    for U := 1 to AsciiTime_ExpectLengths[I] do
    begin
      if Index > Len then
        Break;
      Ch := AsciiDateStr[Index];
      if not (Ch in DigitSymbols) then
      begin
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString(PChar('JvCsvData: illegal character in datetime string: ' + Ch));
        {$ENDIF DEBUGINFO_ON}
        Exit; // failed: invalid character.
      end;
      Values[I] := (Values[I] * 10) + (Ord(Ch) - Ord('0'));
      Inc(Index);

      if Index > Len then
        Break;
    end;

    // if we haven't reached the end of the string, then
    // check for a valid separator character:
    if Index < Len then
      if (AsciiDateStr[Index] <> Separators[I]) and
        (AsciiDateStr[Index] <> Separators2[I]) then
      begin
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString('TimeTAsciiToDateTime: illegal separator Char');
        {$ENDIF DEBUGINFO_ON}
        Exit;
      end;

    // validate ranges:
    if (Values[I] < AsciiTime_MinValue[I]) or (Values[I] > AsciiTime_MaxValue[I]) then
    begin
      // (rom) no OutputDebugString in production code
      {$IFDEF DEBUGINFO_ON}
      OutputDebugString('TimeTAsciiToDateTime: range error');
      {$ENDIF DEBUGINFO_ON}
      Exit; // a value is out of range.
    end;
    Inc(Index);
  end;

  // Now that we probably have a valid value we will try to encode it.
  // EncodeData will catch any invalid date values we have let slip through
  // such as trying to encode February 29 on a non-leap year, or the 31st
  // day of a month with only 30 days, etc.
  try
    Result := EncodeDate({year}Values[1], {month} Values[2], {day} Values[3]) +
      EncodeTime({hour}Values[4], {minute} Values[5], {second} Values[6], {msec} 0);
  except
    on E: EConvertError do
      Result := 0.0; // catch any other conversion errors and just return 0.
  end;
end;

function DateTimeToTimeTHex(ADateTime: TDateTime; TimeZoneCorrection: Integer): string;
var
  Base: TDateTime;
  { DateTimeAsStr: string; //debug Code. }
  SecondsSince1970: Integer;
begin
  try
    Base := EncodeDate(1970, 1, 1);
    SecondsSince1970 := Trunc((ADateTime - Base) * 86400.0);
    Result := IntToHex(SecondsSince1970 - TimeZoneCorrection, 8);
  except
    // Catch Failures!
    Result := '';
  end;
end;

function DateTimeToTimeToIsoAscii(ADateTime: TDateTime): string;
begin
  // ISO DATETIME FORMAT:
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime);
end;

function JvFilePathSplit(FileName: string; var Path, FilenameOnly: string): Boolean;
var
  Len, I: Integer;
begin
  Len := Length(FileName);
  Result := False;
  Path := '';
  FilenameOnly := '';
  for I := Len downto 1 do
    if FileName[I] = PathDelim then
    begin
      Path := Copy(FileName, 1, I);
      FilenameOnly := Copy(FileName, I + 1, Len);
      if (Length(FilenameOnly) > 0) and (Length(Path) > 0) and DirectoryExists(Path) then
        Result := True;
      Exit;
    end;
end;

{ Routine to keep backup copies of old Data files around }

function JvCsvBackupPreviousFiles(const FileName: string; MaxFiles: Integer): Boolean;
var
  BackupFolder, FilenameOnly, BackupFilename, RemoveFile: string;
  I: Integer;
  Found: Boolean;

  function MakeFilename(Index: Integer): string;
  begin
    Result := BackupFolder + FilenameOnly + '.' + IntToStr(Index) + '.bak';
  end;

begin
  Result := False;

  if not FileExists(FileName) then
    Exit; // failed.
  if not JvFilePathSplit(FileName, BackupFolder, FilenameOnly) then
  begin
    FilenameOnly := FileName;
    GetDir(0, BackupFolder);
    BackupFolder := BackupFolder + PathDelim;
  end;
  BackupFolder := BackupFolder + 'Backup'+ PathDelim;
  if not DirectoryExists(BackupFolder) then
		{$IFDEF MSWINDOWS}
    CreateDirectory(PChar(BackupFolder), nil);
    {$ENDIF MSWINDOWS}
    {$IFDEF LINUX}
		ForceDirectories(BackupFolder}
		{$ENDIF LINUX}
  Found := False;
  for I := 0 to MaxFiles - 1 do
  begin
    BackupFilename := MakeFilename(I);
    if not FileExists(BackupFilename) then
    begin
      RemoveFile := MakeFilename((I + 1) mod MaxFiles);
      Found := True;
      Break;
    end;
  end;

  if not Found then
  begin
    I := 1;
    BackupFilename := MakeFilename(I);
    RemoveFile := MakeFilename((I + 1) mod MaxFiles);
  end;

  // We remove an old backup if necessary so that the next time we run
  // we will find the gap and know where to write the next numbered
  // backup. That means that anywhere from zero to 998 backups could exist
  // in a circular fashion. Without this logic, we wouldn't know the next
  // extension number to use.
  if FileExists(RemoveFile) then
    DeleteFile(RemoveFile);
  {$IFDEF MSWINDOWS}  
  Windows.CopyFile(PChar(FileName), PChar(BackupFilename), False);
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}  
  QWindows.CopyFile(PChar(FileName), PChar(BackupFilename), False);
  {$ENDIF LINUX}
  Result := True;
end;

procedure TJvCustomCsvDataSet.SetSeparator(const Value: Char);
var
  S: string;
begin
  if FSeparator <> Value then
  begin
    if Value in cInvalidSeparators then
    begin
      if Value in [#32..#255] then
        S := Value
      else
        S := Format('#%.2d',[Ord(Value)]);
      raise EJvCsvDataSetError.CreateResFmt(@RsECsvInvalidSeparatorFmt,[S]);
    end;
    FSeparator := Value;
  end;
end;

procedure TJvCustomCsvDataSet.LoadFromFile(const FileName: string);
var
  Tmp: Boolean;
begin
  Close;
  Tmp := LoadsFromFile;
  try
    LoadsFromFile := True;
    Self.FileName := FileName;
    Open;
  finally
    LoadsFromFile := Tmp;
  end;
end;

procedure TJvCustomCsvDataSet.SaveToFile(const FileName: string);
begin
  ExportCsvFile(FileName);
end;

{ Extremely ugly hack to copy a JvCsvRow binary record from
  one dataset to another }

procedure TJvCustomCsvDataSet.CloneRow(DataSet: TJvCustomCsvDataSet);
begin
  if not FCursorOpen then
    Exit;
  {basic sanity checks}

  { make sure the range is valid and that the csv schema is the same }
  if (RecNo < 0) or (RecNo >= FData.Count) then
    raise EJvCsvDataSetError.CreateResFmt(@RsEProblemReadingRow, [RecNo]);
  if FCsvFieldDef <> DataSet.FCsvFieldDef then
    raise EJvCsvDataSetError.CreateResFmt(@RsEProblemReadingRow, [RecNo]);

  {the ugly hack:}
  CopyMemory(FData[RecNo], DataSet.FData[DataSet.RecNo], SizeOf(TJvCsvRow));

  PCsvRow(FData[RecNo])^.IsDirty := True;
  FFileDirty := True;

  //DataSet.Last; // Force update of screen.

  Resync([]); // Update Data aware controls.
end;

// get contents of one dataset into this dataset. copies only fields that
// match. Raises an exception if an error occurs. returns # of rows copied.

function TJvCustomCsvDataSet.CopyFromDataset(DataSet: TDataset): Integer;
var
  I, MatchFieldCount: Integer;
  FieldName: string;
  MatchSourceField: array of TField;
  MatchDestField: array of TField;
begin
  // Result := -1;
  SetLength(MatchSourceField, FieldCount);
  SetLength(MatchDestField, FieldCount);
  MatchFieldCount := 0;
  for I := 0 to DataSet.FieldCount-1 do
  begin
    MatchSourceField[MatchFieldCount] := DataSet.Fields.FieldByNumber(I+1);
    Assert(Assigned(MatchSourceField[MatchFieldCount]));
    FieldName := MatchSourceField[MatchFieldCount].FieldName;
    try
      MatchDestField[MatchFieldCount] := FieldByName(FieldName);
      Assert(Assigned(MatchDestField[MatchFieldCount]));
      Inc(MatchFieldCount);
    except
      on E: EDatabaseError do
      begin
        // ignore it.
      end;
    end;
  end;
  {$IFDEF DEBUGINFO_ON}
  OutputDebugString(PChar('MatchFieldCount=' + IntToStr(MatchFieldCount)));
  {$ENDIF DEBUGINFO_ON}
  if MatchFieldCount = 0 then
    JvCsvDatabaseError(FTableName, RsETimeTConvError);
  Result := 0;
  DataSet.First;
  if (not Active) and (not LoadsFromFile) then
    Active := True;

  while not Dataset.Eof do
  begin
    Append;
    for I := 0 to MatchFieldCount-1 do
      MatchDestField[I].Value := MatchSourceField[I].Value;
    Post;
    DataSet.Next;
    Inc(Result);
  end;
end;

end.

