{******************************************************************************}
{* WARNING:  JEDI VCL To CLX Converter generated unit.                        *}
{*           Manual modifications will be lost on next release.               *}
{******************************************************************************}

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

unit JvQCsvData;

interface

uses
  {$IFDEF MSWINDOWS}
  Windows,
  {$ENDIF MSWINDOWS}
  {$IFDEF LINUX}
  QWindows,
  {$ENDIF LINUX}
  SysUtils, Classes, 
  Variants, 
  DB;

const
  MaxCalcDataOffset = 256; // 128 bytes per record for Calculated Field Data.
//  JvCsvSep = ','; // converted to property Separator
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
  EJvCsvDataSetError = class(EDatabaseError);
    // Subclass DB.EDatabaseError so we can work nicely with existing Delphi apps.

  EJvCsvKeyError = class(EDatabaseError); // Key Uniqueness or Key Problem

 {  Special Event Types }
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
    flag: TBookmarkFlag;
    data: Integer;
  end;

    { CSV Data File Row is not very dynamic in this version: }
  PtrToPtrToCsvRow = ^PCsvRow; // bookmark data = double pointer indirection! Fun fun fun!
  PCsvRow = ^TJvCsvRow; // a pointer to a record
  TJvCsvRow = record { this MUST be a record, not a class, and must be a flag data record type }
    fdirty: Boolean; // record is dirty (needs to be written to disk)
    columns: Integer;
    Index: Integer; // FData Index (-1 means not in FData)
    wordfield: array[0..MAXCOLUMNS + 1] of Word;
      // lookup field beginning, Column Data (column dirty bit+column length) }
    Text: array[0..MAXLINELENGTH] of Char; // lookup actual character data.
      // bookmark
    Bookmark: TJvCsvBookmark;

      // filter flag;
    filtered: Boolean; // row is hidden from view right now.
    recursionFlag: Boolean; // helps us fix endless recursion bug in GetFieldData callbacks.
  end;

  { Row collection }
  TJvCsvRows = class(TList)
  protected
    FEnquoteBackslash: Boolean;
      // Optional user data (only allocated if used, how efficient is that, eh.)
    FUserData: array of Pointer;
    FUserTag: array of Integer;
    FUserLength: Integer;

    function GetUserTag(Index: Integer): Integer;
    procedure SetUserTag(Index, Value: Integer);

    function GetUserData(Index: Integer): Pointer;
    procedure SetUserData(Index: Integer; Value: Pointer);

    // Get internal value, return as Variant.





  public
    procedure AddRow( Item: PCsvRow);
    procedure InsertRow(const position: Integer;  Item: PCsvRow);

    procedure AddRowStr(const Item: string;Separator:Char); // convert String->TJvCsvRow
    function GetRowPtr(const RowIndex: Integer): PCsvRow;
    function GetRowStr(const RowIndex: Integer): string;
    procedure SetRowStr(const RowIndex: Integer; Value: string;Separator:Char);
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
  TJvCustomCsvDataSetFilterFunction = function(recno:Integer):Boolean of Object;

  // Easily Customizeable Dataset descendant our CSV handler and
  // any other variants we create:
  TJvCustomCsvDataSet = class(TDataSet)
  private
    FSeparator: Char;
    FOpenFileName:String; // This is the Fully Qualified path and filename expanded from the FTableName property when InternalOpen was last called.
    FValidateHeaderRow: Boolean;
    FExtendedHeaderInfo: Boolean;
    procedure SetSeparator(const Value: Char);
    procedure InternalQuickSort(SortList: PPointerList; L, R: Integer; SortColumns:TArrayOfPCsvColumn; ACount:integer;Ascending: Boolean);
    procedure QuickSort(AList: TList; SortColumns: TArrayOfPCsvColumn; ACount: integer; Ascending:Boolean);
  protected
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
    FPendingCsvHeaderParse:Boolean; // NEW FEB 2004 WP.
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

    { data record holder }
    FCsvColumns: TJvCsvColumns; // Column information
    FData: TJvCsvRows; // Rows are a Collection of data pointers.

    { temporary holding space only, for a tstringlist of the file contents }
    FCsvFileAsStrings: TStringlist;

    {  event pointers }
    FOnSpecialData: TJvCsvOnSpecialData;
    FOnGetFieldData: TJvCsvOnGetFieldData;
      // Helps to allow you to update the contents of your CSV data from some other object in memory.
    FOnSetFieldData: TJvCsvOnSetFieldData;
      // Helps to keep some other thing in sync with the contents of a changing CSV file.

    //  Internal Use Only Protected Methods
//    function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer: PChar; virtual;
    procedure CsvRowInit(RowPtr: PCsvRow);

    //NEW and very handy dandy!
    function GetFieldValueAsVariant(CsvColumnData: PCsvColumn; Field:TField; recordIndex:Integer) : Variant;


    // New filtering on cursor (GetRecord advances the cursor past
    // any hidden rows using InternalSkipForward).
    function InternalSkipFiltered(defaultResult: TGetResult; ForwardBackwardMode: Boolean): TGetResult;

    procedure InternalClearFileStrings;
    function InternalLoadFileStrings: Boolean;
    // Internal methods used by sorting:
    function InternalFieldCompare(Column: PCsvColumn; Left, Right: PCsvRow): Integer;
    function InternalCompare(SortColumns: TArrayOfPCsvColumn; SortColumnCount: Integer; Left, Right: PCsvRow;Ascending:Boolean): Integer;

    // key uniqueness needs this:
    function InternalFindByKey(row: PCsvRow): Integer;

    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data:Pointer' stored in treeviews, etc)
    function GetRowUserData: Pointer;
    procedure SetRowUserData(UserData: Pointer);

    function GetRowTag: Integer;
    procedure SetRowTag(tagValue: Integer);


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
    procedure GetBookmarkData(Buffer: PChar; data: Pointer); override;
    function GetBookmarkFlag(Buffer: PChar): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: Pointer); override;
    procedure InternalSetToRecord(Buffer: PChar); override; // on Insertion???
    procedure SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PChar; data: Pointer); override;

    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: Pointer; Append: Boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
{    procedure InternalInsert; override; }{not needed.}


    // Misc methods:
    procedure InternalClose; override;
//    procedure DestroyFields; override;

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

    function _Enquote(strVal: string): string; virtual;
      // puts whole string in quotes, escapes embedded separators and quote characters!
    function _Dequote(strValue: string): string; virtual; // removes quotes

    property Separator: Char read FSeparator write SetSeparator default ',';

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean;override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

                // Autoincrement feature: Get next available auto-incremented value for numbered/indexed autoincrementing fields.
    function GetAutoincrement(fieldName:String):Integer;


    // SELECT * FROM TABLE WHERE <fieldname> LIKE <pattern>:
    procedure SetFilter(FieldName, pattern: string); // Make Rows Visible Only if they match filterString

    procedure ClearFilter; // Clear all previous SetFilters, shows All Rows. Refresh screen.

    procedure _ClearFilter; // Clear Previous Filtering. DOES NOT REFRESH SCREEN.


    procedure CustomFilter( FilterCallback : TJvCustomCsvDataSetFilterFunction  ); {NEW:APRIL 2004-WP}

    // ----------- THIS IS A DUMMY FUNCTION, DON'T USE IT!:
    function Locate(const KeyFields: string; const KeyValues: Variant;
      Options: TLocateOptions): Boolean; override;

    //------------

    /// procedure FilteredDeletion(Inverted:Boolean); /// XXX TODO?
    /// procedure DeleteRowsMatchingFilter; /// XXX TODO?
    /// procedure DeleteRowsNotMatchingFilter; /// XXX TODO?

    // this is necessary to make bookmarks work as well:
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    // Additional procedures
    procedure EmptyTable;

      // Tells controls to redraw.
    procedure Refresh;

    // Clone current row/record from one CsvDataSet to another (primitive synchronization/copying ability).
    procedure CloneRow(dataset: TJvCustomCsvDataSet);

    // TODO: Implement row/record copy from ANY dataset.
    
    // A fast row lookup function specific to this CSV table object.
    function FindByCsvKey(key: string): Boolean;

    // Sort the table:
    procedure Sort(SortFields: string; Ascending: Boolean);

    // All rows have a UserData and UserTag property, these
    // next two functions quickly set all the userdata and usertag
    // values for all rows, which is a good way to set defaults
    // without having to iterate through the dataset.
    procedure SetAllUserData(data: Pointer);
    procedure SetAllUserTags(tagValue: Integer);

    // The UserData/UserTag properties apply to the row that the
    // cursor is sitting on. Without visibly moving the cursor,
    // its handy to get/set the usertag and data values.
    function GetUserTag(recno: Integer): Integer;
    procedure SetUserTag(recno, newValue: Integer);

    function GetUserData(recno: Integer): Pointer;
    procedure SetUserData(recno: Integer; newValue: Pointer);

    function GetCsvHeader: string; // NEW FEB 2004 WP

    {  Additional Public methods }
    procedure OpenWith(Strings: TStrings); virtual;

    procedure AppendWith(Strings: TStrings); virtual;

    { Special declarations }
    // as long as the field names and positions have not changed.
    procedure AssignFromStrings(const Strings: TStrings); virtual; // update String data directly.
    procedure AssignToStrings(Strings: TStrings); virtual;

    procedure DeleteRows(FromRow, ToRow: Integer); // NEW: Quickly zap a bunch of rows:
    procedure ExportRows(FileName: string; FromRow, ToRow: Integer); // NEW: Quickly save a bunch of rows:

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
    procedure AppendRowString(RowAsString: string);
      // Along with GetRowAsString, easy way to copy a dataset to another dataset!

    function IsKeyUnique: Boolean; // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);
  protected
    property InternalData: TJvCsvRows read FData write FData;
    property AppendedFieldCount: Integer read FAppendedFieldCount;
      // Number of fields not in the file on disk, appended to file as NULLs during import.
      // Per-Record user-data fields:
      //    Each record can have a pointer (for associating each row with an object)
    property UserData: Pointer read GetRowUserData write SetRowUserData;
      //    Each record can have a tag (Integer) (for help in marking rows as Selected/Unselected or some other
      //    end user task)
    property UserTag: Integer read GetRowTag write SetRowTag;

    property FileName: string read FTableName write SetTableName;
    property ReadOnly: Boolean read FReadOnly write FReadOnly default False;
    property Changed: Boolean read FFileDirty write FFileDirty;
//     property DataFileSize: Integer read GetDataFileSize;

     // CSV Table definition properties:
    property CsvFieldDef: string read FCsvFieldDef write SetCsvFieldDef; // Our own "Csv Field Definition String"
    property CsvKeyDef: string read FCsvKeyDef write FCsvKeyDef; // Primary key definition.
    property CsvUniqueKeys: Boolean read FCsvUniqueKeys write FCsvUniqueKeys; // Rows must be unique on the primary key.
    // if HasHeaderRow is True, calidate that it conforms to CvsFieldDef
    property ValidateHeaderRow:Boolean read FValidateHeaderRow write FValidateHeaderRow default True;
    property ExtendedHeaderInfo:Boolean read FExtendedHeaderInfo write FExtendedHeaderInfo;

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
    property OpenFileName:String read FOpenFileName; // Set in InternalOpen, used elsewhere.
    property FieldDefs stored FieldDefsStored;
    property TableName: string read FTableName; // Another name, albeit read only, for the FileName property!
    property HasHeaderRow: Boolean read FHasHeaderRow write FHasHeaderRow default True;
    property HeaderRow:String read FHeaderRow; // first row of CSV file.
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
procedure StringToCsvRow(const RowString: string; Separator:Char; RowItem: PCsvRow; permitEscapeSequences, EnquoteBackslash: Boolean);

function CsvRowItemCopy(Source, Dest: PCsvRow; FieldIndex, FieldSize: Integer): Boolean;
procedure SetCsvRowItem(pItem: PCsvRow; ColumnIndex: Integer; newValue: string);
function GetCsvRowItem(pItem: PCsvRow; ColumnIndex: Integer): string;
procedure CsvRowSetDirtyBit(row: PCsvRow; ColumnIndex: Integer);
procedure CsvRowClearDirtyBit(row: PCsvRow; ColumnIndex: Integer);
function CsvRowGetDirtyBit(row: PCsvRow; ColumnIndex: Integer): Boolean;
procedure CsvRowSetColumnMarker(row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
function CsvRowGetColumnMarker(row: PCsvRow; ColumnIndex: Integer): Integer;

{ Date/Time String decoding functions }
function TimeTHexToDateTime(HexStr: string; TimeZoneCorrection: Integer): TDateTime;
function TimeTAsciiToDateTime(AsciiDateStr: string): TDateTime;

{ Date/Time string encoding functions }
function DateTimeToTimeToIsoAscii(aDateTime: TDateTime): string;
function DateTimeToTimeTHex(aDateTime: TDateTime; TimeZoneCorrection: Integer): string;

{ Routine to keep backup copies of old data files around }
function JvCsvBackupPreviousFiles(const FileName: string; MaxFiles: Integer): Boolean;

//JvCsvWildcardMatch:
// Recursive wildcard (%=AnyString, ?=SingleChar) matching function with
// Boolean sub expressions (|=or, &=and).
function JvCsvWildcardMatch(data, pattern: string): Boolean;

implementation

uses  
  QForms, QControls,  
  JvQJCLUtils, JvQCsvParse, JvQConsts, JvQResources;

var
  // (rom) disabled unused
  // CallCount: Integer = 0;
  AsciiTime_MinValue: array[1..6] of Integer = (1900, 1, 1, 0, 0, 0);
  AsciiTime_MaxValue: array[1..6] of Integer = (3999, 12, 31, 23, 59, 59);
  AsciiTime_ExpectLengths: array[1..6] of Integer = (4, 2, 2, 2, 2, 2);
const
  // These characters cannot be used for separator for various reasons:
  // Either they are used as field type specifiers, break lines or are used to
  // delimit field content
  cInvalidSeparators = [#0, Backspace, Lf, #12, Cr, #39, '"', '\',
    '$', '%', '&', '@', '#', '^', '!', '-'];

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

  // FBuffer size includes CSV Text buffer, and the bookmark data, followed
  // by space for storing the binary form of a calculated-field:

  // initial FBufferSize size: My theory is that we should pick a conservative
  // estimate plus a margin for error:

  FBufferSize := SizeOf(TJvCsvRow) + MaxCalcDataOffset; //;128; {CalcFieldsSize}
  //; // our regular record + calculated field data.

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
// user-accessible 'Data:Pointer' stored in treeviews, etc)

function TJvCustomCsvDataSet.GetRowUserData: Pointer;
var
  recno: Integer;
begin
  recno := GetRecNo;
  Result := FData.GetUserData(recno);
end;

procedure TJvCustomCsvDataSet.SetRowUserData(UserData: Pointer);
var
  recno: Integer;
begin
  recno := GetRecNo;
  FData.SetUserData(recno, UserData);
end;

function TJvCustomCsvDataSet.GetRowTag: Integer;
var
  recno: Integer;
begin
  recno := GetRecNo;
  Result := FData.GetUserTag(recno);
end;

procedure TJvCustomCsvDataSet.SetRowTag(tagValue: Integer);
var
  recno: Integer;
begin
  recno := GetRecNo;
  FData.SetUserTag(recno, tagValue);
end;

function _WildcardsMatchBoolOp(data, pattern: string; boolOp: Char): Boolean;
var
  subPattern: array[0..20] of string;
  t, Count: Integer;
begin
  Count := StrSplit(pattern, boolOp, {Chr(0)=No Quoting} Chr(0), subPattern, 20);
  if Count > 0 then
  begin
    for t := 0 to Count - 1 do
    begin
      Result := JvCsvWildcardMatch(data, subPattern[t]);
      // If ANY OR True return True;
      // if ANY AND False return False;
      if (boolOp = '|') = Result then
        Exit;
    end;
  end
  else
  begin // split failed...
    Result := False;
    Exit;
  end;
  // if we get here, no short circuit was possible.
  if (boolOp = '|') then
    Result := False // NONE of the OR conditions were met!
  else
    Result := True; // ALL of the AND condition were met!
end;

procedure TJvCustomCsvDataSet.SetAllUserTags(tagValue: Integer);
var
  t: Integer;
begin
  FData.SetUserTag(FData.Count - 1, tagValue);
  for t := 0 to FData.Count - 2 do
    FData.SetUserTag(t, tagValue);
end;

procedure TJvCustomCsvDataSet.SetAllUserData(data: Pointer);
var
  t: Integer;
begin
  FData.SetUserData(FData.Count - 1, data); // Optimization. Ensures we only call SetLength ONCE!
  for t := 0 to FData.Count - 2 do
    FData.SetUserData(t, data);
end;

function TJvCustomCsvDataSet.GetUserTag(recno: Integer): Integer;
begin
  Result := FData.GetUserTag(recno);
end;

procedure TJvCustomCsvDataSet.SetUserTag(recno, newValue: Integer);
begin
  FData.SetUserTag(recno, newValue);
end;

function TJvCustomCsvDataSet.GetUserData(recno: Integer): Pointer;
begin
  Result := FData.GetUserData(recno);
end;

procedure TJvCustomCsvDataSet.SetUserData(recno: Integer; newValue: Pointer);
begin
  FData.SetUserData(recno, newValue);
end;

// Recursive wildcard matching function

function JvCsvWildcardMatch(data, pattern: string): Boolean;
var
  t: Integer;
  firstwildcard: Integer;
  datalength, patternlength, dataposition, patternposition: Integer;
  firstBoolCondition: Integer;
begin
  Result := True;
  patternlength := Length(pattern);
  if patternlength = 0 then
    Exit;
  // no data?
  datalength := Length(data);
  if datalength = 0 then
  begin
    Result := (pattern = '%') or (pattern = '');
    Exit; // definitely no match.
  end;
  // replace all '%%' -> '%' (don't put duplicate wildcards in)
  t := 1;
  while t < patternlength do
  begin
    if (pattern[t] = '%') and (pattern[t + 1] = '%') then
    begin
      pattern := Copy(pattern, 1, t) + Copy(pattern, t + 2, patternlength);
      patternlength := Length(pattern);
    end
    else
      Inc(t);
  end;
  // find any | and split into two or more strings, and run ORs on them
  firstBoolCondition := Pos('&', pattern);
  if firstBoolCondition > 0 then
  begin
    Result := _WildcardsMatchBoolOp(data, pattern, '&');
    Exit;
  end;
  firstBoolCondition := Pos('|', pattern);
  if firstBoolCondition > 0 then
  begin
    Result := _WildcardsMatchBoolOp(data, pattern, '|');
    Exit;
  end;

  firstwildcard := Pos('%', pattern); // wildcards?
  if firstwildcard = 0 then
    firstwildcard := Pos('?', pattern); // other wildcard.

  if firstwildcard <= 0 then
  begin // no wildcard case.
    if data = pattern then
      Result := True
    else
      Result := False;
    Exit; // simple match returns immediately.
  end;
  // wildcard tail?
  if ((firstwildcard = patternlength) and (pattern[1] <> '?')) then
  begin // prefix match
    if Copy(data, 1, patternlength - 1) = Copy(pattern, 1, patternlength - 1) then
      Result := True
    else
      Result := False;
    Exit; // tail case is easy!
  end;
  // match literal characters until we hit wildcards,
  // then search for a wildcard resync, which continues
  // recursively.
  Result := True;
  dataposition := 1;
  patternposition := 1;
  while ((dataposition <= datalength) and (patternposition <= patternlength)) do
  begin
    // WILDCARD HANDLER
    if pattern[patternposition] = '?' then
    begin // match any one character or nothing.
      Inc(patternposition);
      Inc(dataposition);
    end
    else
    if pattern[patternposition] = '%' then
    begin
      if patternposition = patternlength then
      begin // last byte!
        Result := True;
        Exit;
      end;
       // Resync after %:
      t := Pos(pattern[patternposition + 1], data);
      while t > 0 do
      begin // possible resync point!
        Result := JvCsvWildcardMatch(Copy(data, t, Length(data)),
          Copy(pattern, patternposition + 1, patternlength)
          );
        if Result then
          Exit; // found a resync, and rest of strings match
        data := Copy(data, t + 1, datalength);
        datalength := Length(data);
//        dataposition := 0;
        if datalength = 0 then
        begin
          Result := False;
          Exit;
        end;
        t := Pos(pattern[patternposition + 1], data);
      end; { end while loop }
       // failed to resync
      Result := False;
      Exit;
    end
    else
    begin // NORMAL CHARACTER
      if data[dataposition] <> pattern[patternposition] then
      begin
        Result := False; // failed.
        Exit;
      end;
      Inc(dataposition);
      Inc(patternposition);
    end;
  end;
  if (dataposition <= datalength) and (patternposition <= patternlength) then
    Result := False; // there is pattern left over, or data left over.
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
procedure TJvCustomCsvDataSet.CustomFilter( FilterCallback : TJvCustomCsvDataSetFilterFunction  );
var
  t: Integer;
  pRow: PCsvRow;
begin
  Assert(Assigned(FilterCallback));
  // Now check if field value matches given pattern for this row.
  for t := 0 to FData.Count - 1 do
  begin
      pRow := PCsvRow(FData[t]);
      Assert(Assigned(pRow));
      // if custom function returns False, hide the row.
      pRow^.filtered  := not FilterCallback(t);
  end;
  FIsFiltered := True;
  if Active then
    First;
end;

procedure TJvCustomCsvDataSet.SetFilter(FieldName, pattern: string);
  // Make Rows Visible Only if they match filterString
var
  valueLen, t: Integer;
  pRow: PCsvRow;
  fieldRec: PCsvColumn;
  FieldIndex: Integer;
  fieldValue: string;
  //stillVisible : Integer;
  //m:TBookmark;
begin
//  m := GetBookmark;
  fieldRec := FCsvColumns.FindByName(FieldName);
//  stillVisible := 0;
  if not Assigned(fieldRec) then
    Exit;
  FieldIndex := fieldRec^.FPhysical;
  valueLen := Length(pattern); // if valuelen is zero then we are searching for blank or nulls
  pattern := UpperCase(pattern); // make value case insensitive.

  // Now check if field value matches given pattern for this row.
  for t := 0 to FData.Count - 1 do
  begin
    pRow := PCsvRow(FData[t]);
    if not pRow^.filtered then
    begin
      fieldValue := FData.GetARowItem(t, FieldIndex);
      if (Length(fieldValue) > 0) and (fieldValue[1] = '"') then
        fieldValue := _Dequote(fieldValue); // remove quotes.
      if valueLen = 0 then
      begin
        if fieldValue <> '' then // if not empty, hide row.
          pRow^.filtered := True;
      end
      else
      begin
        fieldValue := UpperCase(fieldValue);
        if JvCsvWildcardMatch(fieldValue, pattern) then // hide row if not same prefix
        begin
          // Inc(stillVisible)   // count the number that are still visible.
        end
        else
          pRow^.filtered := True
      end;
    end
  end;
  FIsFiltered := True;
  if Active then
  begin
    //try
    //    GotoBookmark(m);
    //except
    //    on E: EDatabaseError do
    //    begin
    //        First;
    //        Exit;
    //    end;
    //end;
    //if (Self.RecNo>=0) then
    //begin
    //  pRow := PCsvRow( FData[Self.RecNo] );
    //  if (pRow^.filtered) then
    //        First;
    //end
    //else
    First;
  end;
end;

procedure TJvCustomCsvDataSet._ClearFilter; // Clear Previous Filtering.
var
  t: Integer;
  pRow: PCsvRow;
begin
  for t := 0 to FData.Count - 1 do
  begin
    pRow := PCsvRow(FData[t]);
    if Assigned(pRow) then
      pRow^.filtered := False; // clear all filter bits.
  end;
  FIsFiltered := False;
end;

procedure TJvCustomCsvDataSet.ClearFilter; // Clear Previous Filtering.
var
  m: TBookmark;
begin
  m := GetBookmark;
  _ClearFilter;
  // Update screen.
  if Active then
  begin
    if Assigned(m) then
      GotoBookmark(m)
    else
      First;
  end;
end;

function TJvCustomCsvDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Bookmark <> nil) and (PInteger(Bookmark)^ >= 0) and (PInteger(Bookmark)^ < FData.Count);
end;

function TJvCustomCsvDataSet.AllocRecordBuffer: PChar;
var
  RowPtr: PCsvRow;
begin
  RowPtr := AllocMem(FBufferSize {SizeOf(TJvCsvRow)});
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
    FCsvColumns.Clear; // Clear our own CSV related field data
    FData.Clear; // Clear out data
  end;
end;

procedure TJvCustomCsvDataSet.FreeRecordBuffer(var Buffer: PChar);
//var
//  RowPtr:PCsvRow;
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
  t: Integer;
  ColCount: Integer;
begin
  RowPtr^.Index := -1; // Not Yet Indexed
  RowPtr^.fdirty := False;
  RowPtr^.Bookmark.flag := bfEOF;
  RowPtr^.Bookmark.data := ON_BOF_CRACK; // no index into FData yet.
  CsvRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);

  ColCount := FCsvColumns.Count;
  if ColCount <= 0 then
    ColCount := 10;

  for t := 1 to ColCount do
  begin // create an empty line of just commas
    if t < ColCount then
      RowPtr^.Text[t - 1] := FSeparator
    else
      RowPtr^.Text[t - 1] := Chr(0);
    RowPtr^.Text[t] := Chr(0);
    CsvRowSetColumnMarker(RowPtr, {column} t - 1, {marker value} t - 1);
    CsvRowSetColumnMarker(RowPtr, {column} t, {marker value} COLUMN_ENDMARKER);
  end;
end;

function TJvCustomCsvDataSet.IsKeyUnique: Boolean;
  // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
begin
  Result := False; // not yet implemented! XXX
end;


function TJvCustomCsvDataSet.GetFieldValueAsVariant(CsvColumnData: PCsvColumn; Field:TField; recordIndex:Integer) : Variant;
var
  RowPtr: PCsvRow;
  {ActiveRowPtr:PCsvRow;}
  TempString: string;
  PhysicalLocation: Integer;

  l: Integer;
begin
  Assert(Assigned(FCsvColumns));

  if not Assigned(CsvColumnData) then
  begin
    JvCsvDatabaseError(FTableName, Format(RsEUnableToLocateCSVFileInfo, [Field.Name]));
    Exit;
  end;

  PhysicalLocation := CsvColumnData^.FPhysical;

  if (PhysicalLocation<0) and FPendingCsvHeaderParse then
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

  RowPtr := FData[recordIndex];

  TempString := GetCsvRowItem(RowPtr, PhysicalLocation);

  // Strip quotes first!
  if Field.DataType = ftString then
  begin
    l := Length(TempString);
    if l >= 2 then
      if (TempString[1] = '"') and (TempString[l] = '"') then
      begin // quoted string!
        TempString := _Dequote(TempString);
      end;
  end;

  try
    case Field.DataType of
      ftString:     Result := TempString;
      ftInteger:    Result  := StrToInt(TempString);
      ftFloat:      Result  := StrToFloatUS(TempString);
      ftBoolean:
        begin
          if StrToIntDef(TempString, 0) <> 0 then
              Result := True
          else
              Result := False;
        end;
      ftDateTime:
           { one of three different datetime formats}
           if Length(TempString)>0 then 
           case CsvColumnData^.FFlag of
             jcsvAsciiDateTime:
                  Result := TimeTAsciiToDateTime(TempString);
             jcsvGMTDateTime:
                  Result := TimeTHexToDateTime(TempString,0);
             jcsvTZDateTime:
                  Result := TimeTHexToDateTime(TempString, FTimeZoneCorrection);
           end; {end case of jcsv various datetime encodings}
  end; {case}
  except
    Result := Unassigned; // No value.
  end;
end;

// Auto-increment
function TJvCustomCsvDataSet.GetAutoincrement(fieldName:String):Integer;
var
 recindex:Integer;
 FieldLookup: TField;
 CsvColumnData : PCsvColumn;
 max,value:integer;
 RowPtr: PCsvRow;
begin
  result := -1; // failed.
  FieldLookup := FieldByName(fieldName);
  if FieldLookup.DataType <> ftInteger then
      exit; // failed. Can only auto increment on integer fields!

  if not Assigned(FieldLookup) then
      exit; //failed.

  CsvColumnData := FCsvColumns.FindByFieldNo(FieldLookup.FieldNo);
  max := -1;
  for recindex := 0 to Self.FData.Count-1 do
    try
      // skip filtered rows:
       RowPtr := FData[recindex];
       Assert(Assigned(RowPtr)); // FData should never contain nils!
       if (RowPtr^.filtered) then
            continue; // skip filtered row!

      value := GetFieldValueAsVariant(CsvColumnData,FieldLookup,recindex);
      if value > max then
          max := value; // keep maximum.
    except
      on E: EVariantError do
        exit; // failed.
    end;
  if max < 0 then
      result := 0 // autoincrement starts at zero
  else
      result := max+1; // count upwards.
end;


// XXX TODO: REMOVE HARD CODED LIMIT OF 20 FIELDS SEARCHABLE!!!
function TJvCustomCsvDataSet.Locate(const KeyFields: string; const KeyValues: Variant;
  Options: TLocateOptions): Boolean; // override;
  // Options is    [loCaseInsensitive]
  //              or [loPartialKey]
  //              or [loPartialKey,loCaseInsensitive]
  //              or [] {none}
var
  KeyFieldArray: array[0..20] of string;
  FieldLookup: array[0..20] of TField;
  CsvColumnData :array[0..20] of PCsvColumn;
  FieldIndex: array[0..20] of Integer;

  recindex, t, lo, hi, Count, VarCount: Integer;
  Value: Variant;
  MatchCount: Integer;
  StrValueA, StrValueB: string;

begin
  Result := False;
  if not Active then Exit;
  if Pos(',',KeyFields)>0 then
    Count := StrSplit(KeyFields, ',', Chr(0), KeyFieldArray, 20)
  else
    Count := StrSplit(KeyFields, ';', Chr(0), KeyFieldArray, 20);

  if not ((VarType(KeyValues) and varArray) > 0) then
    Exit;
  lo := VarArrayLowBound(KeyValues, 1);
  hi := VarArrayHighBound(KeyValues, 1);
  VarCount := (hi - lo) + 1;
  if VarCount <> Count then
    Exit;
  if Count = 0 then
    Exit;
  if KeyFieldArray[0] = '' then
    Exit;
  for t := 0 to 20 do
  begin
    if t < Count then
    begin
      FieldLookup[t] := FieldByName(KeyFieldArray[t]);
      CsvColumnData[t] := FCsvColumns.FindByFieldNo(FieldLookup[t].FieldNo);
      if not Assigned(FieldLookup[t]) then
        Exit;
      FieldIndex[t] := FieldLookup[t].Index;
    end
    else
    begin
      FieldLookup[t] := nil;
      FieldIndex[t] := -1;
    end;
  end;

   // Now search
  //First;
  for recindex := 0 to Self.FData.Count-1 do
  begin
    MatchCount := 0;
    for t := 0 to Count - 1 do
    begin
      Value := GetFieldValueAsVariant( CsvColumnData[t], FieldLookup[t], recindex);
      if Value = KeyValues[t + lo] then
        Inc(MatchCount)
      else
      if Options <> [] then
      begin
        if VarIsStr(Value) then
        begin
          StrValueA := Value;
          StrValueB := KeyValues[t + lo];
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
            begin
              if Pos(StrValueB, StrValueA) = 1 then
                Inc(MatchCount);
            end;
          end;
        end;
      end;
    end;
    if MatchCount = Count then
    begin
      RecNo := recindex;//Move cursor position.
      Result := True;
      Exit;
    end;
   // Next;
  end;

end;

function TJvCustomCsvDataSet.InternalSkipFiltered(defaultResult: TGetResult; ForwardBackwardMode: Boolean):
  TGetResult;
var
  LimitReached: Boolean;
  RowPtr: PCsvRow;
begin
  Result := defaultResult;
  if FRecordPos < 0 then
    Exit;
  LimitReached := False; // hit BOF or EOF?
  while not LimitReached do
  begin
    { no skippage required }
    RowPtr := PCsvRow(FData.GetRowPtr(FRecordPos));
    if not RowPtr^.filtered then
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
    //Trace(' GetRecord - called when data buffer empty.');
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

        end
      end;

      // default case:
  else
    JvCsvDatabaseError(FTableName, RsEGetMode);
  end; {end case}

  if Result = grOK then
  begin
       //Trace( ' GetRecord FRecordPos='+IntToStr(FRecordPos)+'Result=grOk' );
    try
        { get a record into a buffer }
      RowPtr := PCsvRow(Buffer); // Cast to a Row Data Structure to our own type.
      Move({source:}FData.GetRowPtr(FRecordPos)^, {dest:} RowPtr^, SizeOf(TJvCsvRow));
      RowPtr^.Bookmark.flag := bfCurrent;
      RowPtr^.Bookmark.data := FRecordPos;

        // Update calculated fields for this row:
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);

    except
      on E:EJvCsvDataSetError do
      begin
        raise; // pass our error through.
      end;
      on E:Exception do
      begin
        JvCsvDatabaseError(FTableName, Format(RsEProblemReadingRow, [FRecordPos]) +' ' + E.Message);
      end;
    end;
  end
  else
  begin

      // fudge: Get bookmark into a record for BOF and EOF records:
{      if RowPtr <> NIL then
          RowPtr^.bookmark.data := FRecordPos;}

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

function TJvCustomCsvDataSet._Enquote(strVal: string): string;
  // puts whole string in quotes, escapes embedded commas and quote characters!
  // Can optionally deal with newlines also.
var
  s: string;
  t, l: Integer;
  ch: Char;
  localEnquoteBackslash: Boolean;
begin
  localEnquoteBackslash := FEnquoteBackslash; // can force on, or let it turn on automatically.

  if Pos(strVal, Chr(13)) > 0 then // we are going to need to enquote the backslashes
    localEnquoteBackslash := True; // absolutely need it in just this case.
  if Pos(strVal, Chr(10)) > 0 then
    localEnquoteBackslash := True; // absolutely need it in just this case.

  s := '"';
  l := Length(strVal);
  for t := 1 to l do
  begin
    ch := strVal[t];
    if ch = Chr(13) then
      // slighlty unstandard csv behavior, hopefully transparently interoperable with other apps that read CSVs
      s := s + '\r'
    else
    if ch = Chr(10) then // replace linefeed with \n. slighlty unstandard csv behavior.
      s := s + '\n'
    else
    if localEnquoteBackslash and (ch = '\') then
    begin // it would be ambiguous not to escape this in this case!
      s := s + '\\';
      FEnquoteBackslash := True; // XXX This is a lurking bug. Some day we'll get bit by it.
    end
    else
    if ch = '"' then // always escape quotes by doubling them, since this is standard CSV behaviour
      s := s + '""'
    else
    if Ord(ch) >= 32 then // strip any other low-ascii-unprintables
      s := s + ch;
  end; {for}
  s := s + '"'; // end quote.
  Result := s;
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
  cp, PhysicalLocation: Integer;
  pDestination: PChar;
  CsvColumnData: PCsvColumn;
  DT: TDateTime;
begin

  //Trace( 'SetFieldData '+Field.FieldName );
  pDestination := GetActiveRecordBuffer;
  RowPtr := PCsvRow(pDestination);

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
      OutputDebugString(PChar('JvCsvData.pas: '+Name+'.SetFieldData( Field='+Field.FieldName+',...): Invalid field.Offset in Calculated or Lookup field. '));
      {$ENDIF DEBUGINFO_ON}
      Exit;
    end;
    Inc(pDestination, SizeOf(TJvCsvRow) + Field.Offset);
    Boolean(pDestination[0]) := (Buffer <> nil);
    if Boolean(pDestination[0]) then
      CopyMemory(@pDestination[1], Buffer, Field.DataSize);
    //Result := True; {there is no return value, oops}
    Exit;
  end;

 // If we get here, we are dealing with a physical record:

 // Set a field data, taking the physical to logical ordering translation into
 // account:
  CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CsvColumnData) then
    Exit;

  PhysicalLocation := CsvColumnData^.FPhysical;
  // ----- BUG FIX FEB 2004 WP (Location #1 of 2)
  if (PhysicalLocation<0) and FPendingCsvHeaderParse then
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
            cp := -1
          else
            for cp := 1 to Field.Size - 1 do
              if PChar(Buffer)[cp] = Chr(0) then
                Break;
          if cp > Field.Size - 1 then
            cp := Field.Size - 1;
          NewVal := Copy(PChar(Buffer), 1, cp + 1);
            //----------------------------------------------------------------------------------------------------
            // NEW RULE: If user displayed value contains a comma, a backslash, or a double quote character
            // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
            // and backslashes preceded by a backslash character.
            //----------------------------------------------------------------------------------------------------
          if (Pos(Separator, NewVal) > 0) or
            (Pos(Chr(13), NewVal) > 0) or
            (Pos(Chr(10), NewVal) > 0) or
            (Pos('"', NewVal) > 0) or
            ((Pos('\', NewVal) > 0) and (FEnquoteBackslash)) then
            NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
        end;
      ftInteger:
        begin
          NewVal := IntToStr(PInteger(Buffer)^);
        end;
      ftFloat:
        begin
          NewVal := FloatToStr(PDouble(Buffer)^);
        end;
      ftBoolean:
        begin
          NewVal := IntToStr(Ord(PWordBool(Buffer)^)); // bugfix May 26, 2003 - WP
        end;
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

 // Set new data value (NewVal = String)
  SetCsvRowItem(RowPtr, PhysicalLocation, NewVal);
  if Assigned(FOnSetFieldData) and (RowPtr^.Index >= 0) then
  begin
    FOnSetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName, NewVal);
  end;

 // Set a dirty bit so we remember to write this later:
  CsvRowSetDirtyBit(PCsvRow(pDestination), PhysicalLocation);

 // Set the file-wide dirty bit:
  FFileDirty := True;

 // Notify controls of a field change:
  DataEvent(deFieldChange, Longint(Field));
end;

// Removes first and last character of the string (assumes they are quotes,
// to be called byGetFieldData only!)

function TJvCustomCsvDataSet._Dequote(strValue: string): string;
var
  s: string;
  t, l: Integer;
  ch: Char;
  skipFlag: Boolean;
begin
  l := Length(strValue);
  skipFlag := False;
  s := '';
  if Length(strValue) < 2 then
  begin
    Result := s;
    Exit;
  end;

  for t := 2 to l - 1 do
  begin
    ch := strValue[t];
    if FEnquoteBackslash then
    begin
      if (not skipFlag) and (ch = '\') then
      begin
        skipFlag := True; // whatever is after the backslash is an escaped literal character.
        continue;
      end
      else
      begin
        // backslashed escape codes for carriage return, linefeed.
        if skipFlag then
        begin
          case ch of
          'r':
            ch := Cr;
          's':
            ch := #32;
          't':
            ch := Tab;
          'n':
            ch := Lf;
          end;
        end;
        skipFlag := False;
      end;
    end
    else
    begin
      if (not skipFlag) and (ch = '"') and (t < (l - 1)) and (strValue[t + 1] = '"') then
      begin
        skipFlag := True;
        continue; // skip first of the doubled quote characters.
      end
      else
        skipFlag := False;
    end;
    s := s + ch;
  end;
  Result := s;
end;

// Tells controls to redraw.

procedure TJvCustomCsvDataSet.Refresh;
var
  m: TBookmark;
begin
  if State <> dsBrowse then
    Exit;
  DisableControls;
  try
    m := GetBookmark; // This appears a bit silly but it works very well.
    First; // Redraws all controls once to relocate to top.
    GotoBookmark(m); // Go back where we were. This could Result in some odd scrolling behaviour but I haven't seen it yet.
  finally
    EnableControls;
  end;
end;

function TJvCustomCsvDataSet.GetFieldData(Field: TField; Buffer: Pointer): Boolean;
var
  RowPtr: PCsvRow;
  {ActiveRowPtr:PCsvRow;}
  pSource: PChar;
  UserString, TempString: string;
  PhysicalLocation: Integer;
  CsvColumnData: PCsvColumn;
  aDateTime: TDateTime;
  l: Integer;
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

  pSource := GetActiveRecordBuffer; // This should not be nil EXCEPT if table is Empty or Closed.
  if pSource = nil then
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
  // direct memory copy into calculated field or lookup field data area
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

    Inc(pSource, SizeOf(TJvCsvRow) + Field.Offset);
    if Buffer = nil then
    begin
      // NULL CHECK MEANS THAT SOMEONE IS ASKING IF THIS FIELD HAS A VALUE. RETURN True.
      Result := (Field.DataSize > 0); // Yes, we could read this field if you asked us to!
      Exit;
    end;

    if Field.DataSize <= 0 then
      PChar(Buffer)[0] := Chr(0)
    else // Get the field data from the buffer:
      CopyMemory(Buffer, @pSource[1], Field.DataSize);

    if (Buffer <> nil) and (Field.DataSize > 0) then
       Result := True;
    Exit;
  end;

  //------------------------------------------------------------------------
  // If we get here we must be dealing with a real column of data
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
  if (PhysicalLocation<0) and FPendingCsvHeaderParse then
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

  RowPtr := PCsvRow(pSource);
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
    l := Length(TempString);
    if l >= 2 then
      if (TempString[1] = '"') and (TempString[l] = '"') then
      begin // quoted string!
        TempString := _Dequote(TempString);
      end;
  end;

  // Custom Get Method allows us to create a "Virtual DataSet" where the data
  // in the CSV rows is really just a mirror which can be updated when displayed
  // but which we really are fetching from somewhere else.
  if Assigned(FOnGetFieldData) and (RowPtr^.Index >= 0) and (not RowPtr^.recursionFlag) then
  begin
    RowPtr^.recursionFlag := True;
    UserString := TempString;
    FOnGetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName,
      UserString);
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
    RowPtr^.recursionFlag := False;
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
    Exit; { cannot actually copy data into nil buffer, so returns now. }
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
          StrCopy(FTempBuffer, PChar(TempString)); // we copy in the data portion
          Move(FTempBuffer^, Buffer^, Field.Size + 1); // Buffer^ is now zero padded.
        end;

        // Standard Integer conversion:
      ftInteger: PInteger(Buffer)^ := StrToInt(TempString);

        // Standard Double-precision Float conversion:
      ftFloat:
        begin
          PDouble(Buffer)^ := StrToFloatUS(TempString)
        end;

      ftBoolean:
        begin
          if TempString = '' then
          begin
            PInteger(Buffer)^ := 0;
          end
          else
          begin
            if StrToIntDef(TempString, 0) <> 0 then
              PWordBool(Buffer)^ := True // bugfix May 26, 2003 - WP
            else
              PWordBool(Buffer)^ := False; // bugfix May 26, 2003 - WP
          end;
        end;
      ftDateTime:
        case CsvColumnData^.FFlag of
             // Ascii Date 1999/03/05 08:23:15
          jcsvAsciiDateTime:
            begin
              aDateTime := TimeTAsciiToDateTime(TempString);
              if aDateTime <= 1.0 then
              begin
                Result := False; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(aDateTime);
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
              Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(aDateTime));

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
        end; {end case}
    else // not a valid ftXXXX type for this TDataSet descendant!?
      JvCsvDatabaseError(FTableName, RsEFieldTypeNotHandled);
    end // end case.
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



// Our bookmark data is a pointer to a PCsvData

procedure TJvCustomCsvDataSet.GetBookmarkData(Buffer: PChar; data: Pointer);
begin
// t:= PCsvRow(Buffer)^.bookmark.data;
  PInteger(data)^ := PCsvRow(Buffer)^.Bookmark.data;
end;

function TJvCustomCsvDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PCsvRow(Buffer)^.Bookmark.flag;
end;

// nobody mentioned that I needed this to be overloaded, but I only found
// out when I found that DBGrid and other controls that compare bookmarks
// won't function if you don't provide a non-default implementation of this.

function TJvCustomCsvDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
  v1, v2: Integer;
begin
  v1 := -999999;
  v2 := -999999;
  if Bookmark1 <> nil then
    v1 := Integer(Bookmark1^);
  if Bookmark2 <> nil then
    v2 := Integer(Bookmark2^);
  Result := Bookmark_Eql;
  if v1 < v2 then
    Result := Bookmark_Less
  else
  if v1 > v2 then
    Result := Bookmark_Gtr;
end;

procedure TJvCustomCsvDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCsvRow(Buffer)^.Bookmark.flag := Value;
end;

procedure TJvCustomCsvDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  {Bookmark is just pointer to Integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCustomCsvDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FRecordPos := PCsvRow(Buffer)^.Bookmark.data; //Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:

procedure TJvCustomCsvDataSet.SetBookmarkData(Buffer: PChar; data: Pointer);
begin
  PCsvRow(Buffer)^.Bookmark.data := PInteger(data)^;
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
//   <coldef> = columname:<data-type-character><size>
//
// See top of file!

procedure TJvCustomCsvDataSet.InternalInitFieldDefs;
var
  CsvFieldRec: TJvCsvRow; //record type.
  CsvFieldOption: string;
  CsvFieldName: string;
  pCsvFieldDef: PCsvColumn;
  t, ColNum, pos1: Integer;
  // field options:
  FieldTypeChar: Char;
  VclFieldType: TFieldType;
  FieldLen: Integer;
  FieldType: TJvCsvColumnFlag;
  aCsvFieldDef: string;
  CsvKeys: array of string;
//  FDef:TFieldDef;
begin
//  FFieldsInitialized := True;
  FieldType := jcsvString;
  VclFieldType := ftString;

  // create FieldDefs which map to each field in the data record
  FieldDefs.Clear; // Clear VCL Database field definitions
  FCsvColumns.Clear; // Clear our own CSV related field data

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
    while (CsvRowGetColumnMarker(@CsvFieldRec, ColNum) <> COLUMN_ENDMARKER) do
    begin
      FieldLen := 80; // default.
      CsvFieldOption := GetCsvRowItem(@CsvFieldRec, ColNum); // get a string in the format COLUMNAME:Options

       // Look for Colon or Semicolon:
      pos1 := Pos(':', CsvFieldOption);
      if pos1 <= 0 then
        pos1 := Pos(';', CsvFieldOption);

      if pos1 <= 0 then
      begin
        CsvFieldName := CsvFieldOption;
        CsvFieldOption := '$';
        FieldTypeChar := '$';
      end
      else
      begin
          // extract field name:
        CsvFieldName := Copy(CsvFieldOption, 1, pos1 - 1);
          // If character after the colon is a symbol character, grab
          // it, otherwise default to '$'.
        if Ord(CsvFieldOption[pos1 + 1]) < Ord('A') then
        begin
          FieldTypeChar := CsvFieldOption[pos1 + 1];
          CsvFieldOption := Copy(CsvFieldOption, pos1 + 2, 80);
        end
        else
        begin
          FieldTypeChar := '$';
          CsvFieldOption := Copy(CsvFieldOption, pos1 + 1, 80);
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

        //Was: TFieldDef.Create(FieldDefs, ...., ColNum );
      FieldDefs.Add(CsvFieldName, VclFieldType, FieldLen, False);

      // Now create our internal field data structure:
      pCsvFieldDef := AllocMem(SizeOf(TJvCsvColumn) {+ 8 BIGFudge});
      pCsvFieldDef^.FFlag := FieldType; {jcsvString}
      pCsvFieldDef^.FFieldDef := FieldDefs.Find(CsvFieldName);

      // Note: field order is established when we open the file (later)
      pCsvFieldDef^.FPhysical := -1; // not yet located in the physical file!
      FCsvColumns.AddColumn(pCsvFieldDef);
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
  begin
    FCsvKeyCount := 0;
  end
  else
  begin
    SetLength(CsvKeys, FCsvColumns.Count);
    FCsvKeyCount := StrSplit(FCsvKeyDef, Separator, {Chr(0)=No Quoting} Chr(0), CsvKeys, FCsvColumns.Count);
    SetLength(FCsvKeyFields, FCsvKeyCount);
    if (FCsvKeyCount < 1) or (FCsvKeyCount > FCsvColumns.Count) then
      JvCsvDatabaseError(FTableName, RsEInvalidCsvKeyDef);
    for t := 0 to FCsvKeyCount - 1 do
    begin
      if CsvKeys[t] = '' then
        JvCsvDatabaseError(FTableName, RsEInternalErrorParsingCsvKeyDef);
      pCsvFieldDef := FCsvColumns.FindByName(CsvKeys[t]);
      if not Assigned(pCsvFieldDef) then
      begin
        JvCsvDatabaseError(FTableName, Format(RsEContainsField, [CsvKeys[t]]));
      end
      else
      begin
        pCsvFieldDef^.FKeyFlag := True;
        FCsvKeyFields[t] := pCsvFieldDef;
      end;
    end;
  end;
end; { InternalInitFieldDefs ends }

{ set our position onto the EOF Crack }

procedure TJvCustomCsvDataSet.InternalLast;
begin
//  Eof := True;
  FRecordPos := ON_EOF_CRACK; // FData.Count;
end;

// At shutdown or on user-calling this method, check if data has changed,
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
  FOpenFileName:='';
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

function TJvCustomCsvDataSet.InternalFindByKey(row: PCsvRow): Integer;
var
  t: Integer;
begin
  Result := -1;
  for t := 0 to FData.Count - 1 do
  begin
    if InternalCompare(FCsvKeyFields, FCsvKeyCount, {Left} row, {Right} FData.Items[t], True) = 0 then
    begin
      Result := t;
      Break;
    end;
  end;
end;

function TJvCustomCsvDataSet.FindByCsvKey(key: string): Boolean;
var
  logical_row, physical_row: TJvCsvRow;
  t, recno: Integer;
  str: string;
begin
  Result := False;
  StringToCsvRow(key + Separator, Separator, @logical_row, False, False); // initialize row and put items in their logical order.
  CsvRowInit(@physical_row);
  // Move from Logical (TFieldDef order) to their physical (As found in CSV file) ordering:
  for t := 0 to FCsvKeyCount - 1 do
  begin
    str := GetCsvRowItem(@logical_row, t);
    SetCsvRowItem(@physical_row, FCsvKeyFields[t].FPhysical, str);
      //if (t <> FCsvKeyFields[t].FPhysical) then
      //    OutputDebugString('FindByCsvKey debug');
  end;
  recno := InternalFindByKey(@physical_row);
  if recno < 0 then
    Exit;

  FRecordPos := recno;
  Resync([]);
  Result := True;
end;

{procedure TJvCustomCsvDataSet.InternalInsert;
//var
//  pAddRec : pCsvRow;
begin
// pAddRec := AllocMem(SizeOf(TJvCsvRow));
// StringToCsvRow( FEmptyRowStr, pAddRec, False ); // initialize row.
// FData.AddRow(pAddRec);
// FCurrentRecord := -1;
// Resync([]);
  FRecordPos :=  ON_EOF_CRACK;
//FCurrentRecord := FData.Count;
end;}

procedure TJvCustomCsvDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos: Integer;
  pAddRec: PCsvRow;
//  keyIndex: Integer;
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

  pAddRec := AllocMem(SizeOf(TJvCsvRow));
  if Buffer <> nil then
    Move(PCsvRow(Buffer)^, pAddRec^, SizeOf(TJvCsvRow));

  if pAddRec.Text = '' then
    StringToCsvRow(FEmptyRowStr, Separator, pAddRec, False, False); // initialize row.

  pAddRec.fdirty := True;
  pAddRec.Index := -1; // Was not loaded from the file!

 {
 if FCsvUniqueKeys then
 begin
   keyIndex := InternalFindByKey(pAddRec);
   if keyIndex >= 0 then
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
    pAddRec^.Index := FData.Count;
    FData.AddRow(pAddRec);
    InternalLast;
  end
  else
  begin
    if (FRecordPos = ON_EOF_CRACK) or (FRecordPos = ON_BOF_CRACK) then
    begin
      pAddRec^.Index := FData.Count;
      FData.AddRow(pAddRec);
      InternalLast;
    end
    else
    begin
      RecPos := FRecordPos;
      pAddRec^.Index := RecPos;
      FData.Insert(RecPos, Pointer(pAddRec));
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
   begin
     // Design-time local paths that don't move if the current working
     // directory moves.  These paths reference the directory the program
     // starts in.  To use this at design time you have to enter the
     // table name as '.\Subdirectory\FileName.csv' (or './subdir/...' on Kylix)
     Result := IncludeTrailingPathDelimiter(FInitialWorkingDirectory) +   FTableName;  // SPECIAL CASE.
   end
   else
     Result := ExpandUNCFilename(FTableName); // Expand using current working directory to full path name. DEFAULT BEHAVIOR.
end;

function TJvCustomCsvDataSet.InternalLoadFileStrings: Boolean;
begin
  Result := False;
  // BUGFIX: Problem with tables with LoadsFromFile would not load at all. FEB 2004.
  if FLoadsFromFile and not FileExists(FTableName) then
    Exit; // We can return immediately ONLY if there is no file to load,
          // otherwise this routine is parsing already-loaded data, and we should NOT
          // return, or we won't get our data in the table. -WP.

  //if not FLoadsFromFile then
   // Exit;

  if Assigned(FCsvFileAsStrings) then
  begin
    if FCsvFileAsStrings.Count > 0 then
      Result := True; //loaded already
    Exit; // don't repeat!
  end;

  try // open data file
    FCsvFileAsStrings := TStringlist.Create;

    if FLoadsFromFile then // The IF condition here is NEW!
       FCsvFileAsStrings.LoadFromFile(FOpenFilename);

    if FCsvFileAsStrings.Count > 0 then
      Result := True; // it worked!
  except
          //FTableName := '';
    FCsvFileAsStrings.Free;
    FCsvFileAsStrings := nil;
    raise; // pass exception on in.
  end; {end except}
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

procedure TJvCustomCsvDataSet.AppendPlaceHolderCommasToAllRows(Strings: TStrings);
  // Add 1+ commas to FCsvFileAsStrings[1 .. Count-1]
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
//  Strings: TStringlist;
  TempBuf: array[0..MAXCOLUMNS] of Char;
begin
  if FCursorOpen then
    InternalClose; // close first!

  FOpenFileName := GetFileName; // Always use the same file name to save as you did to load!!! MARCH 2004.WP

  FFileDirty := False;
  if (FTableName = '') and FLoadsFromFile then
    JvCsvDatabaseError(RsENoTableName, RsETableNameRequired);
//  Strings := nil;

  InternalInitFieldDefs; // initialize FieldDef objects

  // Create TField components when no persistent fields have been created
  if DefaultFields then
    CreateFields;
  BindFields(True); // bind FieldDefs to actual data

  if FCsvColumns.Count > 1 then
  begin
     // Create a null terminated string which is just a bunch of commas:
    FillChar(TempBuf, FCsvColumns.Count - 1, Separator);
    TempBuf[FCsvColumns.Count - 1] := Chr(0);
      // When adding an empty row, we add this string as the ascii equivalent:
    FEmptyRowStr := string(TempBuf);
  end
  else
  begin
    FEmptyRowStr := ''; // nothing.
  end;

  FBufferSize := SizeOf(TJvCsvRow) + CalcFieldsSize; // our regular record + calculated field data.
//  if CalcFieldsSize>0 then
//      OutputDebugString('Calculated Fields Debug');
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
  pInsertRec: PCsvRow;
  RecPos: Integer;
  keyIndex: Integer; // If unique key enforcement is on, this is the key search Result.
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
    keyIndex := InternalFindByKey(PCsvRow(ActiveBuffer));
    // If posting an update, keyIndex better be <0 or else equal to FRecordPos!
    // Otherwise, if adding, keyIndex better be <0.

    if keyIndex >= 0 then
      if ((State = dsInsert) and {XXX NEW}(FRecordPos< RecordCount) )
        or ((State = dsEdit) and (keyIndex <> FRecordPos)) then
      begin
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString(PChar('JvCsvDataSet Uniqueness: keyIndex='+IntToStr(keyIndex)+' '+GetRowAsString(keyIndex) ));
        OutputDebugString(PChar('JvCsvDataSet Uniqueness: recordPos='+IntToStr(FRecordPos)+' '+GetRowAsString(FRecordPos) ));
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
    FData.GetRowPtr(RecPos)^.fdirty := True;
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
    pInsertRec := AllocMem(SizeOf(TJvCsvRow));
    Move(PCsvRow(ActiveBuffer)^, pInsertRec^, SizeOf(TJvCsvRow));
    pInsertRec^.fdirty := True;
    FData.Insert(FRecordPos, Pointer(pInsertRec));
    FRecordPos := FData.IndexOf(Pointer(pInsertRec));
    pInsertRec^.Bookmark.data := FRecordPos;
  end
  else
    JvCsvDatabaseError(FTableName, RsECannotPost);
end;

function TJvCustomCsvDataSet.IsCursorOpen: Boolean;
begin
  // "Cursor" is open if data file is open.   File is open if FDataFile's
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

  Result := (PCsvRow(BufPtr)^.Bookmark.data); // Record number.
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
//  File1:TextFile;
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
  strLeft, strRight: string;
  numLeft, numRight, diff: Double;
begin

  strLeft := GetCsvRowItem(Left, Column^.FPhysical);
  strRight := GetCsvRowItem(Right, Column^.FPhysical);

  (*if (Length(strLeft)=0) or (length(strRight)=0) then
  begin
    OutputDebugString('Debugging problem in InternalFieldCompare');
    strLeft  :=GetCsvRowItem( Left, Column^.FPhysical );
  end;*)

  if FCsvCaseInsensitiveComparison then
  begin
    strLeft := UpperCase(strLeft);
    strRight := UpperCase(strRight);
  end;

   // everything sorts via string sort (default) or numeric sort
   // (the only special case so far!)
  case Column^.FFlag of
    jcsvNumeric:
      begin
        numLeft := StrToFloatUSDef(strLeft, -99999.9);
        numRight := StrToFloatUSDef(strRight, -99999.9);
        diff := numLeft - numRight;
        if diff < -0.02 then
          Result := -1
        else
        if diff > 0.02 then
          Result := 1
        else
          Result := 0; // For our purposes, .02 difference or less is a match.
        Exit;
      end;
  else
    Result := StrComp(PChar(strLeft), PChar(strRight));
  end;
end;

// InternalCompare of multiple fields.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCustomCsvDataSet.InternalCompare(SortColumns: TArrayOfPCsvColumn; SortColumnCount: Integer; Left,
  Right: PCsvRow;Ascending:Boolean): Integer;
var
  t: Integer;
begin
  Result := 0;
  // null check, raise exception
  if (not Assigned(Left)) or (not Assigned(Right)) then
    JvCsvDatabaseError(FTableName, RsEInternalCompare);
  // now check each field:
  for t := 0 to SortColumnCount - 1 do
  begin
    if not Assigned(SortColumns[t]) then
      JvCsvDatabaseError(FTableName, RsEInternalCompare); // raise exception
    Result := InternalFieldCompare(SortColumns[t], Left, Right);
    if Result <> 0 then
    begin
      if not Ascending then
        Result := -Result;
           // XXX REPEAT Result := InternalFieldCompare( SortColumns[t],Left,Right);
      Exit; // found greater or less than condition
    end;
  end;
  // now we have compared all fields, and if we get here, they were all
  // equal, and Result is already set to 0.
end;

procedure TJvCustomCsvDataSet.InternalQuickSort(SortList: PPointerList;
  L, R: Integer; SortColumns:TArrayOfPCsvColumn; ACount:integer;Ascending: Boolean);
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

procedure TJvCustomCsvDataSet.QuickSort(AList:TList; SortColumns:TArrayOfPCsvColumn; ACount:integer; Ascending:Boolean);
begin
  if (AList <> nil) and (AList.Count > 1) then
    InternalQuickSort(AList.List, 0, AList.Count - 1, SortColumns, ACount, Ascending);
end;

procedure TJvCustomCsvDataSet.Sort(SortFields: string; Ascending: Boolean);
var
//  Index: array of Pointer;
//  swap: Pointer;
  SortFieldNames: array of string;
  SortColumns: TArrayOfPCsvColumn;
  SortColumnCount: Integer;
//  comparison, t, u, l: Integer;
  t:integer;
begin
  // Create an indexed list which can be sorted more easily than
  // doing an item swap:
//  l := FData.Count;
//  SetLength(Index, l);
//  for t := 0 to l - 1 do
//  begin
//    Index[t] := FData.Items[t]; // Initial values.
//  end;

  SetLength(SortFieldNames, FCsvColumns.Count);
  SortColumnCount := StrSplit(SortFields, Separator, {Chr(0)=No Quoting} Chr(0), SortFieldNames, FCsvColumns.Count);
  SetLength(SortColumns, SortColumnCount);
  if (SortFields = '') or (SortColumnCount = 0) then
    JvCsvDatabaseError(FTableName, RsESortFailedCommaSeparated);

  // Now check if the fields exist, and find the pointers to the fields
  for t := 0 to SortColumnCount - 1 do
  begin
    if SortFieldNames[t] = '' then
      JvCsvDatabaseError(FTableName, RsESortFailedFieldNames);
    SortColumns[t] := FCsvColumns.FindByName(SortFieldNames[t]);
    if not Assigned(SortColumns[t]) then
      JvCsvDatabaseError(FTableName, Format(RsESortFailedInvalidFieldNameInList, [SortFieldNames[t]]));
  end;
  QuickSort(FData, SortColumns, SortColumnCount, AScending);

  //  bubble sort, compare in the middle,
  //  yes I'm feeling lazy today, yes I know a qsort would be better. - WP
(*
  for t := 0 to l - 2 do
  begin
    for u := t + 1 to l - 1 do
    begin
        // Record comparison between two PCsvRows:
      comparison := InternalCompare(SortColumns,
        SortColumnCount,
        PCsvRow(Index[t]),
        PCsvRow(Index[u])
        );
      if not Ascending then
        comparison := comparison * -1; // flip sign of comparison
      if comparison > 0 then
      begin { bubble sort by swaps }
        swap := Index[t];
        Index[t] := Index[u];
        Index[u] := swap;
      end;
    end;
  end;
  // Now build a new Data elements list:
  for t := 0 to l - 1 do
  begin
    FData.Items[t] := Index[t]; // Rewrite pointers to new order!
  end;
  *)
  FFileDirty := True;
  First; // reposition!
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

procedure TJvCustomCsvDataSet.DefChanged(Sender: TObject); //override;
begin
  FStoreDefs := True;
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

function TJvCustomCsvDataSet.FieldDefsStored: Boolean;
begin
  Result := FStoreDefs and (FieldDefs.Count > 0);
end;

function TJvCustomCsvDataSet.GetCanModify: Boolean; //override;
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
  t: Integer;
begin
  try
    for t := 0 to Count - 1 do
    begin
      Result := PCsvColumn(Get(t));
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
  t: Integer;
begin
  Result := nil;
  try
    for t := 0 to Count - 1 do
    begin
      Result := PCsvColumn(Get(t));
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
  t: Integer;
begin
  for t := 0 to Count - 1 do
    FreeMem(Self[t]);
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

procedure TJvCsvRows.AddRow( Item: PCsvRow);
begin
  Add(Pointer(Item));
end;

procedure TJvCsvRows.InsertRow(const position: Integer;  Item: PCsvRow);
begin
  Insert(position, Pointer(Item));
end;

procedure TJvCsvRows.AddRowStr(const Item: string;Separator:Char); // convert String->TJvCsvRow
var
  pNewItem: PCsvRow;
begin
  pNewItem := AllocMem(SizeOf(TJvCsvRow));
  StringToCsvRow(Item, Separator, pNewItem, True, FEnquoteBackslash); // decode a csv line that can contain escape sequences
  AddRow(pNewItem);
end;

function TJvCsvRows.GetRowPtr(const RowIndex: Integer): PCsvRow;
begin
 if (RowIndex>=0) and (RowIndex < Count)then
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

procedure TJvCsvRows.SetRowStr(const RowIndex: Integer; Value: string;Separator:Char);
begin
  StringToCsvRow(Value, Separator, GetRowPtr(RowIndex), True, FEnquoteBackslash);
end;

procedure TJvCsvRows.DeleteRow(const RowIndex: Integer);
var
  p: Pointer;
begin
  if (RowIndex >= 0) and (RowIndex < Count) then
  begin
    p := Self[RowIndex];
    if p <> nil then
      FreeMem(p);
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
  t: Integer;
begin
  for t := 0 to Count - 1 do
    FreeMem(Self[t]);
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
//x:Integer;
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
  startIndex, indexCounter: Integer;
begin
  // CheckInactive; // NO! DON'T DO THIS!
  // if NOT FFieldsInitialized then
  // InternalInitFieldDefs; // must know about field definitions first.
  if Strings = nil then
    Exit;
  FData.EnquoteBackslash := FEnquoteBackslash;

  indexCounter := 0;
  // Skip first row:
  if FHasHeaderRow then
    startIndex := 1
  else
    startIndex := 0;

  for I := startIndex to Strings.Count - 1 do
  begin
    // for now ignore any trace or debug data unless
    // someone has defined an event to handle it.
    if Pos('>>', Strings[I]) = 1 then
    begin
      if Assigned(FOnSpecialData) then
        FOnSpecialData(Self, I, Strings[I]);
      Continue;
    end;
    // Process the row:
    ProcessCsvDataRow(Strings[I], indexCounter);
    Inc(indexCounter);
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
    // copy out the current data set to a TStringList.
    Strings.Clear;

    { Save header row with data rows? }
    if FHasHeaderRow then
    begin
      if ExtendedHeaderInfo then
        Strings.Add(CsvFieldDef)
      else
        Strings.Add(GetColumnsAsString);
    end;

    for I := 0 to FData.Count - 1 do
    begin
      CsvRowToString(FData.GetRowPtr(I), Line);
      Strings.Add(Line);
    end;
  finally
    Strings.EndUpdate;
  end;
end;

procedure TJvCustomCsvDataSet.AppendRowString(RowAsString: string);
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
  Result := GetRowAsString( RecNo );
end;

function TJvCustomCsvDataSet.GetRowAsString(const Index: Integer): string;
var
  GetIndex: Integer;
begin
  if Index < 0 then {lastrow}
    GetIndex := FData.Count - 1
  else
    GetIndex := Index; { actual index specified }

      { return string}
  CsvRowToString(FData.GetRowPtr(GetIndex), Result);
  Result := Result;
end;

// Get names of all the columns as a comma-separated string:

function TJvCustomCsvDataSet.GetColumnsAsString: string;
var
  t: Integer;
begin
 // ColCount:
  if FCsvColumns.Count = 0 then
  begin
    Result := '';
    Exit;
  end;
 // Build a list of column names: <item>, <item>,....,<item>
  Result := FieldDefs[0].Name;
  for t := 1 to FCsvColumns.Count - 1 do
    Result := Result + Separator + FieldDefs[t].Name;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }

procedure TJvCustomCsvDataSet.ProcessCsvHeaderRow;
var
  CsvFieldRec: TJvCsvRow; // CSV Field record type.
  ptrCsvColumn: PCsvColumn;
  CsvFieldName: string;
  ColNum, t: Integer;
begin
  if not ValidateHeaderRow then
  begin
    for t := 0 to FCsvColumns.Count - 1 do
      PCsvColumn(FCsvColumns.Get(t))^.FPhysical := t;
    Exit;
  end;
  FAppendedFieldCount := 0;
   //  Columns Not Yet Found:
  for t := 0 to FCsvColumns.Count - 1 do
    PCsvColumn(FCsvColumns.Get(t))^.FPhysical := -1;

   // Do initial parse.
  StringToCsvRow(FHeaderRow, Separator, @CsvFieldRec, False, False);
  ColNum := 0;
  while (CsvRowGetColumnMarker(@CsvFieldRec, ColNum) <> COLUMN_ENDMARKER) do
  begin
         // Get a string in the format COLUMNAME:Options
    CsvFieldName := StrEatWhiteSpace(GetCsvRowItem(@CsvFieldRec, ColNum));

    if CsvFieldName = '' then
      JvCsvDatabaseError(FTableName, RsEErrorProcessingFirstLine);

    ptrCsvColumn := FCsvColumns.FindByName(CsvFieldName);

    if ptrCsvColumn = nil then
    begin // raise database exception:
      JvCsvDatabaseError(FTableName, Format(RsEFieldInFileButNotInDefinition, [CsvFieldName]));
      Exit;
    end;

    try
      ptrCsvColumn^.FPhysical := ColNum; // numbered from 0.
    except
      JvCsvDatabaseError(FTableName, Format(RsECsvFieldLocationError, [CsvFieldName]));
      Break;
    end;
    Inc(ColNum);
  end; // loop for each column in the physical file's header row.

  // Check that everything was found and physically given a location
  // in the CSV file:
  for t := 0 to FCsvColumns.Count - 1 do
  begin
    ptrCsvColumn := PCsvColumn(FCsvColumns[t]);
    if ptrCsvColumn^.FPhysical < 0 then
    begin
      if not FHasHeaderRow then
      begin
          // If there is no header row we can't cope with fields that aren't in the file
          // because FCsvHeader is not written into the file, so we can't just go expanding
          // what goes into that file.
        JvCsvDatabaseError(FTableName, Format(RsEFieldNotFound, [ptrCsvColumn^.FFieldDef.Name]));
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
        ptrCsvColumn^.FPhysical := ColNum;
        Inc(ColNum);
        CsvFieldName := ptrCsvColumn^.FFieldDef.Name;
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
  pNewRow: PCsvRow;
begin
  if DataRow = '' then
    Exit;
  if Length(DataRow) >= (MAXLINELENGTH - 1) then
    raise EJvCsvDataSetError.CreateResFmt(@RsECsvStringTooLong, [Copy(DataRow, 1, 40)]);
  pNewRow := AllocMem(SizeOf(TJvCsvRow));
  StringToCsvRow(DataRow, Separator, pNewRow, True, FEnquoteBackslash);
  pNewRow^.Index := Index;
  FData.AddRow(pNewRow);
end;

{ This function is handy to save a portion of a csv table that has
grown too large into a file, and then DeleteRows can be called to remove
that section of the file. }

procedure TJvCustomCsvDataSet.ExportRows(FileName: string; FromRow, ToRow: Integer);
var
  t: Integer;
  StrList: TStringlist;
begin
  StrList := TStringlist.Create;
  StrList.Add(Self.FHeaderRow);
  try
    for t := FromRow to ToRow do
    begin
      StrList.Add(FData.GetRowStr(t))
    end;
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
  while (Count > 0) do
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
  Strings: TStringlist;
begin
  Strings := TStringlist.Create;
  AssignToStrings(Strings);
  Strings.SaveToFile(FileName);
  Strings.Free;
end;

function TJvCustomCsvDataSet.GetCsvHeader: string;
var
  f: Text;
  FirstLine: string;
begin
  if (not FLoadsFromFile) or (not FHasHeaderRow) or not (FileExists(FTableName)) then
  begin
    Result := '';
    Exit;
  end;
  { How's this for an ancient Pascal code sequence, AssignFile+Reset is approximately equal to a C fopen() call }
  AssignFile(f, FTableName);
  Reset(f);
  { ReadLn is approximately a gets() call }
  ReadLn(f, FirstLine);
  { And finally, the pascal file close procedure }
  CloseFile(f);
  // return the first line of the file, without the junk
  Result := StrStrip(FirstLine); // in JvCsvParse.pas
end;

{ PROCEDURES: }

// convert CSV Row buffer to a String

procedure CsvRowToString(RowItem: PCsvRow; var RowString: string);
begin
  RowString := string(RowItem.Text);
end;

// convert String into a CSV Row buffer

procedure StringToCsvRow(const RowString: string; Separator:Char; RowItem: PCsvRow; permitEscapeSequences, EnquoteBackslash: Boolean);
var
  t, l, Col: Integer;
  quoteFlag: Boolean;
  skipFlag: Boolean;
  charsInColumn: Integer;
begin
  Col := 0;
  RowItem^.wordfield[0] := 0; // zero out column marker and dirty bit!
  charsInColumn := 0;
  quoteFlag := False;
  skipFlag := False;
  l := Length(RowString);
  for t := 1 to l do
  begin
    Inc(charsInColumn);
    if quoteFlag then
    begin
          // backslash permitted only if specifically enabled by FEnquoteBackslash:
      if permitEscapeSequences and (not skipFlag) and (EnquoteBackslash) and (RowString[t] = '\') then
      begin
        skipFlag := True;
        continue;
      end;
          // doubled quotes handling:
      if permitEscapeSequences and (not skipFlag) and (RowString[t] = '"') and (t < l) and (RowString[t + 1] = '"')
        then
      begin
        skipFlag := True;
        continue;
      end;
          // now if either of the above set the skipFlag True previously, we ALWAYS skip the next character here
          // and turn skipFlag back off
      if permitEscapeSequences and skipFlag then
      begin // skip next character, regardless.
        skipFlag := False;
        continue; // by skipping escaped quotes, we don't turn off quoteFlag in the middle of a string!
      end;
    end;
    //Now we know if we get this far, we are NOT dealing with any escaped characters
    //Any quotes we see here will turn on/off quote mode directly!
    if RowString[t] = '"' then
    begin
      if permitEscapeSequences then
      begin
        quoteFlag := not quoteFlag;
        if quoteFlag and (charsInColumn > 1) then
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

    if (RowString[t] = Separator) and (not quoteFlag) then
    begin
      Inc(Col);
       // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
      RowItem.wordfield[Col] := (Word(t) and $7FFF); {note that we're going from 1..length }
      charsInColumn := 0;
    end;
    if (Col >= MAXCOLUMNS) or (t >= MAXLINELENGTH) then
    begin
      raise ERangeError.CreateResFmt(@RsEInternalLimit, [MAXCOLUMNS]);
      Exit;
    end;
  end; // end of string, new flag:
  Inc(Col);
  if quoteFlag then
  begin
    // (rom) no OutputDebugString in production code
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString('CsvDataSource.pas: StringToCsvRow - Missing end quote character!');
    {$ENDIF DEBUGINFO_ON}
  end;
 // Terminate the column-marker list with a special end-marker:
{RowItem.wordfield[col]   := Word(Length(RowString)+1)AND$7FFF; // length of string
 RowItem.wordfield[col+1] := COLUMN_ENDMARKER; // followed by an end marker}
  RowItem.wordfield[Col] := COLUMN_ENDMARKER; // last one has no end marker
  StrLCopy(RowItem.Text, PChar(RowString), MAXLINELENGTH);
  RowItem.columns := Col; // Check this later!
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

procedure SetCsvRowItem(pItem: PCsvRow; ColumnIndex: Integer; newValue: string);
var
  TempBuf: array[0..MAXLINELENGTH] of Char;
  Copy1, Copy2: Integer;
  Dif, t, Old: Integer;
begin
  Dif := 0;
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
    Exit;

  Copy1 := CsvRowGetColumnMarker(pItem, ColumnIndex);
  if Copy1 = COLUMN_ENDMARKER then
    Exit;
    // Update new rows:  FIX previous fix!
  if (ColumnIndex >= pItem^.columns) then
      pItem^.columns := ColumnIndex+1;

  if Copy1 > MAXLINELENGTH then
    Exit;
 // copy initial part of the csv row:
  if Copy1 > 0 then
  begin
    StrLCopy(TempBuf, pItem.Text, Copy1);
    StrLCat(TempBuf, PChar(newValue), MAXLINELENGTH);
  end
  else
    StrLCopy(TempBuf, PChar(newValue), MAXLINELENGTH);

  Copy2 := CsvRowGetColumnMarker(pItem, ColumnIndex + 1);
  if Copy2 <> COLUMN_ENDMARKER then
  begin
   // difference in length:
    Dec(Copy2); // subtract one.
    if Copy2 < 0 then
      Exit;
    if Length(newValue) = Copy2 - Copy1 then
      Dif := 0
    else
      Dif := Length(newValue) - (Copy2 - Copy1);
    StrLCat(TempBuf, pItem^.Text + Copy2, MAXLINELENGTH);
  end;

 // Copy over the old memory buffer:
  StrLCopy(pItem^.Text, TempBuf, MAXLINELENGTH);

 // Now that we've copied a new item of a different length into the place of the old one
 // we have to update the positions of the columns after ColumnIndex:
  if Dif <> 0 then
    for t := ColumnIndex + 1 to MAXCOLUMNS do
    begin
      Old := CsvRowGetColumnMarker(pItem, t);
      if Old = COLUMN_ENDMARKER then
        Exit;
      CsvRowSetColumnMarker(pItem, t, Old + Dif);
    end;
end;

// Copy an item out of a csv row buffer:

function GetCsvRowItem(pItem: PCsvRow; ColumnIndex: Integer): string;
var
  TempBuf: array[0..MAXLINELENGTH] of Char;
  Copy1, Copy2: Integer;
begin
  Result := '';
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
  begin
    Result := RsErrorRowItem;
  end
  else
  if ColumnIndex < pItem^.columns then
  begin
    Copy1 := CsvRowGetColumnMarker(pItem, ColumnIndex);
    Copy2 := CsvRowGetColumnMarker(pItem, ColumnIndex + 1);

    if Copy1 <> COLUMN_ENDMARKER then
    begin
      if Copy2 = COLUMN_ENDMARKER then // copy the rest of the line
        Copy2 := MAXLINELENGTH - Copy1 // All the characters left in the buffer
      else
        Dec(Copy2);

      if (Copy1 <= MAXLINELENGTH) and (Copy2 <= MAXLINELENGTH) then
      begin
       // Copy out just one column from the string:
        StrLCopy(TempBuf, pItem.Text + Copy1, Copy2 - Copy1);
        PcharEatWhiteChars(@TempBuf[0]);
        Result := string(TempBuf);
      end;
    end;
  end;
end;

{new}

procedure CsvRowSetDirtyBit(row: PCsvRow; ColumnIndex: Integer);
begin
  if row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  row^.fdirty := True; // triggers search for 'dirty bit' in columns
  row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] or $8000);
end;

procedure CsvRowClearDirtyBit(row: PCsvRow; ColumnIndex: Integer);
begin
  if row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] and $7FFF);
end;

function CsvRowGetDirtyBit(row: PCsvRow; ColumnIndex: Integer): Boolean;
begin
  Result := False;
  if row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  if row^.wordfield[ColumnIndex] = COLUMN_ENDMARKER then
    Exit;
  Result := (row^.wordfield[ColumnIndex] and $8000) <> 0;
end;

procedure CsvRowSetColumnMarker(row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
var
  Old: Word;
begin
  if row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  if ColumnMarker < 0 then
    Exit;

  if ColumnMarker = COLUMN_ENDMARKER then
    row^.wordfield[ColumnIndex] := COLUMN_ENDMARKER
  else
  begin
    Old := row^.wordfield[ColumnIndex];
    if Old = COLUMN_ENDMARKER then
      row^.wordfield[ColumnIndex] := ColumnMarker and $7FFF // auto-clear Dirty bit
    else
      row^.wordfield[ColumnIndex] := (Old and $8000) or // Keep Old Dirty Bit
        (Word(ColumnMarker) and $7FFF); // new value.
  end;
end;

function CsvRowGetColumnMarker(row: PCsvRow; ColumnIndex: Integer): Integer;
var
  w: Word;
begin
  Result := -1;
  if row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then
    Exit;
  w := row^.wordfield[ColumnIndex];
  if w = COLUMN_ENDMARKER then
    Result := COLUMN_ENDMARKER
  else
    Result := Integer(w and $7FFF);
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

function TimeTHexToDateTime(HexStr: string; TimeZoneCorrection: Integer): TDateTime;
var
  SecondsSince1970: Double;
  Base: TDateTime;
{ DateTimeAsStr:String; //debug Code.}
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

function TimeTAsciiToDateTime(AsciiDateStr: string): TDateTime;
const
  Separators = '// ::'; // separators in yyyy/mm/dd hh:mm:ss
  Separators2 = '-- --'; // separators in yyyy/mm/dd hh:mm:ss
var
  Values: array[1..6] of Integer; //year,month,day,hour,minute,second in that order.
  ch: Char;
  t, u, len, Index: Integer;
begin
  Result := 0.0; // default Result.
  len := Length(AsciiDateStr);

 // validate ranges:

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiDateStr are we looking at?
  for t := 1 to 6 do
  begin
    Values[t] := 0;
    if (t >= 3) and (Index >= len) then
      Break; // as long as we at least got the date, we can continue.
    for u := 1 to AsciiTime_ExpectLengths[t] do
    begin
      if Index > len then
      begin
        Break;
      end;
      ch := AsciiDateStr[Index];
      if not (ch in DigitSymbols) then
      begin
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString(PChar('JvCsvData:illegal character in datetime string: ' + ch));
        {$ENDIF DEBUGINFO_ON}
        Exit; // failed:invalid character.
      end;
      Values[t] := (Values[t] * 10) + (Ord(ch) - Ord('0'));
      Inc(Index);

      if Index > len then
        Break;
    end;

   // if we haven't reached the end of the string, then
   // check for a valid separator character:
    if Index < len then
      if (AsciiDateStr[Index] <> Separators[t]) and
        (AsciiDateStr[Index] <> Separators2[t]) then
      begin
        // (rom) no OutputDebugString in production code
        {$IFDEF DEBUGINFO_ON}
        OutputDebugString('TimeTAsciiToDateTime:illegal separator Char');
        {$ENDIF DEBUGINFO_ON}
        Exit;
      end;

   // validate ranges:
    if (Values[t] < AsciiTime_MinValue[t]) or (Values[t] > AsciiTime_MaxValue[t]) then
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
    Result := EncodeDate({year}Values[1], {month} Values[2], {day} Values[3])
      + EncodeTime({hour}Values[4], {minute} Values[5], {second} Values[6], {msec} 0);
  except
    on E: EConvertError do
    begin
      Result := 0.0; // catch any other conversion errors and just return 0.
    end;
  end;
end;

function DateTimeToTimeTHex(aDateTime: TDateTime; TimeZoneCorrection: Integer): string;
var
  Base: TDateTime;
{  DateTimeAsStr: String; //debug Code. }
  SecondsSince1970: Integer;
begin
  try
    Base := EncodeDate(1970, 1, 1);
    SecondsSince1970 := Trunc((aDateTime - Base) * 86400.0);
    Result := IntToHex(SecondsSince1970 - TimeZoneCorrection, 8);
  except
     // Catch Failures!
    Result := '';
  end;
end;

function DateTimeToTimeToIsoAscii(aDateTime: TDateTime): string;
begin
  // ISO DATETIME FORMAT:
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', aDateTime);
end;

function JvFilePathSplit(FileName: string; var path, filenameonly: string): Boolean;
var
  len, t: Integer;
begin
  len := Length(FileName);
  Result := False;
  path := '';
  filenameonly := '';
  for t := len downto 1 do
  begin
    if FileName[t] = '\' then
    begin
      path := Copy(FileName, 1, t);
      filenameonly := Copy(FileName, t + 1, len);
      if (Length(filenameonly) > 0) and (Length(path) > 0) and DirectoryExists(path) then
        Result := True;
      Exit;
    end;
  end;
end;

{ Routine to keep backup copies of old data files around }

function JvCsvBackupPreviousFiles(const FileName: string; MaxFiles: Integer): Boolean;
var
  BackupFolder, filenameonly, BackupFilename, RemoveFile: string;
  t: Integer;
  found: Boolean;

  function MakeFilename(Index: Integer): string;
  begin
    Result := BackupFolder + filenameonly + '.' + IntToStr(Index) + '.bak';
  end;

begin
  Result := False;

  if not FileExists(FileName) then
    Exit; // failed.
  if not JvFilePathSplit(FileName, BackupFolder, filenameonly) then
  begin
    filenameonly := FileName;
    GetDir(0, BackupFolder);
    BackupFolder := BackupFolder + '\';
  end;
  BackupFolder := BackupFolder + 'Backup\';
  if not DirectoryExists(BackupFolder) then
    CreateDirectory(PChar(BackupFolder), nil);

  found := False;
  for t := 0 to MaxFiles - 1 do
  begin
    BackupFilename := MakeFilename(t);
    if not FileExists(BackupFilename) then
    begin
      RemoveFile := MakeFilename((t + 1) mod MaxFiles);
      found := True;
      Break;
    end;
  end;

  if not found then
  begin
    t := 1;
    BackupFilename := MakeFilename(t);
    RemoveFile := MakeFilename((t + 1) mod MaxFiles);
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
procedure TJvCustomCsvDataSet.CloneRow(dataset: TJvCustomCsvDataSet);
begin
  if not FCursorOpen then
    Exit;
  {basic sanity checks}

  { make sure the range is valid and that the csv schema is the same }
  if (RecNo < 0) or (RecNo >= FData.Count) then
      raise EJvCsvDataSetError.CreateResFmt(@RsEProblemReadingRow,[RecNo]);
  if FCsvFieldDef <> dataset.FCsvFieldDef then
      raise EJvCsvDataSetError.CreateResFmt(@RsEProblemReadingRow,[RecNo]);

  {the ugly hack:}
  CopyMemory(FData[RecNo], dataset.FData[dataset.RecNo], SizeOf(TJvCsvRow));

  PCsvRow(FData[ RecNo ])^.fdirty := True;
  FFileDirty := True;

//  dataset.Last; // Force update of screen.

  Resync([]); // Update data aware controls.
end;

end.

