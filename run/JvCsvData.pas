unit JvCSVData;

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

Last Modified: 2003-07-29 by Warren Postma - New features (Sorting, Indexing, UserData)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCSVDataSet in-memory-dataset component usable by any VCL Data Aware Controls.
              TJvCSVDataSet appears in the 'Jv Data Access' tab of the Component Pallette.

Known Issues:
  May 26, 2003 - Fixed errors handling null date values.
               - Fixed improper memory access for ftBoolean.
                 Booleans are stored internaly as a 16bit WordBool, inside
                 DataSets and the component was reading/writing a 32 bit value,
                 which could caused all kinds of squirrelly things to happen
                 when the boolean (ftBoolean, CSV type '!') was encountered.
                 Search for the WordBool to see the changes.
-----------------------------------------------------------------------------}

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
// TJvCSVCustomInMemoryDataSet
//
// Internally, we first define a TJvCSVCustomInMemoryDataSet a base class.
// Nothing published.  This exists so you can easily inherit from it
// and define your own version of the component, and publish whatever
// properties and methods you wish to publish, and you can hide or
// override any other elements you don't wish to publish.
//
// How To Use:
// You *must* first set up the important Property
// called CSVFieldDef which describes the expected fields and their types
// since the CSV file itself contains insufficient information to guess the
// field types.
//
// TODO: Make it default to all string types if no field types
// are provided.
//
// Example CSVFieldDef string:
//   ABC:$80,DEFG:$140,HIJKLMN:%,OPQRST:@
//
//   $ = string (ftString) - also used if no character is given.
//   % = whole integer value (ftInteger)
//   & = floating point value (ftFloat)
//   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
//   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
//   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
//   ! = Boolean Field (0 in CSV file=false, not 0 = true, blank = NULL)
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

//  { $ R  JVCSVDATA.DCR }

interface

uses
  Windows,
  Messages,
  DB,
  SysUtils,
  Classes,
  Graphics;

const
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

  PInteger = ^integer;
  PDouble = ^Double;
  PBoolean = ^boolean;
  {$IFNDEF COMPILER6_UP}
  PWordBool = ^WordBool;
  {$ENDIF}
  EJvCSVDataSetError = class(EDatabaseError);
    // Subclass DB.EDatabaseError so we can work nicely with existing Delphi apps.

  EJvCSVKeyError = class(EDatabaseError); // Key Uniqueness or Key Problem

 {  Special Event Types }
  TJvCSVOnSpecialData = procedure(Sender: TObject; Index: integer; NonCSVData: string) of object;

  TJvCSVOnGetFieldData = procedure(Sender: TObject; UserTag: integer; UserData: Pointer; FieldName: string; var Value:
    string) of object;
  TJvCSVOnSetFieldData = procedure(Sender: TObject; UserTag: integer; UserData: Pointer; FieldName: string; Value:
    string) of object;

 { SPECIAL TYPES OF  DATABASE COLUMNS FOR THIS COMPONENT }
 { Columns are numeric, text, or one of two kinds of Specially Encoded date/time formats: }
  TJvCSVColumnFlag = (jCSVNull, jCSVString, jCSVNumeric, jCSVAsciiDateTime, jCSVGMTDateTime, jCSVTZDateTime);

 { pointer to special CSV COLUMN }
  PCSVColumn = ^TJvCSVColumn;
// PFieldDef = ^TFieldDef;

  TJvCSVColumn = record
    FFlag: TJvCSVColumnFlag; // Column CSV Format Flags
    FKeyFlag: boolean; // This column is part of the primary key! (new May 2003-WP)
    FPhysical: integer; // Physical Column Ordering
    FFieldDef: TFieldDef; // Associated FieldDef
  end;

  { CSV COLUMNS are stored in a TList-Collection }
  TJvCSVColumns = class(TList)
  public
    procedure AddColumn(Item: PCSVColumn);
    function FindByFieldNo(FieldNo: integer): PCSVColumn;
    procedure Clear; override;
    function FindByName(FieldName: string): PCSVColumn;
  end;

  TJvCSVBookmark = record
    flag: TBookmarkFlag;
    data: integer;
  end;

    { CSV Data File Row is not very dynamic in this version: }
  PtrToPtrToCSVRow = ^PCSVRow; // bookmark data = double pointer indirection! Fun fun fun!
  PCSVRow = ^TJvCSVRow; // a pointer to a record
  TJvCSVRow = record { this MUST be a record, not a class, and must be a flag data record type }
    fdirty: boolean; // record is dirty (needs to be written to disk)
    columns: integer;
    Index: integer; // FData Index (-1 means not in FData)
    wordfield: array[0..MAXCOLUMNS + 1] of Word;
      // lookup field beginning, Column Data (column dirty bit+column length) }
    Text: array[0..MAXLINELENGTH] of char; // lookup actual character data.
      // bookmark
    Bookmark: TJvCSVBookmark;

      // filter flag;
    filtered: boolean; // row is hidden from view right now.
    recursionFlag: boolean; // helps us fix endless recursion bug in GetFieldData callbacks.

  end;

  { Row collection }
  TJvCSVRows = class(TList)
  protected
    FEnquoteBackslash: boolean;
      // Optional user data (only allocated if used, how efficient is that, eh.)
    FUserData: array of Pointer;
    FUserTag: array of integer;
    FUserLength: integer;

    function GetUserTag(Index: integer): integer;
    procedure SetUserTag(Index, Value: integer);

    function GetUserData(Index: integer): Pointer;
    procedure SetUserData(Index: integer; Value: Pointer);

  public
    procedure AddRow(const Item: PCSVRow);
    procedure InsertRow(const position: integer; const Item: PCSVRow);

    procedure AddRowStr(const Item: string); // convert String->TJvCSVRow
    function GetRowPtr(const RowIndex: integer): PCSVRow;
    function GetRowStr(const RowIndex: integer): string;
    procedure SetRowStr(const RowIndex: integer; Value: string);
    procedure DeleteRow(const RowIndex: integer);
    procedure SetARowItem(const RowIndex, ColumnIndex: integer; Value: string);
    function GetARowItem(const RowIndex, ColumnIndex: integer): string;
    procedure Clear; override;
    property EnquoteBackslash: boolean read FEnquoteBackslash write FEnquoteBackslash;
    property UserTag[Index: integer]: integer read GetUserTag write SetUserTag;
    property UserData[Index: integer]: Pointer read GetUserData write SetUserData;
  end;

  // Easily Customizeable Dataset descendant our CSV handler and
  // any other variants we create:
  TJvCSVCustomInMemoryDataSet = class(TDataSet)

  protected
    FStoreDefs: boolean;
    FEnquoteBackslash: boolean; // causes _Enquote to use Backslashes. NOT the default behaviour.
    FTimeZoneCorrection: integer; // defaults to 0 (none)
    FFileDirty: boolean; // file needs to be written back to disk?

    FCSVFieldDef: string; // Our own "CSV Field Definition String"
    FCSVKeyDef: string; // CSV Key Definition String. Required if FCSVUniqueKeys is true
    FCSVKeyCount: integer; // Set by parsing FCSVKeyDef
    FCSVKeyFields: array of PCSVColumn;

    FCSVUniqueKeys: boolean;
      // CSV Key Uniqueness option.  Also requires that all fields that are part of the Unique Key be Non Null.
    FCSVCaseInsensitiveComparison: boolean;
      // CSV Key Uniqueness and Key Comparisons - case insensitive mode if True, else case sensitive.

    FIsFiltered: boolean; // Filter conditions have been set.

    FEmptyRowStr: string; // A string of just commas (used to add a new empty row)
    FHeaderRow: string; // first row of CSV file.
    FTableName: string; // CSV File Name
    FRecordPos: integer;
    FRecordSize: integer;
    FBufferSize: integer;
    FCursorOpen: boolean;
    FFilterBuffer: PChar; // used when we implement filtering (later)
    FReadOnly: boolean;
    FLoadsFromFile: boolean;
    FHasHeaderRow: boolean;
    FSavesChanges: boolean;
    FAutoBackupCount: integer; // Keep Last N Copies the Old CSV File, updated before each save?
    FInsertBlocked: boolean; // internal way to block new records but allows editing of existing ones!
    FPostBlocked: boolean; // internal way to block posting of changes, but allows inserting of new ones!

    { data record holder }
    FCSVColumns: TJvCSVColumns; // Column information
    FData: TJvCSVRows; // Rows are a Collection of data pointers.

    { temporary holding space only, for a tstringlist of the file contents }
    FCSVFileAsStrings: TStringlist;

    {  event pointers }
    FOnSpecialData: TJvCSVOnSpecialData;
    FOnGetFieldData: TJvCSVOnGetFieldData;
      // Helps to allow you to update the contents of your CSV data from some other object in memory.
    FOnSetFieldData: TJvCSVOnSetFieldData;
      // Helps to keep some other thing in sync with the contents of a changing CSV file.

    //  Internal Use Only Protected Methods
//    function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer: PChar; virtual;
    procedure CSVRowInit(RowPtr: PCSVRow);

    // New filtering on cursor (GetRecord advances the cursor past
    // any hidden rows using InternalSkipForward).
    function InternalSkipFiltered(defaultResult: TGetResult; ForwardBackwardMode: boolean): TGetResult;

    procedure InternalClearFileStrings;
    function InternalLoadFileStrings: boolean;
    // Internal methods used by sorting:
    function InternalFieldCompare(Column: PCSVColumn; Left, Right: PCSVRow): integer;
    function InternalCompare(SortColumns: array of PCSVColumn; SortColumnCount: integer; Left, Right: PCSVRow): integer;

    // key uniqueness needs this:
    function InternalFindByKey(row: PCSVRow): integer;

    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data:Pointer' stored in treeviews, etc)
    function GetRowUserData: Pointer;
    procedure SetRowUserData(UserData: Pointer);

    function GetRowTag: integer;
    procedure SetRowTag(tagValue: integer);

    // protected TDataSet base METHODS:
    procedure SetTableName(const Value: string); virtual;
    function FieldDefsStored: boolean; virtual;
    function GetCanModify: boolean; override; //already virtual!

    // internal calls:
    procedure ProcessCSVHeaderRow(const header: string);
    procedure ProcessCSVDataRow(const datarow: string; Index: integer);
    procedure SetCSVFieldDef(CSVFieldDefs: string);

    { Mandatory VCL TDataSet Overrides - Pure Virtual Methods of Base Class }
    function AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: boolean): TGetResult; override;
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
    procedure InternalAddRecord(Buffer: Pointer; Append: boolean); override;
    procedure InternalDelete; override;
    procedure InternalPost; override;
{    procedure InternalInsert; override; }{not needed.}

    // Misc methods:
    procedure InternalClose; override;
//    procedure DestroyFields; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
    function IsCursorOpen: boolean; override;
    { Optional overrides }
    function GetRecordCount: integer; override;
    function GetRecNo: integer; override;
    procedure SetRecNo(Value: integer); override;

    { dataset designer calls these }
    procedure DefChanged(Sender: TObject); override;

    // handling functions for enquoting,dequoting string fields in CSV files.
    // handles using the default Excel method which is to double the quotes inside
    // quotes.

    function _Enquote(strVal: string): string; virtual;
      // puts whole string in quotes, escapes embedded commas and quote characters!
    function _Dequote(strValue: string): string; virtual; // removes quotes

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): boolean; override;

    // SELECT * FROM TABLE WHERE <fieldname> LIKE <pattern>:
    procedure SetFilter(FieldName, pattern: string); // Make Rows Visible Only if they match filterString

    procedure ClearFilter; // Clear all previous SetFilters, shows All Rows.

    /// procedure FilteredDeletion(Inverted:Boolean); /// XXX TODO?
    /// procedure DeleteRowsMatchingFilter; /// XXX TODO?
    /// procedure DeleteRowsNotMatchingFilter; /// XXX TODO?

    // this is necessary to make bookmarks work as well:
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer; override;

    // Additional procedures
    procedure EmptyTable;

      // Tells controls to redraw.
    procedure Refresh;

    // A fast row lookup function specific to this CSV table object.
    function FindByCSVKey(key: string): boolean;

    // Sort the table:
    procedure Sort(SortFields: string; Ascending: boolean);

    // All rows have a UserData and UserTag property, these
    // next two functions quickly set all the userdata and usertag
    // values for all rows, which is a good way to set defaults
    // without having to iterate through the dataset.
    procedure SetAllUserData(data: Pointer);
    procedure SetAllUserTags(tagValue: integer);

    // The UserData/UserTag properties apply to the row that the
    // cursor is sitting on. Without visibly moving the cursor,
    // its handy to get/set the usertag and data values.
    function GetUserTag(recno: integer): integer;
    procedure SetUserTag(recno, newValue: integer);

    function GetUserData(recno: integer): Pointer;
    procedure SetUserData(recno: integer; newValue: Pointer);

    function GetCSVHeader: string;

    {  Additional Public methods }
    procedure OpenWith(Strings: TStrings); virtual; // does AssignFromStrings then opens table.

    procedure AppendWith(Strings: TStrings); virtual;

    { Special declarations }
    // first time call OpenWith, then you can call AssignFromStrings subsequently,
    // as long as the field names and positions have not changed.
    procedure AssignFromStrings(const Strings: TStrings); virtual; // update String data directly.
    procedure AssignToStrings(Strings: TStrings); virtual;

    procedure DeleteRows(FromRow, ToRow: integer); // NEW: Quickly zap a bunch of rows:
    procedure ExportRows(FileName: string; FromRow, ToRow: integer); // NEW: Quickly save a bunch of rows:

    procedure ExportCSVFile(const FileName: string); virtual;
      // save out to a file. does NOT keep backups! If file exists, it will be
        // overwritten, and NO backups are made!

    procedure Flush; virtual; // Save CSV file to disk if file has changed and SavesChanges is true.
                  // Note: FLUSH will make backup copies if FAutoBackupCount>0!!!

    { Row Access as String }
    function GetRowAsString(const Index: integer): string; virtual;
    function GetColumnsAsString: string; virtual;

    function IsKeyUnique: boolean; // Checks current row's key uniqueness. Note that FCSVKeyDef MUST be set!

    property InternalData: TJvCSVRows read FData write FData;

    // NO published properties! This is a base class only!

  end;

  // TJvCSVDataSet is just a TJvCSVCustomInMemoryDataSet with all properties and events exposed:
  TJvCSVDataSet = class(TJvCSVCustomInMemoryDataSet)

  public
    property TableName: string read FTableName; // Another name, albeit read only, for the FileName property!

      // Per-Record user-data fields:
      //    Each record can have a pointer (for associating each row with an object)
    property UserData: Pointer read GetRowUserData write SetRowUserData;
      //    Each record can have a tag (integer) (for help in marking rows as Selected/Unselected or some other
      //    end user task)
    property UserTag: integer read GetRowTag write SetRowTag;

  published
    property FieldDefs stored FieldDefsStored;
    property Active;
    property FileName: string read FTableName write SetTableName;
    property BufferCount;
    property ReadOnly: boolean read FReadOnly write FReadOnly default false;
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

     // Additional Properties
    property Changed: boolean read FFileDirty write FFileDirty;
//     property DataFileSize: Integer read GetDataFileSize;

     // CSV Table definition properties:
    property CSVFieldDef: string read FCSVFieldDef write SetCSVFieldDef; // Our own "CSV Field Definition String"
    property CSVKeyDef: string read FCSVKeyDef write FCSVKeyDef; // Primary key definition.
    property CSVUniqueKeys: boolean read FCSVUniqueKeys write FCSVUniqueKeys; // Rows must be unique on the primary key.
    property HasHeaderRow: boolean read FHasHeaderRow write FHasHeaderRow default true;
    property CaseInsensitive: boolean read FCSVCaseInsensitiveComparison write FCSVCaseInsensitiveComparison;

     // Properties for Automatically Loading/Saving CSV file when Active property is set true/false:
    property LoadsFromFile: boolean read FLoadsFromFile write FLoadsFromFile default true;
    property SavesChanges: boolean read FSavesChanges write FSavesChanges default true;
    property AutoBackupCount: integer read FAutoBackupCount write FAutoBackupCount;
      // >0 means Keep Last N Copies the Old CSV File, updated before each save?

     // Do field definitions "persist"?
     // Ie: do they get stored in DFM Form file along with the component
    property StoreDefs: boolean read FStoreDefs write FStoreDefs default false;
     { Additional Events }
    property OnSpecialData: TJvCSVOnSpecialData read FOnSpecialData write FOnSpecialData;
    property OnGetFieldData: TJvCSVOnGetFieldData read FOnGetFieldData write FOnGetFieldData;
    property OnSetFieldData: TJvCSVOnSetFieldData read FOnSetFieldData write FOnSetFieldData;

     { value in seconds : to do GMT to EST (ie GMT-5) use value of (-3600*5)
       This is only useful if you use the Hex encoded date-time fields.
     }
    property TimeZoneCorrection: integer read FTimeZoneCorrection write FTimeZoneCorrection default 0;

     { If false (default) we use the more normal CSV rendering of quotes, which is to double them in
       the CSV file, but if this property is true, we use backslash-quote to render quotes in the file,
       which has the side-effect of also requiring all backslashes to themself be escaped by a backslash.
       So filenames would have to be in the form "c:\\directory\\names\\like\\c\\programmers\\do\\it".
       Not recommended behaviour, except when absolutely necessary! }
    property EnquoteBackslash: boolean read FEnquoteBackslash write FEnquoteBackslash default false;
  end;

{ CSV String Processing Functions }
procedure CSVRowToString(RowItem: PCSVRow; var RowString: string);

{ modified! }
procedure StringToCSVRow(const RowString: string; RowItem: PCSVRow; permitEscapeSequences, EnquoteBackslash: boolean);

function CSVRowItemCopy(Source, Dest: PCSVRow; FieldIndex, FieldSize: integer): boolean;
procedure SetCSVRowItem(pItem: PCSVRow; ColumnIndex: integer; newValue: string);
function GetCSVRowItem(pItem: PCSVRow; ColumnIndex: integer): string;
procedure CSVRowSetDirtyBit(row: PCSVRow; ColumnIndex: integer);
procedure CSVRowClearDirtyBit(row: PCSVRow; ColumnIndex: integer);
function CSVRowGetDirtyBit(row: PCSVRow; ColumnIndex: integer): boolean;
procedure CSVRowSetColumnMarker(row: PCSVRow; ColumnIndex: integer; ColumnMarker: integer);
function CSVRowGetColumnMarker(row: PCSVRow; ColumnIndex: integer): integer;

{ Date/Time String decoding functions }
function TimeTHexToDateTime(HexStr: string; TimeZoneCorrection: integer): TDateTime;
function TimeTAsciiToDateTime(AsciiDateStr: string): TDateTime;

{ Date/Time string encoding functions }
function DateTimeToTimeToIsoAscii(aDateTime: TDateTime): string;
function DateTimeToTimeTHex(aDateTime: TDateTime; TimeZoneCorrection: integer): string;

{ Routine to keep backup copies of old data files around }
function JvCSVBackupPreviousFiles(FileName: string; MaxFiles: integer): boolean;

//JvCSVWildcardMatch:
// Recursive wildcard (%=AnyString, ?=SingleChar) matching function with
// boolean sub expressions (|=or, &=and).
function JvCSVWildcardMatch(data, pattern: string): boolean;

implementation

uses
  Forms, Controls, JvCSVParse
  {$IFNDEF COMPILER6_UP}
  , JvJVCLUtils, JvJCLUtils
  {$ENDIF}
  ;

var
  CallCount: integer;

procedure JvCSVDatabaseError(const TableName, Message: string);
begin
  OutputDebugString(PChar('JvCSVDatabaseError in ' + TableName + ': ' + Message));
  raise EJvCSVDataSetError.Create(TableName + ':' + Message);
end;

    // Each ROW Record has an internal Data pointer (similar to the
    // user-accessible 'Data:Pointer' stored in treeviews, etc)

function TJvCSVCustomInMemoryDataSet.GetRowUserData: Pointer;
var
  recno: integer;
begin
  recno := GetRecNo;
  Result := FData.GetUserData(recno);
end;

procedure TJvCSVCustomInMemoryDataSet.SetRowUserData(UserData: Pointer);
var
  recno: integer;
begin
  recno := GetRecNo;
  FData.SetUserData(recno, UserData);
end;

function TJvCSVCustomInMemoryDataSet.GetRowTag: integer;
var
  recno: integer;
begin
  recno := GetRecNo;
  Result := FData.GetUserTag(recno);
end;

procedure TJvCSVCustomInMemoryDataSet.SetRowTag(tagValue: integer);
var
  recno: integer;
begin
  recno := GetRecNo;
  FData.SetUserTag(recno, tagValue);
end;

function _WildcardsMatchBoolOp(data, pattern: string; boolOp: char): boolean;
var
  subPattern: array[0..20] of string;
  t, Count: integer;
begin
  Count := StrSplit(pattern, boolOp, {Chr(0)=No Quoting} Chr(0), subPattern, 20);
  if (Count > 0) then
  begin
    for t := 0 to Count - 1 do
    begin
      Result := JvCSVWildcardMatch(data, subPattern[t]);
          // If ANY OR TRUE return TRUE;
          // if ANY AND FALSE return FALSE;
      if (boolOp = '|') = Result then
      begin
        Exit;
      end;
    end;
  end
  else
  begin // split failed...
    Result := false;
    Exit;
  end;
  // if we get here, no short circuit was possible.
  if (boolOp = '|') then
    Result := false // NONE of the OR conditions were met!
  else
    Result := true; // ALL of the AND condition were met!
end;

procedure TJvCSVCustomInMemoryDataSet.SetAllUserTags(tagValue: integer);
var
//  row: PCSVRow;
  t: integer;
begin
  FData.SetUserTag(FData.Count - 1, tagValue);
  for t := 0 to FData.Count - 2 do
  begin
    FData.SetUserTag(t, tagValue);
  end;
end;

procedure TJvCSVCustomInMemoryDataSet.SetAllUserData(data: Pointer);
var
//  row: PCSVRow;
  t: integer;
begin
  FData.SetUserData(FData.Count - 1, data); // Optimization. Ensures we only call SetLength ONCE!
  for t := 0 to FData.Count - 2 do
  begin
    FData.SetUserData(t, data);
  end;
end;

function TJvCSVCustomInMemoryDataSet.GetUserTag(recno: integer): integer;
begin
  Result := FData.GetUserTag(recno);
end;

procedure TJvCSVCustomInMemoryDataSet.SetUserTag(recno, newValue: integer);
begin
  FData.SetUserTag(recno, newValue);
end;

function TJvCSVCustomInMemoryDataSet.GetUserData(recno: integer): Pointer;
begin
  Result := FData.GetUserData(recno);
end;

procedure TJvCSVCustomInMemoryDataSet.SetUserData(recno: integer; newValue: Pointer);
begin
  FData.SetUserData(recno, newValue);
end;

// Recursive wildcard matching function

function JvCSVWildcardMatch(data, pattern: string): boolean;
var
  t: integer;
  firstwildcard: integer;
  datalength, patternlength, dataposition, patternposition: integer;
  firstBoolCondition: integer;
begin
  Result := true;
  patternlength := Length(pattern);
  if patternlength = 0 then
    Exit;
  // no data?
  datalength := Length(data);
  if (datalength = 0) then
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
  if (firstBoolCondition > 0) then
  begin
    Result := _WildcardsMatchBoolOp(data, pattern, '&');
    Exit;
  end;
  firstBoolCondition := Pos('|', pattern);
  if (firstBoolCondition > 0) then
  begin
    Result := _WildcardsMatchBoolOp(data, pattern, '|');
    Exit;
  end;

  firstwildcard := Pos('%', pattern); // wildcards?
  if (firstwildcard = 0) then
    firstwildcard := Pos('?', pattern); // other wildcard.

  if (firstwildcard <= 0) then
  begin // no wildcard case.
    if (data = pattern) then
      Result := true
    else
      Result := false;
    Exit; // simple match returns immediately.
  end;
  // wildcard tail?
  if ((firstwildcard = patternlength) and (pattern[1] <> '?')) then
  begin // prefix match
    if Copy(data, 1, patternlength - 1) = Copy(pattern, 1, patternlength - 1) then
      Result := true
    else
      Result := false;
    Exit; // tail case is easy!
  end;
  // match literal characters until we hit wildcards,
  // then search for a wildcard resync, which continues
  // recursively.
  Result := true;
  dataposition := 1;
  patternposition := 1;
  while ((dataposition <= datalength) and (patternposition <= patternlength)) do
  begin
    // WILDCARD HANDLER
    if (pattern[patternposition] = '?') then
    begin // match any one character or nothing.
      Inc(patternposition);
      Inc(dataposition);
    end
    else if (pattern[patternposition] = '%') then
    begin
      if (patternposition = patternlength) then
      begin // last byte!
        Result := true;
        Exit;
      end;
       // Resync after %:
      t := Pos(pattern[patternposition + 1], data);
      while t > 0 do
      begin // possible resync point!
        Result := JvCSVWildcardMatch(Copy(data, t, Length(data)),
          Copy(pattern, patternposition + 1, patternlength)
          );
        if (Result) then
          Exit; // found a resync, and rest of strings match
        data := Copy(data, t + 1, datalength);
        datalength := Length(data);
//        dataposition := 0;
        if (datalength = 0) then
        begin
          Result := false;
          Exit;
        end;
        t := Pos(pattern[patternposition + 1], data);
      end; { end while loop }
       // failed to resync
      Result := false;
      Exit;
    end
    else
    begin // NORMAL CHARACTER
      if (data[dataposition] <> pattern[patternposition]) then
      begin
        Result := false; // failed.
        Exit;
      end;
      Inc(dataposition);
      Inc(patternposition);
    end;
  end; {while}
  if ((dataposition <= datalength)
    and (patternposition <= patternlength)
    ) then
  begin
    Result := false; // there is pattern left over, or data left over.
  end;
end;

// NEW: TJvCSVCustomInMemoryDataSet.SetFilter
//
// XXX Simplest possible filtering routine. Not very flexible.
// XXX Todo: Make this more flexible.
// XXX Users can also subclass and write their own filter.
// XXX Perhaps a OnFilter event should be provided, and SetCustomFilter
// XXX method would allow us to do a row by row filtering scan, and then
// XXX hide rows that the user sets HideRow := true in the event handler.
// XXX

procedure TJvCSVCustomInMemoryDataSet.SetFilter(FieldName, pattern: string);
  // Make Rows Visible Only if they match filterString
var
  valueLen, t: integer;
  pRow: PCSVRow;
  fieldRec: PCSVColumn;
  FieldIndex: integer;
  fieldValue: string;
begin
//  fieldIndex := Self.FCSVColumns.
  fieldRec := FCSVColumns.FindByName(FieldName);
  if not Assigned(fieldRec) then Exit;
  FieldIndex := fieldRec^.FPhysical;
  valueLen := Length(pattern); // if valuelen is zero then we are searching for blank or nulls
  pattern := UpperCase(pattern); // make value case insensitive.

  // Now check if field value matches given pattern for this row.
  for t := 0 to FData.Count - 1 do
  begin
    pRow := PCSVRow(FData[t]);
    if (not pRow^.filtered) then
    begin
      fieldValue := FData.GetARowItem(t, FieldIndex);
      if (Length(fieldValue) > 0) and (fieldValue[1] = '"') then
        fieldValue := _Dequote(fieldValue); // remove quotes.
      if (valueLen = 0) then
      begin
        if fieldValue <> '' then // if not empty, hide row.
          pRow^.filtered := true;
      end
      else
      begin
        fieldValue := UpperCase(fieldValue);
        if not JvCSVWildcardMatch(fieldValue, pattern) then // hide row if not same prefix
          pRow^.filtered := true;
      end;

    end
  end;
  FIsFiltered := true;
  First; // redisplay controls
end;

procedure TJvCSVCustomInMemoryDataSet.ClearFilter; // Clear Previous Filtering.
var
  t: integer;
  pRow: PCSVRow;
begin
  for t := 0 to FData.Count - 1 do
  begin
    pRow := PCSVRow(FData[t]);
    if Assigned(pRow) then
      pRow^.filtered := false; // clear all filter bits.
  end;
  FIsFiltered := false;
  First; // redisplay controls
end;

// note that file is not being locked!

{ TJvCSVCustomInMemoryDataSet }

constructor TJvCSVCustomInMemoryDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  // FRecordSize = size of a CSV text buffer and the indexes pointing
  //               into that buffer:

  FRecordSize := sizeof(TJvCSVRow) - sizeof(TJvCSVBookmark);

  // FBuffer size includes CSV Text buffer, and the bookmark data, followed
  // by space for storing the binary form of a calculated-field:

  // initial FBufferSize size: My theory is that we should pick a conservative
  // estimate plus a margin for error:
  FBufferSize := sizeof(TJvCSVRow) + 128; {CalcFieldsSize}
  ; // our regular record + calculated field data.

  FReadOnly := false;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
  FLoadsFromFile := true;
  FSavesChanges := true;
  FHasHeaderRow := true;

  { Additional initialization }
  FCSVColumns := TJvCSVColumns.Create;
  FData := TJvCSVRows.Create;
  FData.EnquoteBackslash := FEnquoteBackslash;

end;

destructor TJvCSVCustomInMemoryDataSet.Destroy;
begin
  InternalClearFileStrings; // delete file strings

  try
    if FCursorOpen then InternalClose;

  except
  end;
  if Assigned(FCSVColumns) then
  begin
    FCSVColumns.Clear;
    FCSVColumns.Free;
  end;
  if Assigned(FData) then
  begin
    FData.Clear;
    FData.Free;
  end;
  inherited Destroy;
end;

function TJvCSVCustomInMemoryDataSet.AllocRecordBuffer: PChar;
var
  RowPtr: PCSVRow;
begin
  RowPtr := AllocMem(FBufferSize {Sizeof(TJvCSVRow)});
//  Trace('AllocRecordBuffer result=$'+IntToHex(Integer(Pointer(RowPtr)),8));
  Result := PChar(RowPtr);
end;

{ calc fields support }

procedure TJvCSVCustomInMemoryDataSet.ClearCalcFields(Buffer: PChar);
begin
     // Assumes that our buffer is a TJvCSVRow followed by
     // a dynamically resized buffer used for calculated field
     // storage:
  FillChar(Buffer[sizeof(TJvCSVRow)], CalcFieldsSize, 0);
end;

{ calc fields support and buffer support }

function TJvCSVCustomInMemoryDataSet.GetActiveRecordBuffer: PChar;
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

procedure TJvCSVCustomInMemoryDataSet.SetCSVFieldDef(CSVFieldDefs: string);
begin
  if (FCSVFieldDef <> CSVFieldDefs) then
  begin
    CheckInActive;
    FCSVFieldDef := CSVFieldDefs;
    FHeaderRow := '';
    FieldDefs.Clear; // Clear VCL Database field definitions
    FCSVColumns.Clear; // Clear our own CSV related field data
    FData.Clear; // Clear out data
  end;
end;

procedure TJvCSVCustomInMemoryDataSet.FreeRecordBuffer(var Buffer: PChar);
//var
//  RowPtr:PCSVRow;
begin
  //Trace( 'FreeRecordBuffer '+IntToHex(Integer(Buffer),8) );
// try
  if Buffer <> nil then
    FreeMem(Buffer);
// except
     //Trace( 'FreeRecordBuffer - Exception freeing '+IntToHex(Integer(Buffer),8) );
//  end;
//  //Trace('TJvCSVCustomInMemoryDataSet.FreeRecordBuffer');

end;

{ called after the record is allocated }

procedure TJvCSVCustomInMemoryDataSet.InternalInitRecord(Buffer: PChar);
var
  RowPtr: PCSVRow;
begin
  //Trace( 'InternalInitRecord '+IntToHex(Integer(Buffer),8) );

  FillChar(Buffer^, FBufferSize, 0);
  RowPtr := PCSVRow(Buffer); // Zero out the buffer.
  CSVRowInit(RowPtr);
end;

// CSVRowInit
//
// Internal handy dandy function to set up a new CSV row.
// which is intially full of just commas.
//

procedure TJvCSVCustomInMemoryDataSet.CSVRowInit(RowPtr: PCSVRow);
var
  t: integer;
  ColCount: integer;
begin
  RowPtr^.Index := -1; // Not Yet Indexed
  RowPtr^.fdirty := false;
  RowPtr^.Bookmark.flag := bfEOF;
  RowPtr^.Bookmark.data := ON_BOF_CRACK; // no index into FData yet.
  CSVRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);

  ColCount := FCSVColumns.Count;
  if ColCount <= 0 then ColCount := 10;

  for t := 1 to ColCount do
  begin // create an empty line of just commas
    if (t < ColCount) then
      RowPtr^.Text[t - 1] := ','
    else
      RowPtr^.Text[t - 1] := Chr(0);
    RowPtr^.Text[t] := Chr(0);
    CSVRowSetColumnMarker(RowPtr, {column} t - 1, {marker value} t - 1);
    CSVRowSetColumnMarker(RowPtr, {column} t, {marker value} COLUMN_ENDMARKER);
  end;
end;

function TJvCSVCustomInMemoryDataSet.IsKeyUnique: boolean;
  // Checks current row's key uniqueness. Note that FCSVKeyDef MUST be set!
begin
  Result := false; // not yet implemented! XXX
end;

function TJvCSVCustomInMemoryDataSet.InternalSkipFiltered(defaultResult: TGetResult; ForwardBackwardMode: boolean):
  TGetResult;
var
  LimitReached: boolean;
  RowPtr: PCSVRow;
begin
  Result := defaultResult;
  if (FRecordPos < 0) then
    Exit;
  LimitReached := false; // hit BOF or EOF?
  while not LimitReached do
  begin
    { no skippage required }
    RowPtr := PCSVRow(FData.GetRowPtr(FRecordPos));
    if (not RowPtr^.filtered) then
      Exit;
    { skippage ensues }
    if (ForwardBackwardMode) then
    begin // ForwardSkip mode
      Inc(FRecordPos);
      if (FRecordPos >= FData.Count) then
      begin
        FRecordPos := ON_EOF_CRACK;
        Result := grEOF;
        Exit;
      end;
    end
    else
    begin // BackwardSkip mode
      Dec(FRecordPos);
      if (FRecordPos < 0) then
      begin // hit BOF_CRACK
        FRecordPos := ON_BOF_CRACK;
        Result := grBOF;
        Exit;
      end;
    end;
  end;
end;

function TJvCSVCustomInMemoryDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: boolean): TGetResult;
var
  RowPtr: PCSVRow;
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
        else if FRecordPos = ON_EOF_CRACK then
        begin
          FRecordPos := FData.Count - 1;

          // NEW FILTERING
          if FIsFiltered then
            Result := InternalSkipFiltered(grOK, false) // skipping backwards.
          else
            Result := grOK;
        end
        else if FRecordPos > 0 then
        begin
          Dec(FRecordPos);

          // NEW FILTERING
          if FIsFiltered then
            Result := InternalSkipFiltered(grOK, false) // skipping backwards.
          else
            Result := grOK;

        end
        else
          Result := grBOF;
      end;

    gmCurrent:
      begin

         //Trace(' GetRecord( Buffer, gmCurrent, DoCheck)');
        if (FRecordPos < 0) then // BOF Crack or EOF Crack?
          Result := grError
        else
          Result := grOK;

          // NEW FILTERING
        if FIsFiltered then
          Result := InternalSkipFiltered(Result, true); // skipping forwards.

      end;
    gmNext:
      begin
         //Trace(' GetRecord( Buffer, gmNext, DoCheck)');
        if FRecordPos = ON_EOF_CRACK then
          Result := grEOF
        else
        begin
          Inc(FRecordPos);

          if (FRecordPos >= FData.Count) then
          begin
            FRecordPos := ON_EOF_CRACK;
            Result := grEOF
          end
          else
          begin
            // NEW FILTERING
            if FIsFiltered then
              Result := InternalSkipFiltered(grOK, true) // skipping forwards.
            else
              Result := grOK;
          end;

        end
      end;

      // default case:
  else
    JvCSVDatabaseError(FTableName, 'GetMode???');
  end; {end case}

  if Result = grOK then
  begin
       //Trace( ' GetRecord FRecordPos='+IntToStr(FRecordPos)+'result=grOk' );
    try
        { get a record into a buffer }
      RowPtr := PCSVRow(Buffer); // Cast to a Row Data Structure to our own type.
      Move({source:}FData.GetRowPtr(FRecordPos)^, {dest:} RowPtr^, sizeof(TJvCSVRow));
      RowPtr^.Bookmark.flag := bfCurrent;
      RowPtr^.Bookmark.data := FRecordPos;

        // Update calculated fields for this row:
      ClearCalcFields(Buffer);
      GetCalcFields(Buffer);

    except
      JvCSVDatabaseError(FTableName, 'Problem reading row ' + IntToStr(FRecordPos));
    end;
  end
  else
  begin

      // fudge: Get bookmark into a record for BOF and EOF records:
{      if RowPtr <> NIL then
          RowPtr^.bookmark.data := FRecordPos;}

    if (Result = grError) and DoCheck then
      JvCSVDatabaseError(FTableName, 'No records');
  end;

//    if (Result = grError) then
          //Trace(' GetRecord result = grError');
//    if (Result = grEof) then
          //Trace(' GetRecord result = grEof');
//     if (Result = grBof) then
          //Trace(' GetRecord result = grBof');

end;

function TJvCSVCustomInMemoryDataSet._Enquote(strVal: string): string;
  // puts whole string in quotes, escapes embedded commas and quote characters!
var
  s: string;
  t, l: integer;
  ch: char;
begin
  s := '"';
  l := Length(strVal);
  for t := 1 to l do
  begin
    ch := strVal[t];
    if FEnquoteBackslash then
    begin // backslash quoting ( C/C++ )
      if ch = '\' then // double each backslash
        s := s + '\\'
      else if ch = '"' then // escape quotes with a backslash
        s := s + '\"'
      else
        s := s + ch;
    end
    else
    begin
                // simpler method: doubled-quotes ( Pascal )
      if ch = '"' then // escape quotes by doubling them.
        s := s + '""'
      else
        s := s + ch;

    end;
  end;
  s := s + '"'; // end quote.
  Result := s;
end;

function TJvCSVCustomInMemoryDataSet.GetRecordSize: Word;
begin
 // In create:
 //    FRecordSize := Sizeof(TJvCSVRow) - Sizeof(TJvCSVBookmark);
  Result := FRecordSize;
end;

procedure TJvCSVCustomInMemoryDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  RowPtr: PCSVRow;
  NewVal: string;
  PhysicalLocation: integer;
  pDestination: PChar;
  CSVColumnData: PCSVColumn;
  DT: TDateTime;
begin

  //Trace( 'SetFieldData '+Field.FieldName );
  pDestination := GetActiveRecordBuffer;
  RowPtr := PCSVRow(pDestination);

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow) = 0) then
  begin
    FHeaderRow := GetColumnsAsString;
    ProcessCSVHeaderRow(FHeaderRow);
  end;

 // If this is a calculated field or lookup field then...
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    if (Field.Offset = 0) then
    begin
      OutputDebugString('JvCSVData.SetFieldData: Invalid field.Offset in Calculated or Lookup field.');
      Exit;
    end;
    Inc(pDestination, sizeof(TJvCSVRow) + Field.Offset);
    boolean(pDestination[0]) := (Buffer <> nil);
    if boolean(pDestination[0]) then
      CopyMemory(@pDestination[1], Buffer, Field.DataSize);
    //result := true; {there is no return value, oops}
    Exit;
  end;

 // If we get here, we are dealing with a physical record:

 // Set a field data, taking the physical to logical ordering translation into
 // account:
  CSVColumnData := FCSVColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CSVColumnData) then
    Exit;

  PhysicalLocation := CSVColumnData^.FPhysical;

  if PhysicalLocation < 0 then
    Exit;

  if Buffer = nil then
    NewVal := ''
  else
    case Field.DataType of
      ftString:
        begin
          NewVal := string(PChar(Buffer));
            //----------------------------------------------------------------------------------------------------
            // NEW RULE: If user displayed value contains a comma, a backslash, or a double quote character
            // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
            // and backslashes preceded by a backslash character.
            //----------------------------------------------------------------------------------------------------
          if (Pos(',', NewVal) > 0)
            or (Pos('"', NewVal) > 0)
            or ((Pos('\', NewVal) > 0) and (FEnquoteBackslash)) then
          begin
            NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
          end;
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
        case CSVColumnData^.FFlag of
              // Localized time in Ascii
          jCSVAsciiDateTime:
            begin
              DT := TimeStampToDateTime(
                MSecsToTimeStamp(
                Double(Buffer^)));
              NewVal := DateTimeToTimeToIsoAscii(DT);
                  //OutputDebugString(PChar('date '+NewVal));

            end;

             // GMT Times are stored in HEX
          jCSVGMTDateTime:
            begin
              DT := TimeStampToDateTime(
                MSecsToTimeStamp(
                Double(Buffer^)));
              NewVal := DateTimeToTimeTHex(DT, 0);

            end;

          jCSVTZDateTime: // Move a GMT time into a timezone:
            begin
              DT := TimeStampToDateTime(
                MSecsToTimeStamp(
                Double(Buffer^)));
              NewVal := DateTimeToTimeTHex(DT, FTimeZoneCorrection);

            end;

        else
          JvCSVDatabaseError(FTableName, 'SetFieldData Error - TimeT-to-DateTime conversion error.');
        end;
    else
      JvCSVDatabaseError(FTableName, 'SetFieldData Error - Field type not handled.');
    end;

 // Set new data value (NewVal = String)
  SetCSVRowItem(RowPtr, PhysicalLocation, NewVal);
  if Assigned(FOnSetFieldData) and (RowPtr^.Index >= 0) then
  begin
    FOnSetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName, NewVal);
  end;

 // Set a dirty bit so we remember to write this later:
  CSVRowSetDirtyBit(PCSVRow(pDestination), PhysicalLocation);

 // Set the file-wide dirty bit:
  FFileDirty := true;

 // Notify controls of a field change:
  DataEvent(deFieldChange, Longint(Field));

end;

// Removes first and last character of the string (assumes they are quotes,
// to be called byGetFieldData only!)

function TJvCSVCustomInMemoryDataSet._Dequote(strValue: string): string;
var
  s: string;
  t, l: integer;
  ch: char;
  skipFlag: boolean;
begin
  l := Length(strValue);
  skipFlag := false;
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
        skipFlag := true; // whatever is after the backslash is an escaped literal character.
        continue;
      end
      else
        skipFlag := false;
    end
    else
    begin
      if (not skipFlag) and (ch = '"') and (t < (l - 1)) and (strValue[t + 1] = '"') then
      begin
        skipFlag := true;
        continue; // skip first of the doubled quote characters.
      end
      else
        skipFlag := false;
    end;
    s := s + ch;
  end;
  Result := s;
end;

// Tells controls to redraw.

procedure TJvCSVCustomInMemoryDataSet.Refresh;
var
  m: TBookmark;
begin
  if State <> dsBrowse then Exit;

  m := GetBookmark; // This appears a bit silly but it works very well.
  First; // Redraws all controls once to relocate to top.
  GotoBookmark(m); // Go back where we were. This could result in some odd scrolling behaviour but I haven't seen it yet.
end;

function TJvCSVCustomInMemoryDataSet.GetFieldData(Field: TField; Buffer: Pointer): boolean;
var
  RowPtr: PCSVRow;
  {ActiveRowPtr:PCSVRow;}
  pSource: PChar;
  pTempBuffer: PChar;
  UserString, TempString: string;
  PhysicalLocation: integer;
  CSVColumnData: PCSVColumn;
  aDateTime: TDateTime;
  l: integer;
  ts: TTimeStamp;
begin
  Result := false;
  //Trace( 'GetFieldData '+Field.FieldName );

  if not FCursorOpen then
    Exit;
  if Field = nil then
    Exit;

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow) = 0) then
  begin
    FHeaderRow := GetColumnsAsString;
    ProcessCSVHeaderRow(FHeaderRow);
  end;

  pSource := GetActiveRecordBuffer;
  if (pSource = nil) then
  begin
    //JvCSVDatabaseError('CSVDataSet.GetFieldData: Unable to get active record buffer');
    Exit;
  end;

  //------------------------------------------------------------------------
  // Calculated and Lookup Field Handling
  //
  // direct memory copy into calculated field or lookup field data area
  //------------------------------------------------------------------------
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    if (Field.Offset = 0) then
    begin
            // Invalid offset!
      OutputDebugString('JvCSVData.GetFieldData: Invalid field.Offset in Calculated or Lookup field.');
    end;
    Inc(pSource, sizeof(TJvCSVRow) + Field.Offset);
    if (pSource[0] = #0) or (Buffer = nil) then
      Exit
    else // Get the field data from the buffer:
      CopyMemory(Buffer, @pSource[1], Field.DataSize);
    Result := true;
    Exit;
  end;

  //------------------------------------------------------------------------
  // If we get here we must be dealing with a real column of data
  // that is part of the CSV file rather than a calculated or lookup
  // field that is just in internal-memory:
  //------------------------------------------------------------------------

  CSVColumnData := FCSVColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CSVColumnData) then
  begin
    JvCSVDatabaseError(FTableName, 'Unable to locate CSV file information for field ' + Field.Name);
    Exit;
  end;
  PhysicalLocation := CSVColumnData^.FPhysical;
  if PhysicalLocation < 0 then
  begin // does it really exist in the CSV Row?
    JvCSVDatabaseError(FTableName, 'Physical location of CSV field ' + Field.FieldName + ' unknown.');
    Exit;
  end;

  //------------------------------------------------------------------------
  // All items in the CSV table are natively stored as strings. Note that
  // an empty string is considered to be a NULL if the field type is anything
  // other than a ftString. There are no NULLs in ftString fields because
  // a CSV file can store an empty string but has no way of indicating a NULL.
  //------------------------------------------------------------------------

  RowPtr := PCSVRow(pSource);

  TempString := GetCSVRowItem(RowPtr, PhysicalLocation);

  // Strip quotes first!
  if (Field.DataType = ftString) then
  begin
    l := Length(TempString);
    if (l >= 2) then
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
    RowPtr^.recursionFlag := true;
    UserString := TempString;
    FOnGetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName,
      UserString);
    if (UserString <> TempString) then
    begin
            // Write changed value back to row:
      SetCSVRowItem(RowPtr, PhysicalLocation, UserString);
      TempString := UserString;
             // Notify controls of a field change:
             //DataEvent(deFieldChange, LongInt(Field));
             // XXX Doesn't do what I needed. left here commented out
             // in case I ever go back and try to get something like this
             // working again.

    end;
    RowPtr^.recursionFlag := false;
  end;

    // NULL:  There are no "Real" NULLS in an ASCII flat file, however for anything
  // other than a string field, we will return "NULL" to indicate there is an
  // empty string in the field.
  if (Field.DataType <> ftString) then
    if Length(TempString) = 0 then
      Exit; // NULL field.

  { If buffer is nil, then we are being asked to do a null check only.}
  if (Buffer = nil) then
  begin
    if Length(TempString) = 0 then
      Result := false
    else
      Result := true;
    Exit; { cannot actually copy data into nil buffer, so returns now. }
  end;

  //------------------------------------------------------------------------
  // If we get here Buffer must NOT be nil. Now we handle
  // some CSV to TField conversions:
  //------------------------------------------------------------------------
  try
    case Field.DataType of
        // Basic string copy, convert from String to fixed-length
        // buffer, padded with NUL i.e. Chr(0):
      ftString:
        begin
          pTempBuffer := AllocMem(Field.Size + 1); // AllocMem fills with zeros
          StrCopy(pTempBuffer, PChar(TempString)); // we copy in the data portion
          Move(pTempBuffer^, Buffer^, Field.Size); // Buffer^ is now zero padded.
          FreeMem(pTempBuffer); // Free the memory we allocated.
        end;

        // Standard Integer conversion:
      ftInteger: PInteger(Buffer)^ := StrToInt(TempString);

        // Standard Double-precision Float conversion:
      ftFloat:
        begin
          PDouble(Buffer)^ := StrToFloat(TempString)
        end;

      ftBoolean:
        begin
          if Length(TempString) = 0 then
          begin
            PInteger(Buffer)^ := 0;
          end
          else
          begin
            if StrToIntDef(TempString, 0) <> 0 then
              PWordBool(Buffer)^ := true // bugfix May 26, 2003 - WP
            else
              PWordBool(Buffer)^ := false; // bugfix May 26, 2003 - WP
          end;
        end;
      ftDateTime:
        case CSVColumnData^.FFlag of
             // Ascii Date 1999/03/05 08:23:15
          jCSVAsciiDateTime:
            begin
              aDateTime := TimeTAsciiToDateTime(TempString);
              if aDateTime <= 1.0 then
              begin
                Result := false; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(aDateTime);
              if (ts.Time = 0) and (ts.Date = 0) then
              begin
                OutputDebugString('DateTimeToTimeStamp internal failure.');
                Exit;
              end;
                // XXX Delphi Weirdness Ahead.  Read docs before you try to
                // understand this. We want to store 8 bytes at Buffer^, this
                // is how we do it.
              Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(aDateTime));

            end;

             // GMT Times are Stored in HEX:
          jCSVGMTDateTime:
            Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString, 0)));

             // Move GMT into a Timezone:
          jCSVTZDateTime:
            Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,
              FTimeZoneCorrection)));

        else
          JvCSVDatabaseError(FTableName, 'GetFieldData Error - TimeT-to-DateTime conversion error.');
        end; {end case}
    else // not a valid ftXXXX type for this TDataSet descendant!?
      JvCSVDatabaseError(FTableName, 'GetFieldData Error- Field type not handled.');
    end // end case.
  except
    on E: EConvertError do
    begin
      Result := false; // return a NULL.
      Exit;
    end;
  end;
  // All is Well.
  Result := true;
end;

// Our bookmark data is a pointer to a PCSVData

procedure TJvCSVCustomInMemoryDataSet.GetBookmarkData(Buffer: PChar; data: Pointer);
//var
//  t:Integer;
begin
// t:= PCSVRow(Buffer)^.bookmark.data;
  PInteger(data)^ := PCSVRow(Buffer)^.Bookmark.data;
end;

function TJvCSVCustomInMemoryDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
  Result := PCSVRow(Buffer)^.Bookmark.flag;
end;

// nobody mentioned that I needed this to be overloaded, but I only found
// out when I found that DBGrid and other controls that compare bookmarks
// won't function if you don't provide a non-default implementation of this.

function TJvCSVCustomInMemoryDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): integer;
var
  v1, v2: integer;
begin
  v1 := -999999;
  v2 := -999999;
  if Bookmark1 <> nil then
    v1 := integer(Bookmark1^);
  if Bookmark2 <> nil then
    v2 := integer(Bookmark2^);
  Result := Bookmark_Eql;
  if v1 < v2 then
    Result := Bookmark_Less
  else if v1 > v2 then
    Result := Bookmark_Gtr;
end;

procedure TJvCSVCustomInMemoryDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCSVRow(Buffer)^.Bookmark.flag := Value;
end;

procedure TJvCSVCustomInMemoryDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  {Bookmark is just pointer to integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCSVCustomInMemoryDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FRecordPos := PCSVRow(Buffer)^.Bookmark.data; //Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:

procedure TJvCSVCustomInMemoryDataSet.SetBookmarkData(Buffer: PChar; data: Pointer);
begin
  PCSVRow(Buffer)^.Bookmark.data := PInteger(data)^;
end;

procedure TJvCSVCustomInMemoryDataSet.InternalFirst;
begin
//  Eof := false;
  FRecordPos := ON_BOF_CRACK;
end;

// CSVFieldDef:
//
// A property of our Data Set called CSVFieldDef is treated as
// declaration of the fields in the CSV table.
//
//   <coldef>,<coldef>,...,<coldef>
//   <coldef> = columname:<data-type-character><size>
//
// See top of file!

procedure TJvCSVCustomInMemoryDataSet.InternalInitFieldDefs;
var
  CSVFieldRec: TJvCSVRow; //record type.
  CSVFieldOption: string;
  CSVFieldName: string;
  pCSVFieldDef: PCSVColumn;
  t, colnum, pos1: integer;
  // field options:
  FieldTypeChar: char;
  VclFieldType: TFieldType;
  FieldLen: integer;
  FieldType: TJvCSVColumnFlag;
  aCSVFieldDef: string;
  CSVKeys: array of string;
//  FDef:TFieldDef;
begin

//  FFieldsInitialized := true;
  FieldType := jCSVString;
  VclFieldType := ftString;

  // create FieldDefs which map to each field in the data record
  FieldDefs.Clear; // Clear VCL Database field definitions
  FCSVColumns.Clear; // Clear our own CSV related field data

  aCSVFieldDef := FCSVFieldDef;
  if Length(aCSVFieldDef) = 0 then
  begin
    if FHasHeaderRow and InternalLoadFileStrings then
      aCSVFieldDef := FCSVFileAsStrings[0];
  end;

  if Length(aCSVFieldDef) > 0 then
  begin
    StringToCSVRow(aCSVFieldDef, @CSVFieldRec, false, false);

    colnum := 0;
    while (CSVRowGetColumnMarker(@CSVFieldRec, colnum) <> COLUMN_ENDMARKER) do
    begin
      FieldLen := 80; // default.
      CSVFieldOption := GetCSVRowItem(@CSVFieldRec, colnum); // get a string in the format COLUMNAME:Options

       // Look for Colon or Semicolon:
      pos1 := Pos(':', CSVFieldOption);
      if (pos1 <= 0) then pos1 := Pos(';', CSVFieldOption);

      if (pos1 <= 0) then
      begin
        CSVFieldName := CSVFieldOption;
        CSVFieldOption := '$';
        FieldTypeChar := '$';
      end
      else
      begin
          // extract field name:
        CSVFieldName := Copy(CSVFieldOption, 1, pos1 - 1);
          // If character after the colon is a symbol character, grab
          // it, otherwise default to '$'.
        if Ord(CSVFieldOption[pos1 + 1]) < Ord('A') then
        begin
          FieldTypeChar := CSVFieldOption[pos1 + 1];
          CSVFieldOption := Copy(CSVFieldOption, pos1 + 2, 80);
        end
        else
        begin
          FieldTypeChar := '$';
          CSVFieldOption := Copy(CSVFieldOption, pos1 + 1, 80);
        end;
        FieldLen := StrToIntDef(CSVFieldOption, DEFAULT_CSV_STR_FIELD);
      end;
      case FieldTypeChar of
        '$':
          begin // $=string
            VclFieldType := ftString;
            FieldType := jCSVString;
          end;
        '%':
          begin // %=Integervalue
            VclFieldType := ftInteger;
            FieldType := jCSVNumeric;
            FieldLen := 0; // automatic.
          end;
        '&':
          begin // &=Float value
            VclFieldType := ftFloat;
            FieldType := jCSVNumeric;
            FieldLen := 0; // automatic.
          end;
        '@':
          begin // @=Datetime as Ascii YYYY/MM/DD HH:MM:SS
            VclFieldType := ftDateTime;
            FieldType := jCSVAsciiDateTime;
            FieldLen := 0; // automatic.
          end;
        '!':
          begin // != boolean field TRUE/FALSE
            VclFieldType := ftBoolean; // boolean field in dataset
            FieldType := jCSVNumeric; // numeric field in file
            FieldLen := 0; // automatic.
          end;
        '#':
          begin // #=Datetime as Seconds since 1970 stored in HEX
            VclFieldType := ftDateTime;
            FieldType := jCSVGMTDateTime;
            FieldLen := 0; // automatic.
          end;

        '-':
          begin // -=Datetime as Seconds since 1970 stored in HEX
            VclFieldType := ftDateTime;
            FieldType := jCSVTZDateTime;
            FieldLen := 0; // automatic.
          end;

      else
        JvCSVDatabaseError(FTableName, 'Invalid field type character: ' + FieldTypeChar);
      end;

      if Length(CSVFieldName) = 0 then
      begin
        JvCSVDatabaseError(FTableName, 'Unexpected error parsing CSV Field Definitions');
        break;
      end;

       // sometime later: unpack the rest of the string
       // and declare ftString,ftFloat,ftInteger,ftDateTime, etc.
       // now add the field:
      Inc(colnum);

       // This may throw an exception. but we'll just allow
       // that as necessary:

        //Was: TFieldDef.Create(FieldDefs, ...., colnum );
      FieldDefs.Add(CSVFieldName, VclFieldType, FieldLen, false);

      // Now create our internal field data structure:
      pCSVFieldDef := AllocMem(sizeof(TJvCSVColumn) {+ 8 BIGFudge});
      pCSVFieldDef^.FFlag := FieldType; {jCSVString}
      pCSVFieldDef^.FFieldDef := FieldDefs.Find(CSVFieldName);

      // Note: field order is established when we open the file (later)
      pCSVFieldDef^.FPhysical := -1; // not yet located in the physical file!
      FCSVColumns.AddColumn(pCSVFieldDef);
    end;

     // if the file doesn't contain this and we haven't
     // generated it yet, generate the header row:
    if (not FHasHeaderRow) and (Length(FHeaderRow) = 0) then
      FHeaderRow := GetColumnsAsString;

    if Length(FHeaderRow) > 0 then
      ProcessCSVHeaderRow(FHeaderRow);
  end
  else
    JvCSVDatabaseError(FTableName, 'Field Definition Error. CSVFieldDef, FieldDefs, and file contents must match.');

  if Length(FCSVKeyDef) = 0 then
  begin
    FCSVKeyCount := 0;
  end
  else
  begin
    SetLength(CSVKeys, FCSVColumns.Count);
    FCSVKeyCount := StrSplit(FCSVKeyDef, ',', {Chr(0)=No Quoting} Chr(0), CSVKeys, FCSVColumns.Count);
    SetLength(FCSVKeyFields, FCSVKeyCount);
    if (FCSVKeyCount < 1) or (FCSVKeyCount > FCSVColumns.Count) then
      JvCSVDatabaseError(FTableName, 'Invalid CSVKeyDef property. InternalInitFieldDefs failed.');
    for t := 0 to FCSVKeyCount - 1 do
    begin
      if (CSVKeys[t] = '') then
        JvCSVDatabaseError(FTableName, 'Internal Error parsing CSVKeyDef. InternalInitFieldDefs failed.');
      pCSVFieldDef := FCSVColumns.FindByName(CSVKeys[t]);
      if not Assigned(pCSVFieldDef) then
      begin
        JvCSVDatabaseError(FTableName, 'CSVKeyDef contains field ''' + CSVKeys[t] +
          ''' which is not defined. InternalInitFieldDefs failed.');
      end
      else
      begin
        pCSVFieldDef^.FKeyFlag := true;
        FCSVKeyFields[t] := pCSVFieldDef;
      end;
    end;
  end;

end; { InternalInitFieldDefs ends }

{ set our position onto the EOF Crack }

procedure TJvCSVCustomInMemoryDataSet.InternalLast;
begin
//  Eof := true;
  FRecordPos := ON_EOF_CRACK; // FData.Count;
end;

// At shutdown or on user-calling this method, check if data has changed,
// and write changes to the file.

procedure TJvCSVCustomInMemoryDataSet.Flush;
begin
  if Length(FTableName) = 0 then
    raise Exception.Create('TJvCSVCustomInMemorYDataSet.FTableName is not set.');

  if FFileDirty and FSavesChanges and (Length(FTableName) > 0) then
  begin
    // Make backup first, if enabled (>2)
    if (FAutoBackupCount > 0) then
    begin
      if (FAutoBackupCount < 10) then FAutoBackupCount := 10; // can't be between 1 and 9, must be at least 10.
      JvCSVBackupPreviousFiles(FTableName, FAutoBackupCount);
    end;
    // Now write new file.
    ExportCSVFile(FTableName);
    FFileDirty := false;
  end;
end;

{procedure TJvCSVCustomInMemoryDataSet.DestroyFields;
begin
 inherited DestroyFields;
 // Clear out local TCSVFieldDefs.
 FCSVColumns.Clear;
end;}

procedure TJvCSVCustomInMemoryDataSet.InternalClose;
begin
  Flush;
  BindFields(false);
  if DefaultFields then DestroyFields;
  FData.Clear;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
end;

procedure TJvCSVCustomInMemoryDataSet.InternalHandleException;
begin
  // standard implementation for this method:
  if Application <> nil then
    Application.HandleException(Self);
end;

procedure TJvCSVCustomInMemoryDataSet.InternalDelete;
begin
  if (FRecordPos >= 0) and (FRecordPos < FData.Count) then
  begin
      // FreeMem performed inside DeleteRow!
    FData.DeleteRow(FRecordPos);
  end;

  if FRecordPos >= FData.Count then
    FRecordPos := FData.Count - 1;

  FFileDirty := true;
end;

{ returns -1 if not found, else returns record index }

function TJvCSVCustomInMemoryDataSet.InternalFindByKey(row: PCSVRow): integer;
var
  t: integer;
begin
  Result := -1;
  for t := 0 to FData.Count - 1 do
  begin
    if InternalCompare(FCSVKeyFields, FCSVKeyCount, {Left} row, {Right} FData.Items[t]) = 0 then
    begin
      Result := t;
      break;
    end;
  end;
end;

function TJvCSVCustomInMemoryDataSet.FindByCSVKey(key: string): boolean;
var
  logical_row, physical_row: TJvCSVRow;
  t, recno: integer;
  str: string;
begin
  Result := false;
  StringToCSVRow(key + ',', @logical_row, false, false); // initialize row and put items in their logical order.
  CSVRowInit(@physical_row);
  // Move from Logical (TFieldDef order) to their physical (As found in CSV file) ordering:
  for t := 0 to FCSVKeyCount - 1 do
  begin
    str := GetCSVRowItem(@logical_row, t);
    SetCSVRowItem(@physical_row, FCSVKeyFields[t].FPhysical, str);
      //if (t <> FCSVKeyFields[t].FPhysical) then
      //    OutputDebugString('FindByCSVKey debug');
  end;
  recno := InternalFindByKey(@physical_row);
  if (recno < 0) then
    Exit;

  FRecordPos := recno;
  Resync([]);
  Result := true;
end;

{procedure TJvCSVCustomInMemoryDataSet.InternalInsert;
//var
//  pAddRec : pCSVRow;
begin
// pAddRec := AllocMem(Sizeof(TJvCSVRow));
// StringToCSVRow( FEmptyRowStr, pAddRec, false ); // initialize row.
// FData.AddRow(pAddRec);
// FCurrentRecord := -1;
// Resync([]);
  FRecordPos :=  ON_EOF_CRACK;
//FCurrentRecord := FData.Count;
end;}

procedure TJvCSVCustomInMemoryDataSet.InternalAddRecord(Buffer: Pointer; Append: boolean);
var
  RecPos: integer;
  pAddRec: PCSVRow;
//  keyIndex: integer;
begin

  if FInsertBlocked then
  begin
    JvCSVDatabaseError(FTableName, 'InternalAddRecord Can''t Add. Insert blocked.');
    Exit;
  end;

  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;

  pAddRec := AllocMem(sizeof(TJvCSVRow));
  if (Buffer <> nil) then
    Move(PCSVRow(Buffer)^, pAddRec^, sizeof(TJvCSVRow));

  if (StrLen(pAddRec.Text) = 0) then
    StringToCSVRow(FEmptyRowStr, pAddRec, false, false); // initialize row.

  pAddRec.fdirty := true;
  pAddRec.Index := -1; // Was not loaded from the file!

 {
 if FCSVUniqueKeys then begin
    keyIndex := InternalFindByKey(pAddRec);
    if keyIndex >= 0 then begin
          JvCSVDatabaseError('Key value is not unique. Adding new record failed.');
          exit; // never get here, since normally JvCSVDatabaseError raises an exception.
    end;
 end;
  }
  FData.EnquoteBackslash := FEnquoteBackslash; // make sure FData is in the right mode.

  FFileDirty := true;
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

function TJvCSVCustomInMemoryDataSet.InternalLoadFileStrings: boolean;
begin
  Result := false;
  if not FileExists(FTableName) then Exit;
  if not FLoadsFromFile then Exit;
  if Assigned(FCSVFileAsStrings) then
  begin
    if FCSVFileAsStrings.Count > 0 then
      Result := true; //loaded already
    Exit; // don't repeat!
  end;

  try // open data file
    FCSVFileAsStrings := TStringlist.Create;
    FCSVFileAsStrings.LoadFromFile(FTableName);
    if FCSVFileAsStrings.Count > 0 then
      Result := true; // it worked!
  except
          //FTableName := '';
    FCSVFileAsStrings.Free;
    FCSVFileAsStrings := nil;
    raise; // pass exception on in.
  end; {end except}
end;

{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This cleans up that TString List. }

procedure TJvCSVCustomInMemoryDataSet.InternalClearFileStrings;
begin
  if Assigned(FCSVFileAsStrings) then
  begin
    FCSVFileAsStrings.Free;
    FCSVFileAsStrings := nil;
  end;
end;

procedure TJvCSVCustomInMemoryDataSet.InternalOpen;
var
//  Strings: TStringlist;
  TempBuf: array[0..MAXCOLUMNS] of char;
begin
  if FCursorOpen then InternalClose; // close first!

  FFileDirty := false;
  if (Length(FTableName) = 0) and FLoadsFromFile then
    JvCSVDatabaseError('noTableName', 'LoadFromFile=True, so a TableName is required');
//  Strings := nil;

  InternalInitFieldDefs; // initialize FieldDef objects

  // Create TField components when no persistent fields have been created
  if DefaultFields then
    CreateFields;
  BindFields(true); // bind FieldDefs to actual data

  if (FCSVColumns.Count > 1) then
  begin
     // Create a null terminated string which is just a bunch of commas:
    FillChar(TempBuf, FCSVColumns.Count - 1, ',');
    TempBuf[FCSVColumns.Count - 1] := Chr(0);
      // When adding an empty row, we add this string as the ascii equivalent:
    FEmptyRowStr := string(TempBuf);
  end
  else
  begin
    FEmptyRowStr := ''; // nothing.
  end;

  FBufferSize := sizeof(TJvCSVRow) + CalcFieldsSize; // our regular record + calculated field data.
  FRecordPos := ON_BOF_CRACK; // initial record pos before BOF
  BookmarkSize := sizeof(integer); // initialize bookmark size for VCL (Integer uses 4 bytes on 32 bit operating systems)

  //Trace( 'InternalOpen: FBufferSize='+IntToStr(FBufferSize) );
  //Trace( 'InternalOpen: CalcFieldsSize='+IntToStr(CalcFieldsSize) );
  //Trace( 'InternalOpen: FieldDefs.Count='+IntToStr(FieldDefs.Count) );

  if InternalLoadFileStrings then // may load the strings if they weren't loaded already!
    AssignFromStrings(FCSVFileAsStrings); // load into memory.

  InternalClearFileStrings; // now unload 'em.

  FCursorOpen := true;
  if Length(FHeaderRow) > 0 then
    ProcessCSVHeaderRow(FHeaderRow);

end;

procedure TJvCSVCustomInMemoryDataSet.InternalPost;
var
  pInsertRec: PCSVRow;
  RecPos: integer;
  keyIndex: integer; // If unique key enforcement is on, this is the key search result.
begin
  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;

  if FPostBlocked then
  begin
    JvCSVDatabaseError(FTableName, 'Posting to this database has been blocked.');
    Exit;
  end;

  { Unique Key Enforcement }
  if FCSVUniqueKeys then
  begin
    keyIndex := InternalFindByKey(PCSVRow(ActiveBuffer));
    // If posting an update, keyIndex better be <0 or else equal to FRecordPos!
    // Otherwise, if adding, keyIndex better be <0.
    if keyIndex >= 0 then
      if (State = dsInsert) or ((State = dsEdit) and (keyIndex <> FRecordPos)) then
      begin
        raise EJvCSVKeyError.Create(FTableName + ' - Key is not unique ');
        Exit; // never get here, since normally JvCSVDatabaseError raises an exception.
      end;
  end;

  if State = dsEdit then
  begin
    FFileDirty := true;
    RecPos := FRecordPos;
    Move(PCSVRow(ActiveBuffer)^, FData.GetRowPtr(RecPos)^, sizeof(TJvCSVRow));
    FData.GetRowPtr(RecPos)^.fdirty := true;
  end
  else if State = dsInsert then
  begin
    if FInsertBlocked then
    begin
      JvCSVDatabaseError(FTableName, 'Can''t insert new row. Insert blocked.');
      Exit;
    end;
    FFileDirty := true;
    pInsertRec := AllocMem(sizeof(TJvCSVRow));
    Move(PCSVRow(ActiveBuffer)^, pInsertRec^, sizeof(TJvCSVRow));
    pInsertRec^.fdirty := true;
    FData.Insert(FRecordPos, Pointer(pInsertRec));
    FRecordPos := FData.IndexOf(Pointer(pInsertRec));
    pInsertRec^.Bookmark.data := FRecordPos;
  end
  else
    JvCSVDatabaseError(FTableName, 'Post: Can''t post. Not in not dsEdit or dsInsert mode');
end;

function TJvCSVCustomInMemoryDataSet.IsCursorOpen: boolean;
begin
  // "Cursor" is open if data file is open.   File is open if FDataFile's
  // Mode includes the FileMode in which the file was open.
 {  Result := TFileRec(FDataFile).Mode <> 0; }
  Result := FCursorOpen; // bogus value: Valid field definition
end;

function TJvCSVCustomInMemoryDataSet.GetRecordCount: integer;
begin
  if (FData.Count > 0) then
    Result := FData.Count
  else
    Result := 0;
end;

function TJvCSVCustomInMemoryDataSet.GetRecNo: integer; {RecNo := FRecordPos+1}
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

  Result := PCSVRow(BufPtr)^.Bookmark.data; // Record number.

end;

procedure TJvCSVCustomInMemoryDataSet.SetRecNo(Value: integer);
begin
  if (Value >= 0) and (Value <= FData.Count - 1) then
  begin
    FRecordPos := Value - 1;
    if (RecordCount > 0) then Resync([]);
  end;
end;

procedure TJvCSVCustomInMemoryDataSet.SetTableName(const Value: string);
begin
  CheckInActive;
  FTableName := Value;
  if ExtractFileExt(FTableName) = '' then
    FTableName := FTableName + '.CSV';

  { update internal filename table }
//  FBmkFileName:= ChangeFileExt(FTableName, '.bmk' ); // bookmark file
end;

(*function TJvCSVCustomInMemoryDataSet.GetDataFileSize: Integer;
//var
//  File1:TextFile;
begin
//  AssignFile(File1,FTableName);
//  Result := FileSize(File1);
//  CloseFile(File1);
  result := 8192; // not implemented yet.
end; *)

procedure TJvCSVCustomInMemoryDataSet.EmptyTable;
begin
   // Erase Rows.
  while (FData.Count > 0) do
    FData.DeleteRow(FData.Count - 1);
   // Refresh controls.
  First;
end;

// InternalCompare of two records, of a specific field index.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCSVRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCSVCustomInMemoryDataSet.InternalFieldCompare(Column: PCSVColumn; Left, Right: PCSVRow): integer;
var
  strLeft, strRight: string;
  numLeft, numRight, diff: Double;
begin

  strLeft := GetCSVRowItem(Left, Column^.FPhysical);
  strRight := GetCSVRowItem(Right, Column^.FPhysical);

  (*if (Length(strLeft)=0) or (length(strRight)=0) then begin
      OutputDebugString('Debugging problem in InternalFieldCompare');
      strLeft  :=GetCSVRowItem( Left, Column^.FPhysical );
  end;*)

  if (FCSVCaseInsensitiveComparison) then
  begin
    strLeft := UpperCase(strLeft);
    strRight := UpperCase(strRight);
  end;

   // everything sorts via string sort (default) or numeric sort
   // (the only special case so far!)
  case Column^.FFlag of
    jCSVNumeric:
      begin
        numLeft := StrToFloatDef(strLeft, -99999.9);
        numRight := StrToFloatDef(strRight, -99999.9);
        diff := numLeft - numRight;
        if (diff < -0.02) then
          Result := -1
        else if (diff > 0.02) then
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
// Record comparison between two PCSVRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCSVCustomInMemoryDataSet.InternalCompare(SortColumns: array of PCSVColumn; SortColumnCount: integer; Left,
  Right: PCSVRow): integer;
var
  t: integer;
begin
  Result := 0;
  // null check, raise exception
  if (not Assigned(Left)) or (not Assigned(Right)) then
  begin
    JvCSVDatabaseError(FTableName, 'InternalCompare. Nil value detected.');
  end;
  // now check each field:
  for t := 0 to SortColumnCount - 1 do
  begin
    if not Assigned(SortColumns[t]) then
      JvCSVDatabaseError(FTableName, 'InternalCompare. Nil value detected.'); // raise exception
    Result := InternalFieldCompare(SortColumns[t], Left, Right);
    if (Result <> 0) then
    begin
           // XXX REPEAT result := InternalFieldCompare( SortColumns[t],Left,Right);
      Exit; // found greater or less than condition
    end;
  end;
  // now we have compared all fields, and if we get here, they were all
  // equal, and result is already set to 0.
end;

procedure TJvCSVCustomInMemoryDataSet.Sort(SortFields: string; Ascending: boolean);
var
  Index: array of Pointer;
  swap: Pointer;
  SortFieldNames: array of string;
  SortColumns: array of PCSVColumn;
  SortColumnCount: integer;
  comparison, t, u, l: integer;
begin
  // Create an indexed list which can be sorted more easily than
  // doing an item swap:
  l := FData.Count;
  SetLength(Index, l);
  for t := 0 to l - 1 do
  begin
    Index[t] := FData.Items[t]; // Initial values.
  end;

  SetLength(SortFieldNames, FCSVColumns.Count);
  SortColumnCount := StrSplit(SortFields, ',', {Chr(0)=No Quoting} Chr(0), SortFieldNames, FCSVColumns.Count);
  SetLength(SortColumns, SortColumnCount);
  if (SortFields = '') or (SortColumnCount = 0) then
    JvCSVDatabaseError(FTableName, 'Sort failed. You must give a comma separated list of field names.');

  // Now check if the fields exist, and find the pointers to the fields
  for t := 0 to SortColumnCount - 1 do
  begin
    if (SortFieldNames[t] = '') then
      JvCSVDatabaseError(FTableName, 'Sort failed. Unable to parse field names. ');
    SortColumns[t] := FCSVColumns.FindByName(SortFieldNames[t]);
    if not Assigned(SortColumns[t]) then
      JvCSVDatabaseError(FTableName, 'Sort failed. Invalid field name in list: ' + SortFieldNames[t]);
  end;

  //  bubble sort, compare in the middle,
  //  yes I'm feeling lazy today, yes I know a qsort would be better. - WP
  for t := 0 to l - 2 do
  begin
    for u := t + 1 to l - 1 do
    begin
        // Record comparison between two PCSVRows:
      comparison := InternalCompare(SortColumns,
        SortColumnCount,
        PCSVRow(Index[t]),
        PCSVRow(Index[u])
        );
      if not Ascending then
        comparison := comparison * -1; // flip sign of comparison
      if (comparison > 0) then
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
  FFileDirty := true;
  First; // reposition!
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

procedure TJvCSVCustomInMemoryDataSet.DefChanged(Sender: TObject); //override;
begin
  FStoreDefs := true;
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }

function TJvCSVCustomInMemoryDataSet.FieldDefsStored: boolean;
begin
  Result := FStoreDefs and (FieldDefs.Count > 0);
end;

function TJvCSVCustomInMemoryDataSet.GetCanModify: boolean; //override;
begin
  Result := not FReadOnly; // You can modify if it's NOT read only.
end;

{ CSVColumns dynamic array of pointers }

procedure TJvCSVColumns.AddColumn(Item: PCSVColumn);
begin
  Add(Pointer(Item));
end;

function TJvCSVColumns.FindByName(FieldName: string): PCSVColumn;
var
  t: integer;
begin
  try
    for t := 0 to Count - 1 do
    begin
      Result := PCSVColumn(Get(t));
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

function TJvCSVColumns.FindByFieldNo(FieldNo: integer): PCSVColumn;
var
  t: integer;
begin
  Result := nil;
  try
    for t := 0 to Count - 1 do
    begin
      Result := PCSVColumn(Get(t));
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

procedure TJvCSVColumns.Clear;
var
  t: integer;
begin
  for t := 0 to Count - 1 do
    FreeMem(Self[t]);
  inherited;
end;

{ CSVRows: dynamic array of pointers }

function TJvCSVRows.GetUserTag(Index: integer): integer;
begin
  if (Index < 0) or (Index >= FUserLength) then
    Result := 0
  else
    Result := FUserTag[Index];
end;

procedure TJvCSVRows.SetUserTag(Index, Value: integer);
begin
  if (Index < 0) or (Index >= Count) then Exit;

  if (Index >= FUserLength) then
  begin
    FUserLength := Index + 1;
    SetLength(FUserTag, FUserLength);
    SetLength(FUserData, FUserLength);
  end;
  FUserTag[Index] := Value;
end;

function TJvCSVRows.GetUserData(Index: integer): Pointer;
begin
  if (Index < 0) or (Index >= FUserLength) then
    Result := nil
  else
    Result := FUserData[Index];
end;

procedure TJvCSVRows.SetUserData(Index: integer; Value: Pointer);
begin
  if (Index < 0) or (Index >= Count) then Exit;
  if (Index >= FUserLength) then
  begin
    FUserLength := Index + 1;
    SetLength(FUserTag, FUserLength);
    SetLength(FUserData, FUserLength);
  end;
  FUserData[Index] := Value;
end;

procedure TJvCSVRows.AddRow(const Item: PCSVRow);
begin
  Add(Pointer(Item));
end;

procedure TJvCSVRows.InsertRow(const position: integer; const Item: PCSVRow);
begin
  Insert(position, Pointer(Item));
end;

procedure TJvCSVRows.AddRowStr(const Item: string); // convert String->TJvCSVRow
var
  pNewItem: PCSVRow;
begin
  pNewItem := AllocMem(sizeof(TJvCSVRow));
  StringToCSVRow(Item, pNewItem, true, FEnquoteBackslash); // decode a CSV line that can contain escape sequences
  AddRow(pNewItem);
end;

function TJvCSVRows.GetRowPtr(const RowIndex: integer): PCSVRow;
begin
  Result := PCSVRow(Get(RowIndex)); // return pointer to a row item.
end;

function TJvCSVRows.GetRowStr(const RowIndex: integer): string;
var
  ResultStr: string;
begin
  CSVRowToString(GetRowPtr(RowIndex), ResultStr);
  Result := ResultStr;
end;

procedure TJvCSVRows.SetRowStr(const RowIndex: integer; Value: string);
begin
  StringToCSVRow(Value, GetRowPtr(RowIndex), true, FEnquoteBackslash);
end;

procedure TJvCSVRows.DeleteRow(const RowIndex: integer);
var
  p: Pointer;
begin
  if (RowIndex >= 0) and (RowIndex < Count) then
  begin
    p := Self[RowIndex];
    if p <> nil then FreeMem(p);
  end;
  Delete(RowIndex);
end;

procedure TJvCSVRows.SetARowItem(const RowIndex, ColumnIndex: integer; Value: string);
begin
  SetCSVRowItem(GetRowPtr(RowIndex), ColumnIndex, Value);
end;

function TJvCSVRows.GetARowItem(const RowIndex, ColumnIndex: integer): string;
begin
  Result := GetCSVRowItem(GetRowPtr(RowIndex), ColumnIndex);
end;

procedure TJvCSVRows.Clear;
var
  t: integer;
begin
  for t := 0 to Count - 1 do
    FreeMem(Self[t]);
  inherited;
end;

{ Call this one first, then AssignFromStrings on subsequent updates only.}

procedure TJvCSVCustomInMemoryDataSet.OpenWith(Strings: TStrings);
begin
  Active := false;
  AssignFromStrings(Strings); // parse strings
end;

{ Call this one first, then AssignFromStrings on subsequent updates only.}

procedure TJvCSVCustomInMemoryDataSet.AppendWith(Strings: TStrings);
//var
//x:Integer;
begin

  //Active := false;
  AssignFromStrings(Strings); // parse strings
  // Refresh.
//  DataEvent(deDataSetChange, 0);
//  DataEvent(deRecordChange, 0);
  Resync([]);
//  DataEvent(deUpdateState, 0);
  if Active then
    Last;

end;

{ Additional Custom Methods - internal use }

procedure TJvCSVCustomInMemoryDataSet.AssignFromStrings(const Strings: TStrings);
var
  HeaderRowFound: boolean;
  t: integer;
  indexCounter: integer;
begin
// CheckInactive;
// if NOT FFieldsInitialized then
// InternalInitFieldDefs; // must know about field definitions first.
  if Strings = nil then Exit;
  FData.EnquoteBackslash := FEnquoteBackslash;

  indexCounter := 0;
  HeaderRowFound := false;
  for t := 0 to Strings.Count - 1 do
  begin
       // for now ignore any trace or debug data unless
       // someone has defined an event to handle it.
    if Pos('>>', Strings[t]) = 1 then
    begin
      if Assigned(FOnSpecialData) then
        FOnSpecialData(Self, t, Strings[t]);
      continue;
    end;

       // Process the row:
    if (not HeaderRowFound) and FHasHeaderRow then
    begin
      HeaderRowFound := true;
      FHeaderRow := Strings[t];
         //Note: later we will call ProcessCSVHeaderRow(FHeaderRow);
    end
    else
    begin
      ProcessCSVDataRow(Strings[t], indexCounter);
      Inc(indexCounter);
    end;
  end;
  if Active then First;
end;

procedure TJvCSVCustomInMemoryDataSet.AssignToStrings(Strings: TStrings);
var
  t: integer;
  Line: string;
begin
  // copy out the current data set to a TStringList.
  Strings.Clear;

  { Save header row with data rows? }
  if FHasHeaderRow then
    Strings.Add(GetColumnsAsString);

  for t := 0 to FData.Count - 1 do
  begin
    CSVRowToString(FData.GetRowPtr(t), Line);
    Strings.Add(Line);
  end;
end;

function TJvCSVCustomInMemoryDataSet.GetRowAsString(const Index: integer): string;
var
  ResultString: string;
begin
  CSVRowToString(FData.GetRowPtr(Index), ResultString);
  Result := ResultString;
end;

// Get names of all the columns as a comma-separated string:

function TJvCSVCustomInMemoryDataSet.GetColumnsAsString: string;
var
  ResultString: string;
  t: integer;
begin
 // ColCount:
  if FCSVColumns.Count = 0 then
  begin
    ResultString := '';
    Result := ResultString;
    Exit;
  end;
 // Build a list of column names: <item>, <item>,....,<item>
  ResultString := FieldDefs[0].Name;
  for t := 1 to FCSVColumns.Count - 1 do
    ResultString := ResultString + ',' + FieldDefs[t].Name;
  Result := ResultString;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }

procedure TJvCSVCustomInMemoryDataSet.ProcessCSVHeaderRow(const header: string);
var
  CSVFieldRec: TJvCSVRow; // CSV Field record type.
  ptrCSVColumn: PCSVColumn;
{  CSVFieldOption:String;}
  CSVFieldName: string;
{  pCSVFieldDef:PCSVColumn;}
  colnum, t: integer;
begin
  if Length(header) = 0 then
    Exit;

 // Initialize all CSV Column locations to a "not found yet" state:
  for t := 0 to FCSVColumns.Count - 1 do
    PCSVColumn(FCSVColumns.Get(t))^.FPhysical := -1;

  StringToCSVRow(header, @CSVFieldRec, false, false);
  colnum := 0;
  while (CSVRowGetColumnMarker(@CSVFieldRec, colnum) <> COLUMN_ENDMARKER) do
  begin
   // Get a string in the format COLUMNAME:Options
    CSVFieldName := StrEatWhiteSpace(GetCSVRowItem(@CSVFieldRec, colnum));

    if (Length(CSVFieldName) = 0) then
      JvCSVDatabaseError(FTableName, 'Error processing first line of CSV file.');

    ptrCSVColumn := FCSVColumns.FindByName(CSVFieldName);

    if (ptrCSVColumn = nil) then
    begin // raise database exception:
      JvCSVDatabaseError(FTableName, 'ProcessCSVHeaderRow:Field ' + CSVFieldName +
        ' found in file, but not in field definitions.');
      Exit;
    end;

    try
      ptrCSVColumn^.FPhysical := colnum; // numbered from 0.
    except
      JvCSVDatabaseError(FTableName, 'CSV field location error: ' + CSVFieldName);
      break;
    end;
    Inc(colnum);
  end;

  // Check that everything was found and physically given a location
  // in the CSV file:

  for t := 0 to FCSVColumns.Count - 1 do
  begin
    ptrCSVColumn := PCSVColumn(FCSVColumns[t]);
    if ptrCSVColumn^.FPhysical < 0 then
    begin
      JvCSVDatabaseError(FTableName, 'Field ' + ptrCSVColumn^.FFieldDef.Name + ' not found in the data file.');
      Exit;
    end;
  end;
end;

procedure TJvCSVCustomInMemoryDataSet.ProcessCSVDataRow(const datarow: string; Index: integer);
var
  pNewRow: PCSVRow;
begin
  if (Length(datarow) = 0) then
    Exit;
  if (Length(datarow) >= (MAXLINELENGTH - 1)) then
  begin
    raise Exception.Create('CSV String is too long: ' + Copy(datarow, 1, 40) + '...');
  end;
  pNewRow := AllocMem(sizeof(TJvCSVRow));
  StringToCSVRow(datarow, pNewRow, true, FEnquoteBackslash);
  pNewRow^.Index := Index;
  FData.AddRow(pNewRow);
end;

{ This function is handy to save a portion of a CSV table that has
grown too large into a file, and then DeleteRows can be called to remove
that section of the file. }

procedure TJvCSVCustomInMemoryDataSet.ExportRows(FileName: string; FromRow, ToRow: integer);
var
  t: integer;
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

procedure TJvCSVCustomInMemoryDataSet.DeleteRows(FromRow, ToRow: integer);
var
  Count: integer;
begin
  Count := (ToRow - FromRow) + 1;
  while (Count > 0) do
  begin
    if (FromRow < FData.Count) then
      FData.DeleteRow(FromRow) // Everything moves down one every time we do this.
    else
      break;
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
  FFileDirty := true;
end;

procedure TJvCSVCustomInMemoryDataSet.ExportCSVFile(const FileName: string); // save out to a file.
var
  Strings: TStringlist;
begin
  Strings := TStringlist.Create;
  AssignToStrings(Strings);
  Strings.SaveToFile(FileName);
  Strings.Free;
end;

function TJvCSVCustomInMemoryDataSet.GetCSVHeader: string;
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
  Result := StrStrip(FirstLine); // in JvCSVParse.pas
end;

{ PROCEDURES: }

// convert CSV Row buffer to a String

procedure CSVRowToString(RowItem: PCSVRow; var RowString: string);
begin
  RowString := string(RowItem.Text);
end;

// convert String into a CSV Row buffer

procedure StringToCSVRow(const RowString: string; RowItem: PCSVRow; permitEscapeSequences, EnquoteBackslash: boolean);
var
  t, l, Col: integer;
  quoteFlag: boolean;
  skipFlag: boolean;
  charsInColumn: integer;
begin
  Col := 0;
  RowItem^.wordfield[0] := 0; // zero out column marker and dirty bit!
  charsInColumn := 0;
  quoteFlag := false;
  skipFlag := false;
  l := Length(RowString);
  for t := 1 to l do
  begin
    Inc(charsInColumn);
    if quoteFlag then
    begin
          // backslash permitted only if specifically enabled by FEnquoteBackslash:
      if permitEscapeSequences and (not skipFlag) and (EnquoteBackslash) and (RowString[t] = '\') then
      begin
        skipFlag := true;
        continue;
      end;
          // doubled quotes handling:
      if permitEscapeSequences and (not skipFlag) and (RowString[t] = '"') and (t < l) and (RowString[t + 1] = '"') then
      begin
        skipFlag := true;
        continue;
      end;
          // now if either of the above set the skipFlag true previously, we ALWAYS skip the next character here
          // and turn skipFlag back off
      if permitEscapeSequences and skipFlag then
      begin // skip next character, regardless.
        skipFlag := false;
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
          OutputDebugString('CSVDataSource.pas: StringToCSVRow - unescaped quote character in middle of string!');
        end;

      end
      else
      begin
        OutputDebugString('CSVDataSource.pas: StringToCSVRow - quote character found where no escape sequences are permitted!');
      end;
    end;

    if ((RowString[t] = ',') and (not quoteFlag)) then
    begin
      Inc(Col);
       // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
      RowItem.wordfield[Col] := (Word(t) and $7FFF); {note that we're going from 1..length }
      charsInColumn := 0;
    end;
    if (Col >= MAXCOLUMNS) or (t >= MAXLINELENGTH) then
    begin
      raise ERangeError.Create('JvCSVData - Internal Limit of MAXCOLUMNS (' + IntToStr(MAXCOLUMNS) +
        ') reached. CSV Data has too many columns');
      Exit;
    end;
  end; // end of string, new flag:
  Inc(Col);
  if quoteFlag then
  begin
    OutputDebugString('CSVDataSource.pas: StringToCSVRow - Missing end quote character!');
  end;
 // Terminate the column-marker list with a special end-marker:
{RowItem.wordfield[col]   := Word(Length(RowString)+1)AND$7FFF; // length of string
 RowItem.wordfield[col+1] := COLUMN_ENDMARKER; // followed by an end marker}
  RowItem.wordfield[Col] := COLUMN_ENDMARKER; // last one has no end marker
  StrLCopy(RowItem.Text, PChar(RowString), MAXLINELENGTH);
  RowItem.columns := Col; // Check this later!
end;

// Copy a single column from one row buffer to another row buffer:

function CSVRowItemCopy(Source, Dest: PCSVRow; FieldIndex, FieldSize: integer): boolean;
var
  TempStr: string;
begin
  TempStr := GetCSVRowItem(Source, FieldIndex);
   // length limiting feature:
  if (FieldSize > 0) then
    if Length(TempStr) > FieldSize then
      TempStr := Copy(TempStr, 1, FieldSize);
  SetCSVRowItem(Dest, FieldIndex, TempStr);
  Result := true;
end;

// Copy an item into a CSV row buffer:

procedure SetCSVRowItem(pItem: PCSVRow; ColumnIndex: integer; newValue: string);
var
  TempBuf: array[0..MAXLINELENGTH] of char;
  Copy1, Copy2: integer;
  Dif, t, Old: integer;
begin
  Dif := 0;
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
    Exit;
  Copy1 := CSVRowGetColumnMarker(pItem, ColumnIndex);
  if (Copy1 = COLUMN_ENDMARKER) then
    Exit;

  if (Copy1 > MAXLINELENGTH) then
    Exit;
 // copy initial part of the CSV row:
  if (Copy1 > 0) then
  begin
    StrLCopy(TempBuf, pItem.Text, Copy1);
    StrLCat(TempBuf, PChar(newValue), MAXLINELENGTH);
  end
  else
    StrLCopy(TempBuf, PChar(newValue), MAXLINELENGTH);

  Copy2 := CSVRowGetColumnMarker(pItem, ColumnIndex + 1);
  if (Copy2 <> COLUMN_ENDMARKER) then
  begin
   // difference in length:
    Dec(Copy2); // subtract one.
    if (Copy2 < 0) then Exit;
    if (Length(newValue) = Copy2 - Copy1) then
      Dif := 0
    else
      Dif := Length(newValue) - (Copy2 - Copy1);
    StrLCat(TempBuf, pItem^.Text + Copy2, MAXLINELENGTH);
  end;

 // Copy over the old memory buffer:
  StrLCopy(pItem^.Text, TempBuf, MAXLINELENGTH);

 // Now that we've copied a new item of a different length into the place of the old one
 // we have to update the positions of the columns after ColumnIndex:
  if (Dif <> 0) then
    for t := ColumnIndex + 1 to MAXCOLUMNS do
    begin
      Old := CSVRowGetColumnMarker(pItem, t);
      if (Old = COLUMN_ENDMARKER) then Exit;
      CSVRowSetColumnMarker(pItem, t, Old + Dif);
    end;

end;

// Copy an item out of a CSV row buffer:

function GetCSVRowItem(pItem: PCSVRow; ColumnIndex: integer): string;
var
  TempBuf: array[0..MAXLINELENGTH] of char;
  Copy1, Copy2: integer;
begin
  if (ColumnIndex < 0) or (ColumnIndex > MAXCOLUMNS) then
  begin
    Result := '<ERROR>';
    Exit;
  end;

  Copy1 := CSVRowGetColumnMarker(pItem, ColumnIndex);
  Copy2 := CSVRowGetColumnMarker(pItem, ColumnIndex + 1);
  if (Copy1 = COLUMN_ENDMARKER) then
  begin
    Result := '';
    Exit;
  end;
  if (Copy2 = COLUMN_ENDMARKER) then // copy the rest of the line
    Copy2 := MAXLINELENGTH - Copy1 // All the characters left in the buffer
  else
    Dec(Copy2);

  if (Copy1 > MAXLINELENGTH) or (Copy2 > MAXLINELENGTH) then
  begin
    Result := '';
    Exit;
  end;

 // Copy out just one column from the string:
  StrLCopy(TempBuf, pItem.Text + Copy1, Copy2 - Copy1);
  PcharEatWs(@TempBuf[0]);
  Result := string(TempBuf);
end;

{new}

procedure CSVRowSetDirtyBit(row: PCSVRow; ColumnIndex: integer);
begin
  if row = nil then Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then Exit;
  row^.fdirty := true; // triggers search for 'dirty bit' in columns
  row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] or $8000);
end;

procedure CSVRowClearDirtyBit(row: PCSVRow; ColumnIndex: integer);
begin
  if row = nil then Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then Exit;
  row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] and $7FFF);
end;

function CSVRowGetDirtyBit(row: PCSVRow; ColumnIndex: integer): boolean;
begin
  Result := false;
  if row = nil then Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then Exit;
  if row^.wordfield[ColumnIndex] = COLUMN_ENDMARKER then Exit;
  Result := (row^.wordfield[ColumnIndex] and $8000) <> 0;
end;

procedure CSVRowSetColumnMarker(row: PCSVRow; ColumnIndex: integer; ColumnMarker: integer);
var
  Old: Word;
begin
  if row = nil then Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then Exit;
  if (ColumnMarker < 0) then Exit;

  if ColumnMarker = COLUMN_ENDMARKER then
    row^.wordfield[ColumnIndex] := COLUMN_ENDMARKER
  else
  begin
    Old := row^.wordfield[ColumnIndex];
    if Old = COLUMN_ENDMARKER then
      row^.wordfield[ColumnIndex] := ColumnMarker and $7FFF // auto-clear Dirty bit
    else
      row^.wordfield[ColumnIndex] := (Old and $8000) // Keep Old Dirty Bit
        or (Word(ColumnMarker) and $7FFF); // new value.
  end;
end;

function CSVRowGetColumnMarker(row: PCSVRow; ColumnIndex: integer): integer;
var
  w: Word;
begin
  Result := -1;
  if row = nil then Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= MAXCOLUMNS) then Exit;
  w := row^.wordfield[ColumnIndex];
  if w = COLUMN_ENDMARKER then
    Result := COLUMN_ENDMARKER
  else
    Result := integer(w and $7FFF);
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
 // we obtain this result.
 // The incoming value in hex will roll over in mid-Janary 2038, and
 // hopefully by then this code won't be in use any more! :-)
 //
 // Note: TDateTime is really a Double floating-point and zero is considered
 // an Error code.
 //------------------------------------------------------------------------------

function TimeTHexToDateTime(HexStr: string; TimeZoneCorrection: integer): TDateTime;
var
  SecondsSince1970: Double;
  Base: TDateTime;
{ DateTimeAsStr:String; //debug Code.}
begin
  Result := 0.0;
  SecondsSince1970 := StrToIntDef('$' + HexStr, 0) + TimeZoneCorrection;
  if (SecondsSince1970 <= 0.0) then Exit;
  Base := EncodeDate(1970, 1, 1);
  Base := Base + (SecondsSince1970 / 86400.0);
{ DateTimeAsStr := FormatDateTime('yyyy/mm/dd hh:nn:ss',Base);}
  Inc(CallCount);
  Result := Base;
end;

function TimeTAsciiToDateTime(AsciiDateStr: string): TDateTime;
const
  Separators = '// ::'; // separators in yyyy/mm/dd hh:mm:ss
  Separators2 = '-- --'; // separators in yyyy/mm/dd hh:mm:ss
var
  Values: array[1..6] of integer; //year,month,day,hour,minute,second in that order.
  ExpectLengths: array[1..6] of integer;
  MinValue: array[1..6] of integer;
  MaxValue: array[1..6] of integer;
  ch: char;
  t, u, len, Index: integer;
//  Done: boolean;
begin
  Result := 0.0; // default result.
  len := Length(AsciiDateStr);
//  Done := false;

 // validate ranges:
  MinValue[1] := 1990;
  MaxValue[1] := 2999; // This code suffers from the Y3K bug. If you care, get a life.

  MinValue[2] := 1; // Hope they never add more months to the year, eh?
  MaxValue[2] := 12;

  MinValue[3] := 1; // We don't bother about checking if the month has 31 days.
  MaxValue[3] := 31;

  MinValue[4] := 0; // We use military time 00 is midnight, 23 is 11 pm.
  MaxValue[4] := 23;

  MinValue[5] := 0; // Minute value is 00 to 59
  MaxValue[5] := 59;

  MinValue[6] := 0; // Second value is 00 to 59
  MaxValue[5] := 59;

 // expect values with length of 4,2,2,2,2,2 ...
  ExpectLengths[1] := 4;
  Values[1] := 0;
  for t := 2 to 6 do
  begin
    ExpectLengths[t] := 2;
    Values[t] := 0;
  end;

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiDateStr are we looking at?
  for t := 1 to 6 do
  begin
    if (t >= 3) and (Index >= len) then
      break; // as long as we at least got the date, we can continue.
    for u := 1 to ExpectLengths[t] do
    begin
      if (Index > len) then
      begin
//        Done := true; // reached end!
        break;
      end;
      ch := AsciiDateStr[Index];
      if (ch < '0') or (ch > '9') then
      begin
        OutputDebugString(PChar('JvCSVData:illegal character in datetime string: ' + ch));
        Exit; // failed:invalid character.
      end;
      Values[t] := (Values[t] * 10) + (Ord(ch) - Ord('0'));
      Inc(Index);

      if (Index > len) then
      begin
//        Done := true; // reached end!
        break;
      end;
    end;

   // if we haven't reached the end of the string, then
   // check for a valid separator character:
    if (Index < len) then
      if (AsciiDateStr[Index] <> Separators[t])
        and (AsciiDateStr[Index] <> Separators2[t]) then
        Exit;

   // validate ranges:
    if (Values[t] < MinValue[t]) or (Values[t] > MaxValue[t]) then
      Exit; // a value is out of range.
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

function DateTimeToTimeTHex(aDateTime: TDateTime; TimeZoneCorrection: integer): string;
var
  Base: TDateTime;
{  DateTimeAsStr    : String; //debug Code. }
  SecondsSince1970: integer;
begin
  try
    Base := EncodeDate(1970, 1, 1);
    SecondsSince1970 := trunc((aDateTime - Base) * 86400.0);
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

function JvFilePathSplit(FileName: string; var path, filenameonly: string): boolean;
var
  len, t: integer;
begin
  len := Length(FileName);
  Result := false;
  path := '';
  filenameonly := '';
  for t := len downto 1 do
  begin
    if FileName[t] = '\' then
    begin
      path := Copy(FileName, 1, t);
      filenameonly := Copy(FileName, t + 1, len);
      if (Length(filenameonly) > 0) and (Length(path) > 0) and DirectoryExists(path) then
        Result := true;
      Exit;
    end;
  end;
end;

{ Routine to keep backup copies of old data files around }

function JvCSVBackupPreviousFiles(FileName: string; MaxFiles: integer): boolean;
var
  BackupFolder, filenameonly, BackupFilename, RemoveFile: string;
  t: integer;
  found: boolean;

  function MakeFilename(Index: integer): string;
  begin
    Result := BackupFolder + filenameonly + '.' + IntToStr(Index) + '.bak';
  end;
begin
  Result := false;

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

  found := false;
  for t := 0 to MaxFiles - 1 do
  begin
    BackupFilename := MakeFilename(t);
    if not FileExists(BackupFilename) then
    begin
      RemoveFile := MakeFilename((t + 1) mod MaxFiles);
      found := true;
      break;
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

  Windows.CopyFile(PChar(FileName), PChar(BackupFilename), false);
  Result := true;
end;

initialization
  CallCount := 0;
end.

