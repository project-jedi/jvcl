{-----------------------------------------------------------------------------
JvCsvDataSet

The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is by Warren Postma.

Contributor(s):  Warren Postma (warrenpstma att hotmail dott com)

2003-07-29 Warren Postma - New features (Sorting, Indexing, UserData)

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org
ii

Description:
  TJvCsvDataSet in-memory-dataset component usable by any
    VCL Data Aware Controls.
              TJvCsvDataSet appears in the 'Jv Data Access' tab of the
    Component Palette.

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
  Sept 3, 2009 - Delphi 2009 Version
               - New Streams based file-I/O system
               - Flexible record size (instead of fixed record size)
               - Can now handle carriage-return and linefeed characters
                 inside quotes. This allows multi-line TMemo fields to work
                properly when attached to JvCsvDataset.

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
//  For most users it is important to first set up the important Property
//  called CsvFieldDef which describes the expected fields and their types
//  since the CSV file itself contains insufficient information to guess the
//  field types.  Also, if you want to create new files, starting from a blank
//  file, you absolutely must set up CsvFieldDef first!
//  If you insist on using a fly-by-the-seat-of-your-pants approach (not setting
//  CsvFieldDef), the component will just read the first line of whatever CSV file
//  you give it, and then assume string field type for all columns, which is
//  fine for some people since the  CSV file really is just strings. This can
//  be handy if you want to write a tool that can open ANY csv file no matter
//  what column names or how many columns it contains.
//
// Example CsvFieldDef string:
//   ABC:$80,DEFG:$140,HIJKLMN:%,OPQRST:@
//
//   $ = string (ftString) - also used if no character is given.
//   % = whole Integer value (ftInteger)
//   & = floating point value (ftFloat)
//   @ = Ascii datetime value (ftDateTime) as YYYY/MM/DD HH:MM:SS (Component Specific)
//   # = Hex-Ascii Timestamp (A93F38C9) seconds since Jan 1, 1970 GMT (Component Specific)
//   ^ = Hex-Ascii Timestamp (A93F38CP) corrected to local timezone (Component Specific)
//   ! = Boolean Field (0 in csv file=false, not 0 = true, blank = NULL)
//   ~ = UTF8 CSV Field (ftWideString) [currently implemented on Delphi 2009 only]
//
// NOTE: YOU SHOULD PROBABLY JUST USE THE BUILT-IN CsvFieldDef PROPERTY EDITOR
// (CLICK the '...' button in the properties inspector beside CsvFieldDef)
// INSTEAD OF MEMORIZING ALL THIS FIELD TYPE STUFF.
//
// Originally written by Warren Postma
// Contact: warren.postma _@_ gmail _D0T_ com
//
// Donated to the Delphi JEDI Project.
// All Copyrights and Ownership donated to the Delphi JEDI Project.
//------------------------------------------------------------------------

unit JvCsvData;
{$M+}   // REQUIRED in D2007.

{$I jvcl.inc}

{$R-} // YOU CANNOT ENABLE RANGE CHECKING IN THIS UNIT! WE DO MANY DYNAMIC-ALLOCATED-MEMORY TECHNIQUES AND
      // GROWABLE-DATA-STRUCTURES AT RUNTIME THAT ARE INCOMPATIBLE WITH DELPHI'S RANGE CHECKING TECHNIQUES.
      // YOU ALSO PROBABLY CAN'T CONVERT THIS UNIT TO DOTNET IN ANY REASONABLE OR EASY WAY. THERE ARE A TON
      // OF UNSAFE LOW LEVEL MEMORY AND POINTER ACTIVITIES IN THIS UNIT!

interface

{$IFDEF COMPILER7_UP}
// The WideString field code will COMPILE on Delphi 7, but the WideString field only WORKS
// on Delphi 2007 and up.
{$DEFINE JVCSV_WIDESTRING}
{$ENDIF COMPILER7_UP}

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  Classes,
  {$IFDEF UNICODE}
  JclBase, // now BOM constants are in JclBase formerly in JclUnicode
  {$ENDIF UNICODE}
  JvJCLUtils,  // some constants needed.
  SysUtils,
  DB;

{$IFNDEF UNICODE}
type
  RawByteString = AnsiString;
{$ENDIF ~UNICODE}

const
  JvCsvRowMagic  = $91AF9823;
  JvCsvRowMagic2 = $91AF9824;
  JvCsvQuote = #34;
  JvCsvLf    = #10;
  JvCsvCR    = #13;

  JvCsv_MaxCalcDataOffset = 256; // # bytes per record for Calculated Field Data.

  JvCsv_MAXCOLUMNS = 120;
  JvCsv_MAXLINELENGTH = 16384;
  JvCsv_DEFAULT_CSV_STR_FIELD = 80; // If CsvFieldDef doesn't have :123 suffix specifying numeric field length, then default to 80 chars.
  JvCsv_MINLINELENGTH = 10;
  JvCsv_COLUMN_ENDMARKER = $FFFF;
  JvCsv_ON_BOF_CRACK = -1;
  JvCsv_ON_EOF_CRACK = -2;


  RowAlreadySaved=0;
  RowNeedsSaving=1;
  { return values from CompareBookmarks: }
  Bookmark_Less = -1; // b1 < b2
  Bookmark_Gtr = 1; // b1 > b2
  Bookmark_Eql = 0; // b1 = b2

  {these values can be changed at runtime, but not at design time,and should only
  be changed before the dataset is opened or any rows are created:}
  JvCsvDefaultTextBufferSize = 2048; // Default memory allocation sizes are the same as before.
  JvCsvDefaultMarginSize = 2;        // Two bytes of margin. Remove at your own risk!


 { fmJVCSV__xxx constants: }
  { File stream mode flags used setting the mode of TJvCsvStream.        }
  { These bits must be in the high-word, because the Least               }
  { Significant 16 bits are reserved for standard file stream mode bits. }
  { Standard system values like fmOpenReadWrite are in SysUtils. }
  fmJVCSV_APPEND_FLAG  = $20000;
  fmJVCSV_REWRITE_FLAG = $10000;
  fmJVCSV_Append          = fmOpenReadWrite or fmJVCSV_APPEND_FLAG;
  fmJVCSV_OpenReadShared  = fmOpenRead      or fmShareDenyWrite;// or fmShareDenyNone;   // yet another friendly mode constant
  fmJVCSV_OpenRewrite     = fmOpenReadWrite or fmJVCSV_REWRITE_FLAG; // yet another friendly mode constant.
  fmJVCSV_Truncate        = fmCreate        or fmJVCSV_REWRITE_FLAG; // yet another friendly mode constant.
  fmJVCSV_Rewrite         = fmCreate        or fmJVCSV_REWRITE_FLAG; // yet another friendly mode constant.

  JvCsvStreamReadChunkSize = 8192; // 8k chunk reads.

type
  EJvCsvDataSetError = class(EDatabaseError);
  // Subclass DB.EDatabaseError so we can work nicely with existing Delphi apps.

  EJvCsvKeyError = class(EDatabaseError); // Key Uniqueness or Key Problem

  {$IFDEF RTL240_UP}
  PJvMemBuffer = PByte;
  TJvRecordBuffer = TRecordBuffer;
  TJvValueBuffer = TValueBuffer;
  TJvBookmark = TBookmark;
  {$ELSE}
  {$IFDEF UNICODE}
  PJvMemBuffer = PByte;
  {$ELSE}
  PJvMemBuffer = PAnsiChar;
  {$ENDIF UNICODE}
  TJvRecordBuffer = Pointer;
  TJvValueBuffer = Pointer;
  TJvBookmark = Pointer;
  {$ENDIF RTL240_UP}

  { Special Event Types }
  TJvCsvOnSpecialData = procedure(Sender: TObject; Index: Integer; NonCsvData: RawByteString) of object;

  TJvCsvOnGetFieldData = procedure(Sender: TObject; UserTag: Integer; UserData: Pointer; FieldName: string;
    var Value: string) of object;
  TJvCsvOnSetFieldData = procedure(Sender: TObject; UserTag: Integer; UserData: Pointer; FieldName: string;
    Value: string) of object;

  { SPECIAL TYPES OF  DATABASE COLUMNS FOR THIS COMPONENT }
  { Columns are numeric, text, or one of two kinds of Specially Encoded date/time formats: }
  TJvCsvColumnFlag = (
    jcsvNull,  // means not a valid type
    jcsvString,
    jcsvNumeric,  // Integer or Float (% or &)
    jcsvAsciiDateTime,
    jcsvGMTDateTime,
    jcsvTZDateTime,
    jcsvAsciiDate,
    jcsvAsciiTime,
    jcsvStringUTF8 // special NEW column type (~) - september 2008
  );

  // SetFilterNum takes one of these as a compareOperator:
  // utility function JvCsvNumCondition uses this too.
  TJvCsvFilterNumCompare = (jfIntEqual, jfIntNotEqual, jfLessThan, jfGreaterThan);
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
  PtrToPtrToCsvRow = ^PCsvRow; // bookmark Data = Double pointer indirection! Fun fun fun!
  PCsvRow = ^TJvCsvRow; // a pointer to a record


  { Memory usage by JvCsvDataSet:

        A row is an allocated BLOCK of memory grabbed by ALLOCMEM and it must be copyable
        as a flat record type, so there can be NO ABSOLUTE POINTER REFERENCES only lookups
        to various points within this current block.

        First we have a JvCsvRow, then at some point after that, a JvCsvRowWordFields record
        is placed, which we must know how to locate by knowing what the effective size of
        the variable length JvCsvRow.Text field is.  After the JvCsvRowWordFields record which is
        also variable length is stored the calculated field data.
  }
  TJvCsvRow = record { this MUST be a record, not a class, and must be a flag Data record type }
    Magic: Cardinal; // This is expected to be a magic value. If it's not, then the CSV data set is corrupt!
    AllocSize: Integer; // Size of this record. You can't copy CSV records between two CSV data sets if their sizes or separators differ!
    IsDirty: Byte; // record is dirty (needs to be written to disk)
    Separator: AnsiChar; // CSV Separator Character
    Columns: Integer;
    Index: Integer; // FData Index (-1 means not in FData)
    Bookmark: TJvCsvBookmark;
    Filtered: Boolean; // row is hidden from view right now.
    RecursionFlag: Boolean; // helps us fix endless recursion bug in GetFieldData callbacks.
    WordFieldsAddr: Integer; // 0 or -1 means WordFieldsAddr not allocated yet! 1..n means how many bytes after first byte of TJvCsvRow is the TJvCsvRowWordFields record.
    TextMaxLen: Integer; // Max size of Text area!
    _Text: array [0..JvCsv_MINLINELENGTH] of AnsiChar; // at least 10 characters of storage. typically MUCH MUCH more (ie 2k typical)
  end;

  TJvCsvRowWordFields = record
    Magic2: Cardinal; // Another magic value.
    WordField: array [0..JvCsv_MAXCOLUMNS + 1] of Word; // Contains a 15 bit offset value in range 0..37267 maximum,
                                                        // the 16th (high bit) reserved as a this-column-dirty-bit!
  end;

  PJvCsvRowWordFields = ^TJvCsvRowWordFields;

  { Row collection }
  TJvCsvRows = class(TList)
  private
    FEnquoteBackslash: Boolean;
    FBackslashCrLf: Boolean; // Are CR/LFs changed to \r and \n?
    FRecordsValid: Boolean;
    // Optional user Data (only allocated if used, how efficient is that, eh.)
    FUserData: array of Pointer;
    FUserTag: array of Integer;
    FUserLength: Integer;
    // new dynamic-allocation-sizing fields:
    FTextBufferSize : Integer; // How big is TJvCsvRow.Text effectively?
    FMarginSize     : Integer; // How much margin space after the calculated fields?

    FSeparator: Char;
    FDecimalSeparator: Char; { NOTE: DEFAULT value for historical backwards compatibilty reasons is the USA default of '.' }
    FFilteredCount: Integer; // number of records that are filtered

    function GetUserTag(Index: Integer): Integer;
    procedure SetUserTag(Index, Value: Integer);
    function GetUserData(Index: Integer): Pointer;
    procedure SetUserData(Index: Integer; Value: Pointer);

    function GetRowAllocSize: Integer;
    function RecordSize: Word;
    procedure InternalInitRecord(Buffer: TJvRecordBuffer);
  protected
     { note these are not intended to be used outside this unit, so they are protected.
       access these throught the CsvDataSet class public or published properties only. }
    property DecimalSeparator : Char   read FDecimalSeparator write FDecimalSeparator default USDecimalSeparator;
    property Separator        : Char   read FSeparator          write FSeparator;
  public
    constructor Create;

    procedure AddRow(Item: PCsvRow);
    function AllocRecordBuffer: PJvMemBuffer;  { was PChar, now in tiburon it is PByte }

    procedure InsertRow(const Position: Integer;  Item: PCsvRow);
    procedure AddRowStr(const Item: string); // convert string->TJvCsvRow
    function GetRowPtr(const RowIndex: Integer): PCsvRow;
    function GetRowAnsiStr(const RowIndex: Integer): string;
    procedure SetRowStr(const RowIndex: Integer; Value: string);
    procedure DeleteRow(const RowIndex: Integer);
    procedure SetARowItem(const RowIndex, ColumnIndex: Integer; Value: string);
    function GetARowItem(const RowIndex, ColumnIndex: Integer): string;
    procedure Clear; override;
    property EnquoteBackslash: Boolean read FEnquoteBackslash write FEnquoteBackslash;
    property BackslashCrLf:Boolean read FBackslashCrLf write FBackslashCrLf; // Are CR/LFs changed to \r and \n?
    property UserTag[Index: Integer]: Integer read GetUserTag write SetUserTag;
    property UserData[Index: Integer]: Pointer read GetUserData write SetUserData;

    property FilteredCount: Integer read FFilteredCount;

    { these properties should ONLY be set before any actual rows have been allocated. }
    property TextBufferSize: Integer read FTextBufferSize write FTextBufferSize; // How big is TJvCsvRow.Text effectively?
    property MarginSize: Integer read FMarginSize write FMarginSize; // How much margin space after the calculated fields? (typically 2 bytes)
  end;

  TArrayOfPCsvColumn = array of PCsvColumn;

  { TJvCustomCsvDataSetFilterFunction: Defines callback function to be passed to CustomFilter routine }
  TJvCustomCsvDataSetFilterFunction = function(RecNo: Integer): Boolean of object;


  //-------------------------------------------------------------------------
  // TJvCsvStream:
  //
  //    Csv File Reader/Writer Class. Contains a JclFileStream.
  //    Encapsulates reading/writing of CSV files.  This version works
  //    only with ASCII files but the plan is to extend it to work with UTF8.
  //
  //  TJvCsvStream contains an internal TFileStream instead of a JCL Stream because it
  //  is higher performance, do NOT replace the TFileStream below with a JCL
  //  stream.
  //-------------------------------------------------------------------------
  TJvCsvStream = class(TObject)
  private
    FStream: TFileStream; // Tried TJclFileStream also but it was too slow! Do NOT use JCL streams here. -wpostma.
    FFilename: string;
    FStreamBuffer: PAnsiChar;
    FStreamIndex: Integer;
    FStreamSize: Integer;
    FLastReadFlag: Boolean;

    procedure _StreamReadBufInit;
  public
    function ReadLine: RawByteString;   { read a string, one per line, wow. Text files. Cool eh?}

    procedure Append;
    procedure Rewrite;

    procedure Write(const s: RawByteString);        {write a string. wow, eh? }
    procedure WriteLine(const s: RawByteString);    {write string followed by Cr+Lf }

    procedure WriteChar(c: AnsiChar);

    procedure WriteCrLf;
    //procedure Write(const s: string);

    function Eof: Boolean; {is at end of file? }

    { MODE is typically a fmJVCSV_xxx constant thatimplies a default set of stream mode bits plus some extended bit flags that are specific to this stream type.}
    constructor Create(const FileName: string; Mode: DWORD = fmJVCSV_OpenReadShared; Rights: Cardinal = 0); reintroduce; virtual;
    destructor Destroy; override;

    function Size: Int64; //override;   // sanity

    { read-only properties at runtime}
    property Filename: string read FFilename;
    property Stream: TFileStream read FStream; { Get at the underlying stream object}
  end;


  //-------------------------------------------------------------------------
  // TJvCustomCsvDataSet: BASE CLASS.
  // Easily Customizeable DataSet descendant our CSV handler and
  // any other variants we create:
  //-------------------------------------------------------------------------
  TJvCustomCsvDataSet = class(TDataSet)
  private
    FOpenFileName: string; // This is the Fully Qualified path and filename expanded from the FTableName property when InternalOpen was last called.
    FValidateHeaderRow: Boolean;
    FExtendedHeaderInfo: Boolean;
    FCreatePaths: Boolean;
    FFormatSettings: TFormatSettings;

    procedure SetSeparator(const Value: Char);
    procedure InternalQuickSort(SortList: PPointerList; L, R: Integer;
      const SortColumns: TArrayOfPCsvColumn; ACount: Integer; const SortAscending: array of Boolean);

    procedure QuickSort(AList: TList; const SortColumns: TArrayOfPCsvColumn; ACount: Integer;
      const SortAscending: array of Boolean);
    procedure AutoCreateDir(const FileName: string);
    function GetEnquoteBackslash: Boolean;
    procedure SetEnquoteBackslash(const Value: Boolean);
    function GetSeparator: Char;
    function GetMarginSize: Integer;
    function GetTextBufferSize: Integer;
    procedure SetMarginSize(const Value: Integer);
    procedure SetTextBufferSize(const Value: Integer);
    function GetBackslashCrLf: Boolean;
    procedure SetBackslashCrLf(const Value: Boolean);
    function GetDecimalSeparator: Char;
    // ----------- THIS IS A DUMMY FUNCTION, DON'T USE IT!:
    function LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;
      SyncCursor: Boolean): Boolean;
    procedure SetDecimalSeparator(const Value: Char);

    function _CsvFloatToStr(Value: Double): string;

    procedure SetRowFiltered(ARow: PCsvRow; AFiltered: Boolean);
  protected
    // (rom) inacceptable names. Probably most of this should be private.
    FTempBuffer: TJvRecordBuffer; // Allocated on first access to field variable data only!
    FInitialWorkingDirectory: string; // Current working dir may change in a delphi app, causing us trouble.
    FStoreDefs: Boolean;
    FTimeZoneCorrection: Integer; // defaults to 0 (none)
    FFileDirty: Boolean; // file needs to be written back to disk?

    FDefaultCsvFieldDefs: Boolean; // True if the CsvFieldDefs come from the file
    FCsvFieldDef: string; // Our own "Csv Field Definition String"
    FCsvKeyDef: string; // CSV Key Definition string. Required if FCsvUniqueKeys is True
    FCsvKeyCount: Integer; // Set by parsing FCsvKeyDef
    FAscending: array of Boolean;

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

    FCursorOpen: Boolean;
    FFilterBuffer: TJvRecordBuffer; // used when we implement filtering (later)
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

    { temporary holding space only, for a TStringList of the file contents }
    //FCsvFileAsStrings: TStringList; // Can't handle embedded cr/lfs in CSV records. So we replace with our own reader.
    FCsvFileTopLine: string; // similar to FHeaderRow, but blank unless we actually loaded from a real CSV file on disk.
    FCsvFileLoaded: Boolean; // Did InternalFileOpen already load the file? (makes it so that Duplicate calls don't re-load the entire file.)
    FCsvStream: TJvCsvStream;
    {$IFDEF UNICODE}
    FUtf8Detected: Boolean;
    {$ENDIF UNICODE}

    FSpecialDataMarker: RawByteString; // Unless defined, OnSpecialData won't ever be called.

    {  event pointers }
    FOnSpecialData: TJvCsvOnSpecialData;         // XXX Deprecated feature - suggest removal. -Wpostma.
    FOnGetFieldData: TJvCsvOnGetFieldData;
      // Helps to allow you to update the contents of your CSV Data from some other object in memory.
    FOnSetFieldData: TJvCsvOnSetFieldData;
      // Helps to keep some other thing in sync with the contents of a changing CSV file.

    FAlwaysEnquoteStrings: Boolean; // Always put Double quotes around strings (for some CSV file reading software this is required.)
    FAlwaysEnquoteFloats: Boolean; // Always put Double quotes around floating point values (useful when DecimalSeparator==CsvSeparator)
    FUseSystemDecimalSeparator: Boolean; // Default is false which always uses US mode.
    FAppendOnly: Boolean; // If true, we don't load the entire content of the CSV from disk, only the last row, and every time we append and write, we only maintain the last row in memory (saves a lot of RAM.)

    procedure SetActive(Value: Boolean); override;

    //  Internal Use Only Protected Methods
    // function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer: TJvRecordBuffer; virtual;
    procedure CsvRowInit(RowPtr: PCsvRow);

    //NEW and very handy dandy!
    function GetFieldValueAsVariant(CsvColumnData: PCsvColumn; Field: TField; RecordIndex: Integer): Variant;

    // New filtering on cursor (GetRecord advances the cursor past
    // any hidden rows using InternalSkipForward).
    function InternalSkipFiltered(DefaultResult: TGetResult; ForwardBackwardMode: Boolean): TGetResult;

    function ReadCsvFileStream: Boolean;
    function WriteCsvFileStream: Boolean;

    // Internal methods used by sorting:
    function InternalFieldCompare(Column: PCsvColumn; Left, Right: PCsvRow): Integer;
    function InternalCompare(const SortColumns: TArrayOfPCsvColumn; SortColumnCount: Integer;
      Left, Right: PCsvRow; const SortAscending: array of Boolean): Integer;

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
    //procedure AppendPlaceHolderCommasToAllRows(Strings: TStrings); // Add placeholders to end of a csv file.
    procedure ProcessCsvHeaderRow;
    procedure ProcessCsvDataRow(const DataRow: string; Index: Integer);
    procedure SetCsvFieldDef(const Value: string);

    { Mandatory VCL TDataSet Overrides - Pure Virtual Methods of Base Class }
    function AllocRecordBuffer: PJvMemBuffer; override;
    procedure FreeRecordBuffer(var Buffer: PJvMemBuffer); override;
    procedure InternalInitRecord(Buffer: PJvMemBuffer); override;
    function GetRecord(Buffer: PJvMemBuffer; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;

    function GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: TJvValueBuffer); override;
    procedure ClearCalcFields(Buffer: PJvMemBuffer); override;

    // Bookmark methods:
    procedure GetBookmarkData(Buffer: PJvMemBuffer; Data: TJvBookmark); override;
    function GetBookmarkFlag(Buffer: PJvMemBuffer): TBookmarkFlag; override;
    procedure InternalGotoBookmark(Bookmark: TJvBookmark); override;
    procedure InternalSetToRecord(Buffer: PJvMemBuffer); override; // on Insertion???
    procedure SetBookmarkFlag(Buffer: PJvMemBuffer; Value: TBookmarkFlag); override;
    procedure SetBookmarkData(Buffer: PJvMemBuffer; Data: TJvBookmark); override;

    // Navigational methods:
    procedure InternalFirst; override;
    procedure InternalLast; override;
    // Editing methods:
    procedure InternalAddRecord(Buffer: TJvRecordBuffer; Append: Boolean); override;
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
    // handles using the default Excel method which is to Double the quotes inside
    // quotes.

    // (rom) inacceptable names
    function _Enquote(const StrVal: string): string; virtual;
    // puts whole string in quotes, escapes embedded separators and quote characters!
    function _Dequote(const StrVal: string): string; virtual; // removes quotes

    property Separator: Char read GetSeparator write SetSeparator default ',';
    property DecimalSeparator: Char read GetDecimalSeparator write SetDecimalSeparator default ' '; // space means system default.

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function BookmarkValid(Bookmark: TBookmark): Boolean; override;
    function GetFieldData(Field: TField; {$IFDEF RTL250_UP}var{$ENDIF} Buffer: TJvValueBuffer): Boolean; override;


    function _AllocateRow: PCsvRow; // Don't try to create your own CsvRow Objects outside JvCsvDataSet by just allocating a TJvCsvDataRow object. Call this instead.

    // Autoincrement feature: Get next available auto-incremented value for numbered/indexed autoincrementing fields.
    function GetAutoincrement(const FieldName: string): Integer;

    // NEW: COPY FROM ANOTHER TDATASET (TTable, TADOTable, TQuery, or whatever)
    function CopyFromDataset(DataSet: TDataSet): Integer;

    // SELECT * FROM TABLE WHERE <fieldname> LIKE <pattern>:
    procedure SetFilter(const FieldName: string; Pattern: string); // Make Rows Visible Only if they match filterString
    procedure SetFilterNum(const FieldName: string; CompareOperator: TJvCsvFilterNumCompare; NumValue: Double);

    // SELECT * FROM TABLE WHERE <fieldname> IS <NULL|NOT NULL>:
    procedure SetFilterOnNull(const FieldName: string; NullFlag: Boolean);


    procedure ClearFilter; // Clear all previous SetFilters, shows All Rows. Refresh screen.
    procedure ClearPreviousFilter; // Clear Previous Filtering. DOES NOT REFRESH SCREEN.


    procedure CustomFilter(FilterCallback: TJvCustomCsvDataSetFilterFunction); {NEW:APRIL 2004-WP}

    function Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions): Boolean; override;
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
    procedure AssignFromStrings(Strings: TStrings); virtual; // update string Data directly.
    procedure AssignToStrings(Strings: TStrings); virtual;

    procedure DeleteRows(FromRow, ToRow: Integer); // NEW: Quickly zap a bunch of rows:
    procedure ExportRows(const FileName: string; FromRow, ToRow: Integer); // NEW: Quickly save a bunch of rows:

    procedure ExportCsvFile(const FileName: string); virtual;
      // save out to a file. does NOT keep backups! If file exists, it will be
        // overwritten, and NO backups are made!

    procedure Flush; virtual; // Save CSV file to disk if file has changed and SavesChanges is True.
    // Note: FLUSH will make backup copies if FAutoBackupCount>0!!!

    function GetAsString(const Row, Column: Integer): string; virtual;

    { Row Access as string }
    function GetRowAsString(const Index: Integer): string; virtual;
    function GetRowAsAnsiString(const Index: Integer): AnsiString; virtual;

    function CurrentRowAsString: string; virtual; // Return any row by index, special: -1 means last row NEW.

    // Return any row by index, special: -1 means last row
    function GetColumnsAsString: string; virtual;
    { Row Append one string }
    procedure AppendRowString(const RowAsString: string);    // Along with GetRowAsString, easy way to copy a dataset to another dataset!
    procedure CreateFields; override;

    function IsKeyUnique: Boolean; // Checks current row's key uniqueness. Note that FCsvKeyDef MUST be set!
    procedure SaveToFile(const FileName: string);
    procedure LoadFromFile(const FileName: string);

    procedure DeleteCsvColumn(const AFieldName: string); // must be done when not Active! [NEW 2007!]
    function Lookup(const KeyFields: string; const KeyValues: Variant; const ResultFields: string): Variant; override;
     {These are made protected so that you can write another derived component which has access to various protected fields }
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
    property Changed: Boolean read FFileDirty write FFileDirty default False;
    // property DataFileSize: Integer read GetDataFileSize;

    // if HasHeaderRow is True, validate that it conforms to CvsFieldDef
    property ValidateHeaderRow: Boolean read FValidateHeaderRow write FValidateHeaderRow default True;
    property ExtendedHeaderInfo: Boolean read FExtendedHeaderInfo write FExtendedHeaderInfo default False;

    property CaseInsensitive: Boolean read FCsvCaseInsensitiveComparison write FCsvCaseInsensitiveComparison default False;

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
    { If False (default) we use the more normal CSV rendering of quotes, which is to Double them in
      the csv file, but if this property is True, we use backslash-quote to render quotes in the file,
      which has the side-effect of also requiring all backslashes to themself be escaped by a backslash.
      So filenames would have to be in the form "c:\\directory\\names\\like\\c\\programmers\\do\\it".
      Not recommended behaviour, except when absolutely necessary! }
    property EnquoteBackslash: Boolean read GetEnquoteBackslash write SetEnquoteBackslash default False;
    property BackslashCrLf:Boolean read GetBackslashCrLf write SetBackslashCrLf; // Are CR/LFs changed to \r and \n?


    {new}
    property CreatePaths: Boolean read FCreatePaths write FCreatePaths default True; // When saving, create subdirectories/paths if it doesn't exist?

    property SpecialDataMarker: RawByteString read FSpecialDataMarker write FSpecialDataMarker; // Unless defined, OnSpecialData won't ever be called. If you have NON-CSV data being parsed/ignored when a certain marker/prefix is found, set it up here.

     { Additional Events }
    property OnSpecialData: TJvCsvOnSpecialData read FOnSpecialData write FOnSpecialData;
    property OnGetFieldData: TJvCsvOnGetFieldData read FOnGetFieldData write FOnGetFieldData;
    property OnSetFieldData: TJvCsvOnSetFieldData read FOnSetFieldData write FOnSetFieldData;
  public
    { these MUST be available at runtime even when the object is of the Custom base class type
      This enables interoperability at design time between non-visual helper components
      and user-derived CsvDataSet descendants }
     // CSV Table definition properties:
    property CsvFieldDef: string read FCsvFieldDef write SetCsvFieldDef; // Our own "Csv Field Definition String"
    property CsvKeyDef: string read FCsvKeyDef write FCsvKeyDef; // Primary key definition.
    property CsvUniqueKeys: Boolean read FCsvUniqueKeys write FCsvUniqueKeys default False; // Rows must be unique on the primary key.
    // not currently valuable, but maybe soon:
    //property CsvColumns: TJvCsvColumns read FCsvColumns;

    property OpenFileName: string read FOpenFileName; // Set in InternalOpen, used elsewhere.
    property FieldDefs stored FieldDefsStored;
    property TableName: string read FTableName; // Another name, albeit read only, for the FileName property!
    property HasHeaderRow: Boolean read FHasHeaderRow write FHasHeaderRow default True;
    property HeaderRow: string read FHeaderRow; // first row of CSV file.
    property SavesChanges: Boolean read FSavesChanges write FSavesChanges default True;

    property AlwaysEnquoteStrings: Boolean read FAlwaysEnquoteStrings write FAlwaysEnquoteStrings default False; // Always put Double quotes around strings (for some CSV file reading software this is required.)
    property AlwaysEnquoteFloats: Boolean read FAlwaysEnquoteFloats write FAlwaysEnquoteFloats default False; // Always put Double quotes around floating point values (useful when DecimalSeparator==CsvSeparator)
    property UseSystemDecimalSeparator: Boolean read FUseSystemDecimalSeparator write FUseSystemDecimalSeparator default False; // Default is false which always uses US mode.  Must be false by default because of existing code assuming this behaviour.

    property AppendOnly: Boolean read FAppendOnly write FAppendOnly default False; // If true, we don't load the entire content of the CSV from disk, only the last row, and every time we append and write, we only maintain the last row in memory (saves a lot of RAM.)

    property TextBufferSize: Integer read GetTextBufferSize write SetTextBufferSize default JvCsvDefaultTextBufferSize; // How big is TJvCsvRow.Text effectively?
    property MarginSize: Integer read GetMarginSize write SetMarginSize default JvCsvDefaultMarginSize; // How much margin space after the calculated fields? (typically 2 bytes)
  end;

  // TJvCsvDataSet is just a TJvCustomCsvDataSet with all properties and events exposed:
  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64{$IFDEF RTL360_UP} or pidWin64x{$ENDIF RTL360_UP})]
  {$ENDIF RTL230_UP}
  TJvCsvDataSet = class(TJvCustomCsvDataSet)
  public
    property TableName;
    property UserData;
    property UserTag;
    property DecimalSeparator;
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
    property AlwaysEnquoteStrings;
    property AlwaysEnquoteFloats;
    property UseSystemDecimalSeparator;
    property AppendOnly; 
  end;

{ CSV string Processing Functions }
procedure JvCsvRowToAnsiString(RowItem: PCsvRow; var RowString: AnsiString);
procedure JvCsvRowToString(RowItem: PCsvRow; var RowString: string);

{ modified! }
procedure JvStringToCsvRow(const RowString: string; Separator: Char;
  RowItem: PCsvRow; PermitEscapeSequences, EnquoteBackslash: Boolean);

function CsvRowItemCopy(Source, Dest: PCsvRow; FieldIndex, FieldSize: Integer): Boolean;
procedure SetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: string);
procedure SetCsvRowItemData(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: RawByteString);
function GetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer): string;
function GetCsvRowItemData(PItem: PCsvRow; ColumnIndex: Integer): RawByteString;
procedure CsvRowSetDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
procedure CsvRowClearDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
function CsvRowGetDirtyBit(Row: PCsvRow; ColumnIndex: Integer): Boolean;
procedure CsvRowSetColumnMarker(Row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
function CsvRowGetColumnMarker(Row: PCsvRow; ColumnIndex: Integer): Integer;

{ Date/Time string decoding functions }

// Decides a TIME_T (A common standard-C-library way of encoding date time values
// as a number of seconds since 12:00 AM Jan 1, 1970 UTC) which is stored in Hex
// in the CSV file.
function JvTimeTHexToDateTime(const HexStr: string; TimeZoneCorrection: Integer): TDateTime;

function JvIsoDateTimeStrToDateTime(const AsciiDateTimeStr: string): TDateTime; // [formerly TimeTAsciiToDateTime]
function JvIsoDateStrToDate(const AsciiDateStr: string): TDateTime; // new.
function JvIsoTimeStrToTime(const AsciiTimeStr: string): TDateTime; // new. If INVALID value: returns -1.0

{ Date/Time string encoding functions }
function JvDateTimeIsoStr(ADateTime: TDateTime): string; // renamed! formerly DateTimeToTimeToIsoAscii

// new: JvDateIsoStr [support function for new Date ASCII CSV column type]
function JvDateIsoStr(ADateTime: TDateTime): string;

// new: JvTimeIsoStr [support function for new Date ASCII CSV column type]
function JvTimeIsoStr(ADateTime: TDateTime): string;


function JvDateTimeToTimeTHex(ADateTime: TDateTime; TimeZoneCorrection: Integer): string;

{ Routine to keep backup copies of old Data files around }
function JvCsvBackupPreviousFiles(const FileName: string; MaxFiles: Integer): Boolean;

//JvCsvWildcardMatch:
// Recursive wildcard (%=AnyString, ?=SingleChar) matching function with
// Boolean sub expressions (|=or, &=and).
function JvCsvWildcardMatch(Data, Pattern: string): Boolean;

// numeric filter helper function:
function JvCsvNumCondition(FieldValue: Double; CompareOperator: TJvCsvFilterNumCompare; NumValue: Double): Boolean;


{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

//var
//  DebugPJvCsvRowWordFields : PJvCsvRowWordFields; // XXX debug code
  
implementation

uses
  Variants, Controls, Forms,
  JclSysUtils, JclAnsiStrings,
  JvCsvParse, JvConsts, JvResources;

const
  // These characters cannot be used for separator for various reasons:
  // Either they are used as field type specifiers, break lines or are used to
  // delimit field content
  cInvalidSeparators = [#0, Backspace, Lf, #12, Cr, #39, '"', '\',
    '$', '%', '&', '@', '#', '^', '!', '-', '/', '*' ];

var
// These arrays are needed by the string-input validation routines
// that validate the ascii input for ISO date/time formats:

                                                     //YYYY  MM  DD  HH  NN  SS
  AsciiTime_MinValue: array [1..6] of Integer =      (1900,  1,  1,  0,  0,  0);
  AsciiTime_MaxValue: array [1..6] of Integer =      (3999, 12, 31, 23, 59, 59);
  AsciiTime_ExpectLengths: array [1..6] of Integer = (   4,  2,  2,  2,  2,  2);


function StrLLen(P: PAnsiChar; MaxLen: Integer): Integer;
begin
  for Result := 0 to MaxLen - 1 do
    if P[Result] = #0 then
      Exit;
  Result := MaxLen;
end;

procedure JvCsvDatabaseError(const TableName, Msg: string);
begin
  raise EJvCsvDataSetError.CreateResFmt(@RsECsvErrFormat, [TableName, Msg]);
end;

procedure JvCsvDatabaseError2(const TableName, Msg: string; Code: Integer);
begin
  raise EJvCsvDataSetError.CreateResFmt(@RsECsvErrFormat2, [TableName, Msg, Code]);
end;

function JvCsvStrToFloatDef(const StrValue: string; DefValue: Double; ASeparator: Char): Double;
begin
  { does not raise exceptions}
  Result := JvSafeStrToFloatDef(StrValue, DefValue, ASeparator); // JvJCLUtils
end;

function JvCsvStrToFloat(const StrValue: string; ASeparator: Char): Double;
begin
  { raises EConvertError exception }
  Result := JvSafeStrToFloat(StrValue, ASeparator); // JvJCLUtils
end;

{ Trim TRAILING CrLf but not leading, or middle, or spaces }
function JvTrimAnsiStringCrLf(const S: RawByteString): RawByteString;
var
  I: Integer;
begin
  for I := Length(S) downto 1 do
  begin
    if (S[I] <> #10) and (S[I] <> #13) then
    begin
      Result := Copy(S, 1, I);
      Exit;
    end;
  end;
  Result := S;
end;

function JvTrimStringCrLf(const S: string): string;
var
  I: Integer;
begin
  for I := Length(S) downto 1 do
  begin
    if (S[I] <> #10) and (S[I] <> #13) then
    begin
      Result := Copy(S, 1, I);
      Exit;
    end;
  end;
  Result := S;
end;

{Word fields are fixed size, but variable location, so we need to FIND them.}
function GetWordFields(RowItem: PCsvRow): PJvCsvRowWordFields;
var
  P: PAnsiChar;
  Magic: Cardinal;
begin
  Assert(Assigned(RowItem));

  if RowItem^.Magic <> JvCsvRowMagic then
    raise EJvCsvDataSetError.Create('Internal error. Memory corruption suspected in CsvRow memory area');
  Assert(RowItem^.WordFieldsAddr > 0);
  P := Pointer(RowItem);
  Inc(P, RowItem.WordFieldsAddr);

  Result := PJvCsvRowWordFields(P);
  Magic := Result^.Magic2;
  if Magic <> JvCsvRowMagic2 then
    raise EJvCsvDataSetError.Create('Memory Corruption Suspected in WordFields area of CsvRow memory'); // memory corruption check!
end;

{Calculated data area is now after the word fields, and we need to locate the area } 
function GetCalcDataOffset(RowItem:PCsvRow):Integer;
begin
  Assert(Assigned(RowItem));
  Assert(RowItem^.Magic = JvCsvRowMagic);
  Assert(RowItem^.AllocSize>0);
  Assert(RowItem^.WordFieldsAddr > 0);

  Result := RowItem^.WordFieldsAddr + SizeOf(TJvCsvRowWordFields);

  Assert(Result < RowItem^.AllocSize);
end;

//-------------------------------------------------------------------------
// TJvCsvStream METHODS
//-------------------------------------------------------------------------
function GetFileSizeEx(h: THandle; FileSize: PULargeInteger): BOOL; stdcall;
  external Kernel32;

procedure TJvCsvStream.Append; 
begin
  Stream.Seek(0, soFromEnd);
end;

constructor TJvCsvStream.Create(const FileName: string; Mode: DWORD; Rights: Cardinal);
var
  IsAppend: Boolean;
  IsRewrite: Boolean;
begin
  inherited Create;
  FFilename := FileName;

  FLastReadFlag := False;
  IsAppend := (Mode and fmJVCSV_APPEND_FLAG) <> 0;
  IsRewrite := (Mode and fmJVCSV_REWRITE_FLAG) <> 0;

  FStream := TFileStream.Create(Filename, {16 lower bits only}Word(Mode), Rights);

  //Stream := FStream; { this makes everything in the base class actually work if we inherited from Easy Stream}

  if IsAppend then
    Self.Append  // seek to the end.
  else
    Stream.Position := 0;

  if IsRewrite then
    Rewrite;

  _StreamReadBufInit;
end;

destructor TJvCsvStream.Destroy;
begin
  if Assigned(FStream) then
    FStream.Position := 0; // avoid nukage
  FreeAndNil(FStream);
  FreeMem(FStreamBuffer); // Buffered reads for speed.
  inherited Destroy;
end;

function TJvCsvStream.Eof: Boolean;
begin
  if not Assigned(FStream) then
    Result := False
    //Result := True
  else
    Result := FLastReadFlag and (FStreamIndex >= FStreamSize);
    //Result := FStream.Position >= FStream.Size;
end;

{ TJvCsvStream.ReadLine:
  This reads a line of text, normally terminated by carriage return and/or linefeed
  but it is a bit special, and adapted for CSV usage because CR/LF characters
  inside quotes are read as a single line.

  This is a VERY PERFORMANCE CRITICAL function. We loop tightly inside here.
  So there should be as few procedure-calls inside the repeat loop as possible.

  This code is entirely new in JVCL 3.36 or later, added in September 2008.
}
function TJvCsvStream.ReadLine: RawByteString;
var
  Buf: array of AnsiChar;
  n: Integer;
  QuoteFlag: Boolean;
  LStreamBuffer: PAnsiChar;
  LStreamSize: Integer;
  LStreamIndex: Integer;

  procedure FillStreamBuffer;
  begin
    FStreamSize := Stream.Read(LStreamBuffer[0], JvCsvStreamReadChunkSize);
    LStreamSize := FStreamSize;
    if LStreamSize = 0 then
    begin
      if FStream.Position >= FStream.Size then
        FLastReadFlag := True
      else
        raise EJvCsvDataSetError.CreateResFmt(@RsECannotReadCsvFile, [FFilename]);
    end
    else
    if LStreamSize < JvCsvStreamReadChunkSize then
      FLastReadFlag := True;
    FStreamIndex := 0;
    LStreamIndex := 0;
  end;

begin
  { Ignore linefeeds, read until carriage return, strip carriage return, and return it }
  SetLength(Buf, 150);

  n := 0;
  QuoteFlag := False;

  LStreamBuffer := FStreamBuffer;
  LStreamSize := FStreamSize;
  LStreamIndex := FStreamIndex;
  while True do
  begin
    if n >= Length(Buf) then
      SetLength(Buf, n + 100);

    if LStreamIndex >= LStreamSize then
      FillStreamBuffer;

    if LStreamIndex >= LStreamSize then
      Break;

    Buf[n] := LStreamBuffer[LStreamIndex];
    Inc(LStreamIndex);

    case Buf[n] of
      JvCsvQuote: {34} // quote
        QuoteFlag := not QuoteFlag;
      JvCsvLf: {10} // linefeed
        if not QuoteFlag then
          Break;
      JvCsvCR: {13} // carriage return
        begin
          if not QuoteFlag then
          begin
            { If it is a CRLF we must skip the LF. Otherwise the next call to ReadLine
              would return an empty line. }
            if LStreamIndex >= LStreamSize then
              FillStreamBuffer;
            if LStreamBuffer[LStreamIndex] = JvCsvLf then
              Inc(LStreamIndex);

            Break;
          end;
        end
    end;
    Inc(n);
  end;
  FStreamIndex := LStreamIndex;

  SetString(Result, PAnsiChar(@Buf[0]), n);
end;

procedure TJvCsvStream.Rewrite;
begin
  if Assigned(FStream) then
    FStream.Size := 0;// truncate!
end;

function TJvCsvStream.Size: Int64; { Get file size }
begin
  if Assigned(FStream) then
    GetFileSizeEx(FStream.Handle, PULargeInteger(@Result)) {int64 Result}
  else
    Result := 0;
end;

{ Look at this. A stream that can handle a string parameter. What will they think of next? }
procedure TJvCsvStream.Write(const s: RawByteString);
begin
  Stream.Write(s[1], Length(s)); {The author of TStreams would like you not to be able to just write Stream.Write(s).  Weird. }
end;

procedure TJvCsvStream.WriteChar(c: AnsiChar);
begin
  Stream.Write(c, SizeOf(AnsiChar));
end;

procedure TJvCsvStream.WriteCrLf;
begin
  WriteChar(#13);
  WriteChar(#10);
end;

procedure TJvCsvStream.WriteLine(const s: RawByteString);
begin
  Write(s);
  WriteCrLf;
end;

procedure TJvCsvStream._StreamReadBufInit;
begin
  if not Assigned(FStreamBuffer) then
  begin
    //FStreamBuffer := AllocMem(JvCsvStreamReadChunkSize);
    GetMem(FStreamBuffer, JvCsvStreamReadChunkSize);
  end;
end;

//-------------------------------------------------------------------------
// TJvCustomCsvDataSet METHODS
//-------------------------------------------------------------------------

constructor TJvCustomCsvDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FData := TJvCsvRows.Create;

  { this is updated later, to modify to have custom DecimalSeparator }
  {$IFDEF RTL220_UP}
  FFormatSettings := FormatSettings.Create;
  {$ELSE ~RTL220_UP}
  {$IFDEF RTL150_UP}
  GetLocaleFormatSettings(LOCALE_SYSTEM_DEFAULT, FFormatSettings);
  {$ELSE ~RTL150_UP}
  FFormatSettings.DecimalSeparator := DecimalSeparator;
  {$ENDIF ~RTL150_UP}
  {$ENDIF ~RTL220_UP}

  Separator := ','; // set After creating FData!

  FCreatePaths := True; // Creates subdirectories automatically when saving.

  FInitialWorkingDirectory := GetCurrentDir; // from SysUtils;

  FReadOnly := False;
  FCursorOpen := False;
  FRecordPos := JvCsv_ON_BOF_CRACK;
  FLoadsFromFile := True;
  FSavesChanges := True;
  FHasHeaderRow := True;
  FValidateHeaderRow := True;

  { Additional initialization }
  FCsvColumns := TJvCsvColumns.Create;

//  FData.EnquoteBackslash := FEnquoteBackslash;
  //FCsvFileAsStrings := TStringList.Create;
end;

destructor TJvCustomCsvDataSet.Destroy;
begin
  //FCsvFileAsStrings.Free;
  FreeMem(FTempBuffer); // Free the memory we allocated.
  FreeAndNil(FCsvStream);
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

function TJvCustomCsvDataSet.GetSeparator: Char;
begin
  Assert(Assigned(FData));
  Result := FData.Separator;
end;

function TJvCustomCsvDataSet._CsvFloatToStr(Value: Double): string;
begin
  // raises exception EJvConvertError (same as EConvertError)
  FFormatSettings.DecimalSeparator := GetDecimalSeparator;
  Result := FloatToStr(Value{$IFDEF RTL150_UP}, FFormatSettings{$ENDIF RTL150_UP});
end;

function TJvCustomCsvDataSet.GetTextBufferSize: Integer;
begin
  Assert(Assigned(FData));
  Result := FData.TextBufferSize;
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

procedure TJvCustomCsvDataSet.SetRowFiltered(ARow: PCsvRow; AFiltered: Boolean);
begin
  if ARow^.Filtered <> AFiltered then
  begin
    if AFiltered then
      Inc(FData.FFilteredCount)
    else
      Dec(FData.FFilteredCount);
    ARow^.Filtered := AFiltered;
  end;
end;

procedure TJvCustomCsvDataSet.SetRowTag(TagValue: Integer);
var
  RecNo: Integer;
begin
  RecNo := GetRecNo;
  FData.SetUserTag(RecNo, TagValue);
end;

{ Filtering via wildcards requires the following helper function: }
function _WildcardsMatchBoolOp(const Data, Pattern: string; BoolOp: Char): Boolean;
var
  SubPattern: array [0..20] of string;
  I, Count: Integer;
begin
  Count := JvStrSplit(Pattern, BoolOp, {Chr(0)=No Quoting} Chr(0), SubPattern, 20);
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

procedure TJvCustomCsvDataSet.SetActive(Value: Boolean);
begin
  inherited;
  FFileDirty := False;
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

// JvCsv Numeric filtering helper function:
function JvCsvNumCondition(FieldValue: Double; CompareOperator: TJvCsvFilterNumCompare; NumValue: Double): Boolean;
begin
  case CompareOperator of
    jfIntEqual:
      Result := Trunc(FieldValue) = Trunc(NumValue);

    jfIntNotEqual:
      Result := Trunc(FieldValue) <> Trunc(NumValue);

    jfLessThan:
      Result := FieldValue < NumValue;

    jfGreaterThan:
      Result := FieldValue > NumValue;
  else
    Result := False;
  end;
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
  begin
    if (Pattern[I] = '%') and (Pattern[I + 1] = '%') then
    begin
      Pattern := Copy(Pattern, 1, I) + Copy(Pattern, I + 2, PatternLength);
      PatternLength := Length(Pattern);
    end
    else
      Inc(I);
  end;
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
    SetRowFiltered(PRow, not FilterCallback(I));
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
        SetRowFiltered(PRow, True);
    end;
  end;
  FIsFiltered := True;
  if Active then
    First;
end;

procedure TJvCustomCsvDataSet.SetMarginSize(const Value: Integer);
begin
  if Active then
    raise Exception.Create('Can''t change memory properties on an active data set');

  Assert(Assigned(FData));
  FData.MarginSize := Value;
end;

// Numeric Filtering: Make Rows Visible Only if they match an Integer or floating point numeric comparison operator.
// evaluate condition:
//   [FieldName]  [numericoperator: < > = <> ] [Numeric Value Parameter]
procedure TJvCustomCsvDataSet.SetFilterNum(const FieldName: string; CompareOperator: TJvCsvFilterNumCompare;
  NumValue: Double);
var
  I: Integer;
  PRow: PCsvRow;
  FieldRec: PCsvColumn;
  FieldIndex: Integer;
  sFieldValue: string;
  FieldValue: Double;
begin
  FieldRec := FCsvColumns.FindByName(FieldName);
  if not Assigned(FieldRec) then
    Exit;
  FieldIndex := FieldRec^.FPhysical;

  // Now check if field value matches given pattern for this row.
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    if not PRow^.Filtered then
    begin
      sFieldValue := FData.GetARowItem(I, FieldIndex);
      if (Length(sFieldValue) > 0) and (sFieldValue[1] = '"') then
        sFieldValue := _Dequote(sFieldValue); // remove quotes.
      try
        FieldValue := JvCsvStrToFloat(sFieldValue, GetDecimalSeparator); // remember, this baby throws EConvertError on exception!

        //  if { FieldValue  [ = <> > < ] NumValue } then....
        if JvCsvNumCondition(FieldValue, CompareOperator, NumValue) then // hide row if not same prefix
        begin
          // Inc(stillVisible)   // count the number that are still visible.
        end
        else
          SetRowFiltered(PRow, True)
      except
        on E: EConvertError do
          SetRowFiltered(PRow, True); // hide error rows.
      end;
    end;{if not already hidden!}
  end;{ for loop}
  FIsFiltered := True;
  if Active then
    First;
end;

// string Filtering: Make Rows Visible Only if they match filterString
procedure TJvCustomCsvDataSet.SetFilter(const FieldName: string; Pattern: string);
var
  ValueLen, I: Integer;
  PRow: PCsvRow;
  FieldRec: PCsvColumn;
  FieldIndex: Integer;
  FieldValue: string;
begin
  FieldRec := FCsvColumns.FindByName(FieldName);
  if not Assigned(FieldRec) then
    Exit;
  FieldIndex := FieldRec^.FPhysical;
  ValueLen := Length(Pattern); // if valuelen is zero then we are searching for blank or nulls
  Pattern := {Ansi}UpperCase(Pattern); // make value case insensitive.

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
          SetRowFiltered(PRow, True);
      end
      else
      begin
        FieldValue := {Ansi}UpperCase(FieldValue);

        if JvCsvWildcardMatch(FieldValue, Pattern) then // hide row if not same prefix
        begin
          // Inc(stillVisible)   // count the number that are still visible.
        end
        else
          SetRowFiltered(PRow, True);
      end;
    end
  end;
  FIsFiltered := True;
  if Active then
    First;
end;

{ Some advanced JvCsvData set users, like WP himself, the original component
  author need to create and modify CSV records directly. For example, a query
  engine constructed using JvCsvDataSet, may wish to move tuples (row memory objects)
  directly from one dataset to another, not value-by-value but the whole row
  at once. }
function TJvCustomCsvDataSet._AllocateRow: PCsvRow;
begin
  Result := PCsvRow(AllocMem(FData.GetRowAllocSize));
  FData.InternalInitRecord(TJvRecordBuffer(Result)); // {was PChar(Result) }
end;

procedure TJvCustomCsvDataSet.ClearPreviousFilter; // Clear Previous Filtering.
var
  I: Integer;
  PRow: PCsvRow;
begin
  for I := 0 to FData.Count - 1 do
  begin
    PRow := PCsvRow(FData[I]);
    if Assigned(PRow) then
      SetRowFiltered(PRow, False); // clear all filter bits.
  end;
  FIsFiltered := False;
end;

procedure TJvCustomCsvDataSet.ClearFilter; // Clear Previous Filtering.
var
  M: TBookmark;
begin
  M := GetBookmark;
  try
    ClearPreviousFilter;
    // Update screen.
    if Active then
      if Assigned(M) then
        GotoBookmark(M)
      else
        First;
  finally
    FreeBookmark(M);
  end;
end;

function TJvCustomCsvDataSet.BookmarkValid(Bookmark: TBookmark): Boolean;
begin
  Result := (Bookmark <> nil) and (PInteger(Bookmark)^ >= 0) and (PInteger(Bookmark)^ < FData.Count);
end;

{ You shouldn't create a "TJvCsvRow-memory-buffer-record-aggregate" anywhere else than here. }
function TJvCustomCsvDataSet.AllocRecordBuffer: PJvMemBuffer;
begin
  Assert(Assigned(FData));
  Result := FData.AllocRecordBuffer;
end;

{ calc fields support }

procedure TJvCustomCsvDataSet.ClearCalcFields(Buffer: PJvMemBuffer);
begin
  // Assumes that our buffer is a TJvCsvRow followed by
  // a dynamically resized buffer used for calculated field
  // storage:
  FillChar(Buffer[GetCalcDataOffset(PCsvRow(Buffer))], JvCsv_MaxCalcDataOffset{CalcFieldsSize}, {initbytevalue}0);
end;

{ calc fields support and buffer support }

function TJvCustomCsvDataSet.GetActiveRecordBuffer: TJvRecordBuffer;
begin
  case State of
    dsBrowse:
      if IsEmpty then
        Result := nil
      else
        Result := TJvRecordBuffer(ActiveBuffer);
    dsCalcFields:
      Result := TJvRecordBuffer(CalcBuffer);
    dsFilter:
      Result := FFilterBuffer;
    dsEdit, dsInsert:
      Result := TJvRecordBuffer(ActiveBuffer);
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
    FDefaultCsvFieldDefs := False;
    FHeaderRow := '';
    FieldDefs.Clear; // Clear VCL Database field definitions
    FCsvColumns.Clear; // Clear our own CSV related field Data
    FData.Clear; // Clear out Data
  end;
end;

procedure TJvCustomCsvDataSet.SetDecimalSeparator(const Value: Char);
begin
  FData.DecimalSeparator := Value;
end;

procedure TJvCustomCsvDataSet.SetEnquoteBackslash(const Value: Boolean);
begin
  Assert(Assigned(FData));
  FData.FEnquoteBackslash := Value;
end;

function TJvCustomCsvDataSet.GetEnquoteBackslash: Boolean;
begin
  Assert(Assigned(FData));
  Result := FData.FEnquoteBackslash;
end;

procedure TJvCustomCsvDataSet.FreeRecordBuffer(var Buffer: PJvMemBuffer);
begin
  if Buffer <> nil then
    FreeMem(Buffer);
end;

{ called after the record is allocated }

procedure TJvCustomCsvDataSet.InternalInitRecord(Buffer: PJvMemBuffer);
var
  RowPtr: PCsvRow;
begin
  //Trace( 'InternalInitRecord '+IntToHex(Integer(Buffer),8) );
  Assert(Assigned(FData));
  FData.InternalInitRecord(Buffer);
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
  RowPtrText: PAnsiChar;
begin
  RowPtr^.Index := -1; // Not Yet Indexed
  RowPtr^.IsDirty := RowAlreadySaved;
  
  RowPtr^.Bookmark.Flag := bfEOF;
  RowPtr^.Bookmark.Data := JvCsv_ON_BOF_CRACK; // no index into FData yet.
  CsvRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);

  ColCount := FCsvColumns.Count;
  if ColCount <= 0 then
    ColCount := 10;

  RowPtrText := @RowPtr^._Text[0];
  for I := 1 to ColCount do
  begin // create an empty line of just commas
    if I < ColCount then
      RowPtrText[I - 1] := AnsiChar(Separator)
    else
      RowPtrText[I - 1] := #0;
    RowPtrText[I] := #0;
    CsvRowSetColumnMarker(RowPtr, {column} I - 1, {marker value} I - 1);
    CsvRowSetColumnMarker(RowPtr, {column} I, {marker value} JvCsv_COLUMN_ENDMARKER);
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

  // Strip quotes first (Both floating point and string fields can get enquoted on us)
  if Field.DataType in [ftString, ftFloat] then
  begin
    L := Length(TempString);
    if L >= 2 then
      if (TempString[1] = '"') and (TempString[L] = '"') then
        TempString := _Dequote(TempString); // quoted string or floating point value.
  end;

  try
    case Field.DataType of
      ftString:
        Result := TempString;
      ftInteger:
        Result := StrToInt(TempString);
      ftFloat:
        { Default CLASSIC behaviour of this component is to encode outgoing data in US
          format regardless of system regional settings. This has become more flexible now,
          but we still default at designtime-defaults to using a DOT. }
        Result := JvCsvStrToFloat(TempString, GetDecimalSeparator);

      ftBoolean:
        Result := StrToIntDef(TempString, 0) <> 0;
      ftDateTime:
         { one of three different datetime formats}
         if TempString <> '' then
           case CsvColumnData^.FFlag of
             jcsvAsciiTime:
               Result := JvIsoDateTimeStrToDateTime(TempString);
             jcsvAsciiDate:
               Result := JvIsoDateTimeStrToDateTime(TempString);
             jcsvAsciiDateTime:
               Result := JvIsoDateTimeStrToDateTime(TempString);
             jcsvGMTDateTime:
               Result := JvTimeTHexToDateTime(TempString, 0);
             jcsvTZDateTime:
               Result := JvTimeTHexToDateTime(TempString, FTimeZoneCorrection);
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
    Exit; // failed. Can only auto increment on Integer fields!

  if not Assigned(FieldLookup) then
    Exit; //failed.

  CsvColumnData := FCsvColumns.FindByFieldNo(FieldLookup.FieldNo);
  Max := -1;
  for RecIndex := 0 to FData.Count - 1 do
    try
      // skip filtered rows:
      RowPtr := FData[RecIndex];
      Assert(Assigned(RowPtr)); // FData should never contain nils!
      if not RowPtr^.Filtered then // skip filtered row!
      begin
        Value := GetFieldValueAsVariant(CsvColumnData, FieldLookup, RecIndex);
        if Value > Max then
          Max := Value; // keep maximum.
      end;
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
function TJvCustomCsvDataSet.LocateRecord(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions;
  SyncCursor: Boolean): Boolean;
  // Options is    [loCaseInsensitive]
  //              or [loPartialKey]
  //              or [loPartialKey,loCaseInsensitive]
  //              or [] {none}
var
  StrValueA: string;
  StrValueB: string;
  KeyFieldArray: array [0..20] of string;
  FieldLookup: array [0..20] of TField;
  CsvColumnData: array [0..20] of PCsvColumn;
  FieldIndex: array [0..20] of Integer;
  RecIndex, I, Lo, Hi, Count, VarCount: Integer;
  Value: Variant;
  MatchCount: Integer;
  CompareResult: Boolean;
begin
  Result := False;
  Lo := -1;
//  Hi := -1;  // Value is never used

  if not Active then
    Exit;
  if Pos(',', KeyFields) > 0 then
    Count := JvStrSplit(KeyFields, ',', #0, KeyFieldArray, 20)
  else
    Count := JvStrSplit(KeyFields, ';', #0, KeyFieldArray, 20);

  // Single value need not be an array type!
  if (VarType(KeyValues) and VarArray) > 0 then
  begin
    Lo := VarArrayLowBound(KeyValues, 1);
    Hi := VarArrayHighBound(KeyValues, 1);
    VarCount := (Hi - Lo) + 1;
  end
  else
    VarCount := 1;
  if (VarCount <> Count) or (Count = 0) or (KeyFieldArray[0] = '') then
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
  for RecIndex := 0 to FData.Count - 1 do
  begin
    if not PCsvRow(FData.Items[RecIndex])^.Filtered then // Mantis #5108: Don't need to trap filtered records
    begin
      MatchCount := 0;
      for I := 0 to Count - 1 do
      begin
        Value := GetFieldValueAsVariant(CsvColumnData[I], FieldLookup[I], RecIndex);
        if Lo < 0 then // non-vararray!
          CompareResult := Value = KeyValues
        else // vararray!
          CompareResult := Value = KeyValues[I + Lo];

        if CompareResult then
          Inc(MatchCount)
        else
        if Options <> [] then
        begin
          if VarIsStr(Value) then
          begin
            StrValueA := Value;
            if Lo < 0 then
              StrValueB := KeyValues
            else
              StrValueB := KeyValues[I + Lo];
            if loCaseInsensitive in Options then
              CompareResult := {Ansi}CompareText(StrValueA, StrValueB) = 0
            else
              CompareResult := StrValueA = StrValueB;

            if CompareResult then
              Inc(MatchCount)
            else
            begin
              if loPartialKey in Options then
              begin
                if loCaseInsensitive in Options then
                begin
                  StrValueA := {Ansi}UpperCase(StrValueA);
                  StrValueB := {Ansi}UpperCase(StrValueB);
                end;
                if Pos(StrValueB, StrValueA) = 1 then
                  Inc(MatchCount);
              end;
            end;
          end;
        end;
      end;

      if MatchCount = Count then
      begin
        if SyncCursor then
          RecNo := RecIndex // Move cursor position
        else
          FRecordPos := RecIndex;
        Result := True;
        Exit;
      end;
    end;
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
        FRecordPos := JvCsv_ON_EOF_CRACK;
        Result := grEOF;
        Exit;
      end;
    end
    else
    begin // BackwardSkip mode
      Dec(FRecordPos);
      if FRecordPos < 0 then
      begin // hit BOF_CRACK
        FRecordPos := JvCsv_ON_BOF_CRACK;
        Result := grBOF;
        Exit;
      end;
    end;
  end;
end;

function TJvCustomCsvDataSet.GetRecord(Buffer: PJvMemBuffer; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowPtr: PCsvRow;
begin
  AnsiChar(Buffer[0]) := #0;
  Result := grEOF;
  if FData.Count < 1 then
  begin
    //Trace(' GetRecord - called when Data buffer empty.');
    Exit;
  end;
  case GetMode of
    gmPrior:
      begin
        //Trace(' GetRecord(Buffer, gmPrior, DoCheck)');
        if FRecordPos = JvCsv_ON_BOF_CRACK then
          Result := grBOF
        else
        if FRecordPos = JvCsv_ON_EOF_CRACK then
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
         //Trace(' GetRecord(Buffer, gmCurrent, DoCheck)');
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
         //Trace(' GetRecord(Buffer, gmNext, DoCheck)');
        if FRecordPos = JvCsv_ON_EOF_CRACK then
          Result := grEOF
        else
        begin
          Inc(FRecordPos);

          if FRecordPos >= FData.Count then
          begin
            FRecordPos := JvCsv_ON_EOF_CRACK;
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
      Move(FData.GetRowPtr(FRecordPos)^, RowPtr^, FData.GetRowAllocSize);
      RowPtr^.Bookmark.Flag := bfCurrent;
      RowPtr^.Bookmark.Data := FRecordPos;

      // Update calculated fields for this row:
      ClearCalcFields(Buffer);
      GetCalcFields({$IFDEF RTL250_UP}TRecBuf{$ENDIF}(Buffer));
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
  LocalEnquoteBackslash := GetEnquoteBackslash; // can force on, or let it turn on automatically.

  if Pos(StrVal, Cr) > 0 then // we are going to need to enquote the backslashes
    LocalEnquoteBackslash := True; // absolutely need it in just this case.
  if Pos(StrVal, Lf) > 0 then
    LocalEnquoteBackslash := True; // absolutely need it in just this case.

  S := '"';
  L := Length(StrVal);
  for I := 1 to L do
  begin
    Ch := StrVal[I];
    if (Ch = Cr) and (BackslashCrLf) then
      // slighlty unstandard csv behavior, hopefully transparently interoperable with other apps that read CSVs
      S := S + '\r'
    else
    if (Ch = Lf) and (BackslashCrLf)  then // replace linefeed with \n. slightly nonstandard csv behavior.
      S := S + '\n'
    else
    if LocalEnquoteBackslash and (Ch = '\') then
    begin // it would be ambiguous not to escape this in this case!
      S := S + '\\';
      EnquoteBackslash := True; // XXX This is a lurking bug. Some day we'll get bit by it.
    end
    else
    if Ch = '"' then // always escape quotes by doubling them, since this is standard CSV behaviour
      S := S + '""'
    else
    if Ch = Tab then
      S := S + Ch // keep tabs! NEW Sept 2004! WP.
    else
    //if (Ch >= ' ') then // we used to strip any other low-ascii-unprintables but we don't anymore!
    S := S + Ch;
  end;
  S := S + '"'; // end quote.
  Result := S;
end;

function TJvCustomCsvDataSet.GetRecordSize: Word;
begin
  Result := FData.RecordSize;
end;

procedure TJvCustomCsvDataSet.SetFieldData(Field: TField; Buffer: TJvValueBuffer);
var
  RowPtr: PCsvRow;
  NewVal: string;
  CP, PhysicalLocation: Integer;
  PDestination: PJvMemBuffer;
  CsvColumnData: PCsvColumn;
  DT: TDateTime;
  ATimeStamp: TTimeStamp;
  {$IFDEF JVCSV_WIDESTRING}
    {$IFDEF COMPILER12_UP}
  NewUniVal: UnicodeString;
    {$ELSE}
  NewUniVal: WideString;
  {$ENDIF COMPILER12_UP}
  {$ENDIF JVCSV_WIDESTRINGS}
begin
  //Trace( 'SetFieldData '+Field.FieldName );
  PDestination := GetActiveRecordBuffer;
  RowPtr := PCsvRow(PDestination);
  Assert(RowPtr.Magic = JvCsvRowMagic, 'Internal data corruption detected in JvCustomCsvDataSet.SetFieldData');

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
    if (Field.Offset < 0) or (Field.Offset + Field.DataSize > JvCsv_MaxCalcDataOffset) then
      Exit;
    Inc(PDestination, GetCalcDataOffset(RowPtr) + Field.Offset);

    PDestination[0] := {$IFNDEF RTL200_UP}AnsiChar{$ENDIF RTL200_UP}(Ord(Buffer <> nil));

    if AnsiChar(PDestination[0]) <> #0 then
      Move({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^, PDestination[1], Field.DataSize);
    //Result := True; {there is no return value, oops}
    // Notify controls of a field change:
    DataEvent(deFieldChange, NativeInt(Field));
    Exit;
  end;

  // If we get here, we are dealing with a physical record:

  // Set a field Data, taking the physical to logical ordering translation into
  // account:
  CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
  if not Assigned(CsvColumnData) then
  begin
    {$IFDEF DEBUGINFO_ON}
    OutputDebugString('JvCsvData.pas: Column data corrupt or missing.');
    {$ENDIF DEBUGINFO_ON}
    Exit;
  end;

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
      {$IFDEF JVCSV_WIDESTRING}
      ftWideString:
          { New and only working in Delphi 2009.
            Private-Convention-Warning: Wide fields ALWAYS enquoted.
          }
        begin
          {$IFDEF COMPILER12_UP}
          NewUniVal := UnicodeString(PWideChar(Buffer));
          NewVal := _Enquote(NewUniVal);
          {$ELSE}
          { NOTE: This dies on Delphi 7, because Buffer was never initialized!}
          NewUniVal := WideString(PWideChar(Buffer));
          //NewUniVal := PWideString(Buffer)^; {doesn't work either}
          NewVal := _Enquote(Utf8Encode(NewUniVal));
          {$ENDIF COMPILER12_UP}
        end;
      {$ENDIF JVCSV_WIDESTRING}
      ftString:
        begin
          // ftString appears to be limited to ANSI Strings even in Delphi 2009.
          CP := StrLLen(PAnsiChar(Buffer), Field.Size);
          SetString(NewVal, PAnsiChar(Buffer), CP);
          //----------------------------------------------------------------------------------------------------
          // string ENQUOTING IN CSV: If user displayed value contains a comma, a backslash, or a Double quote character
          // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
          // and backslashes preceded by a backslash character.
          //----------------------------------------------------------------------------------------------------
          if AlwaysEnquoteStrings
            or (Pos(Separator, NewVal) > 0)
            or (Pos(Cr,        NewVal) > 0)
            or (Pos(Lf,        NewVal) > 0)
            or (Pos('"',       NewVal) > 0)
            or ((Pos('\',      NewVal) > 0) and EnquoteBackslash)
          then
            NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
        end;
      ftInteger:
        begin
          NewVal := IntToStr(PInteger(Buffer)^);
        end;

      ftFloat:
        begin
          NewVal := _CsvFloatToStr(PDouble(Buffer)^);
          if AlwaysEnquoteFloats or (Separator = GetDecimalSeparator) then
            NewVal := _Enquote(NewVal); // puts whole string in quotes, escapes embedded commas and quote characters!
        end;

      ftBoolean:
        NewVal := IntToStr(Ord(PWordBool(Buffer)^)); // bugfix May 26, 2003 - WP

      // There are two ways of handling date and time:
      ftDate: // NEW: TDateField support!
        if (CsvColumnData^.FFlag = jcsvAsciiDate) then
        begin
          ATimeStamp.Time := 0;
          ATimeStamp.Date := Integer({$IFDEF RTL240_UP}PInteger(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^);
          DT := TimeStampToDateTime(ATimeStamp);
          NewVal := JvDateIsoStr(DT);
        end
        else
          JvCsvDatabaseError2(FTableName, RsEFieldTypeNotHandled, Ord(CsvColumnData^.FFlag));
      ftTime: // NEW: TTimeField support!
        if CsvColumnData^.FFlag = jcsvAsciiTime then
        begin
          ATimeStamp.Time := LongInt({$IFDEF RTL240_UP}PLongInt(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^);
          ATimeStamp.Date := DateDelta;
          DT := TimeStampToDateTime(ATimeStamp);
          NewVal := JvTimeIsoStr(DT);
        end
        else
          JvCsvDatabaseError2(FTableName, RsEFieldTypeNotHandled, Ord(CsvColumnData^.FFlag));
      ftDateTime:
        case CsvColumnData^.FFlag of
          // Localized date only (no time) in Ascii
          jcsvAsciiDate:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^)));
              NewVal := JvDateIsoStr(DT);
            end;
          // Localized time only (no date) in Ascii
          jcsvAsciiTime:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^)));
              NewVal := JvTimeIsoStr(DT);
            end;
          // Localized date+time in Ascii
          jcsvAsciiDateTime:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^)));
              NewVal := JvDateTimeIsoStr(DT);
            end;
          // GMT Times are stored in HEX
          jcsvGMTDateTime:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^)));
              NewVal := JvDateTimeToTimeTHex(DT, 0);
            end;
          jcsvTZDateTime: // Move a GMT time into a timezone:
            begin
              DT := TimeStampToDateTime(MSecsToTimeStamp(Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^)));
              NewVal := JvDateTimeToTimeTHex(DT, FTimeZoneCorrection);
            end;
        else
          JvCsvDatabaseError2(FTableName, RsETimeTConvError, Ord(CsvColumnData^.FFlag));
        end;
    else
      JvCsvDatabaseError2(FTableName, RsEFieldTypeNotHandled, Ord(CsvColumnData^.FFlag));
    end;

  // Set new Data value (NewVal = string)
  SetCsvRowItem(RowPtr, PhysicalLocation, NewVal);
  if Assigned(FOnSetFieldData) and (RowPtr^.Index >= 0) then
    FOnSetFieldData(Self, FData.GetUserTag(RowPtr^.Index), FData.GetUserData(RowPtr^.Index), Field.FieldName, NewVal);

  // Set a dirty bit so we remember to write this later:
  CsvRowSetDirtyBit(PCsvRow(PDestination), PhysicalLocation);

  // Set the file-wide dirty bit:
  FFileDirty := True;

  // Notify controls of a field change:
  DataEvent(deFieldChange, NativeInt(Field));
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
    if EnquoteBackslash then
    begin
      if not SkipFlag and (Ch = '\') then
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
      if not SkipFlag and (Ch = '"') and (I < (L - 1)) and (StrVal[I + 1] = '"') then
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
    try
      First; // Redraws all controls once to relocate to top.
      GotoBookmark(M); // Go back where we were. This could Result in some odd scrolling behaviour but I haven't seen it yet.
    finally
      FreeBookmark(M);
    end;
  finally
    EnableControls;
  end;
end;

function TJvCustomCsvDataSet.GetFieldData(Field: TField; {$IFDEF RTL250_UP}var{$ENDIF} Buffer: TJvValueBuffer): Boolean;
var
  RowPtr: PCsvRow;
  PSource: PJvMemBuffer;
  UserString, TempString: string;
  AnsiStr: AnsiString;
  PhysicalLocation: Integer;
  CsvColumnData: PCsvColumn;
  ADateTime: TDateTime;
  length_n: Integer;
  ts: TTimeStamp;
  {$IFDEF JVCSV_WIDESTRING}
    {$IFDEF COMPILER12_UP}
  GetUniValue: UnicodeString;
    {$ELSE}
  GetUniValue: WideString;
    {$ENDIF COMPILER12_UP}
  {$ENDIF JVCSV_WIDESTRING}
begin
  Result := False;

  //Trace( 'GetFieldData '+Field.FieldName );

  if not FCursorOpen or (Field = nil) then
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
    // It is possible we could raise an exception here:
    // "GetActiveRecordBuffer is nil but table is not empty. (Internal Fault Condition)."
    Exit;
  end;

  RowPtr := PCsvRow(PSource);

  //------------------------------------------------------------------------
  // Calculated and Lookup Field Handling
  //
  // direct memory copy into calculated field or lookup field Data area
  //------------------------------------------------------------------------
  if (Field.FieldKind = fkCalculated) or (Field.FieldKind = fkLookup) then
  begin
    if (Field.Offset < 0) or (Field.Offset + Field.DataSize > JvCsv_MaxCalcDataOffset) then
    begin
      //It is possible we could raise an exception here:
      //Invalid field.Offset in Calculated or Lookup field
      Exit;
    end;


    Inc(PSource, GetCalcDataOffset(RowPtr) + Field.Offset);
    if Buffer = nil then
    begin
      // NULL CHECK MEANS THAT SOMEONE IS ASKING IF THIS FIELD HAS A VALUE. RETURN True.
      Result := (Field.DataSize > 0); // Yes, we could read this field if you asked us to!
      Exit;
    end;

    if Field.DataSize <= 0 then
      PAnsiChar(Buffer)[0] := Chr(0)
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


  if Field.Offset + Field.DataSize > JvCsv_MAXLINELENGTH then
    Exit;

  TempString := GetCsvRowItem(RowPtr, PhysicalLocation);

  // Strip quotes first!
  if Field.DataType = ftString then
  begin
    length_n := Length(TempString);
    if length_n >= 2 then
      if (TempString[1] = '"') and (TempString[length_n] = '"') then
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
      //DataEvent(deFieldChange, NativeInt(Field));
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
      {$IFDEF JVCSV_WIDESTRING}
      ftWideString:
        begin
          {$IFDEF COMPILER12_UP}
          GetUniValue := _Dequote(TempString);
          {$ELSE}
          GetUniValue := UTF8Decode(_Dequote(TempString));
          {$ENDIF COMPILER12_UP}
          length_n := Length(GetUniValue) * SizeOf(WideChar);
          if (length_n > FData.FTextBufferSize) then
            length_n := FData.FTextBufferSize;
          if (length_n > Field.Size * SizeOf(WideChar)) then
            length_n := Field.Size * SizeOf(WideChar);
          MoveMemory({dest} Buffer, {src} PWideChar(GetUniValue), {count} length_n);
          PWideChar({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})[Length(GetUniValue)] := WideChar(0);  { wide terminal }
        end;
      {$ENDIF JVCSV_WIDESTRING}

      // Basic string copy, convert from string to fixed-length
      // buffer, padded with NUL i.e. Chr(0):
      ftString:
        begin
          AnsiStr := AnsiString(TempString);
          length_n := Length(AnsiStr);
          if (length_n > FData.FTextBufferSize) then
            length_n := FData.FTextBufferSize;
          if (length_n > Field.Size) then
            length_n := Field.Size;

          MoveMemory({dest} Buffer, {src} PAnsiChar(AnsiStr), {count} length_n);
          PAnsiChar({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})[length_n] := #0;
        end;
      // Standard Integer conversion:
      ftInteger:
        PInteger({$IFDEF RTL240_UP}PInteger(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})^ := StrToInt(TempString);
      // Standard Double-precision Float conversion:
      ftFloat:
        PDouble({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})^ := JvCsvStrToFloat(TempString, GetDecimalSeparator); // was StrToFloatUS
      ftBoolean:
        if TempString = '' then
          PInteger({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})^ := 0
        else
          PWordBool({$IFDEF RTL240_UP}PByte(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP})^ := StrToIntDef(TempString, 0) <> 0; // bugfix May 26, 2003 - WP

      ftDate:
        if CsvColumnData^.FFlag = jcsvAsciiDate then
        // Ascii Date yyyy/mm/ddd
        begin
          ADateTime := JvIsoDateStrToDate(TempString);
          if ADateTime <= 1.0 then
          begin
            Result := False; { field is NULL, no date/time value }
            Exit;
          end;
          // XXX Delphi Weirdness Ahead.  Read docs before you try to
          // understand this. The data in Buffer^ is an Integer timestamp
          Integer({$IFDEF RTL240_UP}PInteger(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := DateTimeToTimeStamp(ADateTime).Date;
        end else
          JvCsvDatabaseError(FTableName, RsETimeTConvError);

      ftTime:
        if CsvColumnData^.FFlag = jcsvAsciiTime then
        begin
          ADateTime := JvIsoTimeStrToTime(TempString);
          if ADateTime < 0.0 then
          begin
            Result := False; { field is NULL, no date/time value }
            Exit;
          end;
          // The data in Buffer^ is an Integer timestamp
          Integer({$IFDEF RTL240_UP}PInteger(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := DateTimeToTimeStamp(ADateTime).Time;
        end else
          JvCsvDatabaseError(FTableName, RsETimeTConvError);

      ftDateTime:
        case CsvColumnData^.FFlag of
          // Ascii Date yyyy/mm/ddd
          jcsvAsciiDate:
            begin
              ADateTime := JvIsoDateStrToDate(TempString);
              if ADateTime <= 1.0 then
              begin
                Result := False; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(ADateTime);
              if (ts.Time = 0) and (ts.Date = 0) then
                Exit;
              // XXX Delphi Weirdness Ahead.  Read docs before you try to
              // understand this. We want to store 8 bytes at Buffer^, this
              // is how we do it.
              Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := TimeStampToMSecs(DateTimeToTimeStamp(ADateTime));
            end;

          // Ascii Time 08:23:15
          jcsvAsciiTime:
            begin
              ADateTime := JvIsoTimeStrToTime(TempString);
              if ADateTime <= 1.0 then
              begin
                Result := False; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(ADateTime);
              if (ts.Time = 0) and (ts.Date = 0) then
                Exit;
              // XXX Delphi Weirdness Ahead.  Read docs before you try to
              // understand this. We want to store 8 bytes at Buffer^, this
              // is how we do it.
              Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := TimeStampToMSecs(DateTimeToTimeStamp(ADateTime));
            end;


          // Ascii Date 1999/03/05 08:23:15
          jcsvAsciiDateTime:
            begin
              ADateTime := JvIsoDateTimeStrToDateTime(TempString);
              if ADateTime <= 1.0 then
              begin
                Result := False; { field is NULL, no date/time value }
                Exit;
              end;
              ts := DateTimeToTimeStamp(ADateTime);
              if (ts.Time = 0) and (ts.Date = 0) then
                Exit;
              // XXX Delphi Weirdness Ahead.  Read docs before you try to
              // understand this. We want to store 8 bytes at Buffer^, this
              // is how we do it.
              Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := TimeStampToMSecs(DateTimeToTimeStamp(ADateTime));
            end;

          // GMT Times are Stored in HEX:
          jcsvGMTDateTime:
            Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := TimeStampToMSecs(DateTimeToTimeStamp(JvTimeTHexToDateTime(TempString, 0)));
          // Move GMT into a Timezone:
          jcsvTZDateTime:
            Double({$IFDEF RTL240_UP}PDouble(@Buffer[0]){$ELSE}Buffer{$ENDIF RTL240_UP}^) := TimeStampToMSecs(DateTimeToTimeStamp(JvTimeTHexToDateTime(TempString,
              FTimeZoneCorrection)));
        else
          JvCsvDatabaseError(FTableName, RsETimeTConvError);
        end;
    else // not a valid ftXXXX type for this TDataSet descendant!?
      JvCsvDatabaseError2(FTableName, RsEFieldTypeNotHandled, Ord(Field.DataType));
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

function TJvCustomCsvDataSet.GetBackslashCrLf: Boolean;
begin
  if Assigned(FData) then
    Result := FData.BackslashCrLf
  else
    Result := False;
end;

procedure TJvCustomCsvDataSet.GetBookmarkData(Buffer: PJvMemBuffer; Data: TJvBookmark);
begin
  PInteger(Data)^ := PCsvRow(Buffer)^.Bookmark.Data;
end;

function TJvCustomCsvDataSet.GetBookmarkFlag(Buffer: PJvMemBuffer): TBookmarkFlag;
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

procedure TJvCustomCsvDataSet.SetBookmarkFlag(Buffer: PJvMemBuffer; Value: TBookmarkFlag);
begin
  PCsvRow(Buffer)^.Bookmark.Flag := Value;
end;

procedure TJvCustomCsvDataSet.InternalGotoBookmark(Bookmark: TJvBookmark);
begin
  {Bookmark is just pointer to Integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCustomCsvDataSet.InternalSetToRecord(Buffer: PJvMemBuffer);
begin
  FRecordPos := PCsvRow(Buffer)^.Bookmark.Data; // Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:

procedure TJvCustomCsvDataSet.SetBackslashCrLf(const Value: Boolean);
begin
  if Assigned(FData) then
    FData.BackslashCrLf := Value;
end;

procedure TJvCustomCsvDataSet.SetBookmarkData(Buffer: PJvMemBuffer; Data: TJvBookmark);
begin
  PCsvRow(Buffer)^.Bookmark.Data := PInteger(Data)^;
end;

procedure TJvCustomCsvDataSet.InternalFirst;
begin
  FRecordPos := JvCsv_ON_BOF_CRACK;
end;

// CsvFieldDef:
//
// A property of our Data Set called CsvFieldDef is treated as
// declaration of the fields in the CSV table.
//
//   <coldef>,<coldef>,...,<coldef>
//   <coldef> = columname:<Data-type-character><size>
//
// See comments at the top of this unit for a discussion of the
// various VCL DB field types like Integer and string, and how
// they map to the special "FieldTypeChar" values defined here.
procedure TJvCustomCsvDataSet.InternalInitFieldDefs;
var
  CsvFieldOption : string;
  CsvFieldName   : string;
  aCsvFieldDef   : string;
  CsvKeys        : array of string;

  CsvFieldRec    : PCsvRow; //record type.
  PCsvFieldDef   : PCsvColumn;

  I              : Integer;
  ColNum         : Integer;
  Pos1           : Integer;
  SymbolOrdinal  : Integer;
  FieldTypeChar  : Char;       // field options (%=Integer, etc)
  VclFieldType   : TFieldType; // official VCL field type
  FieldLen       : Integer;
  FieldType      : TJvCsvColumnFlag;
  CsvMaxLen      : Integer; // Safety!
begin
  CsvMaxLen := 0;
  CsvFieldRec := PCsvRow(FData.AllocRecordBuffer);
  try
    FieldType := jcsvString;
    VclFieldType := ftString;

    // create FieldDefs which map to each field in the Data record
    FieldDefs.Clear; // Clear VCL Database field definitions
    FCsvColumns.Clear; // Clear our own CSV related field Data

    FDefaultCsvFieldDefs := False;
    aCsvFieldDef := CsvFieldDef;
    if aCsvFieldDef = '' then
    begin
      if FHasHeaderRow and ReadCsvFileStream then
      begin
        aCsvFieldDef := FCsvFileTopLine;
        {$IFDEF DEBUGINFO_ON}
        if aCsvFieldDef = '' then
          OutputDebugString('Top line of file empty. CsvFieldDef not provided either.');
        {$ENDIF DEBUGINFO_ON}
      end;

      if ExtendedHeaderInfo then
        CsvFieldDef := aCsvFieldDef;
      FDefaultCsvFieldDefs := True;
    end;

    if aCsvFieldDef <> '' then
    begin
      if (Separator <> ',') and (Pos(Separator, aCsvFieldDef) = 0) then
        JvStringToCsvRow(aCsvFieldDef, ',', CsvFieldRec, False, False) { workaround for serious annoyance }
      else
        JvStringToCsvRow(aCsvFieldDef, Separator, CsvFieldRec, False, False);

      ColNum := 0;
      while CsvRowGetColumnMarker(CsvFieldRec, ColNum) <> JvCsv_COLUMN_ENDMARKER do
      begin
        FieldLen := 80; // default.
        CsvFieldOption := GetCsvRowItem(CsvFieldRec, ColNum); // get a string in the format COLUMNAME:Options

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
          // it, otherwise default to '$'.  New ~ tilda character ordinal is 126.
          // Other symbols are all less than 65 (capital A).
          SymbolOrdinal := Ord(CsvFieldOption[Pos1 + 1]);
          if (SymbolOrdinal < Ord('A')) or (SymbolOrdinal >= Ord('~')) then
          begin
            FieldTypeChar := CsvFieldOption[Pos1 + 1];
            CsvFieldOption := Copy(CsvFieldOption, Pos1 + 2, 80);
          end
          else
          begin
            FieldTypeChar := '$';
            CsvFieldOption := Copy(CsvFieldOption, Pos1 + 1, 80);
          end;
          FieldLen := StrToIntDef(CsvFieldOption, JvCsv_DEFAULT_CSV_STR_FIELD);

        end;
        case FieldTypeChar of
          '$':
            begin // $=string
              CsvMaxLen := CsvMaxLen + FieldLen;
              VclFieldType := ftString;
              FieldType := jcsvString;
            end;
          '%':
            begin // %=Integervalue
              VclFieldType := ftInteger;
              FieldType := jcsvNumeric;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 5;
            end;
          '&':
            begin // &=Float value
              VclFieldType := ftFloat;
              FieldType := jcsvNumeric;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 5;
            end;
          '@':
            begin // @=Datetime as Ascii YYYY/MM/DD HH:MM:SS
              VclFieldType := ftDateTime;
              FieldType := jcsvAsciiDateTime;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 20;
            end;
          '/':
            begin // /=date only as ascii YYYY/MM/DD
              VclFieldType := ftDate;
              FieldType := jcsvAsciiDate;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 10;
            end;
          '*':
            begin // *=time only as HH:MM:SS
              VclFieldType := ftTime;
              FieldType := jcsvAsciiTime;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 10;
            end;
          '!':
            begin // != Boolean field True/False
              VclFieldType := ftBoolean; // Boolean field in dataset
              FieldType := jcsvNumeric; // numeric field in file
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 1;
            end;
          '#':
            begin // #=Datetime as Seconds since 1970 stored in HEX
              VclFieldType := ftDateTime;
              FieldType := jcsvGMTDateTime;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 10;
            end;
          '-':
            begin // -=Datetime as Seconds since 1970 stored in HEX
              VclFieldType := ftDateTime;
              FieldType := jcsvTZDateTime;
              FieldLen := 0; // automatic.
              CsvMaxLen := CsvMaxLen + 10;
            end;
          {$IFDEF JVCSV_WIDESTRING}
          '~':
           begin // New UTF8 string type. [ Delphi 2009 only ]
             CsvMaxLen := CsvMaxLen + FieldLen;
             VclFieldType := ftWideString;
             FieldType := jcsvStringUTF8;
           end
          {$ENDIF JVCSV_WIDESTRING}
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

        if (CsvMaxLen > FData.TextBufferSize) then
          raise Exception.Create('Too many fields, or too many long string fields in this record. You must increase the internal record size of the CsvDataSet');

        // This may throw an exception. but we'll just allow
        // that as necessary:

        //Was: TFieldDef.Create(FieldDefs, ...., ColNum);
        FieldDefs.Add(CsvFieldName, VclFieldType, FieldLen, False);

        // Now create our internal field Data structure:
        PCsvFieldDef := AllocMem(SizeOf(TJvCsvColumn) {+ 8 BIGFudge});
        PCsvFieldDef^.FFlag := FieldType; {such as jcsvString}
        PCsvFieldDef^.FFieldDef := FieldDefs.Find(CsvFieldName);

        // Note: field order is established when we open the file (later)
        PCsvFieldDef^.FPhysical := -1; // not yet located in the physical file!
        FCsvColumns.AddColumn(PCsvFieldDef);
      end;

      // if the file doesn't contain this and we haven't
      // generated it yet, generate the header row:
      if not FHasHeaderRow and (FHeaderRow = '') then
        FHeaderRow := GetColumnsAsString;

      if FHeaderRow <> '' then
        ProcessCsvHeaderRow; // process FHeaderRow
    end
    else
      JvCsvDatabaseError(FTableName, RsEFieldDefinitionError);

    if FCsvKeyDef = '' then
      FCsvKeyCount := 0
    else
    begin
      SetLength(CsvKeys, FCsvColumns.Count);
      FCsvKeyCount := JvStrSplit(FCsvKeyDef, Separator, {Chr(0)=No Quoting} #0, CsvKeys, FCsvColumns.Count);
      SetLength(FCsvKeyFields, FCsvKeyCount);
      if (FCsvKeyCount < 1) or (FCsvKeyCount > FCsvColumns.Count) then
        JvCsvDatabaseError(FTableName, RsEInvalidCsvKeyDef);
      for I := 0 to FCsvKeyCount - 1 do
      begin
        if CsvKeys[I] = '' then
          JvCsvDatabaseError(FTableName, RsEInternalErrorParsingCsvKeyDef);
        PCsvFieldDef := FCsvColumns.FindByName(string(CsvKeys[I]));
        if not Assigned(PCsvFieldDef) then
          JvCsvDatabaseError(FTableName, Format(RsEContainsField, [CsvKeys[I]]))
        else
        begin
          PCsvFieldDef^.FKeyFlag := True;
          FCsvKeyFields[I] := PCsvFieldDef;
        end;
      end;
    end;
    // New:Array of Booleans used for ascending order on primary key sorting!
    SetLength(FAscending, FCsvKeyCount + 1);
    for I := 0 to Length(FAscending) - 1 do
      FAscending[I] := True;
  finally
     FreeMem(CsvFieldRec);
  end;
end;

{ set our position onto the EOF Crack }

procedure TJvCustomCsvDataSet.InternalLast;
begin
//  Eof := True;
  FRecordPos := JvCsv_ON_EOF_CRACK; // FData.Count;
end;

// At shutdown or on user-calling this method, check if Data has changed,
// and write changes to the file.
procedure TJvCustomCsvDataSet.Flush;
var
  n: Integer;
  CsvLine: AnsiString;
begin
  if FFileDirty and FSavesChanges then
  begin
    if FTableName = '' then
      raise EJvCsvDataSetError.CreateRes(@RsETableNameNotSet);

    // Make backup first, if enabled (>2)
    if FAutoBackupCount > 0 then
    begin
      if FAutoBackupCount < 10 then
        FAutoBackupCount := 10; // can't be between 1 and 9, must be at least 10.
      JvCsvBackupPreviousFiles(FOpenFileName, FAutoBackupCount);
    end;
    // Now write new file.
    // creates a TStringList: old method: ExportCsvFile(FOpenFileName);

    if not WriteCsvFileStream then
      raise EJvCsvDataSetError.CreateFmt('Unable to write to csv file %s', [FTableName]);

    if FHasHeaderRow then
      FCsvStream.WriteLine(AnsiString(FHeaderRow));

    for n := 0 to RecordCount-1 do
    begin
      CsvLine := JvTrimAnsiStringCrLf(GetRowAsAnsiString(n));
      FCsvStream.WriteLine(CsvLine);
    end;
    FreeAndNil(FCsvStream);
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
    Exit;

  Flush;
  BindFields(False);
  {$IFNDEF HAS_AUTOMATIC_DB_FIELDS}
  if DefaultFields then
  {$ENDIF !HAS_AUTOMATIC_DB_FIELDS}
    DestroyFields;
  FData.Clear;
  FCursorOpen := False;
  FRecordPos := JvCsv_ON_BOF_CRACK;
  FOpenFileName := '';
  FCsvFileLoaded := False;
  FData.FRecordsValid := False;
  if FDefaultCsvFieldDefs then
  begin
    FCsvFieldDef := '';
    FHeaderRow := '';
    FieldDefs.Clear;
    Fields.Clear;
  end;
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
    if InternalCompare(FCsvKeyFields, FCsvKeyCount, {Left} Row, {Right} FData.Items[I], FAscending) = 0 then
    begin
      Result := I;
      Break;
    end;
end;

function TJvCustomCsvDataSet.FindByCsvKey(const Key: string): Boolean;
var
  LogicalRow, PhysicalRow: PCsvRow;
  I, RecNo: Integer;
  aStr: string;
begin
  Result := False;
  LogicalRow := PCsvRow(FData.AllocRecordBuffer);
  PhysicalRow := PCsvRow(FData.AllocRecordBuffer);
  try
    JvStringToCsvRow(Key + Separator, Separator, LogicalRow, False, False); // initialize row and put items in their logical order.
    CsvRowInit(PhysicalRow);
    // Move from Logical (TFieldDef order) to their physical (As found in CSV file) ordering:
    for I := 0 to FCsvKeyCount - 1 do
    begin
      aStr := GetCsvRowItem(LogicalRow, I);
      SetCsvRowItem(PhysicalRow, FCsvKeyFields[I].FPhysical, aStr);
    end;
    RecNo := InternalFindByKey(PhysicalRow);
    if RecNo < 0 then
      Exit;

    FRecordPos := RecNo;
    Resync([]);
    Result := True;
  finally
    FreeMem(LogicalRow);
    FreeMem(PhysicalRow);
  end;
end;

procedure TJvCustomCsvDataSet.InternalAddRecord(Buffer: TJvRecordBuffer; Append: Boolean);
var
  RecPos: Integer;
  PAddRec: PCsvRow;
//  KeyIndex: Integer;
  RowPtrText: PAnsiChar;
begin
  if FInsertBlocked then
  begin
    JvCsvDatabaseError(FTableName, RsEInsertBlocked);
    Exit;
  end;

  if FRecordPos = JvCsv_ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = JvCsv_ON_EOF_CRACK then
    FRecordPos := FData.Count;

  PAddRec := PCsvRow(AllocMem(FData.GetRowAllocSize));


  if Buffer <> nil then
    Move(PCsvRow(Buffer)^, PAddRec^, FData.GetRowAllocSize)
  else
    FData.InternalInitRecord(TJvRecordBuffer(PAddRec));

  RowPtrText := @PAddRec^._Text[0];
  if RowPtrText[0] = #0 then
    JvStringToCsvRow(FEmptyRowStr, Separator, PAddRec, False, False); // initialize row.

  PAddRec^.IsDirty := RowNeedsSaving;

  PAddRec^.Index := -1; // Was not loaded from the file!

//  FData.EnquoteBackslash := EnquoteBackslash; // {finally stupidity like this is solved}

  FFileDirty := True;
  if Append then
  begin //this is the parameter not a TDataSet method invocation!
    PAddRec^.Index := FData.Count;
    FData.AddRow(PAddRec);
    InternalLast;
  end
  else
  begin
    if (FRecordPos = JvCsv_ON_EOF_CRACK) or (FRecordPos = JvCsv_ON_BOF_CRACK) then
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
  Assert(Length(FTableName) <> 0, RsEInvalidTableName);

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

function TJvCustomCsvDataSet.GetMarginSize: Integer;
begin
  Assert(Assigned(FData));
  Result := FData.MarginSize;
end;

{ ReadCsvFileStream:
  formerly InternalLoadFileStrings, which used to use a TStringLIst to load CSVs.
  WHich is fine for toy use, but not for real world cases like when CR/LF characters
  are embedded. Also, it's wasteful of memory to create a giant in-memory TStringList and then
  copy it to our table record objects, so now we read the disk file, and create our
  single set of objects, saving memory, and maybe even being faster, especially when memory
  is not plentiful or the CSV file being loaded is very large. }
function TJvCustomCsvDataSet.ReadCsvFileStream: Boolean;
var
  Line: RawByteString;
begin
  Result := False;
  if (FTableName = '') or not FileExists(FTableName) then
    Exit; // We can return immediately ONLY if there is no file to load,
          // otherwise this routine is parsing already-loaded Data, and we should NOT
          // return, or we won't get our Data in the table. -WP.

  if FCsvFileLoaded then
  begin
    Result := True; //loaded already! just return true and quit.
    Exit; // don't repeat!
  end;

  FCsvFileLoaded := True;

  if FLoadsFromFile then 
  begin
    if not Assigned(FCsvStream) then
      FCsvStream := TJvCsvStream.Create(FOpenFileName)
    else
      FCsvStream.Stream.Position := 0; // rewind.

    if FHasHeaderRow then
      Line := JvTrimAnsiStringCrLf(FCsvStream.ReadLine);

    {$IFDEF UNICODE}
    //-------------------------------------------------------------------------
    // This is the first unicode-friendly feature in JvCsvDataSet...
    // We can at least still open UTF8 files if they are really just ASCII
    // files plus a BOM marker like Windows notepad and some other apps add.
    //-------------------------------------------------------------------------
    FUtf8Detected := False; {Future.}
    if Length(Line) > 3 then
    begin
      // JvCsvData can detect the standard UTF-8 mark and work anyways when it is present
      // but cannot yet decode any special characters. This is a step on the route to proper
      // UTF-8 support.  [uses JclUnicode.BOM_UTF8 to detect.]
      if (BOM_UTF8[0] = Ord(Line[1])) and
         (BOM_UTF8[1] = Ord(Line[2])) and
         (BOM_UTF8[2] = Ord(Line[3])) then
      begin
        // strip UTF-8 marker:
        FCsvFileTopLine := Utf8ToAnsi(Copy(Line, 4, Length(Line)));
        FUtf8Detected := True; {future.}
      end
      else
        FCsvFileTopLine := string(AnsiString(Line));
    end
    else
    {$ENDIF UNICODE}
      FCsvFileTopLine := string(AnsiString(Line));

    Result := True; // it worked!
  end;
end;

function TJvCustomCsvDataSet.WriteCsvFileStream: Boolean;
begin
  Result := False;
  if FTableName = '' then
    Exit; // We can return immediately ONLY if there is no file to load,
          // otherwise this routine is parsing already-loaded Data, and we should NOT
          // return, or we won't get our Data in the table. -WP.
  FreeAndNil(FCsvStream);
  FCsvStream := TJvCsvStream.Create(FOpenFileName, fmJVCSV_Truncate);
  Result := True; // it worked!
end;


{
XXXX  TODO: FIX ME ! XXXX

During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This cleans up that TString List.

  NOTE: After removing the TStringLIst.LoadFromFile method of loading
  up the CSV Data Set, this code was not needed,but at some point I need to
  reimplement it, which is why I didn't delete this block of commented out code.

  There are cases where a user of this CSV Data Set component will add fields
  to his CSV component, then open an old CSV file, and we want a transparent
  upgrade of the customer/end-user's CSV file to contain the new CSV commas. Perhaps
  the CSV parser/stream readline component needs to have a fixup mode?
  }


// Add 1+ commas to FCsvFileAsStrings[1 .. Count-1]

(*procedure TJvCustomCsvDataSet.AppendPlaceHolderCommasToAllRows(Strings: TStrings);
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
end;   *)

procedure TJvCustomCsvDataSet.InternalOpen;
var
  AppendStr: string;
  CsvLine: RawByteString;
  Counter: Integer;
  csvFileExists: Boolean;
begin
  if FCursorOpen then
    InternalClose; // close first!

  Counter := 0;

  FFileDirty := False;
  if FLoadsFromFile then
  begin
    if FTableName = '' then
      JvCsvDatabaseError(RsENoTableName, RsETableNameRequired);

    FOpenFileName := GetFileName; // Always use the same file name to save as you did to load!!! MARCH 2004.WP
  end
  else
    FOpenFileName := '';

    // Create TField components when no persistent fields have been created
  {$IFNDEF HAS_AUTOMATIC_DB_FIELDS}
  if DefaultFields then
  {$ENDIF !HAS_AUTOMATIC_DB_FIELDS}
    CreateFields  // InternalInitFieldDefs is called inside
  {$IFDEF HAS_AUTOMATIC_DB_FIELDS}
    ;
  {$ELSE}
  else
    InternalInitFieldDefs; // initialize FieldDef objects.
  {$ENDIF HAS_AUTOMATIC_DB_FIELDS}

  BindFields(True); // bind FieldDefs to actual Data

  if FCsvColumns.Count > 1 then
  begin
    // Create a null terminated string which is just a bunch of commas:
    SetLength(FEmptyRowStr, FCsvColumns.Count - 1);
    // When adding an empty row, we add this string as the ascii equivalent:
    FillNativeChar(FEmptyRowStr[1], FCsvColumns.Count - 1, Separator);
  end
  else
    FEmptyRowStr := ''; // nothing.

  FRecordPos := JvCsv_ON_BOF_CRACK; // initial record pos before BOF
  BookmarkSize := SizeOf(Integer);
  // initialize bookmark size for VCL (Integer uses 4 bytes on 32 bit operating systems)

  csvFileExists := False;
  try
    if FLoadsFromFile then // ReadCsvFileStream:Creates file stream and start reading it. Sets FCsvFileTopLine.
      csvFileExists := ReadCsvFileStream;

    if FHasHeaderRow then
    begin
      if csvFileExists and not ExtendedHeaderInfo and (FCsvFileTopLine <> '') then
        FHeaderRow := FCsvFileTopLine
      else
      begin
        FHeaderRow := GetColumnsAsString;     // creating a new file! set up HeaderRow
        FCsvFileTopLine := FHeaderRow;
      end;

      if FHeaderRow <> '' then
        try
          ProcessCsvHeaderRow;
        except
          FHeaderRow := '';
          FreeAndNil(FCsvStream);
          raise;
        end;

      if FAppendedFieldCount > 0 then
      begin
        SetLength(AppendStr, FAppendedFieldCount);
        FillNativeChar(AppendStr[1], FAppendedFieldCount, Separator);
      end;
    end;

    // Load rows from disk to memory, using Stream object to read line by line.
    if FLoadsFromFile and Assigned(FCsvStream) then
    begin
      while not FCsvStream.Eof do
      begin
        CsvLine := JvTrimAnsiStringCrLf(FCsvStream.ReadLine);// leading space, trailing space and crlf are removed by Trim!
        if CsvLine <> '' then
        begin
          if (FSpecialDataMarker <> '')
            and (Pos(FSpecialDataMarker, CsvLine) = 1)
            and Assigned(FOnSpecialData) then
          begin
            // This very rarely used feature should
            // probably be removed from the JVCL? -WPostma.
            FOnSpecialData(Self, Counter, CsvLine);
          end
          else
          begin
            // Process the row:
            {$IFDEF UNICODE}
            if FUtf8Detected then
              ProcessCsvDataRow(Utf8ToAnsi(CsvLine), Counter)
            else
            {$ENDIF UNICODE}
              ProcessCsvDataRow(string(AnsiString(CsvLine)), Counter);
            Inc(Counter);
          end;
        end;
      end; {while}
    end;{if}
    if Active then
      First;
    FCursorOpen := True;
  except
    FCsvFileLoaded := False;
    raise;
  end;

  if FUseSystemDecimalSeparator then
    FData.DecimalSeparator := JclFormatSettings.DecimalSeparator;

  { clean up stream object }
  FreeAndNil(FCsvStream);
end;

procedure TJvCustomCsvDataSet.InternalPost;
var
  PInsertRec: PCsvRow;
  RecPos: Integer;
  KeyIndex: Integer; // If unique key enforcement is on, this is the key search Result.
begin
  if FRecordPos = JvCsv_ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = JvCsv_ON_EOF_CRACK then
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
        raise EJvCsvKeyError.CreateResFmt(@RsEKeyNotUnique, [FTableName]);
      end;
  end;

  if State = dsEdit then
  begin
    FFileDirty := True;
    RecPos := FRecordPos;
    Move(PCsvRow(ActiveBuffer)^, FData.GetRowPtr(RecPos)^, FData.GetRowAllocSize);
    FData.GetRowPtr(RecPos)^.IsDirty := RowNeedsSaving;
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
    PInsertRec := PCsvRow(AllocRecordBuffer);
    Move(PCsvRow(ActiveBuffer)^, PInsertRec^, FData.GetRowAllocSize);
    PInsertRec^.IsDirty := RowNeedsSaving;
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
    Result := FData.Count - FData.FilteredCount
  else
    Result := 0;
end;

function TJvCustomCsvDataSet.GetRecNo: Integer; {RecNo := FRecordPos+1}
var
  BufPtr: PJvMemBuffer;
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

  if Assigned(FData) and  (FData.FRecordsValid) then
  begin
    if State = dsCalcFields then
      BufPtr := TJvRecordBuffer(CalcBuffer)
    else
      BufPtr := TJvRecordBuffer(ActiveBuffer);
    Result := (PCsvRow(BufPtr)^.Bookmark.Data); // Record number.
  end
  else
   Result := 0;
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
//  FBmkFileName := ChangeFileExt(FTableName, '.bmk'); // bookmark file
end;

procedure TJvCustomCsvDataSet.SetTextBufferSize(const Value: Integer);
begin
  if Active then
    raise Exception.Create('Can''t change memory properties on an active data set');
  Assert(Assigned(FData));
  FData.TextBufferSize := Value;
end;

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

   // everything sorts via string sort (default) or numeric sort
   // (the only special case so far!)
  case Column^.FFlag of
    jcsvNumeric:
      begin
        NumLeft := JvCsvStrToFloatDef(StrLeft, -99999.9, GetDecimalSeparator);
        NumRight := JvCsvStrToFloatDef(StrRight, -99999.9, GetDecimalSeparator);
        Diff := NumLeft - NumRight;
        if Diff < -0.02 then
          Result := -1
        else
        if Diff > 0.02 then
          Result := 1
        else
          Result := 0; // For our purposes, .02 difference or less is a match.
      end;
  else
    if FCsvCaseInsensitiveComparison then
      Result := {Ansi}CompareText(StrLeft, StrRight)
    else
      Result := CompareStr(StrLeft, StrRight);
  end;
end;

// InternalCompare of multiple fields.
// INTERNAL USE ONLY METHOD:
// Record comparison between two PCsvRows:
// Returns 0 if Left=Right, 1 if Left>Right, -1 if Left<Right

function TJvCustomCsvDataSet.InternalCompare(const SortColumns: TArrayOfPCsvColumn;
  SortColumnCount: Integer; Left, Right: PCsvRow; const SortAscending: array of Boolean): Integer;
var
  I: Integer;
begin
  Result := 0;
  Assert(Length(SortAscending) >= SortColumnCount);

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
      if not SortAscending[I] then // inverts comparison Result when Descending!
        Result := -Result;
           // XXX REPEAT Result := InternalFieldCompare(SortColumns[I],Left,Right);
      Exit; // found greater or less than condition
    end;
  end;
  // now we have compared all fields, and if we get here, they were all
  // equal, and Result is already set to 0.
end;

procedure TJvCustomCsvDataSet.InternalQuickSort(SortList: PPointerList;
  L, R: Integer; const SortColumns: TArrayOfPCsvColumn; ACount: Integer; const SortAscending: array of Boolean);
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
      while InternalCompare(SortColumns, ACount, SortList^[I], P, SortAscending) < 0 do
        Inc(I);
      while InternalCompare(SortColumns, ACount, SortList^[J], P, SortAscending) > 0 do
        Dec(J);
      if I <= J then
      begin
        if I <> J then
        begin
          T := SortList^[I];
          SortList^[I] := SortList^[J];
          SortList^[J] := T;
        end;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      InternalQuickSort(SortList, L, J, SortColumns, ACount, SortAscending);
    L := I;
  until I >= R;
end;

procedure TJvCustomCsvDataSet.QuickSort(AList: TList; const SortColumns: TArrayOfPCsvColumn;
  ACount: Integer; const SortAscending: array of Boolean);
begin
  if (AList <> nil) and (AList.Count > 1) then
    InternalQuickSort({$IFDEF RTL230_UP}@{$ENDIF RTL230_UP}AList.List, 0, AList.Count - 1, SortColumns, ACount, SortAscending);
end;

procedure TJvCustomCsvDataSet.Sort(const SortFields: string; Ascending: Boolean);
var
//  Index: array of Pointer;
//  swap: Pointer;
  SortFieldNames: array of string;
  SortAscending: array of Boolean;
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
  SetLength(SortAscending,  FCsvColumns.Count);
  SortColumnCount := JvStrSplit(SortFields, Separator, {Chr(0)=No Quoting} #0, SortFieldNames, FCsvColumns.Count);
  SetLength(SortColumns, SortColumnCount);
  if (SortFields = '') or (SortColumnCount = 0) then
    JvCsvDatabaseError(FTableName, RsESortFailedCommaSeparated);

  // Now check if the fields exist, and find the pointers to the fields
  for I := 0 to SortColumnCount - 1 do
  begin
    SortAscending[I] := Ascending;
    if SortFieldNames[I] = '' then
      JvCsvDatabaseError(FTableName, RsESortFailedFieldNames);
    if SortFieldNames[I][1] = '!' then
    begin
      SortAscending[I] := not SortAscending[I];
      SortFieldNames[I] := Copy(SortFieldNames[I],2,Length(SortFieldNames[I]));
    end;
    SortColumns[I] := FCsvColumns.FindByName(string(SortFieldNames[I]));
    if not Assigned(SortColumns[I]) then
      JvCsvDatabaseError(FTableName, Format(RsESortFailedInvalidFieldNameInList, [SortFieldNames[I]]));
  end;
  QuickSort(FData, SortColumns, SortColumnCount, SortAscending);
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
        if SameText(Result.FFieldDef.Name, FieldName) then
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
  end;
  Result := nil;
end;

procedure TJvCsvColumns.Clear;
var
  I: Integer;
begin
  for I := 0 to Count - 1 do
    FreeMem(Items[I]);
  inherited Clear;
end;

{ CsvRows: dynamic array of pointers }

constructor TJvCsvRows.Create;
begin
  FTextBufferSize := JvCsvDefaultTextBufferSize;
  FMarginSize := JvCsvDefaultMarginSize;

{ DecimalSeparator:
  This 'US' constant value is important for backwards compatibility.
  DO NOT CHANGE this default, it would break people's code.
}
  FDecimalSeparator := USDecimalSeparator;
end;

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
  if Item.Filtered then
    Inc(FFilteredCount);
end;

procedure TJvCsvRows.InsertRow(const Position: Integer; Item: PCsvRow);
begin
  Insert(Position, Pointer(Item));
  if Item.Filtered then
    Inc(FFilteredCount);
end;

procedure TJvCsvRows.InternalInitRecord(Buffer: TJvRecordBuffer);
var
  RowPtr: PCsvRow;
  P: PByte;
begin
  RowPtr := PCsvRow(Buffer);
  RowPtr.Magic := JvCsvRowMagic;
  RowPtr.TextMaxLen := FTextBufferSize;
  RowPtr.WordFieldsAddr := (SizeOf(TJvCsvRow) - JvCsv_MINLINELENGTH) + FTextBufferSize;
  RowPtr.AllocSize := GetRowAllocSize;
  RowPtr.Separator := AnsiChar(Separator);

  // initialize magic in WordFields:
  P := Buffer;
  Inc(P, RowPtr.WordFieldsAddr);
  PJvCsvRowWordFields(P)^.Magic2 := JvCsvRowMagic2;
  //DebugPJvCsvRowWordFields := PJvCsvRowWordFields(P);

  FRecordsValid := True;
end;

function TJvCsvRows.RecordSize: Word;
begin
  Result := GetRowAllocSize;  { - BookmarkSize??? }
end;

procedure TJvCsvRows.AddRowStr(const Item: string); // convert string->TJvCsvRow
var
  PNewItem: PCsvRow;
begin
  PNewItem := PCsvRow(AllocRecordBuffer);
  JvStringToCsvRow(Item, Separator, PNewItem, True, FEnquoteBackslash); // decode a csv line that can contain escape sequences
  AddRow(PNewItem);
end;

function TJvCsvRows.AllocRecordBuffer: PJvMemBuffer;
begin
  Assert(FTextBufferSize >= JvCsv_MINLINELENGTH);
  Result := AllocMem(GetRowAllocSize);  {was SizeOf(TJvCsvRow)}
  InternalInitRecord(Result);
end;

function TJvCsvRows.GetRowAllocSize: Integer;
begin
  Assert(FTextBufferSize >= JvCsv_MINLINELENGTH);
  Result := (SizeOf(TJvCsvRow) - JvCsv_MINLINELENGTH) + FTextBufferSize + SizeOf(TJvCsvRowWordFields) + JvCsv_MaxCalcDataOffset + FMarginSize;
end;

function TJvCsvRows.GetRowPtr(const RowIndex: Integer): PCsvRow;
begin
  if (RowIndex >= 0) and (RowIndex < Count) then
    Result := PCsvRow(Get(RowIndex)) // return pointer to a row item.
  else
    raise EJvCsvDataSetError.CreateRes(@RsECsvNoRecord); { NO Such Record }
end;

function TJvCsvRows.GetRowAnsiStr(const RowIndex: Integer): string;
begin
  JvCsvRowToString(GetRowPtr(RowIndex), Result);
end;

procedure TJvCsvRows.SetRowStr(const RowIndex: Integer; Value: string);
begin
  JvStringToCsvRow(Value, Separator, GetRowPtr(RowIndex), True, FEnquoteBackslash);
end;

procedure TJvCsvRows.DeleteRow(const RowIndex: Integer);
var
  P: Pointer;
begin
  if (RowIndex >= 0) and (RowIndex < Count) then
  begin
    if PCsvRow(List{$IFNDEF RTL230_UP}^{$ENDIF !RTL230_UP}[RowIndex]).Filtered then
      Dec(FFilteredCount);
    P := Items[RowIndex];
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
    FreeMem(Items[I]);
  FFilteredCount := 0;
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

procedure TJvCustomCsvDataSet.AssignFromStrings(Strings: TStrings);
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
//  FData.EnquoteBackslash := FEnquoteBackslash; {stupidity solved}

  IndexCounter := 0;
  // Skip first Row:
  if FHasHeaderRow then
    StartIndex := 1
  else
    StartIndex := 0;

  for I := StartIndex to Strings.Count - 1 do
  begin
    if Assigned(FOnSpecialData) and (FSpecialDataMarker <> '') and
       (Pos(FSpecialDataMarker, AnsiString(Strings[I])) = 1) then
    begin
        // XXX Deprecated feature
      FOnSpecialData(Self, I, AnsiString(Strings[I]));
    end
    else
    begin
      // Process the row normally:
      ProcessCsvDataRow(Strings[I], IndexCounter);
      Inc(IndexCounter);
    end;
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
      JvCsvRowToString(FData.GetRowPtr(I), Line);
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

function TJvCustomCsvDataSet.GetAsString(const Row, Column: Integer): string;
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

function TJvCustomCsvDataSet.CurrentRowAsString: string;
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
  JvCsvRowToString(FData.GetRowPtr(GetIndex), Result);
end;

function TJvCustomCsvDataSet.GetRowAsAnsiString(const Index: Integer): AnsiString;
begin
  Result := AnsiString(GetRowAsString(Index));
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
    Result := Result + string(Separator) + FieldDefs[I].Name;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }

procedure TJvCustomCsvDataSet.ProcessCsvHeaderRow;
var
  CsvFieldRec: PCsvRow; // CSV Field record type.
  PtrCsvColumn: PCsvColumn;
  CsvFieldName: string;
  ColNum, I: Integer;
  ColonPos: Integer;
begin
  CsvFieldRec := PCsvRow(FData.AllocRecordBuffer);
  try
    if not ValidateHeaderRow then
    begin
      for I := 0 to FCsvColumns.Count - 1 do
        PCsvColumn(FCsvColumns[I])^.FPhysical := I;
      Exit;
    end;
    FAppendedFieldCount := 0;
    //  Columns Not Yet Found:
    for I := 0 to FCsvColumns.Count - 1 do
      PCsvColumn(FCsvColumns[I])^.FPhysical := -1;

    // Do initial parse.
    JvStringToCsvRow(FHeaderRow, Separator, CsvFieldRec, False, False);
    ColNum := 0;
    while CsvRowGetColumnMarker(CsvFieldRec, ColNum) <> JvCsv_COLUMN_ENDMARKER do
    begin
      // Get a string in the format COLUMNAME:Options
      CsvFieldName := JvTrimStringCrLf(StrEatWhiteSpace(GetCsvRowItem(CsvFieldRec, ColNum))); // Where did #A come from?

      // Mantis 3192: Remove the options from the field name or FindByName will
      // never find the column which will lead to a Database error being triggered
      ColonPos := Pos(':', CsvFieldName);
      if (ColonPos > 0) then
        CsvFieldName := Copy(CsvFieldName, 1, ColonPos - 1);

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
        end;
      end;
    end;
  finally
    FreeMem(CsvFieldRec);
  end;
end;

procedure TJvCustomCsvDataSet.ProcessCsvDataRow(const DataRow: string; Index: Integer);
var
  PNewRow: PCsvRow;
begin
  if DataRow = '' then
    Exit;
  if Length(DataRow) >= JvCsv_MAXLINELENGTH - 1 then
    raise EJvCsvDataSetError.CreateResFmt(@RsECsvStringTooLong, [Copy(DataRow, 1, 40)]);
  PNewRow := PCsvRow(AllocRecordBuffer);
  JvStringToCsvRow(DataRow, Separator, PNewRow, True, EnquoteBackslash);
  PNewRow^.Index := Index;
  FData.AddRow(PNewRow);
end;

procedure TJvCustomCsvDataSet.AutoCreateDir(const FileName: string);
var
  Path: string;
begin
  if CreatePaths then
  begin
    Path := ExtractFilePath(FileName);
    if Path <> '' then
      if not DirectoryExists(Path) then
        ForceDirectories(Path);
  end;
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
  StrList.Add(string(FHeaderRow));
  try
    for I := FromRow to ToRow do
      StrList.Add(string(FData.GetRowAnsiStr(I)));
    AutoCreateDir(FileName);
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
    AutoCreateDir(FileName);
    Strings.SaveToFile(FileName);
  finally
    Strings.Free;
  end;
end;

{ GetCsvHeader:

  XXX Normally you don't need to call this function, if you
      have already opened the file, then use the HeaderRow
      property instead!

  This routine loads just the first line of a CSV file
  from the disk. This way you can peek at a CSV file, get
  the first line, parse it or do something else with it, before
  you decide if you want this component to open the rest
  of the file.
}
function TJvCustomCsvDataSet.GetCsvHeader: string;
var
  F: Text;
  FirstLine: string;
begin
  if not FLoadsFromFile or not FHasHeaderRow or not FileExists(FTableName) then
  begin
    Result := '';
    Exit;
  end;

  { XXX THIS Set of code sometimes FAILS randomly on Delphi 2007
     when applications are running on Vista.  TODO: REWRITE using
     streams, get rid of all AssignFile/Reset/ReadLn(file) code! }
  { How's this for an ancient Pascal code sequence, AssignFile+Reset is approximately equal to a C fopen() call }
  AssignFile(F, FTableName);
  Reset(F);
  { ReadLn is approximately a gets() call }
  ReadLn(F, FirstLine);
  { And finally, the pascal file close procedure }
  CloseFile(F);

  // return the first line of the file, without the junk
  Result := JvStrStrip(FirstLine); // in JvCsvParse.pas
end;

function TJvCustomCsvDataSet.GetDecimalSeparator: Char;
begin
  Result := FData.DecimalSeparator;
end;

{ PROCEDURES: }

// convert CSV Row buffer to a single-byte AnsiString
procedure JvCsvRowToAnsiString(RowItem: PCsvRow; var RowString: AnsiString);
var
  RowStr: string;
begin
  JvCsvRowToString(RowItem, RowStr);
  RowString := AnsiString(RowStr);
end;

procedure JvCsvRowToString(RowItem: PCsvRow; var RowString: string);
var
  P: PAnsiChar;
begin
  p := @RowItem^._Text[0];
  {$IFDEF COMPILER12_UP}
  RowString := Utf8ToAnsi(P);
  {$ELSE}
  RowString := P;
  {$ENDIF COMPILER12_UP}
end;

//JvCsvRowDeleteColumn: NEW 2007
// used by TJvCustomCsvDataSet.DeleteCsvColumn...
// delete a column from a PCsvRow object:
function JvCsvRowDeleteColumn(RowItem: PCsvRow; Column, Count: Integer): RawByteString;
var
  From1, From2: Integer;
  To1, To2: Integer;
  RowItemText: PAnsiChar;
begin
  Assert(Column >= 0);
  RowItemText := @RowItem^._Text[0];
  if (Column > 0) and (Column < Count - 1) then
  begin
    { del middle Column}
    From1 := 0;
    To1 := CsvRowGetColumnMarker(RowItem, Column) - 1; //RowItem^.WordField[Column];
    From2 := CsvRowGetColumnMarker(RowItem, Column + 1); //RowItem^.WordField[Column];
    To2 := CsvRowGetColumnMarker(RowItem, Count);

    Result := Copy(RowItemText, From1, To1) + Copy(RowItemText, From2, To2);
  end else
  if Column= Count - 1 then
  begin
    {del last Column}
    From1 := 0;
    To1 := CsvRowGetColumnMarker(RowItem, Column)-1;
    Result := Copy(RowItemText,From1,To1);
  end else
  begin
    {del first Column }
    From1 := CsvRowGetColumnMarker(RowItem, Column + 1) + 1; //RowItem^.WordField[Column];
    To1 := CsvRowGetColumnMarker(RowItem, Count);
    Result := Copy(RowItemText, From1, To1);
  end;
end;

// convert string into a CSV Row buffer
procedure JvAnsiStringToCsvRow(const RowString: RawByteString; Separator: AnsiChar;
  RowItem: PCsvRow; PermitEscapeSequences, EnquoteBackslash: Boolean);
var
  I, L, Col: Integer;
  QuoteFlag: Boolean;
  SkipFlag: Boolean;
//  CharsInColumn: Integer; { was part of validating that row did not exceed limits}
  WordFields: PJvCsvRowWordFields;
  RowItemText: PAnsiChar;
begin
  Assert(Assigned(RowItem));
  Assert(RowItem^.Magic = JvCsvRowMagic, 'Internal data corruption in JvCsvRow data storage area');
  Col := 0;
  WordFields := GetWordFields(RowItem);
  WordFields.WordField[0] := 0; // zero out column marker and dirty bit!
//  CharsInColumn := 0;
  QuoteFlag := False;
  SkipFlag := False;
  L := Length(RowString);
  for I := 1 to L do
  begin
    //Inc(CharsInColumn);
    if QuoteFlag then
    begin
      // backslash permitted only if specifically enabled by FEnquoteBackslash:
      if PermitEscapeSequences and not SkipFlag and EnquoteBackslash and (RowString[I] = '\') then
      begin
        SkipFlag := True;
        Continue;
      end;
      // doubled quotes handling:
      if PermitEscapeSequences and not SkipFlag and (RowString[I] = '"') and
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
      if PermitEscapeSequences then
        QuoteFlag := not QuoteFlag;

    if (RowString[I] = Separator) and not QuoteFlag then
    begin
      Inc(Col);
      // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
      WordFields.WordField[Col] := (Word(I) and $7FFF); {note that we're going from 1..length }
      //CharsInColumn := 0;
    end;
    if (Col >= JvCsv_MAXCOLUMNS) or (I >= JvCsv_MAXLINELENGTH) then
      raise ERangeError.CreateResFmt(@RsEInternalLimit, [JvCsv_MAXCOLUMNS]);
  end; // end of string, new flag:
  Inc(Col);

  { if we were tracking it, we could check CharsInColumn not too large, here? }

  WordFields.WordField[Col] := JvCsv_COLUMN_ENDMARKER; // last one has no end marker
  RowItemText := @RowItem._Text[0];
  StrLCopyA(RowItemText, PAnsiChar(RowString), RowItem.TextMaxLen);

  RowItem.Columns := Col; // Check this later!
end;

procedure JvStringToCsvRow(const RowString: string; Separator: Char;
  RowItem: PCsvRow; PermitEscapeSequences, EnquoteBackslash: Boolean);
begin
  {$IFDEF COMPILER12_UP}
  JvAnsiStringToCsvRow(AnsiToUtf8(RowString), AnsiChar(Separator), RowItem, PermitEscapeSequences, EnquoteBackslash);
  {$ELSE}
  JvAnsiStringToCsvRow(RowString, Separator, RowItem, PermitEscapeSequences, EnquoteBackslash);
  {$ENDIF COMPILER12_UP}
end;

// Copy a single column from one row buffer to another row buffer:

function CsvRowItemCopy(Source, Dest: PCsvRow; FieldIndex, FieldSize: Integer): Boolean;
var
  TempStr: RawByteString;
begin
  TempStr := GetCsvRowItemData(Source, FieldIndex);
  // length limiting feature:
  if FieldSize > 0 then
    if Length(TempStr) > FieldSize then
      TempStr := Copy(TempStr, 1, FieldSize);
  SetCsvRowItemData(Dest, FieldIndex, TempStr);
  Result := True;
end;

// Copy an item into a csv row buffer:

procedure SetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: string);
begin
  {$IFDEF COMPILER12_UP}
  SetCsvRowItemData(PItem, ColumnIndex, AnsiToUtf8(NewValue));
  {$ELSE}
  SetCsvRowItemData(PItem, ColumnIndex, AnsiString(NewValue));
  {$ENDIF COMPILER12_UP}
end;

procedure SetCsvRowItemData(PItem: PCsvRow; ColumnIndex: Integer; const NewValue: RawByteString);
var
  TempBuf, RowItemText: PAnsiChar;
  Copy1, Copy2: Integer;
  Diff, I, Old: Integer;
begin
  Assert(Assigned(PItem));
  Assert(PItem^.Magic = JvCsvRowMagic);
  Assert(PItem^.TextMaxLen>0);
  Assert(PItem^.Separator<>Chr(0));
  Diff := 0;
  if (ColumnIndex < 0) or (ColumnIndex > JvCsv_MAXCOLUMNS) then
    Exit;
  RowItemText := @PItem^._Text[0];
  Copy1 := CsvRowGetColumnMarker(PItem, ColumnIndex);
  if Copy1 = JvCsv_COLUMN_ENDMARKER then
  begin
    // Fix CSV damage:
    StrLCatA(RowItemText, @(PItem^.Separator), Length(RowItemText) + 1); // XXX todo later: Make sure we add however many commas or whatever are needed!
    Copy1 := StrLenA(RowItemText);
    CsvRowSetColumnMarker(PItem, ColumnIndex, Copy1);
  end;
    // Update new rows:  FIX previous fix!
  if (ColumnIndex >= PItem^.Columns) then
      PItem^.Columns := ColumnIndex+1;

  if Copy1 > JvCsv_MAXLINELENGTH then
    Exit;
 // copy initial part of the csv row:

  TempBuf := AllocMem(PItem^.TextMaxLen + 1);
  try
    if Copy1 > 0 then
    begin
      StrLCopyA(TempBuf, RowItemText, Copy1);
      StrLCatA(TempBuf, PAnsiChar(NewValue), PItem^.TextMaxLen);
    end
    else
      StrLCopyA(TempBuf, PAnsiChar(NewValue), PItem^.TextMaxLen);

    Copy2 := CsvRowGetColumnMarker(PItem, ColumnIndex + 1);
    if Copy2 <> JvCsv_COLUMN_ENDMARKER then
    begin
      // difference in length:
      Dec(Copy2); // subtract one.
      if Copy2 < 0 then
        Exit;
      if Length(NewValue) = Copy2 - Copy1 then
        Diff := 0
      else
        Diff := Length(NewValue) - (Copy2 - Copy1);
      StrLCatA(TempBuf, RowItemText + Copy2, PItem^.TextMaxLen);
    end;

    // Copy over the old memory buffer:
    StrLCopyA(RowItemText, TempBuf, PItem^.TextMaxLen);

    // Now that we've copied a new item of a different length into the place of the old one
    // we have to update the positions of the columns after ColumnIndex:
    if Diff <> 0 then
      for I := ColumnIndex + 1 to JvCsv_MAXCOLUMNS do
      begin
        Old := CsvRowGetColumnMarker(PItem, I);
        if Old = JvCsv_COLUMN_ENDMARKER then
          Exit;
        CsvRowSetColumnMarker(PItem, I, Old + Diff);
      end;

  finally
    FreeMem(TempBuf);
  end;
end;

// Copy an item out of a csv row buffer:

function GetCsvRowItem(PItem: PCsvRow; ColumnIndex: Integer): string;
begin
  {$IFDEF COMPILER12_UP}
  Result := Utf8ToAnsi(GetCsvRowItemData(PItem, ColumnIndex));
  {$ELSE}
  Result := GetCsvRowItemData(PItem, ColumnIndex);
  {$ENDIF COMPILER12_UP}
end;

function GetCsvRowItemData(PItem: PCsvRow; ColumnIndex: Integer): RawByteString;
var
  TempBuf: PAnsiChar;
  Copy1, Copy2: Integer;
  OutText: PAnsiChar;
begin
  Result := '';
  if (ColumnIndex < 0) or (ColumnIndex > JvCsv_MAXCOLUMNS) then
    Result := AnsiString(RsErrorRowItem)
  else
  if ColumnIndex < PItem^.Columns then
  begin
    Copy1 := CsvRowGetColumnMarker(PItem, ColumnIndex);
    Copy2 := CsvRowGetColumnMarker(PItem, ColumnIndex + 1);
    if (Copy1 <> JvCsv_COLUMN_ENDMARKER) and (Copy1 > Copy2) then
      raise Exception.Create('GetCsvRowItemData: Column index values are corrupt.');

    if Copy1 <> JvCsv_COLUMN_ENDMARKER then
    begin
      if Copy2 = JvCsv_COLUMN_ENDMARKER then // copy the rest of the line
        Copy2 := JvCsv_MAXLINELENGTH - Copy1 // All the characters left in the buffer
      else
        Dec(Copy2);

      if (Copy1 <= JvCsv_MAXLINELENGTH) and (Copy2 <= JvCsv_MAXLINELENGTH) then
      begin
       // Copy out just one column from the string:
        TempBuf := AllocMem(PItem^.TextMaxLen);
        try
          OutText := @PItem^._Text[0];
          Inc(OutText, Copy1);
          StrLCopyA(TempBuf, OutText, Copy2 - Copy1);
          JvEatWhitespaceChars(PAnsiChar(@TempBuf[0]));
          Result := TempBuf;
        finally
          FreeMem(TempBuf);
        end;
      end;
    end;
  end;
end;

{new}

procedure CsvRowSetDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
var
  WordFields:PJvCsvRowWordFields;
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= JvCsv_MAXCOLUMNS) then
    Exit;
  WordFields := GetWordFields(Row);
  Row^.IsDirty := RowNeedsSaving; // triggers search for 'dirty bit' in columns
  WordFields.WordField[ColumnIndex] := (WordFields.WordField[ColumnIndex] or $8000);
end;

procedure CsvRowClearDirtyBit(Row: PCsvRow; ColumnIndex: Integer);
var
  WordFields:PJvCsvRowWordFields;
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= JvCsv_MAXCOLUMNS) then
    Exit;
  WordFields := GetWordFields(Row);
  WordFields.WordField[ColumnIndex] := (WordFields.WordField[ColumnIndex] and $7FFF);
end;

function CsvRowGetDirtyBit(Row: PCsvRow; ColumnIndex: Integer): Boolean;
var
  WordFields:PJvCsvRowWordFields;
begin
  Result := False;
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= JvCsv_MAXCOLUMNS) then
    Exit;
  WordFields := GetWordFields(Row);
  if WordFields.WordField[ColumnIndex] = JvCsv_COLUMN_ENDMARKER then
    Exit;
  Result := (WordFields.WordField[ColumnIndex] and $8000) <> 0;
end;

procedure CsvRowSetColumnMarker(Row: PCsvRow; ColumnIndex: Integer; ColumnMarker: Integer);
var
  Old: Word;
  WordFields:PJvCsvRowWordFields;
begin
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= JvCsv_MAXCOLUMNS) then
    Exit;
  if ColumnMarker < 0 then
    Exit;
  WordFields := GetWordFields(Row);

  if ColumnMarker = JvCsv_COLUMN_ENDMARKER then
    WordFields.WordField[ColumnIndex] := JvCsv_COLUMN_ENDMARKER
  else
  begin
    Old := WordFields.WordField[ColumnIndex];
    if Old = JvCsv_COLUMN_ENDMARKER then
      WordFields.WordField[ColumnIndex] := ColumnMarker and $7FFF // auto-clear Dirty bit
    else
      WordFields.WordField[ColumnIndex] := (Old and $8000) or // Keep Old Dirty Bit
        (Word(ColumnMarker) and $7FFF); // new value.
  end;
  //DebugPJvCsvRowWordFields := WordFields;
end;

function CsvRowGetColumnMarker(Row: PCsvRow; ColumnIndex: Integer): Integer;
var
  W: Word;
  WordFields:PJvCsvRowWordFields;
begin
  Result := -1;
  if Row = nil then
    Exit;
  if (ColumnIndex < 0) or (ColumnIndex >= JvCsv_MAXCOLUMNS) then
    Exit;
  WordFields := GetWordFields(Row);
  W := WordFields^.WordField[ColumnIndex];
  if W = JvCsv_COLUMN_ENDMARKER then
    Result := JvCsv_COLUMN_ENDMARKER
  else
    Result := Integer(W and $7FFF);
end;

{endnew}

 //------------------------------------------------------------------------------
 // TimeTHexToDateTime
 //
 // The Delphi TDateTime format is a whole number representing days since Dec 30, 1899.
 // Whereas the TIME_T type is a standard C library time representing seconds since
 // 12:00 UTC on Jan 1 1970.
 //
 // We accept the hex encoded TIME_T and convert it to a TDateTime by getting
 // the base date (Jan 1, 1970) and adding 1 for every day since then, and
 // a fractional part representing the seconds.  By dividing the seconds
 // by the number of seconds in a day (24*24*60=86400) we obtain this Result.
 //
 // The incoming value in hex will roll over in mid-Janary 2038, and
 // hopefully by then this code won't be in use any more! :-)
 //
 // Note: TDateTime is really a Double floating-point and zero is considered
 // an invalidate date indicator. (on screen this appears as 1899/nn/nn
 //------------------------------------------------------------------------------

function JvTimeTHexToDateTime(const HexStr: string; TimeZoneCorrection: Integer): TDateTime;
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

// JvIsoDateTimeStrToDateTime [formerly TimeTAsciiToDateTime]
function JvIsoDateTimeStrToDateTime(const AsciiDateTimeStr: string): TDateTime;
const
  Separators  =  '// ::'; // separators in yyyy/mm/dd hh:mm:ss
  Separators2 =  '-- --'; // separators in yyyy/mm/dd hh:mm:ss
var
  Values: array [1..6] of Integer; //year,month,day,hour,minute,second in that order.
  Ch: Char;
  I, U, Len, Index: Integer;
begin
  Result := 0.0; // default Result.
  Len := Length(AsciiDateTimeStr);

 // validate ranges:
  for I := 1 to 6 do
    Values[I] := 0;

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiDateTimeStr are we looking at?
  for I := 1 to 6 do
  begin
    if (I >= 3) and (Index >= Len) then
      Break; // as long as we at least got the date, we can continue.
    for U := 1 to AsciiTime_ExpectLengths[I] do
    begin
      if Index > Len then
        Break;
      Ch := AsciiDateTimeStr[Index];
      if not ((Ch <= #255) and (AnsiChar(Ch) in DigitSymbols)) then
      begin
        //could raise exception here:
        //illegal character in datetime string
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
      if (AsciiDateTimeStr[Index] <> Separators[I]) and
        (AsciiDateTimeStr[Index] <> Separators2[I]) then
      begin
        Exit;
      end;

    // validate ranges:
    if (Values[I] < AsciiTime_MinValue[I]) or (Values[I] > AsciiTime_MaxValue[I]) then
      Exit; // a value is out of range.

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

function JvIsoDateStrToDate(const AsciiDateStr: string): TDateTime; // new.
const
  Separators = '//'; // separators in yyyy/mm/dd [a custom nearly-ISO-compliant format]
  Separators2 = '--'; // separators in yyyy-mm-dd [the real ISO date format uses dashes as separators]
var
  Values: array [1..3] of Integer; //year,month,day
  Ch: Char;
  I, U, Len, Index: Integer;
begin
  Result := 0.0; // default Result.
  Len := Length(AsciiDateStr);

 // validate ranges:
  for I := 1 to 3 do
    Values[I] := 0;

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiDateStr are we looking at?
  for I := 1 to 3 do
  begin
    //if (I >= 3) and (Index >= Len) then
    //  Break; // as long as we at least got the date, we can continue.
    for U := 1 to AsciiTime_ExpectLengths[I] do
    begin
      if Index > Len then
        Break;
      Ch := AsciiDateStr[Index];
      if not ((Ch <= #255) and (AnsiChar(Ch) in DigitSymbols)) then
        Exit; // failed: invalid character.

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
        Exit; // illegal separator character
      end;

    // validate ranges:
    if (Values[I] < AsciiTime_MinValue[I]) or (Values[I] > AsciiTime_MaxValue[I]) then
    begin
      Exit; // a value is out of range.
    end;
    Inc(Index);
  end;

  // Now that we probably have a valid value we will try to encode it.
  // EncodeData will catch any invalid date values we have let slip through
  // such as trying to encode February 29 on a non-leap year, or the 31st
  // day of a month with only 30 days, etc.
  try
    Result := EncodeDate({year}Values[1], {month} Values[2], {day} Values[3]);
  except
    on E: EConvertError do
      Result := 0.0; // catch any other conversion errors and just return 0.
  end;
end;

function JvIsoTimeStrToTime(const AsciiTimeStr: string): TDateTime; // new.
const
  Separators = '::'; // separators hh:mm:ss
  Separators2 = '--'; // separators hh-mm-ss
var
  Values: array [1..3] of Integer; //year,month,day,hour,minute,second in that order.
  Ch: Char;
  I, U, Len, Index: Integer;
begin
  Result := -1.0; // default Result. (flag for invalid time. 0.0 is a VALID time = midnight!)
  Len := Length(AsciiTimeStr);

 // validate ranges:
  for I := 1 to 3 do
    Values[I] := 0;

 // T loops through each value we are looking for (1..6):
  Index := 1; // what character in AsciiTimeStr are we looking at?
  for I := 1 to 6 do
  begin
    if (I >= 3) and (Index >= Len) then
      Break; // as long as we at least got the date, we can continue.

    {Note: AsciiTime_ExpectLengths[ 3+I]:skip to the ISO time fields widths!}
    for U := 1 to AsciiTime_ExpectLengths[3+I] do
    begin
      if Index > Len then
        Break;
      Ch := AsciiTimeStr[Index];
      if not ((Ch <= #255) and (AnsiChar(Ch) in DigitSymbols)) then
        Exit; // failed: invalid character.
      Values[I] := (Values[I] * 10) + (Ord(Ch) - Ord('0'));
      Inc(Index);

      if Index > Len then
        Break;
    end;

    // if we haven't reached the end of the string, then
    // check for a valid separator character:
    if Index < Len then
      if (AsciiTimeStr[Index] <> Separators[I]) and
         (AsciiTimeStr[Index] <> Separators2[I]) then
        Exit;

    // validate ranges:
    if (Values[I] < AsciiTime_MinValue[3+I]) or (Values[I] > AsciiTime_MaxValue[3+I]) then
      Exit; // a value is out of range.

    Inc(Index);
  end;

  // Now that we probably have a valid value we will try to encode it.
  // EncodeData will catch any invalid date values we have let slip through
  // such as trying to encode February 29 on a non-leap year, or the 31st
  // day of a month with only 30 days, etc.
  try
    Result := EncodeTime({hour}Values[1], {minute} Values[2], {second} Values[3], {msec} 0);
  except
    on E: EConvertError do
      Result := -1.0; // catch any other conversion errors and just return 0.  //ahuser: ??? -1.0 is not 0.0
  end;
end;

function JvDateTimeToTimeTHex(ADateTime: TDateTime; TimeZoneCorrection: Integer): string;
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

// JvDateTimeIsoStr [formerly called DateTimeToTimeToIsoAscii]
// [support function for DateTime ASCII CSV column type]
// Name changed by Warren. Only used internally here. The name was misleading,
// complex, and bizarre. It had to change. Curse me or Thank me as you like. :-)
function JvDateTimeIsoStr(ADateTime: TDateTime): string;
begin
  // ISO DATETIME FORMAT:
  Result := FormatDateTime('yyyy-mm-dd hh:nn:ss', ADateTime);
end;

// new: JvDateIsoStr - support function for ASCII DATE csv column type
function JvDateIsoStr(ADateTime: TDateTime): string;
begin
  // ISO DATE FORMAT:
  Result := FormatDateTime('yyyy-mm-dd', ADateTime);
end;

// new: JvTimeIsoStr - support function for ASCII TIME csv column type
function JvTimeIsoStr(ADateTime: TDateTime): string;
begin
  // ISO DATE FORMAT:
  Result := FormatDateTime('hh:nn:ss', ADateTime);
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
    ForceDirectories(BackupFolder);
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


  Windows.CopyFile(PChar(FileName), PChar(BackupFilename), False);
  Result := True;
end;

procedure TJvCustomCsvDataSet.SetSeparator(const Value: Char);
var
  S: string;
begin
  if Separator <> Value then
  begin
    if (Value <= #255) and (AnsiChar(Value) in cInvalidSeparators) then
    begin
      case Value of
        #32..#255:
          S := Value
      else
        S := Format('#%.2d',[Ord(Value)]);
        raise EJvCsvDataSetError.CreateResFmt(@RsECsvInvalidSeparatorFmt,[S]);
      end;
    end;
    FData.Separator := Value;
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

  if (DataSet.FData.GetRowAllocSize <> FData.GetRowAllocSize) then
    raise EJvCsvDataSetError.Create('TJvCustomCsvDataSet Internal Error. GetRowAllocSize mismatch.'); // TODO Add to resources!

  {copy the memory record }
  CopyMemory(FData[RecNo], DataSet.FData[DataSet.RecNo], FData.GetRowAllocSize);

  PCsvRow(FData[RecNo])^.IsDirty := RowNeedsSaving;
  FFileDirty := True;

  //DataSet.Last; // Force update of screen.

  Resync([]); // Update Data aware controls.
end;

// get contents of one dataset into this dataset. copies only fields that
// match. Raises an exception if an error occurs. returns # of rows copied.

function TJvCustomCsvDataSet.CopyFromDataset(DataSet: TDataSet): Integer;
var
  I, MatchFieldCount: Integer;
  StrValue, FieldName: string;
  MatchSourceField: array of TField;
  MatchDestField: array of TField;
begin
  // Result := -1;
  SetLength(MatchSourceField, FieldCount);
  SetLength(MatchDestField, FieldCount);
  MatchFieldCount := 0;
  for I := 0 to DataSet.FieldCount-1 do
  begin
    MatchSourceField[MatchFieldCount] := DataSet.Fields[I];
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
  if MatchFieldCount = 0 then
    JvCsvDatabaseError(DataSet.Name, RsENoFieldNamesMatch);
  Result := 0;
  DataSet.First;
  if not Active and not LoadsFromFile then
    Active := True;

  while not DataSet.Eof do
  begin
    Append;
    for I := 0 to MatchFieldCount-1 do
      if MatchSourceField[I].DataType=ftString then
      begin
        if MatchSourceField[I].IsNull then
          StrValue := ''
        else
          StrValue := MatchSourceField[I].Value;
        MatchDestField[I].Value := StrValue;
      end
      else
        MatchDestField[I].Value := MatchSourceField[I].Value;
    Post;
    DataSet.Next;
    Inc(Result);
  end;
end;

procedure TJvCustomCsvDataSet.CreateFields;
begin
  InternalInitFieldDefs;
  inherited CreateFields;
end;

//-------------------------------------------------------------------------
//DeleteCsvColumn
//
// When JvCsvDataSet can't open a file, because it contains a
// column in the file that is not defined in our CsvFieldDef, this can
// create a difficult-to-solve problem.  If we drop an obsolete or pointless
// CSV field from our CsvFieldDef, and we have stuff out there in the
// field that contains that data, new versions of our program won't read
// the files anymore.
//
// CSV file editing is not for the faint of heart, and so this method
// is considered less destructive than requiring users to repair
// their files manually.
//
// To use this method:
//         Create a CsvDataSet object with a CsvFieldDef='' (empty)
//
//         Set the Filename property to a fully qualified filename, that
//         must exist, set Active to true, then to False, so we've read
//         the column definitions!
//
//         Call this method, specify a column name that must be removed.
//         The saved file won't contain the column you've removed.
//
// Of course, this method is inherently destructive of of your data.
// Be careful. Make your own backup before you use this method.
//-------------------------------------------------------------------------
procedure TJvCustomCsvDataSet.DeleteCsvColumn(const AFieldName: string);
var
  CsvCol: PCsvColumn;
  ColNum: Integer;
  I: Integer;
  PTempRow: PCsvRow;
  DataRow: AnsiString;
  NewDataRow: AnsiString;
begin
  // filename must be set for this method to work
  Assert(FileName <> '');
  Assert(FileExists(Filename)); // ahuser: Wouldn't it be better to use a normal exception for this?

  // you should set the filename, but not the CsvFieldDef in this case,
  // because in this mode, the CsvDataSet is supposed to be able to open
  // ANY csv file. This is the idea. Open the file, browse it, close it,
  // do some modifications like adding or removing columns, then save those
  // changes, then re-open the csv file to view the change.
  if (FCsvFieldDef <> '') or Active then
    raise Exception.Create('This special method DeleteCsvColumn cannot be called except when CsvFieldDef is not set, and no table is currently open!');

  PTempRow := PCsvRow(AllocRecordBuffer);
  try
     // begin an internal load:
    FOpenFileName := GetFileName; // Always use the same file name to save as you did to load!!! MARCH 2004.WP

    InternalInitFieldDefs; // also loads entire file!
    InternalOpen;

    Assert(FCsvColumns.Count > 1); // why delete last column in an empty file? besides, it would die later on anyways, opening the empty file.

    CsvCol := FCsvColumns.FindByName(AFieldName);
    if not Assigned(CsvCol) then
      JvCsvDatabaseError(FTableName, Format(RsEFieldNotFound, [AFieldName]));

    ColNum := CsvCol.FPhysical;

    if not WriteCsvFileStream then
      raise Exception.Create('Unable to write to CSV file.');

    Assert(Assigned(FCsvStream));

    {Write header row minus column}
    if FHasHeaderRow then
    begin
      // ahuser: DataRow is not initialized and so it is '', is this right?
      JvAnsiStringToCsvRow(DataRow, AnsiChar(Separator), PTempRow, True, EnquoteBackslash);
      NewDataRow := JvCsvRowDeleteColumn(PTempRow, ColNum, FCsvColumns.Count);
      FCsvStream.WriteLine(NewDataRow);
    end;

    {Write data rows minus column}
    for I := 0 to RecordCount - 1 do
    begin
      DataRow := AnsiString(GetRowAsString(I));
      if Length(DataRow) >= JvCsv_MAXLINELENGTH - 1 then
        raise EJvCsvDataSetError.CreateResFmt(@RsECsvStringTooLong, [Copy(DataRow, 1, 40)]);
      JvAnsiStringToCsvRow(DataRow, AnsiChar(Separator), PTempRow, True, EnquoteBackslash);

      // delete column:
      NewDataRow := JvCsvRowDeleteColumn(PTempRow, ColNum, FCsvColumns.Count);
      FCsvStream.WriteLine(NewDataRow);
    end;

    FreeAndNil(FCsvStream); // close and finalize stream.

    //InternalClearFileStrings;

    Self.FieldDefs.Clear;
    Assert(FieldDefs.Count = 0);
    Self.Fields.Clear;
    Assert(Fields.Count = 0);

    FHeaderRow := ''; // clear header row
    FData.Clear;
  finally
    FreeMem(PTempRow);
  end;
end;

function TJvCustomCsvDataSet.Locate(const KeyFields: string; const KeyValues: Variant; Options: TLocateOptions):
    Boolean;
begin
  DoBeforeScroll;
  Result := LocateRecord(KeyFields, KeyValues, Options, False);
  if Result then
  begin
    Resync([rmExact, rmCenter]);
    DoAfterScroll;
  end;
end;

function TJvCustomCsvDataSet.Lookup(const KeyFields: string; const KeyValues: Variant;
  const ResultFields: string): Variant;
begin
  Result := Null;
  if LocateRecord(KeyFields, KeyValues, [], True) then
    Result := FieldValues[ResultFields];
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
