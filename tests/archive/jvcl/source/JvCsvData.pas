unit JvCsvData;

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvaDsgn.PAS, released on 2002-07-04.

The Initial Developers of the Original Code are: Andrei Prygounkov <a.prygounkov@gmx.de>
Copyright (c) 1999, 2002 Andrei Prygounkov   
All Rights Reserved.

Contributor(s):  Warren Postma (warrenpstma@hotmail.com)

Last Modified: 2003-04-26

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

description : TJvCsvDataSet in-memory-dataset component usable by any VCL Data Aware Controls.
              TJvCsvDataSet appears in the 'Jv Data Access' tab of the Component Pallette.

Known Issues:
  May 26, 2003 - Fixed errors handling null date values.
               - Fixed improper memory access for ftBoolean.
                 Booleans are stored internaly as a 16bit WordBool, inside
                 DataSets and the component was reading/writing a 32 bit value,
                 which could caused all kinds of squirrelly things to happen
                 when the boolean (ftBoolean, csv type '!') was encountered.
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
// TJvCsvCustomInMemoryDataSet
//
// Internally, we first define a TJvCsvCustomInMemoryDataSet a base class.
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
// TODO: Make it default to all string types if no field types
// are provided.
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

interface

uses Windows,
     Messages,
     Db,
     SysUtils,
     Classes,
     Graphics; 

const
   MAXCOLUMNS    = 30;
   DEFAULT_CSV_STR_FIELD= 80;
   MAXLINELENGTH = 512;
   COLUMN_ENDMARKER = $FFFF;
   ON_BOF_CRACK  = -1;
   ON_EOF_CRACK  = -2;
   
   { return values from CompareBookmarks: }
   Bookmark_Less = -1; // b1 < b2
   Bookmark_Gtr  = 1;  // b1 > b2
   Bookmark_Eql  = 0;  // b1 = b2


type

 PInteger = ^Integer;
 PDouble = ^Double;
 PBoolean = ^Boolean;

 {  Special Event Types }
 TOnSpecialData = procedure(Sender : TObject; Index : Integer; NonCsvData : String) of Object;


 { SPECIAL TYPES OF  DATABASE COLUMNS FOR THIS COMPONENT }
 { Columns are numeric, text, or one of two kinds of Specially Encoded date/time formats: }
 TJvCsvColumnFlag = (zcsvNull, zcsvString,zcsvNumeric,zcsvAsciiDateTime, zcsvGMTDateTime, zcsvTZDateTime);

 { pointer to special CSV COLUMN }
 PCsvColumn = ^TJvCsvColumn;
// PFieldDef = ^TFieldDef;
 
 TJvCsvColumn = record
     FFlag       : TJvCsvColumnFlag; // Column CSV Format Flags 
     FPhysical   : Integer;           // Physical Column Ordering 
     FFieldDef   : TFieldDef;         // Associated FieldDef     
  end;

  { CSV COLUMNS are stored in a TList-Collection }
  TJvCsvColumns = class (TList)
    public
      procedure AddColumn(Item:PCsvColumn);
      function  FindByFieldNo(FieldNo:Integer):PCsvColumn;
      procedure Clear; override;
      function  FindByName(FieldName:String):PCsvColumn;
   end;
   

   TJvCsvBookmark = record
       flag : TBookmarkFlag;
       data : Integer;
   end;

    { CSV Data File Row is not very dynamic in this version: }
  PtrToPtrToCsvRow = ^PCsvRow; // bookmark data = double pointer indirection! Fun fun fun!
  PCsvRow = ^TJvCsvRow; // a pointer to a record
  TJvCsvRow = record { this MUST be a record, not a class, and must be a flag data record type }
      fdirty   : Boolean; // record is dirty (needs to be written to disk)
      columns  : Integer;
      wordfield: array[0..MAXCOLUMNS+1]    of Word; // lookup field beginning, Column Data (column dirty bit+column length) }
      text     : array[0..MAXLINELENGTH]   of Char; // lookup actual character data.

      // bookmark
      bookmark : TJvCsvBookmark;
  end;

  { Row collection }
  TJvCsvRows = class (TList)
    protected
      FEnquoteBackslash:Boolean;
    public
      procedure AddRow     (const Item:PCsvRow);
      procedure AddRowStr  (const Item:String); // convert String->TJvCsvRow
      function  GetRowPtr  (const RowIndex:Integer):PCsvRow;
      function  GetRowStr  (const RowIndex:Integer):String;
      procedure SetRowStr  (const RowIndex:Integer;Value:String);
      procedure DeleteRow  (const RowIndex:Integer);
      procedure SetARowItem(const RowIndex,ColumnIndex:Integer; Value:String);
      function  GetARowItem(const RowIndex,ColumnIndex:Integer):String;
      procedure Clear; override;
      property EnquoteBackslash:Boolean read FEnquoteBackslash write FEnquoteBackslash;
   end;



  // Easily Customizeable Dataset descendant our CSV handler and
  // any other variants we create:
  TJvCsvCustomInMemoryDataSet = class(TDataSet)

  protected
    FStoreDefs: Boolean;
    FEnquoteBackslash:Boolean; // causes _Enquote to use Backslashes. NOT the default behaviour.
    FTimeZoneCorrection:Integer; // defaults to 0 (none)
    FFileDirty : Boolean; // file needs to be written back to disk?
    FCsvFieldDef: String;            // Our own "Csv Field Definition String"
    FEmptyRowStr:String; // A string of just commas (used to add a new empty row)
    FHeaderRow:String; // first row of CSV file.
    FTableName: string; // CSV File Name
    FRecordPos: Integer;
    FRecordSize:Integer;
    FBufferSize:Integer;
    FCursorOpen:Boolean;
    FFilterBuffer: PChar; // used when we implement filtering (later)
    FReadOnly : Boolean;
    FLoadsFromFile:Boolean;
    FHasHeaderRow:Boolean;
    FSavesChanges:Boolean;
    FInsertBlocked:Boolean; // internal way to block new records

    { data record holder }
    FCsvColumns:TJvCsvColumns; // Column information
    FData:TJvCsvRows;// Rows are a Collection of data pointers.

    { temporary holding space only, for a tstringlist of the file contents }
    FCsvFileAsStrings : TStringList;

    {  event pointers }
    FOnSpecialData : TOnSpecialData;

    //  Internal Use Only Protected Methods
    function GetDataFileSize: Integer; virtual;
    function GetActiveRecordBuffer:  PChar; virtual;
    procedure CsvRowInit(RowPtr:PCsvRow);
    procedure InternalClearFileStrings;
    function InternalLoadFileStrings:Boolean;




    // protected TDataSet base METHODS:
    procedure SetTableName(const Value: string); virtual;
    function  FieldDefsStored: Boolean; virtual;
    function  GetCanModify: Boolean; override; //already virtual!

    // internal calls:
    procedure ProcessCsvHeaderRow(const header:String);
    procedure ProcessCsvDataRow(const datarow:String);
    procedure SetCsvFieldDef( CsvFieldDefs:String);



    { Mandatory VCL TDataSet Overrides - Pure Virtual Methods of Base Class }
    function  AllocRecordBuffer: PChar; override;
    procedure FreeRecordBuffer(var Buffer: PChar); override;
    procedure InternalInitRecord(Buffer: PChar); override;
    function  GetRecord(Buffer: PChar; GetMode: TGetMode;
      DoCheck: Boolean): TGetResult; override;
    function  GetRecordSize: Word; override;
    procedure SetFieldData(Field: TField; Buffer: Pointer); override;
    procedure ClearCalcFields(Buffer:PChar); override;


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
{    procedure InternalInsert; override; } {not needed.}

    // Misc methods:
    procedure InternalClose; override;
//    procedure DestroyFields; override;

    procedure InternalHandleException; override;
    procedure InternalInitFieldDefs; override;
    procedure InternalOpen; override;
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

    function _Enquote(strVal:String):String; virtual; // puts whole string in quotes, escapes embedded commas and quote characters!
    function _Dequote(strValue:String):String; virtual; // removes quotes


  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetFieldData(Field: TField; Buffer: Pointer): Boolean; override;

    // this is necessary to make bookmarks work as well:
    function CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer; override;

    // Additional procedures
    procedure EmptyTable;

    function GetCsvHeader:String;

    {  Additional Public methods }
    procedure OpenWith(Strings:TStrings); virtual; // does AssignFromStrings then opens table.

    procedure AppendWith(Strings:TStrings); virtual;

    { Special declarations }
    // first time call OpenWith, then you can call AssignFromStrings subsequently,
    // as long as the field names and positions have not changed.
    procedure AssignFromStrings(const Strings:TStrings);virtual; // update String data directly.
    procedure AssignToStrings(Strings:TStrings);virtual;
    procedure ExportCsvFile(const Filename:String);virtual; // save out to a file.
    procedure Flush;virtual; // Save CSV file to disk if file has changed and SavesChanges is true.

    { Row Access as String }
    function GetRowAsString(const Index:Integer):String;virtual;
    function GetColumnsAsString:String;virtual;

    property InternalData:TJvCsvRows read FData write FData;

    // NO published properties! This is a base class only!

  end;

  // TJvCsvDataSet is just a TJvCsvCustomInMemoryDataSet with all properties and events exposed:
   TJvCsvDataSet = class(TJvCsvCustomInMemoryDataSet)

    public
      property TableName : String read FTableName; // Another name, albeit read only, for the FileName property!
      
    published
     property FieldDefs stored FieldDefsStored;
     property Active;
     property FileName: string read FTableName write SetTableName;
     property BufferCount;
     property ReadOnly : Boolean read FReadOnly write FReadOnly default False;
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
     property Changed:Boolean read FFileDirty write FFileDirty;
     property DataFileSize: Integer read GetDataFileSize;

     property CsvFieldDef:String read FCsvFieldDef write SetCsvFieldDef; // Our own "Csv Field Definition String"

     property LoadsFromFile:Boolean read FLoadsFromFile write FLoadsFromFile default true;
     property SavesChanges:Boolean read FSavesChanges write FSavesChanges default true;
     property HasHeaderRow:Boolean read FHasHeaderRow write FHasHeaderRow default true;
     // Do field definitions "persist"?
     // Ie: do they get stored in DFM Form file along with the component
     property StoreDefs: Boolean read FStoreDefs write FStoreDefs default False;
     { Additional Events }
     property OnSpecialData : TOnSpecialData read FOnSpecialData write FOnSpecialData;

     { value in seconds : to do GMT to EST (ie GMT-5) use value of (-3600*5) }
     property TimeZoneCorrection:Integer read FTimeZoneCorrection write FTimeZoneCorrection default 0;

     { If false (default) we use the more normal CSV rendering of quotes, which is to double them in
       the csv file, but if this property is true, we use backslash-quote to render quotes in the file,
       which has the side-effect of also requiring all backslashes to themself be escaped by a backslash.
       So filenames would have to be in the form "c:\\directory\\names\\like\\c\\programmers\\do\\it".
       Not recommended behaviour, except when absolutely necessary! }
     property EnquoteBackslash :Boolean read FEnquoteBackslash write FEnquoteBackslash default false;
   end;

{ CSV String Processing Functions }
procedure  CsvRowToString( RowItem:PCsvRow ; var RowString:String );

{ modified! }
procedure StringToCsvRow( const RowString:String   ; RowItem:PCsvRow ; permitEscapeSequences,enquoteBackslash:Boolean );


function   CsvRowItemCopy( Source,Dest:PCsvRow; FieldIndex,FieldSize:Integer ):Boolean;
procedure  SetCsvRowItem ( pItem:PCsvRow; ColumnIndex:Integer; NewValue:String);
function   GetCsvRowItem ( pItem:PCsvRow; ColumnIndex:Integer):String;
procedure  CsvRowSetDirtyBit(row:pCsvRow;ColumnIndex:Integer);
procedure  CsvRowClearDirtyBit(row:pCsvRow;ColumnIndex:Integer);
function   CsvRowGetDirtyBit(row:pCsvRow;ColumnIndex:Integer):Boolean;
procedure  CsvRowSetColumnMarker(row:pCsvRow;ColumnIndex:Integer;ColumnMarker:Integer);
function   CsvRowGetColumnMarker(row:pCsvRow;ColumnIndex:Integer):Integer;


{ Date/Time String decoding functions }
function TimeTHexToDateTime( HexStr:String ; TimeZoneCorrection : Integer ) : TDateTime;
function TimeTAsciiToDateTime ( AsciiDateStr : String ) : TDateTime;

{ Date/Time string encoding functions }
function DateTimeToTimeToIsoAscii(aDateTime:TDateTime):String;
function DateTimeToTimeTHex(aDateTime:TDateTime; TimeZoneCorrection:Integer ):String;




implementation

uses BDE, DBTables, DBConsts, Forms, Controls, Dialogs, JvCsvParse;

var
  CallCount : Integer;


// note that file is not being locked!

{ TJvCsvCustomInMemoryDataSet }

constructor TJvCsvCustomInMemoryDataSet.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);


  // FRecordSize = size of a csv text buffer and the indexes pointing
  //               into that buffer:

  FRecordSize := SizeOf(TJvCsvRow) - Sizeof(TJvCsvBookmark);

  // FBuffer size includes CSV Text buffer, and the bookmark data, followed
  // by space for storing the binary form of a calculated-field:

  // initial FBufferSize size: My theory is that we should pick a conservative
  // estimate plus a margin for error:
  FBufferSize := SizeOf(TJvCsvRow) + 128; {CalcFieldsSize}; // our regular record + calculated field data.

  FReadOnly := false;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
  FLoadsFromFile := true;
  FSavesChanges := true;
  FHasHeaderRow := true;

  { Additional initialization }
  FCsvColumns:= TJvCsvColumns.Create;
  FData := TJvCsvRows.Create;
  FData.EnquoteBackslash := FEnquoteBackslash;

end;

destructor TJvCsvCustomInMemoryDataSet.Destroy;
begin
  InternalClearFileStrings; // delete file strings

 try
  if FCursorOpen then InternalClose;

 except
 end;
  if Assigned(FCsvColumns) then begin
    FCsvColumns.Clear;
    FCsvColumns.Free;
  end;
  if Assigned(FData) then begin
    FData.Clear;
    FData.Free;
  end;
  inherited Destroy;
end;


function TJvCsvCustomInMemoryDataSet.AllocRecordBuffer: PChar;
var
  RowPtr:PCsvRow;
begin
  RowPtr := AllocMem(FBufferSize{Sizeof(TJvCsvRow)});
//  Trace('AllocRecordBuffer result=$'+IntToHex(Integer(Pointer(RowPtr)),8));
  result := PChar(RowPtr);
end;

{ calc fields support }
procedure TJvCsvCustomInMemoryDataSet.ClearCalcFields(Buffer: PChar);
begin
     // Assumes that our buffer is a TJvCsvRow followed by
     // a dynamically resized buffer used for calculated field
     // storage:
     FillChar(Buffer[Sizeof(TJvCsvRow)],CalcFieldsSize,0);
end;

{ calc fields support and buffer support }
function TJvCsvCustomInMemoryDataSet.GetActiveRecordBuffer:  PChar;
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
          Result:=FFilterBuffer;
          
  dsEdit,dsInsert:
          Result:=ActiveBuffer;
  else
      Result:=nil;
  end;
end;


procedure TJvCsvCustomInMemoryDataSet.SetCsvFieldDef( CsvFieldDefs:String);
begin
 if (FCsvFieldDef <> CsvFieldDefs) then begin
    CheckInactive;
    FCsvFieldDef := CsvFieldDefs;
    FHeaderRow := ''; 
    FieldDefs.Clear;   // Clear VCL Database field definitions
    FCsvColumns.Clear; // Clear our own CSV related field data
    FData.Clear;       // Clear out data
 end;
end;

procedure TJvCsvCustomInMemoryDataSet.FreeRecordBuffer(var Buffer: PChar);
//var
//  RowPtr:PCsvRow;
begin
  //Trace( 'FreeRecordBuffer '+IntToHex(Integer(Buffer),8) );
// try 
    if Buffer <> NIL then
       FreeMem(Buffer);
// except
     //Trace( 'FreeRecordBuffer - Exception freeing '+IntToHex(Integer(Buffer),8) );
//  end;
//  //Trace('TJvCsvCustomInMemoryDataSet.FreeRecordBuffer');

end;

{ called after the record is allocated }
procedure TJvCsvCustomInMemoryDataSet.InternalInitRecord(Buffer: PChar);
var
  RowPtr:PCsvRow;
begin
  //Trace( 'InternalInitRecord '+IntToHex(Integer(Buffer),8) );

  FillChar ( Buffer^, FBufferSize, 0 );
  RowPtr := PCsvRow( Buffer ); // Zero out the buffer.
  CsvRowInit(RowPtr);
end;

// CsvRowInit
//
// Internal handy dandy function to set up a new csv row.
// which is intially full of just commas.
//
procedure TJvCsvCustomInMemoryDataSet.CsvRowInit(RowPtr:PCsvRow);
var
  T : Integer;
  ColCount:Integer;
begin
  RowPtr^.fdirty := false;
  RowPtr^.bookmark.flag := bfEOF;
  RowPtr^.bookmark.data := ON_BOF_CRACK; // no index into FData yet.
  CsvRowSetColumnMarker(RowPtr, {column} 0, {marker value} 0);
  

  ColCount := FCsvColumns.Count;
  if ColCount <= 0 then ColCount := 10;

  for t := 1 to ColCount do begin // create an empty line of just commas
    if (t <ColCount) then
        RowPtr^.text  [t-1] := ','
    else
        RowPtr^.text  [t-1] := Chr(0);
    RowPtr^.text  [t]   := Chr(0);
    CsvRowSetColumnMarker(RowPtr,{column}t-1,{marker value}t-1);
    CsvRowSetColumnMarker(RowPtr,{column}t,{marker value}COLUMN_ENDMARKER);
  end;
end;



function TJvCsvCustomInMemoryDataSet.GetRecord(Buffer: PChar; GetMode: TGetMode;
  DoCheck: Boolean): TGetResult;
var
  RowPtr:PCsvRow;
begin

 Buffer[0] := Chr(0);
 Result := grEOF;
 if FData.Count < 1 then begin
    //Trace(' GetRecord - called when data buffer empty.');
    exit;
 end;
 case GetMode of
      gmPrior:
       begin
        //Trace(' GetRecord( Buffer, gmPrior, DoCheck)');
        if FRecordPos = ON_BOF_CRACK then
           result := grBOF
        else if FRecordPos = ON_EOF_CRACK then begin
           FRecordPos := FData.Count-1;
           result := grOk;
        end else if FRecordPos > 0 then begin
           Dec(FRecordPos);
           result := grOk;
        end else
           result := grBOF;
       end;


      gmCurrent:
        begin
         //Trace(' GetRecord( Buffer, gmCurrent, DoCheck)');
         if (FRecordPos<0) then // BOF Crack or EOF Crack?
            result := grError
         else
            result := grOk;
         end;
      gmNext:
        begin
         //Trace(' GetRecord( Buffer, gmNext, DoCheck)');
         if FRecordPos = ON_EOF_CRACK then
            result := grEOF
         else begin
          Inc(FRecordPos);
          if (FRecordPos >= FData.Count) then begin
            FRecordPos := ON_EOF_CRACK;
            result := grEOF
          end else
            result := grOk;
        end
       end;
        
      // default case:
      else
        DatabaseError('GetMode???');
    end; {end case}

    if Result = grOk then
    begin
       //Trace( ' GetRecord FRecordPos='+IntToStr(FRecordPos)+'result=grOk' );
      try
        { get a record into a buffer }
        RowPtr := PCsvRow(Buffer); // Cast to a Row Data Structure to our own type.
        Move( {source:}FData.GetRowPtr(FRecordPos)^,  {dest:}RowPtr^, SizeOf(TJvCsvRow) );
        RowPtr^.bookmark.flag := bfCurrent;
        RowPtr^.bookmark.data := FRecordPos;

        // Update calculated fields for this row:
        ClearCalcFields(Buffer);
        GetCalcFields(Buffer);

      except
        DatabaseError('Problem reading row '+IntToStr(FRecordPos));
      end;
    end
    else begin

      // fudge: Get bookmark into a record for BOF and EOF records:
{      if RowPtr <> NIL then
          RowPtr^.bookmark.data := FRecordPos;}

      if (Result = grError) and DoCheck then
       DatabaseError('No records');
    end;
    
//    if (Result = grError) then
          //Trace(' GetRecord result = grError');
//    if (Result = grEof) then
          //Trace(' GetRecord result = grEof');
//     if (Result = grBof) then
          //Trace(' GetRecord result = grBof');





end;

function TJvCsvCustomInMemoryDataSet._Enquote(strVal:String):String; // puts whole string in quotes, escapes embedded commas and quote characters!
var
  s:String;
  t,l:Integer;
  ch:Char;
begin
   s := '"';
   l := Length(strVal);
   for t := 1 to l do begin
        ch := strVal[t];
        if FEnquoteBackslash then begin // backslash quoting ( C/C++ )
        if ch = '\' then   // double each backslash
                s := s + '\\'
        else if ch = '"' then // escape quotes with a backslash
                s := s + '\"'
        else
                s := s + ch;
        end else begin
                // simpler method: doubled-quotes ( Pascal )
                if ch = '"' then // escape quotes by doubling them.
                        s := s + '""'
                else
                        s := s + ch;

        end;
   end;
   s := s + '"'; // end quote.
   result := s;
end;


function TJvCsvCustomInMemoryDataSet.GetRecordSize: Word;
begin
 // In create:
 //    FRecordSize := Sizeof(TJvCsvRow) - Sizeof(TJvCsvBookmark);
 result := FRecordSize;
end;

procedure TJvCsvCustomInMemoryDataSet.SetFieldData(Field: TField; Buffer: Pointer);
var
  NewVal:String;
  PhysicalLocation:Integer;
  pDestination:PChar;
  CsvColumnData :PCsvColumn;
  DT:TDateTime;
begin

  //Trace( 'SetFieldData '+Field.FieldName );
 pDestination :=GetActiveRecordBuffer;

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow)=0) then begin
      FHeaderRow := GetColumnsAsString;
      ProcessCsvHeaderRow(FHeaderRow);
  end;
 

 // If this is a calculated field or lookup field then...
 if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then begin

    Inc(pDestination,Sizeof(TJvCsvRow)+Field.Offset);
    Boolean(pDestination[0]):=(Buffer<>nil);
    if Boolean(pDestination[0]) then
        CopyMemory(@pDestination[1],Buffer,Field.DataSize);
    //result := true; {there is no return value, oops}
    exit;
 end;

 // If we get here, we are dealing with a physical record:

 // Set a field data, taking the physical to logical ordering translation into
 // account:
 CsvColumnData := FCsvColumns.FindByFieldNo(Field.FieldNo);
 if not Assigned(CsvColumnData) then
    exit;

 PhysicalLocation := CsvColumnData^.FPhysical;

 if PhysicalLocation < 0 then
     exit;

 if Buffer = NIL then
   NewVal := ''
 else
  case Field.DataType of
       ftString:
          begin
            NewVal := String(PChar(Buffer));
            //----------------------------------------------------------------------------------------------------
            // NEW RULE: If user displayed value contains a comma, a backslash, or a double quote character
            // then we MUST encode the whole string as a string literal in quotes with the embeddded quotes
            // and backslashes preceded by a backslash character.
            //----------------------------------------------------------------------------------------------------
            if     ( Pos(',',NewVal) > 0 )
                or ( Pos('"',NewVal) > 0 )
                or (( Pos('\',NewVal) > 0 ) and (FEnquoteBackslash))
                then begin
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
                      NewVal := IntToStr( Ord(PWordBool(Buffer)^)); // bugfix May 26, 2003 - WP
          end;

         // There are two ways of handling date and time:
       ftDateTime:
         case CsvColumnData^.FFlag of
              // Localized time in Ascii
             zcsvAsciiDateTime:
                begin
                  DT := TimeStampToDateTime(
                                MSecsToTimeStamp(
                                    Double(Buffer^) ));
                  NewVal := DateTimeToTimeToIsoAscii(DT);
                  OutputDebugString(PChar('date '+NewVal));

                end;

             // GMT Times are stored in HEX
             zcsvGMTDateTime:
                begin
                DT := TimeStampToDateTime(
                           MSecsToTimeStamp(
                                   Double(Buffer^) ));
                 NewVal := DateTimeToTimeTHex( DT, 0);

                end;

             zcsvTZDateTime: // Move a GMT time into a timezone:
                begin
                  DT := TimeStampToDateTime(
                                MSecsToTimeStamp(
                                    Double(Buffer^) ));
                 NewVal := DateTimeToTimeTHex( DT, FTimeZoneCorrection);

                end;

             else
              DatabaseError('TJvCsvCustomInMemoryDataSet.SetFieldData - TimeT-to-DateTime conversion error.');
         end;
       else
         DatabaseError('TJvCsvCustomInMemoryDataSet.SetFieldData - Field type not handled.');
  end;

 // Set new data value (NewVal = String)
 SetCsvRowItem( PCsvRow(pDestination{was:ActiveBuffer}), PhysicalLocation, NewVal );

 // Set a dirty bit so we remember to write this later:
 CsvRowSetDirtyBit(PCsvRow(pDestination),PhysicalLocation);

 // Set the file-wide dirty bit:
 FFileDirty := true;

 // Notify controls of a field change:
 DataEvent(deFieldChange, LongInt(Field));

end;




// Removes first and last character of the string (assumes they are quotes,
// to be called byGetFieldData only!)
function TJvCsvCustomInMemoryDataSet._Dequote(strValue:String):String;
var
 s:String;
 t,l:Integer;
 ch:Char;
 skipFlag:Boolean;
begin
  l := Length(strValue);
  skipFlag := false;
  s := '';
  if Length(StrValue)<2 then begin
        result := s;
        exit;
  end;

  for t := 2 to l-1 do begin
        ch := strValue[t];
        if FEnquoteBackslash then begin
           if (not skipFlag) and ( ch = '\' ) then begin
                skipFlag := true; // whatever is after the backslash is an escaped literal character.
                continue;
           end else
                skipFlag := false;
        end else begin
                if (not skipFlag) and (ch = '"') and (t < (l-1)) and ( strValue[t+1] = '"') then begin
                        skipFlag := true;
                        continue; // skip first of the doubled quote characters.
                end else
                        skipFlag := false;
        end;
        s := s + ch;
  end;
  result := s;
end;

function TJvCsvCustomInMemoryDataSet.GetFieldData(Field: TField; Buffer: Pointer):Boolean;
var
  {RowPtr:PCsvRow;}
  {ActiveRowPtr:PCsvRow;}
  pSource:PChar;
  pTempBuffer:PChar;
  TempString:String;
  PhysicalLocation:Integer;
  CsvColumnData :PCsvColumn;
  aDateTime:TDateTime;
  l:Integer;
begin
  result := false;
  //Trace( 'GetFieldData '+Field.FieldName );

  if not FCursorOpen then
    Exit;
  if Field = NIL then
    exit;

 // Dynamic CSV Column Ordering: If we didn't start by
 // assigning column orders when we opened the table,
 // we've now GOT to assume a physical ordering:
  if (Length(FHeaderRow)=0) then begin
      FHeaderRow := GetColumnsAsString;
      ProcessCsvHeaderRow(FHeaderRow);
  end;


  pSource := GetActiveRecordBuffer;
  if (pSource=nil) then begin
    //DatabaseError('CsvDataSet.GetFieldData: Unable to get active record buffer');
    Exit;
  end;
   
  //------------------------------------------------------------------------
  // Calculated and Lookup Field Handling
  //
  // direct memory copy into calculated field or lookup field data area
  //------------------------------------------------------------------------
  if (Field.FieldKind=fkCalculated) or (Field.FieldKind=fkLookup) then begin
      Inc(pSource,Sizeof(TJvCsvRow)+Field.Offset);
      if (pSource[0]=#0) or (Buffer=nil) then
         Exit
      else // Get the field data from the buffer:
         CopyMemory(Buffer, @pSource[1], Field.DataSize);
      result := true;
      exit;
  end;

  //------------------------------------------------------------------------
  // If we get here we must be dealing with a real column of data
  // that is part of the CSV file rather than a calculated or lookup
  // field that is just in internal-memory:
  //------------------------------------------------------------------------

  CsvColumnData := FCsvColumns.FindByFieldNo( Field.FieldNo );
  if NOT Assigned(CsvColumnData) then begin
    DatabaseError('Unable to locate CSV file information for field '+Field.Name);
    exit;
  end;
  PhysicalLocation := CsvColumnData^.FPhysical;
  if PhysicalLocation < 0 then begin // does it really exist in the CSV Row?
    DatabaseError('Physical location of CSV field '+Field.FieldName+' in table '+ FTableName +'unknown.');
    exit;
  end;

  //------------------------------------------------------------------------
  // All items in the CSV table are natively stored as strings. Note that
  // an empty string is considered to be a NULL if the field type is anything
  // other than a ftString. There are no NULLs in ftString fields because
  // a CSV file can store an empty string but has no way of indicating a NULL.
  //------------------------------------------------------------------------

  TempString:=GetCsvRowItem( PCsvRow(pSource{was:ActiveBuffer}), PhysicalLocation);

  // NULL:  There are no "Real" NULLS in an ASCII flat file, however for anything
  // other than a string field, we will return "NULL" to indicate there is an
  // empty string in the field.
  if (Field.DataType <> ftString) then
     if Length(TempString) = 0 then
           exit; // NULL field.
  //------------------------------------------------------------------------
  // Handle some type conversions:
  //------------------------------------------------------------------------
 try
  case Field.DataType of
        // Basic string copy, convert from String to fixed-length
        // buffer, padded with NUL i.e. Chr(0):
       ftString:
          begin
                // NEW: Don't display the quotes and decode the
                // escape sequences before giving string values to the end
                // user!
                l := Length(TempString);
                if (l>=2) then 
                  if (TempString[1] = '"') and (TempString[l] = '"') then begin // quoted string!
                          TempString := _Dequote(TempString);
                  end;

               pTempBuffer := AllocMem(Field.Size+1);  // AllocMem fills with zeros
               StrCopy(pTempBuffer,PChar(TempString)); // we copy in the data portion
               Move(pTempBuffer^,Buffer^,Field.Size);  // Buffer^ is now zero padded.
               FreeMem(pTempBuffer);                   // Free the memory we allocated.
          end;

        // Standard Integer conversion:
       ftInteger:   PInteger(Buffer)^ := StrToInt(TempString);

        // Standard Double-precision Float conversion:
       ftFloat:     PDouble(Buffer)^ := StrToFloat(TempString);

       ftBoolean:
         begin
             if Length(TempString)=0 then begin
                 PInteger(Buffer)^ := 0;
             end else begin
             if StrToIntDef(TempString,0) <> 0 then
                 PWordBool(Buffer)^ := True // bugfix May 26, 2003 - WP
              else
                 PWordBool(Buffer)^ := False; // bugfix May 26, 2003 - WP
             end;
           end;
       ftDateTime:
         case CsvColumnData^.FFlag of
             // Ascii Date 1999/03/05 08:23:15
             zcsvAsciiDateTime:
             begin
               aDateTime := TimeTAsciiToDateTime(TempString);
               if aDateTime <= 1.0 then begin
                        result := false; { field is NULL, no date/time value }
                        exit;
               end;
               Double(Buffer^) := TimeStampToMSecs(
                                    DateTimeToTimeStamp(
                                      aDateTime) );
               if Double(Buffer^) = 0 then exit;
             end;

             // GMT Times are Stored in HEX:
             zcsvGMTDateTime:
                Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,0)));

             // Move GMT into a Timezone:
             zcsvTZDateTime:
                Double(Buffer^) := TimeStampToMSecs(DateTimeToTimeStamp(TimeTHexToDateTime(TempString,FTimeZoneCorrection)));


         else
             DatabaseError('TJvCsvCustomInMemoryDataSet.GetFieldData - TimeT-to-DateTime conversion error.');
         end;   {end case}
       else // not a valid ftXXXX type for this TDataSet descendant!?
           DatabaseError('TJvCsvCustomInMemoryDataSet.GetFieldData - Field type not handled.');
     end // end case.
  except
    on E:EConvertError do begin
        result := false; // return a NULL.
        exit;
    end;
  end;
  // All is Well.
  result := true;
end;



// Our bookmark data is a pointer to a PCsvData
procedure TJvCsvCustomInMemoryDataSet.GetBookmarkData(Buffer: PChar; Data: Pointer);
//var
//  t:Integer;
begin
// t:= PCsvRow(Buffer)^.bookmark.data;
 PInteger(Data)^ := PCsvRow(Buffer)^.bookmark.data;
end;

function TJvCsvCustomInMemoryDataSet.GetBookmarkFlag(Buffer: PChar): TBookmarkFlag;
begin
   result := PCsvRow(Buffer)^.bookmark.flag;
end;

// nobody mentioned that I needed this to be overloaded, but I only found
// out when I found that DBGrid and other controls that compare bookmarks
// won't function if you don't provide a non-default implementation of this.
function TJvCsvCustomInMemoryDataSet.CompareBookmarks(Bookmark1, Bookmark2: TBookmark): Integer;
var
 v1,v2:Integer;
begin
 v1 := -999999;
 v2 := -999999;
 if Bookmark1 <> NIL then
    v1 := Integer(Bookmark1^);
 if Bookmark2 <> NIL then
    v2 := Integer(Bookmark2^);
 result := Bookmark_Eql;
 if v1 < v2 then
        result := Bookmark_Less
 else if v1 > v2 then
        result := Bookmark_Gtr;
end;

procedure TJvCsvCustomInMemoryDataSet.SetBookmarkFlag(Buffer: PChar; Value: TBookmarkFlag);
begin
  PCsvRow(Buffer)^.bookmark.flag := Value;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalGotoBookmark(Bookmark: Pointer);
begin
  {Bookmark is just pointer to integer}
  FRecordPos := PInteger(Bookmark)^;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalSetToRecord(Buffer: PChar);
begin
  FRecordPos := PCsvRow(Buffer)^.bookmark.data;//Look up index from the record.
//  Resync([]);
end;

// Also used when inserting:
procedure TJvCsvCustomInMemoryDataSet.SetBookmarkData(Buffer: PChar; Data: Pointer);
begin
  PCsvRow(Buffer)^.bookmark.data := PInteger(Data)^; 
end;

procedure TJvCsvCustomInMemoryDataSet.InternalFirst;
begin
//  Eof := false;
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

procedure TJvCsvCustomInMemoryDataSet.InternalInitFieldDefs;
var
  CsvFieldRec     : TJvCsvRow; //record type.
  CsvFieldOption  : String;
  CsvFieldName    : String;
  pCsvFieldDef    : PCsvColumn;
  {t, }colnum, pos1 : Integer;
  // field options:
  FieldTypeChar:Char;
  VclFieldType : TFieldType;
  FieldLen     : Integer;
  ZtrFieldType : TJvCsvColumnFlag;
  aCsvFieldDef : String;
//  FDef:TFieldDef;
begin

//  Application.MessageBox('InternalInitFieldDefs','Debug',MB_OK);

//  FFieldsInitialized := true;
  ZtrFieldType := zcsvString;
  VclFieldType := ftString;

  // create FieldDefs which map to each field in the data record
  FieldDefs.Clear;   // Clear VCL Database field definitions
  FCsvColumns.Clear; // Clear our own CSV related field data

  aCsvFieldDef :=   FCsvFieldDef;
  if Length(aCsvFieldDef)=0 then begin
        if FHasHeaderRow and InternalLoadFileStrings then
                aCsvFieldDef := FCsvFileAsStrings[0];
  end;

  if Length(aCsvFieldDef) > 0 then begin
     StringToCsvRow(aCsvFieldDef,@CsvFieldRec, false,false);
     colnum := 0;
     while (CsvRowGetColumnMarker(@CsvFieldRec,colnum) <> COLUMN_ENDMARKER) do begin
       FieldLen := 80; // default.
       CsvFieldOption := GetCsvRowItem( @CsvFieldRec, colnum ); // get a string in the format COLUMNAME:Options
       
       // Look for Colon or Semicolon:
       pos1 := Pos(':',CsvFieldOption);
       if (pos1<=0) then pos1 := Pos(';',CsvFieldOption);

       if (pos1<=0) then begin
            CsvFieldName := CsvFieldOption;
            CsvFieldOption := '$';
            FieldTypeChar := '$';
       end else begin
          // extract field name:
          CsvFieldName := Copy(CsvFieldOption,1,pos1-1);
          // If character after the colon is a symbol character, grab
          // it, otherwise default to '$'.
          if Ord( CsvFieldOption[pos1+1] ) < Ord('A') then begin
             FieldTypeChar := CsvFieldOption[pos1+1];
             CsvFieldOption := Copy(CsvFieldOption,pos1+2,80);
          end else begin
             FieldTypeChar := '$';
             CsvFieldOption := Copy(CsvFieldOption,pos1+1,80);
          end;
          FieldLen := StrToIntDef(CsvFieldOption,DEFAULT_CSV_STR_FIELD);
       end;
       case FieldTypeChar of
          '$': begin // $=string
                 VclFieldType := ftString;
                 ZtrFieldType := zcsvString;
               end;
          '%': begin // %=Integervalue
                 VclFieldType := ftInteger;
                 ZtrFieldType := zcsvNumeric;
                 FieldLen := 0; // automatic.
               end;
          '&': begin // &=Float value
                 VclFieldType := ftFloat;
                 ZtrFieldType := zcsvNumeric;
                 FieldLen := 0; // automatic.
               end;
          '@': begin // @=Datetime as Ascii YYYY/MM/DD HH:MM:SS
                 VclFieldType := ftDateTime;
                 ZtrFieldType := zcsvAsciiDateTime;
                 FieldLen := 0; // automatic.
               end;
          '!': begin // != boolean field TRUE/FALSE
                 VclFieldType := ftBoolean; // boolean field in dataset
                 ZtrFieldType := zcsvNumeric;// numeric field in file
                 FieldLen := 0; // automatic.
               end;
          '#': begin // #=Datetime as Seconds since 1970 stored in HEX
                 VclFieldType := ftDateTime;
                 ZtrFieldType := zcsvGMTDateTime;
                 FieldLen := 0; // automatic.
               end;

          '-': begin // -=Datetime as Seconds since 1970 stored in HEX
                 VclFieldType := ftDateTime;
                 ZtrFieldType := zcsvTZDateTime;
                 FieldLen := 0; // automatic.
               end;

          else
               DatabaseError('Invalid field type character: '+FieldTypeChar );
       end;


       if Length(CsvFieldName)=0 then begin
           DatabaseError('Unexpected error parsing CSV Field Definitions');
           break;
       end;

       // sometime later: unpack the rest of the string
       // and declare ftString,ftFloat,ftInteger,ftDateTime, etc.
       // now add the field:
      Inc(colnum);

       // This may throw an exception. but we'll just allow
       // that as necessary:

        //Was: TFieldDef.Create(FieldDefs, ...., colnum );
      FieldDefs.Add( CsvFieldName, VclFieldType, FieldLen, False );

      // Now create our internal field data structure:
      pCsvFieldDef := AllocMem(Sizeof(TJvCsvColumn) {+ 8 BIGFudge});
      pCsvFieldDef^.FFlag := ZtrFieldType; {zcsvString}
      pCsvFieldDef^.FFieldDef :=  FieldDefs.Find(CsvFieldName);

      // Note: field order is established when we open the file (later)
      pCsvFieldDef^.FPhysical := -1; // not yet located in the physical file!
      FCsvColumns.AddColumn(pCsvFieldDef);
     end;

     // if the file doesn't contain this and we haven't
     // generated it yet, generate the header row:
     if (NOT FHasHeaderRow) and ( length(FHeaderRow)=0 ) then
        FHeaderRow := GetColumnsAsString;

     if Length(FHeaderRow) > 0 then
       ProcessCsvHeaderRow(FHeaderRow);
  end else
    DatabaseError('Field Definition Error. Please make sure the CsvFieldDef property and the FieldDefs match each other, and match the contents of the csv file itself.');

//  Application.MessageBox(PChar('FieldDefs.Count = '+IntToStr(FieldDefs.Count)),'debug',MB_OK);
 //Trace( 'InternalInitFieldDefs Count='+IntToStr(FieldDefs.Count) );
 // for t := 0 to FieldDefs.Count-1 do
   // Trace( '  field:'+FieldDefs[t].Name  );
end;

{ set our position onto the EOF Crack }
procedure TJvCsvCustomInMemoryDataSet.InternalLast;
begin
//  Eof := true;
  FRecordPos := ON_EOF_CRACK; // FData.Count; 
end;

// At shutdown or on user-calling this method, check if data has changed,
// and write changes to the file.
procedure TJvCsvCustomInMemoryDataSet.Flush;
begin
  if FFileDirty and FSavesChanges and (Length(FTableName)>0) then
    ExportCsvFile( FTableName );
end;


{procedure TJvCsvCustomInMemoryDataSet.DestroyFields;
begin
 inherited DestroyFields;
 // Clear out local TZTRCsvFieldDefs.
 FCsvColumns.Clear;
end;}

procedure TJvCsvCustomInMemoryDataSet.InternalClose;
begin
  Flush;
  BindFields(False);
  if DefaultFields then DestroyFields;
  FData.Clear;
  FCursorOpen := false;
  FRecordPos := ON_BOF_CRACK;
end;

procedure TJvCsvCustomInMemoryDataSet.InternalHandleException;
begin
  // standard implementation for this method:
  Application.HandleException(Self);
end;

procedure TJvCsvCustomInMemoryDataSet.InternalDelete;
begin
  if (FRecordPos >= 0) and (FRecordPos < FData.Count) then begin
      // FreeMem performed inside DeleteRow!
      FData.DeleteRow(FRecordPos);
  end;
  
  if FRecordPos >= FData.Count then
      FRecordPos := FData.Count-1;

  FFileDirty := True;
end;


{procedure TJvCsvCustomInMemoryDataSet.InternalInsert;
//var
//  pAddRec : pCsvRow;
begin
// pAddRec := AllocMem(Sizeof(TJvCsvRow));
// StringToCsvRow( FEmptyRowStr, pAddRec, false ); // initialize row.
// FData.AddRow(pAddRec);
// FCurrentRecord := -1;
// Resync([]);
  FRecordPos :=  ON_EOF_CRACK;
//FCurrentRecord := FData.Count;
end;}

procedure TJvCsvCustomInMemoryDataSet.InternalAddRecord(Buffer: Pointer; Append: Boolean);
var
  RecPos  : Integer;
  pAddRec : pCsvRow;

begin

 if FInsertBlocked then begin
    DatabaseError('TJvCsvCustomInMemoryDataSet.InternalAddRecord: Can''t Add. Insert blocked.');
    exit;
 end;

 if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
 if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;



 pAddRec := AllocMem(Sizeof(TJvCsvRow));
 if (Buffer <> NIL) then
     Move( PCsvRow(Buffer)^, pAddRec^, Sizeof(TJvCsvRow) );

 if ( StrLen(pAddRec.text) = 0 ) then
  StringToCsvRow( FEmptyRowStr, pAddRec, false, false ); // initialize row.

 pAddRec.FDirty := true;

 FData.EnquoteBackslash := FEnquoteBackslash; // make sure FData is in the right mode.
 
 FFileDirty := True;
 if Append then begin //this is the parameter not a TDataSet method invocation!
     FData.AddRow(pAddRec);
     InternalLast;
 end else begin
     if (FRecordPos = ON_EOF_CRACK) or (FRecordPos = ON_BOF_CRACK ) then begin
        FData.AddRow(pAddRec);
        InternalLast;
     end else begin
        RecPos := FRecordPos;
        FData.Insert(RecPos,Pointer(pAddRec));
    end;
 end;
end;


{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This creates that TString List. }
function TJvCsvCustomInMemoryDataSet.InternalLoadFileStrings:Boolean;
begin
  result := false;
  if not FileExists(FTableName) then exit;
  if not FLoadsFromFile then exit;
  if Assigned(FCsvFileAsStrings) then begin
        if FCsvFileAsStrings.Count >0 then
                result := true; //loaded already
        exit; // don't repeat!
  end;

   try   // open data file
          FCsvFileAsStrings:= TStringList.Create;
          FCsvFileAsStrings.LoadFromFile(FTableName);
          if FCsvFileAsStrings.Count >0 then
                  result := true; // it worked!
   except
          //FTableName := '';
          FCsvFileAsStrings.Free;
          FCsvFileAsStrings := nil;
          raise; // pass exception on in.
    end; {end except}
end;


{ During the parsing stage only, we have the contents of the file loaded in memory
  as a TString LIst. This cleans up that TString List. }
procedure TJvCsvCustomInMemoryDataSet.InternalClearFileStrings;
begin
  if Assigned(FCsvFileAsStrings) then begin
          FCsvFileAsStrings.Free;
          FCsvFileAsStrings := nil;
  end;
end;


procedure TJvCsvCustomInMemoryDataSet.InternalOpen;
var
  Strings : TStringList;
  TempBuf : Array[0..MAXCOLUMNS] of char;
begin
  if FCursorOpen then InternalClose; // close first!


  FFileDirty := False;
  if ( Length(FTableName) = 0 ) and FLoadsFromFile then
     DatabaseError('LoadFromFile=True, so a TableName is required');
  Strings:=NIL;

  InternalInitFieldDefs;             // initialize FieldDef objects
  // Create TField components when no persistent fields have been created
  if DefaultFields then
      CreateFields;
  BindFields(True);                 // bind FieldDefs to actual data

  if (FCsvColumns.Count>1) then begin
     // Create a null terminated string which is just a bunch of commas:
      FillChar(TempBuf, FCsvColumns.Count - 1, ',');
      TempBuf[FCsvColumns.Count-1] := Chr(0);
      // When adding an empty row, we add this string as the ascii equivalent:
      FEmptyRowStr := String(TempBuf);
  end else begin
      FEmptyRowStr := ''; // nothing.
  end;


  FBufferSize := SizeOf(TJvCsvRow) + CalcFieldsSize; // our regular record + calculated field data.
  FRecordPos := ON_BOF_CRACK;                  // initial record pos before BOF
  BookmarkSize := SizeOf(Integer);   // initialize bookmark size for VCL (Integer uses 4 bytes on 32 bit operating systems)

  //Trace( 'InternalOpen: FBufferSize='+IntToStr(FBufferSize) );
  //Trace( 'InternalOpen: CalcFieldsSize='+IntToStr(CalcFieldsSize) );
  //Trace( 'InternalOpen: FieldDefs.Count='+IntToStr(FieldDefs.Count) );

  if InternalLoadFileStrings then  // may load the strings if they weren't loaded already!
          AssignFromStrings(FCsvFileAsStrings); // load into memory.


  InternalClearFileStrings; // now unload 'em.

   FCursorOpen := true;
   if Length(FHeaderRow) >0 then
              ProcessCsvHeaderRow(FHeaderRow);

end;

procedure TJvCsvCustomInMemoryDataSet.InternalPost;
var
 pInsertRec:PCsvRow;
 RecPos:Integer;
begin
  if FRecordPos = ON_BOF_CRACK then
    FRecordPos := 0;
  if FRecordPos = ON_EOF_CRACK then
    FRecordPos := FData.Count;



 if State = dsEdit then begin
     FFileDirty := True;
     RecPos := FRecordPos;
     Move( PCsvRow(ActiveBuffer)^, FData.GetRowPtr(RecPos)^, Sizeof(TJvCsvRow) );
     FData.GetRowPtr(RecPos)^.FDirty := true;
 end else if State = dsInsert then begin
     if FInsertBlocked then begin
          DatabaseError('TJvCsvCustomInMemoryDataSet.Post: Can''t post. Insert blocked.');
          exit;
     end;
     FFileDirty := True;
     pInsertRec := AllocMem(Sizeof(TJvCsvRow));
     Move( PCsvRow(ActiveBuffer)^, pInsertRec^, Sizeof(TJvCsvRow));
     pInsertRec^.FDirty := true;
     FData.Insert(FRecordPos,Pointer(pInsertRec));
     FRecordPos := FData.IndexOf(Pointer(pInsertRec));
     pInsertRec^.bookmark.data := FRecordPos;
 end else
     DatabaseError('TJvCsvCustomInMemoryDataSet.Post: Can''t post. Not in not dsEdit or dsInsert mode');
end;


function TJvCsvCustomInMemoryDataSet.IsCursorOpen: Boolean;
begin
  // "Cursor" is open if data file is open.   File is open if FDataFile's
  // Mode includes the FileMode in which the file was open.
 {  Result := TFileRec(FDataFile).Mode <> 0; }
  Result := FCursorOpen; // bogus value: Valid field definition
end;

function TJvCsvCustomInMemoryDataSet.GetRecordCount: Integer;
begin
 if ( FData.Count > 0) then
  Result := FData.Count
 else
  Result := 0;
end;

function TJvCsvCustomInMemoryDataSet.GetRecNo: Integer; {RecNo := FRecordPos+1}
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
    BufPtr := CalcBuffer else
    BufPtr := ActiveBuffer;

  Result := PCsvRow(BufPtr)^.bookmark.data; // Record number.

end;

procedure TJvCsvCustomInMemoryDataSet.SetRecNo(Value: Integer);
begin
  if (Value >= 0) and (Value <= FData.Count-1) then
  begin
    FRecordPos := Value - 1;
    if (RecordCount > 0) then Resync([]);
  end;
end;

procedure TJvCsvCustomInMemoryDataSet.SetTableName(const Value: string);
begin
  CheckInactive;
  FTableName := Value;
  if ExtractFileExt(FTableName) = '' then
    FTableName := FTableName + '.csv';

  { update internal filename table }
//  FBmkFileName:= ChangeFileExt(FTableName, '.bmk' ); // bookmark file
end;

function TJvCsvCustomInMemoryDataSet.GetDataFileSize: Integer;
//var
//  File1:TextFile;
begin
//  AssignFile(File1,FTableName);
//  Result := FileSize(File1);
//  CloseFile(File1);
  result := 8192; // not implemented yet.
end;

procedure TJvCsvCustomInMemoryDataSet.EmptyTable;
begin
  Close;
  DeleteFile(FTableName);
//  DeleteFile(FBmkFileName);
  Open;
end;


{ Support Delphi VCL TDataSetDesigner's field persistence }
procedure TJvCsvCustomInMemoryDataSet.DefChanged(Sender: TObject); //override;
begin
  FStoreDefs := true;
end;

{ Support Delphi VCL TDataSetDesigner's field persistence }
function TJvCsvCustomInMemoryDataSet.FieldDefsStored: Boolean;
begin
  Result := FStoreDefs and (FieldDefs.Count > 0);
end;

function TJvCsvCustomInMemoryDataSet.GetCanModify: Boolean; //override;
begin
 result := NOT FReadOnly; // You can modify if it's NOT read only.
end;




{ CsvColumns dynamic array of pointers }

procedure TJvCsvColumns.AddColumn(Item:PCsvColumn);
begin
   Add(Pointer(Item));
end;


function  TJvCsvColumns.FindByName(FieldName:String):PCsvColumn;
var
 t:Integer;
begin
  try
   for t := 0 to Count-1 do begin
      result := PCsvColumn(Get(t));
      if Assigned(Result.FFieldDef) then
        // Case insensitive field name matching:
        if CompareText(Result.FFieldDef.Name,FieldName)=0 then
            exit; //return that field was found! 
   end;
  except
   // ignore exceptions
  end;
  result := NIL;
end;

function  TJvCsvColumns.FindByFieldNo(FieldNo:Integer):PCsvColumn;
var
 t:Integer;
begin
  result := NIL;
  try
   for t := 0 to Count-1 do begin
      result := PCsvColumn(Get(t));
      if Assigned(Result) then
        if Assigned(Result^.FFieldDef) then
          if Result^.FFieldDef.FieldNo = FieldNo then
            exit; //return that field was found!
   end;
  except
   // ignore exceptions
   result := NIL;
  end;
end;


procedure TJvCsvColumns.Clear;
var
 t:Integer;
begin
 for t := 0 to Count-1 do
        FreeMem(Self[t]);
 inherited;
end;


{ CsvRows: dynamic array of pointers }

procedure TJvCsvRows.AddRow(const Item:PCsvRow);
begin
 Add(Pointer(Item));
end;

procedure TJvCsvRows.AddRowStr(const Item:String); // convert String->TJvCsvRow
var
  pNewItem : PCsvRow;
begin
  pNewItem := AllocMem(Sizeof(TJvCsvRow));
  StringToCsvRow(Item, pNewItem, true, FEnquoteBackslash); // decode a csv line that can contain escape sequences
  AddRow(pNewItem);
end;

function TJvCsvRows.GetRowPtr(const RowIndex:Integer):PCsvRow;
begin
  result := PCsvRow(Get(RowIndex)); // return pointer to a row item.
end;

function  TJvCsvRows.GetRowStr(const RowIndex:Integer):String;
var
 ResultStr:String;
begin
  CsvRowToString( GetRowPtr(RowIndex), ResultStr );
  result := ResultStr;
end;

procedure TJvCsvRows.SetRowStr(const RowIndex:Integer;Value:String);
begin
  StringToCsvRow(Value,GetRowPtr(RowIndex), true, FEnquoteBackslash );
end;

procedure TJvCsvRows.DeleteRow(const RowIndex:Integer);
var
  p:Pointer;
begin
 if (RowIndex>=0) and (RowIndex < Count) then begin
     p := Self[RowIndex];
     if p <> NIL then FreeMem(p);
 end;
 Delete(RowIndex);
end;

procedure TJvCsvRows.SetARowItem(const RowIndex,ColumnIndex:Integer; Value:String);
begin
  SetCsvRowItem(GetRowPtr(RowIndex),ColumnIndex,Value);
end;

function  TJvCsvRows.GetARowItem(const RowIndex,ColumnIndex:Integer):String;
begin
  result := GetCsvRowItem(GetRowPtr(RowIndex),ColumnIndex);
end;


procedure TJvCsvRows.Clear;
var
    t:Integer;
begin
  for t := 0 to Count-1 do
      FreeMem(Self[t]);
  inherited;
end;



{ Call this one first, then AssignFromStrings on subsequent updates only.}
procedure TJvCsvCustomInMemoryDataSet.OpenWith(Strings:TStrings);
begin
  Active := false;
  AssignFromStrings(Strings); // parse strings
end;

{ Call this one first, then AssignFromStrings on subsequent updates only.}
procedure TJvCsvCustomInMemoryDataSet.AppendWith(Strings:TStrings);
//var
//x:Integer;
begin

  //Active := false;
  AssignFromStrings(Strings); // parse strings
  // Refresh.
//  DataEvent(deDataSetChange, 0);
//  DataEvent(deRecordChange, 0);
//  Resync([]);
//  DataEvent(deUpdateState, 0);
 if Active then
  Last;

end;


{ Additional ZTR Methods Implementation  - internal use }
procedure TJvCsvCustomInMemoryDataSet.AssignFromStrings(const Strings:TStrings);
var
 HeaderRowFound:Boolean;
 t:Integer;
begin
// CheckInactive;
// if NOT FFieldsInitialized then
// InternalInitFieldDefs; // must know about field definitions first.
 if Strings = NIL then exit;
 FData.EnquoteBackslash := FEnquoteBackslash;

 HeaderRowFound := false;
 for t := 0 to Strings.Count-1 do begin
       // for now ignore any trace or debug data unless
       // someone has defined an event to handle it.
       if Pos('>>',Strings[t]) = 1 then begin
           if Assigned(FOnSpecialData) then
              FOnSpecialData(Self,t,Strings[t]);
           continue;
       end;

       // Process the row:
       if (NOT HeaderRowFound) and FHasHeaderRow then begin
         HeaderRowFound := true;
         FHeaderRow := Strings[t];
         //Note: later we will call ProcessCsvHeaderRow(FHeaderRow);
       end else
         ProcessCsvDataRow(Strings[t]);
 end;
end;

procedure TJvCsvCustomInMemoryDataSet.AssignToStrings(Strings:TStrings);
var
 t:Integer;
 Line:String;
begin
  // copy out the current data set to a TStringList.
  Strings.Clear;

  { Save header row with data rows? }
  if FHasHeaderRow then
    Strings.Add(GetColumnsAsString);

  for t := 0 to FData.Count-1 do begin
      CsvRowToString(FData.GetRowPtr(t),Line);
      Strings.Add(Line);
  end;
end;

function TJvCsvCustomInMemoryDataSet.GetRowAsString(const Index:Integer):String;
var
 ResultString:String;
begin
 CsvRowToString( FData.GetRowPtr(Index), ResultString );
 result := ResultString;
end;


// Get names of all the columns as a comma-separated string:
function TJvCsvCustomInMemoryDataSet.GetColumnsAsString:String;
var
 ResultString:String;
 t:Integer;
begin
 // ColCount:
 if FCsvColumns.count = 0 then begin
     ResultString := '';
     result := ResultString;
     exit;
 end;
 // Build a list of column names: <item>, <item>,....,<item>
 ResultString := FieldDefs[0].Name;
 for t := 1 to FCsvColumns.count-1 do
     ResultString := ResultString + ',' + FieldDefs[t].Name;
 result := ResultString;
end;

{ protected internal procedure - now that we have a list of fields that
  are supposed to exist in this dataset we have a real CSV header which we
  are hoping contains header information }
procedure TJvCsvCustomInMemoryDataSet.ProcessCsvHeaderRow(const header:String);
var
  CsvFieldRec : TJvCsvRow; // CSV Field record type.
  ptrCsvColumn  : PCsvColumn;
{  CsvFieldOption:String;}
  CsvFieldName:String;
{  pCsvFieldDef:PCsvColumn;}
  colnum, t:Integer;
begin
  if Length(header) = 0 then
     exit;

 // Initialize all ZTR CSV Column locations to a "not found yet" state:
 for t := 0 to FCsvColumns.Count-1 do
     PCsvColumn(FCsvColumns.Get(t))^.FPhysical := -1;

 StringToCsvRow(header,@CsvFieldRec, false, false);
 colnum := 0;
 while (CsvRowGetColumnMarker(@CsvFieldRec,colnum) <> COLUMN_ENDMARKER) do begin
   // Get a string in the format COLUMNAME:Options
   CsvFieldName := StrEatWhiteSpace(GetCsvRowItem( @CsvFieldRec, colnum ));

   if (Length(CsvFieldName)=0) then
      DatabaseError('ProcessCsvHeaderRow:Error processing first line of CSV file.',Self);

   ptrCsvColumn := FCsvColumns.FindByName(CsvFieldName);

   if (ptrCsvColumn=NIL) then begin // raise database exception:
     DatabaseError('ProcessCsvHeaderRow:Field '+CsvFieldName+' found in file, but not in field definitions.',Self);
     exit;
   end;
   
   try
     ptrCsvColumn^.FPhysical := colnum; // numbered from 0.
   except
     DatabaseError('ZTRDataSource field location error: '+CsvFieldName );
     break;
   end;
   inc(Colnum);
 end;

  // Check that everything was found and physically given a location
  // in the CSV file:

 for t := 0 to FCsvColumns.Count-1 do begin
    ptrCsvColumn := PCsvColumn(FCsvColumns[t]);
    if ptrCsvColumn^.FPhysical < 0 then begin
       DatabaseError('ZTRDataSource : Field '+ptrCsvColumn^.FFieldDef.Name+' not found in the data file.');
       exit;
    end;
 end;
end;

procedure TJvCsvCustomInMemoryDataSet.ProcessCsvDataRow(const datarow:String);
var
 pNewRow:PCsvRow;
begin
  if (Length(DataRow)=0) then
          exit;
  pNewRow := AllocMem(sizeof(TJvCsvRow));
  StringToCsvRow(datarow,pNewRow, true, FEnquoteBackslash);
  FData.AddRow(pNewRow);
end;


procedure TJvCsvCustomInMemoryDataSet.ExportCsvFile(const Filename:String); // save out to a file.
var
 Strings:TStringList;
begin
 Strings := TStringList.Create;
 AssignToStrings(Strings);
 Strings.SaveToFile(Filename);
 Strings.Free;
end;

function TJvCsvCustomInMemoryDataSet.GetCsvHeader:String;
var
  f:Text;
  FirstLine:String;
begin
  if (not FLoadsFromFile) or (not FHasHeaderRow) or not (FileExists(FTableName)) then begin
        result := '';
        exit;
  end;
  { How's this for an ancient Pascal code sequence, AssignFile+Reset is approximately equal to a C fopen() call }
  AssignFile(F, FTableName);
  Reset(F);
  { ReadLn is approximately a gets() call }
  ReadLn(F,FirstLine);
  { And finally, the pascal file close procedure }
  CloseFile(F);
  // return the first line of the file, without the junk
  result := StrStrip(FirstLine); // in JvCsvParse.pas
end;



{ PROCEDURES: }

// convert CSV Row buffer to a String
procedure CsvRowToString( RowItem:PCsvRow ; var RowString:String );
begin
 RowString := String(RowItem.Text);
end;

// convert String into a CSV Row buffer
procedure StringToCsvRow( const RowString:String   ; RowItem:PCsvRow ; permitEscapeSequences,enquoteBackslash:Boolean );
var
  t,l,col:Integer;
  quoteFlag:Boolean;
  skipFlag:Boolean;
  charsInColumn:Integer;
begin
 col := 0;
 RowItem^.wordfield[0] := 0; // zero out column marker and dirty bit!
 charsInColumn := 0;
 quoteFlag := false;
 skipFlag := false;
 l := Length(RowString); 
 for t := 1 to l do begin
    Inc(charsInColumn);
    if quoteFlag then begin
          // backslash permitted only if specifically enabled by FEnquoteBackslash:
          if permitEscapeSequences and (not skipFlag) and (enquoteBackslash) and (RowString[t] = '\') then begin
              skipFlag := true;
              continue;
          end;
          // doubled quotes handling:
          if permitEscapeSequences and (not skipFlag) and (RowString[t] = '"') and (t<l) and (RowString[t+1] = '"') then begin
              skipFlag := true;
              continue;
          end;
          // now if either of the above set the skipFlag true previously, we ALWAYS skip the next character here
          // and turn skipFlag back off
          if permitEscapeSequences and skipFlag then begin // skip next character, regardless.
              skipFlag := false;
              continue; // by skipping escaped quotes, we don't turn off quoteFlag in the middle of a string!
          end;
    end;
    //Now we know if we get this far, we are NOT dealing with any escaped characters
    //Any quotes we see here will turn on/off quote mode directly!
    if RowString[t] = '"' then begin
       if permitEscapeSequences then begin
        quoteFlag := not quoteFlag;
        if quoteFlag and (charsInColumn >1) then begin
             OutputDebugString('CsvDataSource.pas: StringToCsvRow - unescaped quote character in middle of string!');
        end;
             
       end else begin
              OutputDebugString('CsvDataSource.pas: StringToCsvRow - quote character found where no escape sequences are permitted!');
       end;
    end;

    if ((RowString[t] = ',') and (not quoteFlag))  then begin
       Inc(col);
       // implicitly set Length (low 15 bits) and clear dirty bit (high bit):
       RowItem.wordfield[col] := (Word(t) AND $7FFF); {note that we're going from 1..length }
       charsInColumn := 0;
    end;
    if (col >= MAXCOLUMNS) OR (t >= MAXLINELENGTH) then exit;
 end;    // end of string, new flag:
 Inc(col);
 if quoteFlag then begin
        OutputDebugString('CsvDataSource.pas: StringToCsvRow - Missing end quote character!');
 end;
 // Terminate the column-marker list with a special end-marker:
{RowItem.wordfield[col]   := Word(Length(RowString)+1)AND$7FFF; // length of string
 RowItem.wordfield[col+1] := COLUMN_ENDMARKER; // followed by an end marker}
 RowItem.wordfield[col] := COLUMN_ENDMARKER; // last one has no end marker 
 StrLCopy(RowItem.text, PChar(RowString), MAXLINELENGTH);
 RowItem.columns := col; // Check this later!
end;

// Copy a single column from one row buffer to another row buffer:
function CsvRowItemCopy( Source,Dest:PCsvRow; FieldIndex,FieldSize:Integer ):Boolean;
var
 TempStr:String;
begin
   TempStr := GetCsvRowItem(Source,FieldIndex);
   // length limiting feature:
   if (FieldSize>0) then
       if Length(TempStr)>FieldSize then
          TempStr := Copy(TempStr,1,FieldSize);
   SetCsvRowItem(Dest,FieldIndex,TempStr);
   result := true;
end;


// Copy an item into a csv row buffer:
procedure SetCsvRowItem( pItem:PCsvRow; ColumnIndex:Integer; NewValue:String);
var
  TempBuf:Array[0..MAXLINELENGTH] of Char;
  Copy1,Copy2:Integer;
  Dif,t,Old:Integer;
begin
 Dif := 0;
 if (ColumnIndex<0) OR (ColumnIndex>MAXCOLUMNS) then
    exit;
 Copy1 := CsvRowGetColumnMarker(pItem,ColumnIndex);
 if (Copy1 = COLUMN_ENDMARKER) then
              exit;

 if (Copy1 > MAXLINELENGTH) then
              exit;
 // copy initial part of the csv row:
 if (Copy1 > 0 ) then begin
   StrLCopy( TempBuf, pItem.text, Copy1 );
   StrLCat( TempBuf, PChar(NewValue), MAXLINELENGTH );
 end else
   StrLCopy( Tempbuf, PChar(NewValue), MAXLINELENGTH );

 Copy2 := CsvRowGetColumnMarker(pItem,ColumnIndex+1);
 if (Copy2 <> COLUMN_ENDMARKER) then begin
   // difference in length:
   Dec(Copy2); // subtract one.
   if (Copy2 < 0) then exit;
   if (Length(NewValue) = Copy2-Copy1) then
      Dif := 0
   else
      Dif := Length(NewValue) - (Copy2-Copy1);
   StrLCat( TempBuf, pItem^.text+Copy2,  MAXLINELENGTH );
 end;

 // Copy over the old memory buffer:
 StrLCopy( pItem^.text,  TempBuf, MAXLINELENGTH );

 // Now that we've copied a new item of a different length into the place of the old one
 // we have to update the positions of the columns after ColumnIndex:
 if (Dif <> 0) then
   for t := ColumnIndex+1 to MAXCOLUMNS do begin
     Old := CsvRowGetColumnMarker(pItem,t); 
     if (Old=COLUMN_ENDMARKER) then exit;
     CsvRowSetColumnMarker(pItem,t,Old+Dif );
   end;

end;

// Copy an item out of a csv row buffer:
function GetCsvRowItem( pItem:PCsvRow; ColumnIndex:Integer):String;
var
  TempBuf:Array[0..MAXLINELENGTH] of Char;
  Copy1,Copy2:Integer;
begin
 if (ColumnIndex<0) OR (ColumnIndex>MAXCOLUMNS) then begin
    result := '<ERROR>';
    exit;
 end;

 Copy1 := CsvRowGetColumnMarker(pItem,ColumnIndex);
 Copy2 := CsvRowGetColumnMarker(pItem,ColumnIndex+1);
 if (Copy1 = COLUMN_ENDMARKER) then begin
              result := '';
              exit;
 end;
 if (Copy2 = COLUMN_ENDMARKER) then // copy the rest of the line
    Copy2 := MAXLINELENGTH-Copy1 // All the characters left in the buffer
 else
    Dec(Copy2);
    
 if (Copy1 > MAXLINELENGTH) or (Copy2 > MAXLINELENGTH) then begin
     result := '';
     exit;
 end;

 // Copy out just one column from the string:
 StrLCopy( TempBuf, pItem.Text+Copy1, Copy2-Copy1 );
 PcharEatWs(@TempBuf[0]);
 result := String(TempBuf);
end;

{new}
  procedure CsvRowSetDirtyBit(row:pCsvRow;ColumnIndex:Integer);
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    row^.fdirty := true; // triggers search for 'dirty bit' in columns
    row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] OR $8000);
  end;

  procedure CsvRowClearDirtyBit(row:pCsvRow;ColumnIndex:Integer);
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    row^.wordfield[ColumnIndex] := (row^.wordfield[ColumnIndex] AND $7FFF);
  end;

  function CsvRowGetDirtyBit(row:pCsvRow;ColumnIndex:Integer):Boolean;
  begin
    result := false;
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    if row^.wordfield[ColumnIndex] = COLUMN_ENDMARKER then exit;
    result := (row^.wordfield[ColumnIndex] AND $8000) <> 0;
  end;

  procedure CsvRowSetColumnMarker(row:pCsvRow;ColumnIndex:Integer;ColumnMarker:Integer);
  var
   Old:Word;
  begin
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    if (ColumnMarker<0) then exit;

    if ColumnMarker = COLUMN_ENDMARKER then
        row^.wordfield[ColumnIndex] := COLUMN_ENDMARKER
    else begin
        Old := row^.wordfield[ColumnIndex];
        if Old = COLUMN_ENDMARKER then
            row^.wordfield[ColumnIndex] := ColumnMarker AND $7FFF // auto-clear Dirty bit
        else
            row^.wordfield[ColumnIndex] := ( Old AND $8000 ) // Keep Old Dirty Bit
                   OR ( Word( ColumnMarker ) AND $7FFF ); // new value.
    end;
  end;

  function CsvRowGetColumnMarker(row:pCsvRow;ColumnIndex:Integer):Integer;
  var
    w:Word;
  begin
    result := -1;
    if ROW = NIL then exit;
    if (ColumnIndex<0)or(ColumnIndex>=MAXCOLUMNS) then exit;
    w := row^.wordfield[ColumnIndex];
    if  w = COLUMN_ENDMARKER then
      result := COLUMN_ENDMARKER
    else
      result := Integer( w and $7FFF );
  end;
{endnew}


 //------------------------------------------------------------------------------
 // ZtrHexToDateTime
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
function TimeTHexToDateTime( HexStr:String ; TimeZoneCorrection:Integer ) : TDateTime;
var
 SecondsSince1970 : Double;
 Base:TDateTime;
{ DateTimeAsStr:String; //debug Code.}
begin
 result := 0.0;
 SecondsSince1970 := StrToIntDef('$' + HexStr,0)+TimeZoneCorrection;
 if (SecondsSince1970 <= 0.0) then exit;
 Base := EncodeDate(1970, 1, 1);
 Base := Base + (SecondsSince1970 / 86400.0);
{ DateTimeAsStr := FormatDateTime('yyyy/mm/dd hh:nn:ss',Base);}
 Inc(CallCount);
 result := Base;
end;


function TimeTAsciiToDateTime( AsciiDateStr:String ):TDateTime;
const
 Separators =  '// ::';  // separators in yyyy/mm/dd hh:mm:ss
 Separators2 =  '-- --';  // separators in yyyy/mm/dd hh:mm:ss
var
 Values       : Array [1..6] of Integer; //year,month,day,hour,minute,second in that order.
 ExpectLengths:Array [1..6] of Integer;
 MinValue:Array [1..6] of Integer;
 MaxValue:Array [1..6] of Integer;
 ch           : Char;
 t,u,len,index: Integer;
begin
 result := 0.0; // default result.
 len := Length(AsciiDateStr);

 // validate ranges:
 MinValue[1] := 1990;
 MaxValue[1] := 2999; // This code suffers from the Y3K bug. If you care, get a life.

 MinValue[2] := 1;  // Hope they never add more months to the year, eh?
 MaxValue[2] := 12;

 MinValue[3] := 1;  // We don't bother about checking if the month has 31 days.
 MaxValue[3] := 31;

 MinValue[4] := 0;  // We use military time 00 is midnight, 23 is 11 pm.
 MaxValue[4] := 23;

 MinValue[5] := 0;  // Minute value is 00 to 59
 MaxValue[5] := 59;

 MinValue[6] := 0;  // Second value is 00 to 59
 MaxValue[5] := 59;

 // expect values with length of 4,2,2,2,2,2 ...
 ExpectLengths[1] := 4;
 values[1]        := 0;
 for t := 2 to 6 do begin
   ExpectLengths[t] := 2;
   values[t]        := 0;
 end;



 // T loops through each value we are looking for (1..6):
 index := 1; // what character in AsciiDateStr are we looking at?
 for t := 1 to 6 do begin
   if (t>=3) and (index >= len) then
        break; // as long as we at least got the date, we can continue.
   for u := 1 to ExpectLengths[t] do begin
      if (index > len) then
            exit;
      ch := AsciiDateStr[index];
      if (ch < '0') OR (ch > '9') then exit; // failed:invalid character.
      values[t] := (values[t]*10)+ (Ord(ch)-Ord('0'));
      inc(Index);

      if (index > len) then
            break;
   end;
   
   // if we haven't reached the end of the string, then
   // check for a valid separator character:
   if (index<len) then
     if (AsciiDateStr[index] <> Separators[t])
        and (AsciiDateStr[index] <> Separators2[t]) then
        exit;

   // validate ranges:
   if (Values[t] < MinValue[t]) OR (Values[t] > MaxValue[t]) then
        exit; // a value is out of range.
   Inc(Index);
 end;

  // Now that we probably have a valid value we will try to encode it.
  // EncodeData will catch any invalid date values we have let slip through
  // such as trying to encode February 29 on a non-leap year, or the 31st
  // day of a month with only 30 days, etc.
 try
  result :=   EncodeDate( {year} Values[1], {month}  Values[2], {day}    Values[3] )
            + EncodeTime( {hour} Values[4], {minute} Values[5], {second} Values[6], {msec} 0 );
 except
   on E:EConvertError do begin
     result := 0.0; // catch any other conversion errors and just return 0.
   end;
 end;
end;


function DateTimeToTimeTHex(aDateTime:TDateTime; TimeZoneCorrection:Integer ):String;
var
  Base             : TDateTime;
{  DateTimeAsStr    : String; //debug Code. }
  SecondsSince1970 : Integer;
begin
  try
     Base := EncodeDate(1970, 1, 1);
     SecondsSince1970 := Trunc( (aDateTime-Base) * 86400.0 );
     result := IntToHex(SecondsSince1970-TimeZoneCorrection, 8);
  except
     // Catch Failures!
     result := '';
  end;
end;

function DateTimeToTimeToIsoAscii(aDateTime:TDateTime):String;
begin
  // ISO DATETIME FORMAT:
 result := FormatDateTime( 'yyyy-mm-dd hh:nn:ss', aDateTime );
end;





initialization
  CallCount := 0;
end.

