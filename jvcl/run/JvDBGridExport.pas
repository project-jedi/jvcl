{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBGridExport.pas, released on 2004-01-15

The Initial Developer of the Original Code is Lionel Renayud
Portions created by Lionel Renayud are Copyright (C) 2004 Lionel Renayud.
All Rights Reserved.

Contributor(s):

Last Modified: 2004-01-15

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}
unit JvDBGridExport;

interface

uses
  Windows, Classes, SysUtils, DB, DBGrids,
//  Word2000,
  JvComponent, JvTypes;

type
  TExportDestination = (edFile, edClipboard);
  TExportSeparator = (esTab, esSemiColon, esComma, esSpace, esPipe);
  TWordOrientation = (woPortrait, woLandscape);

type
  EJvExportDBGridException = class(EJVCLException);
  TWordGridFormat = $10..$17;

{ avoid Office TLB imports }
const
  wdDoNotSaveChanges = 0;

  wdTableFormatGrid1 = TWordGridFormat($10);
  wdTableFormatGrid2 = TWordGridFormat($11);
  wdTableFormatGrid3 = TWordGridFormat($12);
  wdTableFormatGrid4 = TWordGridFormat($13);
  wdTableFormatGrid5 = TWordGridFormat($14);
  wdTableFormatGrid6 = TWordGridFormat($15);
  wdTableFormatGrid7 = TWordGridFormat($16);
  wdTableFormatGrid8 = TWordGridFormat($17);

  xlPortrait = $01;
  xlLandscape = $02;

type
  TJvExportProgressEvent = procedure(Sender: TObject; Min, Max, Position: Cardinal; const AText: string; var AContinue: boolean) of object;
  TJvCustomDBGridExport = class(TJvComponent)
  private
    FGrid: TDBGrid;
    FCaption: string;
    FFilename: TFilename;
    FOnProgress: TJvExportProgressEvent;
    FLastExceptionMessage: string;
    FSilent: boolean;
    FOnException: TNotifyEvent;
  protected
    procedure HandleException;
    function ExportField(aField: TField): boolean;
    function DoProgress(Min, Max, Position: Cardinal; const AText: string): boolean; virtual;
    function DoExport: boolean; virtual; abstract;
    procedure DoSave; virtual;
    procedure DoClose; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function ExportGrid: boolean;
    property LastExceptionMessage: string read FLastExceptionMessage;
  published
    property Caption: string read FCaption write FCaption;
    property Filename: TFilename read FFilename write FFilename;
    property Grid: TDBGrid read FGrid write FGrid;
    property Silent: boolean read FSilent write FSilent default true;
    property OnProgress: TJvExportProgressEvent read FOnProgress write FOnProgress;
    property OnException:TNotifyEvent read FOnException write FOnException;
  end;

  TJvCustomDBGridExportClass = class of TJvCustomDBGridExport;

  TJvDBGridWordExport = class(TJvCustomDBGridExport)
  private
    FWord: OleVariant;
    FVisible: boolean;
    FOrientation: TWordOrientation;
    FWordFormat: TWordGridFormat;
    FClose: boolean;
    FRunningInstance:boolean;
  protected
    procedure DoSave; override;
    function DoExport: boolean; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filename;
    property Caption;
    property Grid;
    property OnProgress;

    property Close: boolean read FClose write FClose default true;
    property WordFormat: TWordGridFormat read FWordFormat write FWordFormat default wdTableFormatGrid3;
    property Visible: boolean read FVisible write FVisible default false;
    property Orientation: TWordOrientation read FOrientation write FOrientation default woPortrait;
  end;
                      
  TJvDBGridExcelExport = class(TJvCustomDBGridExport)
  private
    FExcel: OleVariant;
    FVisible: boolean;
    FOrientation: TWordOrientation;
    FClose: boolean;
    FRunningInstance:boolean;
    function IndexFieldToExcel(index: integer): string;
  protected
    procedure DoSave; override;
    function DoExport: boolean; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filename;
    property Caption;
    property Grid;
    property OnProgress;

    property Close: boolean read FClose write FClose default true;
    property Visible: boolean read FVisible write FVisible default false;
    property Orientation: TWordOrientation read FOrientation write FOrientation default woPortrait;
  end;

  TJvDBGridHTMLExport = class(TJvCustomDBGridExport)
  private
    FDocument: TStringList;
    FDocTitle: string;
    FFooter: TStrings;
    FHeader: TStrings;
    FIncludeColumnHeader: boolean;
    procedure SetFooter(const Value: TStrings);
    procedure SetHeader(const Value: TStrings);
  protected
    procedure DoSave; override;
    function DoExport: boolean; override;
    procedure DoClose; override;
    procedure SetDefaultData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filename;
    property Caption;
    property Grid;
    property OnProgress;
    property IncludeColumnHeader:boolean read FIncludeColumnHeader write FIncludeColumnHeader default true;
    property Header: TStrings read FHeader write SetHeader;
    property Footer: TStrings read FFooter write SetFooter;
    property DocTitle: string read FDocTitle write FDocTitle;
  end;

  TJvDBGridCSVExport = class(TJvCustomDBGridExport)
  private
    FDocument: TStringList;
    FDestination: TExportDestination;
    FExportSeparator: TExportSeparator;
    procedure SetExportSeparator(const Value: TExportSeparator);
    function SeparatorToString(aSeparator: TExportSeparator): string;
    procedure SetDestination(const Value: TExportDestination);
  protected
    function DoExport: boolean; override;
    procedure DoSave; override;
    procedure DoClose; override;
  public
    Separator: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Filename;
    property Caption;
    property Grid;
    property OnProgress;

    property Destination: TExportDestination read FDestination write SetDestination default edFile;
    property ExportSeparator: TExportSeparator read FExportSeparator write SetExportSeparator default esTab;
  end;

function WordGridFormatIdentToInt(const Ident: string; var Value: Longint): Boolean;
function IntToWordGridFormatIdent(Value: Longint; var Ident: string): Boolean;
procedure GetWordGridFormatValues(Proc: TGetStrProc);

implementation

uses
{$IFDEF COMPILER6_UP}
  Variants,
{$ENDIF COMPILER6_UP}

//  Excel2000,
  JclRegistry,
  Forms, Controls, Clipbrd, ComObj, Graphics;

resourcestring
  RsDataSetIsUnassigned = 'Dataset or DataSource unassigned';
  RsGridIsUnassigned = 'No grid assigned';
  RsHTMLExportDocTitle = 'Grid to HTML Export';
  RsExportWord = 'Exporting to MS Word...';
  RsExportExcel = 'Exporting to MS Excel...';
  RsExportHTML = 'Exporting to HTML...';
  RsExportFile = 'Exporting to CSV/Text...';
  RsExportClipboard = 'Exporting to Clipboard...';

// ***********************************************************************
// TJvCustomDBGridExport
// ***********************************************************************

constructor TJvCustomDBGridExport.Create(AOwner: TComponent);
begin
  inherited;
  FSilent := true;
end;

function TJvCustomDBGridExport.DoProgress(Min, Max, Position: Cardinal;
  const AText: string): boolean;
begin
  Result := true;
  if Assigned(FOnProgress) then
    FOnProgress(self, Min, max, Position, AText, Result);
end;

procedure TJvCustomDBGridExport.DoSave;
begin
  if FileExists(Filename) then DeleteFile(Filename);
end;

function TJvCustomDBGridExport.ExportField(aField: TField): boolean;
begin
  Result := not (aField.DataType in [ftUnknown, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftADT,
      ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
      ftInterface, ftIDispatch, ftGuid]);
end;

function TJvCustomDBGridExport.ExportGrid: boolean;
begin
  if not Assigned(Grid) then
    raise EJvExportDBGridException.Create(RsGridIsUnassigned);
  if not Assigned(Grid.DataSource) or not Assigned(Grid.DataSource.DataSet) then
    raise EJvExportDBGridException.Create(RsDataSetIsUnassigned);
//  if Filename = '' then
//    raise EJvExportDBGridException.Create(RsFilenameEmpty);
  Result := DoExport;
  if Result then
    DoSave;
  DoClose;
end;

procedure TJvCustomDBGridExport.HandleException;
begin
  if ExceptObject <> nil then
  begin
    if ExceptObject is Exception then
      FLastExceptionMessage := Exception(ExceptObject).Message;
    if not Silent then
      raise ExceptObject at ExceptAddr
    else if Assigned(FOnException) then
      FOnException(self);
  end;
end;

procedure TJvCustomDBGridExport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = Grid) then
    Grid := nil;
end;

// ***********************************************************************
// TJvDBGridWordExport
// ***********************************************************************

constructor TJvDBGridWordExport.Create(AOwner: TComponent);
begin
  inherited;
  Caption := RsExportWord;
  FWord := Unassigned;
  FVisible := false;
  FOrientation := woPortrait;
  FWordFormat := wdTableFormatGrid3;
  FClose := true;
end;

destructor TJvDBGridWordExport.Destroy;
begin
  DoClose;
  inherited;
end;

function TJvDBGridWordExport.DoExport: boolean;
var
  i, j, k: integer;
  lTable: OleVariant;
  ARecNo, lRecCount, lColCount: integer;
  lColVisible: integer;
  lRowCount: integer;
  lBookmark: TBookmark;
begin
  Result := true;
  FRunningInstance := true;
  try
    // get running instance
    FWord := GetActiveOleObject('Word.Application');
  except
    FRunningInstance := false;
    try
      // create new
      FWord := CreateOLEObject('Word.Application');
    except
      FWord := Unassigned;
      HandleException;
//      raise EJvExportDBGridException.Create(RsNoWordApplication);
    end;
  end;

  if VarIsEmpty(FWord) then Exit;

  try
    FWord.Visible := FVisible;
    FWord.Documents.Add;

    lColCount := Grid.Columns.Count;
    lColVisible := 0;
    for i := 1 to lColCount do
      if Grid.Columns[i - 1].Visible then Inc(lColVisible);

    lRowCount := Grid.DataSource.DataSet.RecordCount;
    FWord.ActiveDocument.Range.Font.Name := Grid.Font.Name;
    FWord.ActiveDocument.Range.Font.Size := Grid.Font.Size;
    if Orientation = woPortrait then
      FWord.ActiveDocument.PageSetup.Orientation := 0
    else
      FWord.ActiveDocument.PageSetup.Orientation := 1;
    lTable := FWord.ActiveDocument.Tables.Add(FWord.ActiveDocument.Range, lRowCount + 1, lColVisible);
    FWord.ActiveDocument.Range.InsertAfter('Date ' + Datetimetostr(Now));
    lTable.AutoFormat(Format := WordFormat); // FormatNum, 1, 1, 1, 1, 1, 0, 0, 0, 1

    k := 1;
    for i := 1 to lColCount do
      if Grid.Columns[i - 1].Visible then
      begin
        lTable.Cell(1, k).Range.InsertAfter(Grid.Columns[i - 1].Title.Caption);
        Inc(k);
      end;

    j := 2;
    with Grid.DataSource.DataSet do
    begin
      lRecCount := RecordCount;
      ARecNo := 0;
      DoProgress(0, lRecCount, ARecNo, Caption);
      DisableControls;
      lBookmark := GetBookmark;
      First;
      try
        while not Eof do
        begin
          k := 1;
          for i := 1 to lColCount do
            if (Grid.Columns[i - 1].Visible and not Fields[i - 1].IsNull and ExportField(Fields[i - 1])) then
            try
              lTable.Cell(j, k).Range.InsertAfter(string(Fields[i - 1].Value));
              Inc(k);
            except
              Result := false;
              HandleException;
              // Remember problem but continue
            end;
          Next;
          Inc(j);
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then Last;
        end;
        DoProgress(0, lRecCount, lRecCount, Caption);
      finally
        try
          if BookmarkValid(lBookMark) then
            GotoBookmark(lBookmark);
        except
          HandleException;
        end;
        if lBookMark <> nil then
          FreeBookmark(lBookmark);
        EnableControls;
      end;
    end;
    lTable.UpdateAutoFormat;
  except
    HandleException;
  end;
end;

procedure TJvDBGridWordExport.DoSave;
var
  lName: OleVariant;
begin
  inherited;
  if VarIsEmpty(FWord) then Exit;
  try
    lName := OleVariant(FileName);
    FWord.ActiveDocument.SaveAs(lName);
  except
    HandleException;
  end;
end;

procedure TJvDBGridWordExport.DoClose;
begin
  if not VarIsEmpty(FWord) and FClose then
  try
    FWord.ActiveDocument.Close(wdDoNotSaveChanges, EmptyParam, EmptyParam);
    if not FRunningInstance then
      FWord.Quit; // only quit if we created the instance ourselves
    FWord := Unassigned;
  except
    HandleException;
  end;
end;

// ***********************************************************************
// TJvDBGridExcelExport
// ***********************************************************************

constructor TJvDBGridExcelExport.Create(AOwner: TComponent);
begin
  inherited;
  Caption := RsExportExcel;
  FExcel := Unassigned;
  FVisible := false;
  FOrientation := woPortrait;
  FClose := true;
end;

destructor TJvDBGridExcelExport.Destroy;
begin
  DoClose;
  inherited;
end;

function TJvDBGridExcelExport.IndexFieldToExcel(index: integer): string;
begin
  // Max colonne : ZZ => index = 702
  if (index > 26) then
    Result := Chr(64 + ((index - 1) div 26)) + Chr(65 + ((index - 1) mod 26))
  else Result := Chr(64 + index);
end;

function TJvDBGridExcelExport.DoExport: boolean;
var
  i, j, k: integer;
  lTable: OleVariant;
  lCell: OleVariant;
  ARecNo, lRecCount, lColCount: integer;
  lBookmark: TBookmark;
begin
  Result := true;
  FRunningInstance := true;
  try
    // get running instance
    FExcel := GetActiveOleObject('Excel.Application');
  except
    FRunningInstance := false;
    try
      // create new instance
      FExcel := CreateOLEObject('Excel.Application');
    except
      FExcel := Unassigned;
      HandleException;
    end;
  end;

  if VarIsEmpty(FExcel) then Exit;
  try
    FExcel.WorkBooks.Add;
    FExcel.Visible := Visible;

    lColCount := Grid.Columns.Count;

    lTable := FExcel.ActiveWorkbook.ActiveSheet;
    if Orientation = woPortrait then
      lTable.PageSetup.Orientation := xlPortrait
    else lTable.PageSetup.Orientation := xlLandscape;

    k := 1;
    for i := 1 to lColCount do
      if Grid.Columns[i - 1].Visible then
      begin
        lCell := lTable.Range[IndexFieldToExcel(k) + '1'];
        lCell.Value := Grid.Columns[i - 1].Title.Caption;
        Inc(k);
      end;

    j := 1;
    with Grid.DataSource.DataSet do
    begin
      ARecNo := 0;
      lRecCount := RecordCount;
      DoProgress(0, lRecCount, ARecNo, Caption);
      DisableControls;
      lBookmark := GetBookmark;
      First;
      try
        while not Eof do
        begin
          Inc(j);
          k := 1;
          for i := 1 to lColCount do
            if (Grid.Columns[i - 1].Visible and ExportField(Fields[i - 1])) then
            begin
              lCell := lTable.Range[IndexFieldToExcel(k) + IntToStr(j)];
              Inc(k);
              try
                lCell.Value := string(Fields[i - 1].Value);
              except
                Result := false;
                HandleException;
              end;
            end;
          Next;
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then Last;
        end;
        DoProgress(0, lRecCount, lRecCount, Caption);
      finally
        try
          if BookMarkValid(lBookMark) then
            GotoBookmark(lBookmark);
        except
          HandleException;
        end;
        if lBookmark <> nil then
          FreeBookmark(lBookmark);
        EnableControls;
      end;
    end;
  except
    HandleException;
  end;
end;

procedure TJvDBGridExcelExport.DoSave;
var
  lName: OleVariant;
begin
  inherited;
  if not VarIsEmpty(FExcel) then 
  try
    lName := OleVariant(FileName);
    FExcel.ActiveWorkbook.SaveAs(lName);
  except
    HandleException;
  end;
end;

procedure TJvDBGridExcelExport.DoClose;
begin
  if not VarIsEmpty(FExcel) and FClose then
  try
    FExcel.ActiveWorkbook.Saved := true; // Avoid Excel's save prompt
    FExcel.ActiveWorkbook.Close;
    if not FRunningInstance then
      FExcel.Quit; // only quit if we created the instance ourselves
    FExcel := Unassigned;
  except
    HandleException;
  end;
end;

// ***********************************************************************
// TJvDBGridHTMLExport
// ***********************************************************************

constructor TJvDBGridHTMLExport.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TStringList.Create;
  Caption := RsExportHTML;
  FDocTitle := RsHTMLExportDocTitle;
  FHeader := TStringlist.Create;
  FFooter := TStringlist.Create;
  FIncludeColumnHeader := true;
  SetDefaultData;
end;

destructor TJvDBGridHTMLExport.Destroy;
begin
  FDocument.Free;
  inherited;
end;

procedure TJvDBGridHTMLExport.DoClose;
begin
 // do nothing
end;

function TJvDBGridHTMLExport.DoExport: boolean;
var
  i: integer;
  ARecNo, lRecCount, lColCount: integer;
  lBookmark: TBookmark;
  lString, lText: string;
  function AlignmentToHTML(AAlign: TAlignment): string;
  begin
    case AAlign of
      taLeftJustify:
        Result := 'left';
      taRightJustify:
        Result := 'right';
      taCenter:
        Result := 'center';
    end;
  end;
  function ColorToHTML(AColor: TColor): string;
  var r, g, b: byte;
  begin
    AColor := ColorToRGB(AColor);
    r := GetRValue(AColor);
    g := GetGValue(AColor);
    b := GetBValue(AColor);
    Result := Format('%.2x%.2x%.2x', [r, g, b]);
  end;
  function FontSubstitute(const Name: string): string;
  const
    cIsNT: array[boolean] of PChar = ('SOFTWARE\Microsoft\Windows\CurrentVersion\FontSubstitutes', 'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes');
  begin
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE, cIsNT[Win32Platform = VER_PLATFORM_WIN32_NT], Name, Name);
  end;
  function FontSizeToHTML(PtSize: integer): integer;
  begin
    case abs(PtSize) of
      0..8:
        Result := 1;
      9..10:
        Result := 2;
      11..12:
        Result := 3;
      13..17:
        Result := 4;
      18..23:
        Result := 5;
      24..35:
        Result := 6;
    else
      Result := 7;
    end;
  end;
  function FontToHTML(AFont: TFont; EncloseText: string): string;
  begin
    if fsBold in AFont.Style then
      EncloseText := '<b>' + EncloseText + '</b>';
    if fsItalic in AFont.Style then
      EncloseText := '<i>' + EncloseText + '</i>';
    if fsUnderline in AFont.Style then
      EncloseText := '<u>' + EncloseText + '</u>';
    if fsStrikeout in AFont.Style then
      EncloseText := '<s>' + EncloseText + '</s>';
    Result := Format('<font face="%s" color="#%s" size="%d">%s</font>',
      [FontSubstitute(AFont.Name), ColorToHTML(AFont.Color), FontSizeToHTML(AFont.Size), EncloseText]);
  end;
begin
  FDocument.Clear;
  lColCount := Grid.Columns.Count;
  Result := true;
  try
    lString := StringReplace(Header.Text,'<#TITLE>',DocTitle,[rfReplaceAll, rfIgnoreCase]);
    FDocument.Add(lString);
    FDocument.Add('<table width="90%" border="1" cellspacing="0" cellpadding="0">');
    if IncludeColumnHeader then
    begin
      lString := '<tr>';
      for i := 1 to lColCount do
        with Grid.Columns[i - 1] do
          if Visible then
          begin
            lString := lString + Format('<th bgcolor="#%s" align="%s">%s</th>',
              [ColorToHTML(Title.Color), AlignmentToHTML(Alignment), FontToHTML(Title.Font, Title.Caption)]);
          end;
      lString := lString + '</tr>';
    end;
    FDocument.Add(lString);

    with Grid.DataSource.DataSet do
    begin
      ARecNo := 0;
      lRecCount := RecordCount;
      DoProgress(0, lRecCount, ARecNo, Caption);
      DisableControls;
      lBookmark := GetBookmark;
      First;
      try
        while not Eof do
        begin
          lString := '<tr>';
          for i := 1 to lColCount do
            if (Grid.Columns[i - 1].Visible and ExportField(Fields[i - 1])) then
            try
              if Fields[i - 1].IsNull then
                lText := '&nbsp;'
              else
                lText := Fields[i - 1].AsString;
              lString := lString + Format('<td bgcolor="#%s" align="%s">%s</td>',
                [ColorToHTML(Grid.Columns[i - 1].Color),
                AlignmentToHTML(Grid.Columns[i - 1].Alignment),
                  FontToHTML(Grid.Columns[i - 1].Font, lText)]);
            except
              Result := false;
              HandleException;
            end;
          lString := lString + '</tr>';
          FDocument.Add(lString);
          Next;
          if not DoProgress(0, lRecCount, ARecNo, Caption) then Last;
        end;
        FDocument.Add('</table>');
        FDocument.AddStrings(Footer);
        DoProgress(0, lRecCount, lRecCount, Caption);
      finally
        try
          if BookmarkValid(lBookmark) then
            GotoBookmark(lBookmark);
        except
          HandleException;
        end;
        if lBookmark <> nil then
          FreeBookmark(lBookmark);
        EnableControls;
      end;
    end;
  except
    HandleException;
  end;
end;

procedure TJvDBGridHTMLExport.DoSave;
begin
  inherited;
  FDocument.SaveToFile(FileName);
end;

// ***********************************************************************
// TJvDBGridCSVExport
// ***********************************************************************

constructor TJvDBGridCSVExport.Create(AOwner: TComponent);
begin
  inherited;
  FDocument := TStringList.Create;
  FDestination := edFile;
  ExportSeparator := esTab;
  Caption := RsExportFile;
end;

destructor TJvDBGridCSVExport.Destroy;
begin
  FDocument.Free;
  inherited;
end;

function TJvDBGridCSVExport.SeparatorToString(aSeparator: TExportSeparator): string;
begin
  case aSeparator of
    esTab: Result := #9;
    esSemiColon: Result := ';';
    esComma: Result := ',';
    esSpace: Result := ' ';
    esPipe: Result := '|';
  end;
end;

procedure TJvDBGridCSVExport.SetExportSeparator(const Value: TExportSeparator);
begin
  FExportSeparator := Value;
  Separator := SeparatorToString(FExportSeparator);
end;

procedure TJvDBGridCSVExport.SetDestination(const Value: TExportDestination);
begin
  FDestination := Value;
  if FDestination = edFile then
    Caption := RsExportFile
  else
    Caption := RsExportClipboard;
end;

function TJvDBGridCSVExport.DoExport: boolean;
var
  i: integer;
  ARecNo, lRecCount, lColCount: integer;
  lBookmark: TBookmark;
  lString, lField: string;
begin
  FDocument.Clear;
  lColCount := Grid.Columns.Count;
  Result := true;
  try
    with Grid.DataSource.DataSet do
    begin
      ARecNo := 0;
      lRecCount := RecordCount;
      DoProgress(0, lRecCount, ARecNo, Caption);
      DisableControls;
      lBookmark := GetBookmark;
      First;
      try
        while not Eof do
        begin
          lString := '';
          for i := 1 to lColCount do
            if (Grid.Columns[i - 1].Visible and ExportField(Fields[i - 1])) then
            try
              if not Fields[i - 1].IsNull then
              begin
                lField := Fields[i - 1].AsString;
                if Pos(Separator, lField) <> 0 then
                  lString := lString + '"' + lField + '"'
                else lString := lString + lField;
              end;
              lString := lString + Separator;
            except
              Result := false;
              HandleException;
            end;
          FDocument.Add(lString);
          Next;
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then Last;
        end;
        DoProgress(0, lRecCount, lRecCount, Caption);
      finally
        try
          if BookmarkValid(lBookMark) then
            GotoBookmark(lBookmark);
        except
          HandleException;
        end;
        if lBookMark <> nil then
          FreeBookmark(lBookmark);
        EnableControls;
      end;
    end;
  except
    HandleException;
  end;
end;

procedure TJvDBGridCSVExport.DoSave;
begin
  inherited;
  if Destination = edFile then
    FDocument.SaveToFile(FileName)
  else
    Clipboard.AsText := FDocument.Text;
end;


procedure TJvDBGridCSVExport.DoClose;
begin
  // do nothing
end;

procedure TJvDBGridHTMLExport.SetDefaultData;
begin
  FHeader.Add('<html><head><title><#TITLE></title></head><body>');
  FFooter.Add('</body></html>');
end;

procedure TJvDBGridHTMLExport.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

procedure TJvDBGridHTMLExport.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

type
  TGridValue = packed record
    Value:integer;
    Name:PChar;
  end;

const
  GridFormats: array[$10..$17] of TGridValue =
    ((Value:$10;Name:'wdTableFormatGrid1'),
    (Value:$11;Name:'wdTableFormatGrid2'),
    (Value:$12;Name:'wdTableFormatGrid3'),
    (Value:$13;Name:'wdTableFormatGrid4'),
    (Value:$14;Name:'wdTableFormatGrid5'),
    (Value:$15;Name:'wdTableFormatGrid6'),
    (Value:$16;Name:'wdTableFormatGrid7'),
    (Value:$17;Name:'wdTableFormatGrid8'));

function WordGridFormatIdentToInt(const Ident: string; var Value: Longint): Boolean;
var i:integer;
begin
  for i := Low(GridFormats) to High(GridFormats) do
    if SameText(GridFormats[i].Name, Ident) then
    begin
      Result := true;
      Value := GridFormats[i].Value;
      Exit;
    end;
  Result := false;
end;

function IntToWordGridFormatIdent(Value: Longint; var Ident: string): Boolean;
var i:integer;
begin
  for i := Low(GridFormats) to High(GridFormats) do
    if GridFormats[i].Value = Value then
    begin
      Result := true;
      Ident := GridFormats[i].Name;
      Exit;
    end;
  Result := false;
end;

procedure GetWordGridFormatValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(GridFormats) to High(GridFormats) do Proc(GridFormats[I].Name);
end;


initialization
  RegisterIntegerConsts(TypeInfo(TWordGridFormat),WordGridFormatIdentToInt, IntToWordGridFormatIdent);

end.

