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

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

{$I jvcl.inc}
{$I windowsonly.inc}

unit JvDBGridExport;

interface

uses
  Windows, Classes, SysUtils, DB, DBGrids,
  JvComponent, JvSimpleXml, JvTypes;

type
  TExportDestination = (edFile, edClipboard);
  TExportSeparator = (esTab, esSemiColon, esComma, esSpace, esPipe);
  TWordOrientation = (woPortrait, woLandscape);

  EJvExportDBGridException = class(EJVCLException);
  TWordGridFormat = $10..$17;

  TOleServerClose = (scNever, scNewInstance, scAlways);

  TRecordColumn = record
    Visible: Boolean;
    Exportable: Boolean;
    ColumnName: string;
    Column: TColumn;
    Field: TField;
  end;

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
  TJvExportProgressEvent = procedure(Sender: TObject; Min, Max, Position: Cardinal;
    const AText: string; var AContinue: Boolean) of object;

  TJvCustomDBGridExport = class(TJvComponent)
  private
    FGrid: TDBGrid;
    FColumnCount: Integer;
    FRecordColumns: array of TRecordColumn;
    FCaption: string;
    FFileName: TFileName;
    FOnProgress: TJvExportProgressEvent;
    FLastExceptionMessage: string;
    FSilent: Boolean;
    FOnException: TNotifyEvent;
    procedure CheckVisibleColumn;
  protected
    procedure HandleException;
    function ExportField(AField: TField): Boolean;
    function DoProgress(Min, Max, Position: Cardinal; const AText: string): Boolean; virtual;
    function DoExport: Boolean; virtual; abstract;
    procedure DoSave; virtual;
    procedure DoClose; virtual; abstract;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    function ExportGrid: Boolean;
  published
    // (p3) these should be published: all exporters must support them
    property Caption: string read FCaption write FCaption;
    property Grid: TDBGrid read FGrid write FGrid;
    property FileName: TFileName read FFileName write FFileName;
    property Silent: Boolean read FSilent write FSilent default True;
    property OnProgress: TJvExportProgressEvent read FOnProgress write FOnProgress;
    property OnException: TNotifyEvent read FOnException write FOnException;
    property LastExceptionMessage: string read FLastExceptionMessage;
  end;

  TJvCustomDBGridExportClass = class of TJvCustomDBGridExport;

  TJvDBGridWordExport = class(TJvCustomDBGridExport)
  private
    FWord: OleVariant;
    FVisible: Boolean;
    FOrientation: TWordOrientation;
    FWordFormat: TWordGridFormat;
    FClose: TOleServerClose;
    FRunningInstance: Boolean;
  protected
    procedure DoSave; override;
    function DoExport: Boolean; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName;
    property Caption;
    property Grid;
    property OnProgress;
    property Close: TOleServerClose read FClose write FClose default scNewInstance;
    property WordFormat: TWordGridFormat read FWordFormat write FWordFormat default wdTableFormatGrid3;
    property Visible: Boolean read FVisible write FVisible default False;
    property Orientation: TWordOrientation read FOrientation write FOrientation default woPortrait;
  end;

  TJvDBGridExcelExport = class(TJvCustomDBGridExport)
  private
    FExcel: OleVariant;
    FVisible: Boolean;
    FAutoFit: Boolean;
    FOrientation: TWordOrientation;
    FClose: TOleServerClose;
    FRunningInstance: Boolean;
    function IndexFieldToExcel(Index: Integer): string;
  protected
    procedure DoSave; override;
    function DoExport: Boolean; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName;
    property Caption;
    property Grid;
    property OnProgress;
    property Close: TOleServerClose read FClose write FClose default scNewInstance;
    property Visible: Boolean read FVisible write FVisible default False;
    property Orientation: TWordOrientation read FOrientation write FOrientation default woPortrait;
    property AutoFit: Boolean read FAutoFit write FAutoFit;
  end;

  TJvDBGridHTMLExport = class(TJvCustomDBGridExport)
  private
    FDocument: TStringList;
    FDocTitle: string;
    FHeader: TStringList;
    FFooter: TStringList;
    FIncludeColumnHeader: Boolean;
    function GetHeader: TStrings;
    function GetFooter: TStrings;
    procedure SetHeader(const Value: TStrings);
    procedure SetFooter(const Value: TStrings);
  protected
    procedure DoSave; override;
    function DoExport: Boolean; override;
    procedure DoClose; override;
    procedure SetDefaultData;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName;
    property Caption;
    property Grid;
    property OnProgress;
    property IncludeColumnHeader: Boolean read FIncludeColumnHeader write FIncludeColumnHeader default True;
    property Header: TStrings read GetHeader write SetHeader;
    property Footer: TStrings read GetFooter write SetFooter;
    property DocTitle: string read FDocTitle write FDocTitle;
  end;

  TJvDBGridCSVExport = class(TJvCustomDBGridExport)
  private
    FDocument: TStringList;
    FDestination: TExportDestination;
    FExportSeparator: TExportSeparator;
    procedure SetExportSeparator(const Value: TExportSeparator);
    function SeparatorToString(ASeparator: TExportSeparator): string;
    procedure SetDestination(const Value: TExportDestination);
  protected
    function DoExport: Boolean; override;
    procedure DoSave; override;
    procedure DoClose; override;
  public
    Separator: string;
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName;
    property Caption;
    property Grid;
    property OnProgress;

    property Destination: TExportDestination read FDestination write SetDestination default edFile;
    property ExportSeparator: TExportSeparator read FExportSeparator write SetExportSeparator default esTab;
  end;

  TJvDBGridXMLExport = class(TJvCustomDBGridExport)
  private
    FXML: TJvSimpleXML;
    function ClassNameNoT(AField: TField): string;
  protected
    function DoExport: Boolean; override;
    procedure DoSave; override;
    procedure DoClose; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property FileName;
    property Caption;
    property Grid;
    property OnProgress;
  end;

function WordGridFormatIdentToInt(const Ident: string; var Value: Longint): Boolean;
function IntToWordGridFormatIdent(Value: Longint; var Ident: string): Boolean;
procedure GetWordGridFormatValues(Proc: TGetStrProc);

implementation

uses
  {$IFDEF COMPILER6_UP}
  Variants,
  {$ENDIF COMPILER6_UP}
  ComObj, Graphics, Clipbrd,
  JclRegistry,
  JvConsts, JvResources;

//=== { TJvCustomDBGridExport } ==============================================

constructor TJvCustomDBGridExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSilent := True;
end;

function TJvCustomDBGridExport.DoProgress(Min, Max, Position: Cardinal;
  const AText: string): Boolean;
begin
  Result := True;
  if Assigned(FOnProgress) then
    FOnProgress(Self, Min, Max, Position, AText, Result);
end;

procedure TJvCustomDBGridExport.DoSave;
begin
  if FileExists(FileName) then
    DeleteFile(FileName);
end;

function TJvCustomDBGridExport.ExportField(AField: TField): Boolean;
begin
  Result := not (AField.DataType in [ftUnknown, ftBlob, ftGraphic,
    ftParadoxOle, ftDBaseOle, ftTypedBinary, ftCursor, ftADT,
    ftArray, ftReference, ftDataSet, ftOraBlob, ftOraClob, ftVariant,
    ftInterface, ftIDispatch, ftGuid]);
end;

procedure TJvCustomDBGridExport.CheckVisibleColumn;
var
  I: Integer;
begin
  FColumnCount := Grid.Columns.Count;
  SetLength(FRecordColumns, FColumnCount);
  for I := 0 to FColumnCount - 1 do
  begin
    FRecordColumns[I].Column := Grid.Columns[I];
    FRecordColumns[I].Visible := Grid.Columns[I].Visible;
    FRecordColumns[I].ColumnName := Grid.Columns[I].Title.Caption;
    FRecordColumns[I].Field := Grid.Columns[I].Field;
    if FRecordColumns[I].Visible and (FRecordColumns[I].Field <> nil) then
      FRecordColumns[I].Exportable := ExportField(FRecordColumns[I].Field)
    else
      FRecordColumns[I].Exportable := False;
  end;
end;

function TJvCustomDBGridExport.ExportGrid: Boolean;
begin
  if not Assigned(Grid) then
    raise EJvExportDBGridException.CreateRes(@RsEGridIsUnassigned);
  if not Assigned(Grid.DataSource) or not Assigned(Grid.DataSource.DataSet) then
    raise EJvExportDBGridException.CreateRes(@RsEDataSetDataSourceIsUnassigned);
//  if FileName = '' then
//    raise EJvExportDBGridException.Create(RsFilenameEmpty);
  CheckVisibleColumn;
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
    else
    if Assigned(FOnException) then
      FOnException(Self);
  end;
end;

procedure TJvCustomDBGridExport.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if (Operation = opRemove) and (AComponent = Grid) then
    Grid := nil;
end;

//=== { TJvDBGridWordExport } ================================================

constructor TJvDBGridWordExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsExportWord;
  FWord := Unassigned;
  FVisible := False;
  FOrientation := woPortrait;
  FWordFormat := wdTableFormatGrid3;
  FClose := scNewInstance;
end;

destructor TJvDBGridWordExport.Destroy;
begin
  DoClose;
  inherited Destroy;
end;

function TJvDBGridWordExport.DoExport: Boolean;
const
  cWordApplication = 'Word.Application';
var
  I, J, K: Integer;
  lTable: OleVariant;
  ARecNo, lRecCount: Integer;
  lColVisible: Integer;
  lRowCount: Integer;
  lBookmark: TBookmark;
begin
  Result := True;
  FRunningInstance := True;
  try
    // get running instance
    FWord := GetActiveOleObject(cWordApplication);
  except
    FRunningInstance := False;
    try
      // create new
      FWord := CreateOleObject(cWordApplication);
    except
      FWord := Unassigned;
      HandleException;
//      raise EJvExportDBGridException.Create(RsNoWordApplication);
    end;
  end;

  if VarIsEmpty(FWord) then
    Exit;

  try
    FWord.Visible := FVisible;
    FWord.Documents.Add;

    lColVisible := 0;
    for I := 1 to FColumnCount do
      if Grid.Columns[I - 1].Visible then
        Inc(lColVisible);

    lRowCount := Grid.DataSource.DataSet.RecordCount;
    FWord.ActiveDocument.Range.Font.Name := Grid.Font.Name;
    FWord.ActiveDocument.Range.Font.Size := Grid.Font.Size;
    if Orientation = woPortrait then
      FWord.ActiveDocument.PageSetup.Orientation := 0
    else
      FWord.ActiveDocument.PageSetup.Orientation := 1;
    lTable := FWord.ActiveDocument.Tables.Add(FWord.ActiveDocument.Range, lRowCount + 1, lColVisible);
    FWord.ActiveDocument.Range.InsertAfter('Date ' + DateTimeToStr(Now));
    // (rom) This is correct Delphi. See "positional parameters" in the Delphi help.
    lTable.AutoFormat(Format := WordFormat); // FormatNum, 1, 1, 1, 1, 1, 0, 0, 0, 1

    K := 1;
    for I := 0 to FColumnCount - 1 do
      if FRecordColumns[I].Visible then
      begin
        lTable.Cell(1, K).Range.InsertAfter(FRecordColumns[I].ColumnName);
        Inc(K);
      end;

    J := 2;
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
          K := 1;
          for I := 0 to FColumnCount - 1 do
          begin
            if FRecordColumns[I].Exportable and not FRecordColumns[I].Field.IsNull then
            try
              lTable.Cell(J, K).Range.InsertAfter(string(FRecordColumns[I].Field.Value));
            except
              Result := False;
              HandleException;
              // Remember problem but continue
            end;
            if FRecordColumns[I].Visible then
              Inc(K);
          end;
          Next;
          Inc(J);
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then
            Last;
        end;
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
    lTable.UpdateAutoFormat;
  except
    HandleException;
  end;
end;

procedure TJvDBGridWordExport.DoSave;
var
  lName: OleVariant;
begin
  inherited DoSave;
  if VarIsEmpty(FWord) then
    Exit;
  try
    lName := OleVariant(FileName);
    FWord.ActiveDocument.SaveAs(lName);
  except
    HandleException;
  end;
end;

procedure TJvDBGridWordExport.DoClose;
begin
  if not VarIsEmpty(FWord) and (FClose <> scNever) then
  try
    if (FClose = scAlways) or not FRunningInstance then
    begin
      FWord.ActiveDocument.Close(wdDoNotSaveChanges, EmptyParam, EmptyParam);
      FWord.Quit;
    end;
    FWord := Unassigned;
  except
    HandleException;
  end;
end;

//=== { TJvDBGridExcelExport } ===============================================

constructor TJvDBGridExcelExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Caption := RsExportExcel;
  FExcel := Unassigned;
  FVisible := False;
  FOrientation := woPortrait;
  FClose := scNewInstance;
end;

destructor TJvDBGridExcelExport.Destroy;
begin
  DoClose;
  inherited Destroy;
end;

function TJvDBGridExcelExport.IndexFieldToExcel(Index: Integer): string;
begin
  // Max column : ZZ => Index = 702
  if Index > 26 then
    Result := Chr(64 + ((Index - 1) div 26)) + Chr(65 + ((Index - 1) mod 26))
  else
    Result := Chr(64 + Index);
end;

function TJvDBGridExcelExport.DoExport: Boolean;
const
  cExcelApplication = 'Excel.Application';
var
  I, J, K: Integer;
  lTable: OleVariant;
  lCell: OleVariant;
  ARecNo, lRecCount: Integer;
  lBookmark: TBookmark;
begin
  Result := True;
  FRunningInstance := True;
  try
    // get running instance
    FExcel := GetActiveOleObject(cExcelApplication);
  except
    FRunningInstance := False;
    try
      // create new instance
      FExcel := CreateOleObject(cExcelApplication);
    except
      FExcel := Unassigned;
      HandleException;
    end;
  end;

  if VarIsEmpty(FExcel) then
    Exit;
  try
    FExcel.WorkBooks.Add;
    FExcel.Visible := Visible;

    lTable := FExcel.ActiveWorkbook.ActiveSheet;

    if Orientation = woPortrait then
      lTable.PageSetup.Orientation := xlPortrait
    else
      lTable.PageSetup.Orientation := xlLandscape;

    K := 1;
    for I := 0 to FColumnCount - 1 do
      if FRecordColumns[I].Visible then
      begin
        lCell := lTable.Range[IndexFieldToExcel(K) + '1'];
        lCell.Value := FRecordColumns[I].ColumnName;
        Inc(K);
      end;

    J := 1;
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
          Inc(J);
          K := 1;
          for I := 0 to FColumnCount - 1 do
          begin
            if FRecordColumns[I].Exportable then
            begin
              lCell := lTable.Range[IndexFieldToExcel(K) + IntToStr(J)];
              try
                // Do not cast with string !
                lCell.Value := FRecordColumns[I].Field.Value;
              except
                Result := False;
                HandleException;
              end;
            end;
            if FRecordColumns[I].Visible then
              Inc(K);
          end;
          Next;
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then
            Last;
        end;
        if AutoFit then
          try
            lTable.Columns.AutoFit; // NEW! Autofit!
          except
             {$IFDEF DEBUGINFO_ON}
             on E: Exception do
               OutputDebugString(PChar('lTable.Columns.AutoFit failed. ' + E.Message));
             {$ENDIF DEBUGINFO_ON}
          end;
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

procedure TJvDBGridExcelExport.DoSave;
var
  lName: OleVariant;
begin
  inherited DoSave;
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
  if not VarIsEmpty(FExcel) and (FClose = scNever) then
  begin
    FExcel.Visible := True;
    Exit;
  end;

  if not VarIsEmpty(FExcel) and (FClose <> scNever) then
  try
    FExcel.ActiveWorkbook.Saved := True; // Avoid Excel's save prompt
    if (Close = scAlways) or not FRunningInstance then
    begin
      FExcel.ActiveWorkbook.Close;
      FExcel.Quit;
    end;
    FExcel := Unassigned;
  except
    HandleException;
  end;
end;

//=== { TJvDBGridHTMLExport } ================================================

constructor TJvDBGridHTMLExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := TStringList.Create;
  Caption := RsExportHTML;
  FDocTitle := RsHTMLExportDocTitle;
  FHeader := TStringList.Create;
  FFooter := TStringList.Create;
  FIncludeColumnHeader := True;
  SetDefaultData;
end;

destructor TJvDBGridHTMLExport.Destroy;
begin
  FFooter.Free;
  FHeader.Free;
  FDocument.Free;
  inherited Destroy;
end;

procedure TJvDBGridHTMLExport.SetDefaultData;
begin
  Header.Add('<html><head><title><#TITLE></title>');
  Header.Add('<style type=text/css>');
  Header.Add('#STYLE');
  Header.Add('</style>');
  Header.Add('</head><body>');

  Footer.Add('</body></html>');
end;

function TJvDBGridHTMLExport.GetFooter: TStrings;
begin
  Result := FFooter;
end;

procedure TJvDBGridHTMLExport.SetFooter(const Value: TStrings);
begin
  FFooter.Assign(Value);
end;

function TJvDBGridHTMLExport.GetHeader: TStrings;
begin
  Result := FHeader;
end;

procedure TJvDBGridHTMLExport.SetHeader(const Value: TStrings);
begin
  FHeader.Assign(Value);
end;

procedure TJvDBGridHTMLExport.DoClose;
begin
 // do nothing
end;

function TJvDBGridHTMLExport.DoExport: Boolean;
var
  I: Integer;
  ARecNo, lRecCount: Integer;
  lBookmark: TBookmark;
  lString, lText, lHeader, lStyle: string;

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
  var
    r, g, b: byte;
  begin
    AColor := ColorToRGB(AColor);
    r := GetRValue(AColor);
    g := GetGValue(AColor);
    b := GetBValue(AColor);
    Result := Format('%.2x%.2x%.2x', [r, g, b]);
  end;

  function FontSubstitute(const Name: string): string;
  const
    cFontKey: array [Boolean] of PChar =
     ('SOFTWARE\Microsoft\Windows\CurrentVersion\FontSubstitutes',
      'SOFTWARE\Microsoft\Windows NT\CurrentVersion\FontSubstitutes');
  begin
    Result := RegReadStringDef(HKEY_LOCAL_MACHINE,
      cFontKey[Win32Platform = VER_PLATFORM_WIN32_NT], Name, Name);
  end;

  function FontSizeToHTML(PtSize: Integer): Integer;
  begin
    case Abs(PtSize) of
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

  function FontStyleToHTML(AFont: TFont): string;
  begin
    Result := '';
    if fsBold in AFont.Style then
      Result := 'FONT-WEIGHT: bold; ';
    if fsItalic in AFont.Style then
      Result := Result + 'FONT-STYLE: italic; ';
    if fsUnderline in AFont.Style then
      if fsStrikeout in AFont.Style then
        Result := Result + 'TEXT-DECORATION: underline line-through; '
      else
        Result := Result + 'TEXT-DECORATION: underline; '
    else
    if fsStrikeout in AFont.Style then
      Result := Result + 'TEXT-DECORATION: line-through; ';
  end;

begin
  FDocument.Clear;

  Result := True;
  try
    // Create Style like :
    //.Column0 {FONT-FAMILY: Arial; FONT-SIZE: 12px; FONT-WEIGHT: bold; FONT-STYLE: italic
    //      TEXT-ALIGN: right; COLOR: #FFFFFF; BACKGROUND: #9924A7}

    lStyle := '';
    lString := '<tr>';
    for I := 0 to FColumnCount - 1 do
      if FRecordColumns[I].Visible then
        with FRecordColumns[I].Column do
        begin
          lString := lString + Format('<th bgcolor="#%s" align="%s">%s</th>',
            [ColorToHTML(Title.Color), AlignmentToHTML(Alignment), FontToHTML(Title.Font, Title.Caption)]);
          lStyle := lStyle +
            Format('.Column%d {FONT-FAMILY: %s; FONT-SIZE: %dpt; %s TEXT-ALIGN: %s; COLOR: #%s; BACKGROUND: #%s;}'#13#10,
            [I, FontSubstitute(Font.Name), Font.Size, FontStyleToHTML(Font),
            AlignmentToHTML(Alignment), ColorToHTML(Font.Color), ColorToHTML(Color)]);
        end;
    lString := lString + '</tr>';
    lHeader := StringReplace(Header.Text, '<#TITLE>', DocTitle, [rfReplaceAll, rfIgnoreCase]);
    lHeader := StringReplace(lHeader, '#STYLE', lStyle, [rfReplaceAll, rfIgnoreCase]);

    FDocument.Add(lHeader);
    FDocument.Add('<table width="90%" border="1" cellspacing="0" cellpadding="0">');
    if IncludeColumnHeader then
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
          for I := 0 to FColumnCount - 1 do
            with FRecordColumns[I] do
              if Visible then
              begin
                if Exportable and not Field.IsNull then
                try
                  lText := Field.AsString;
                  if lText = '' then
                    lText := '&nbsp;';
                except
                  Result := False;
                  HandleException;
                end
                else
                  lText := '&nbsp;';

                lString := lString + Format('<td class="column%d">%s</td>',
                  [I, lText]);
              end;
          lString := lString + '</tr>';
          FDocument.Add(lString);
          Next;
          if not DoProgress(0, lRecCount, ARecNo, Caption) then
            Last;
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
  inherited DoSave;
  FDocument.SaveToFile(FileName);
end;

//=== { TJvDBGridCSVExport } =================================================

constructor TJvDBGridCSVExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDocument := TStringList.Create;
  FDestination := edFile;
  ExportSeparator := esTab;
  Caption := RsExportFile;
end;

destructor TJvDBGridCSVExport.Destroy;
begin
  FDocument.Free;
  inherited Destroy;
end;

function TJvDBGridCSVExport.SeparatorToString(ASeparator: TExportSeparator): string;
begin
  case ASeparator of
    esTab:
      Result := Tab;
    esSemiColon:
      Result := ';';
    esComma:
      Result := ',';
    esSpace:
      Result := ' ';
    esPipe:
      Result := '|';
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

function TJvDBGridCSVExport.DoExport: Boolean;
var
  I: Integer;
  ARecNo, lRecCount: Integer;
  lBookmark: TBookmark;
  lString, lField: string;
begin
  FDocument.Clear;
  Result := True;
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
          for I := 0 to FColumnCount - 1 do
            if FRecordColumns[I].Exportable then
            try
              if not FRecordColumns[I].Field.IsNull then
              begin
                lField := FRecordColumns[I].Field.AsString;
                if Pos(Separator, lField) <> 0 then
                  lString := lString + '"' + lField + '"'
                else
                  lString := lString + lField;
              end;
              lString := lString + Separator;
            except
              Result := False;
              HandleException;
            end;
          FDocument.Add(lString);
          Next;
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then
            Last;
        end;
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

procedure TJvDBGridCSVExport.DoSave;
begin
  inherited DoSave;
  if Destination = edFile then
    FDocument.SaveToFile(FileName)
  else
    Clipboard.AsText := FDocument.Text;
end;

procedure TJvDBGridCSVExport.DoClose;
begin
  // do nothing
end;

//=== { TJvDBGridXMLExport } =================================================

constructor TJvDBGridXMLExport.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FXML := TJvSimpleXML.Create(nil);
  FXML.Options := [sxoAutoCreate, sxoAutoIndent];
end;

destructor TJvDBGridXMLExport.Destroy;
begin
  FXML.Free;
  inherited Destroy;
end;

// From DSDEfine of Delphi designer

function TJvDBGridXMLExport.ClassNameNoT(AField: TField): string;
begin
  Result := AField.ClassName;
  if Result[1] = 'T' then
    Delete(Result, 1, 1);
  if CompareText('Field', Copy(Result, Length(Result) - 4, 5)) = 0 then { do not localize }
    Delete(Result, Length(Result) - 4, 5);
end;

// The structure of the xml file is inspired of the xml export
// create by Delphi with TClientDataSet

function TJvDBGridXMLExport.DoExport: Boolean;
var
  I: Integer;
  ARecNo, lRecCount: Integer;
  lBookmark: TBookmark;
  lRootNode: TJvSimpleXmlElemClassic;
  lDataNode: TJvSimpleXmlElem;
  lFieldsNode: TJvSimpleXmlElem;
  lRecordNode: TJvSimpleXmlElem;
begin
  Result := True;
  FXML.Root.Clear;

  // create root node
  FXML.Root.Name := 'DATAPACKET';
  lRootNode := FXML.Root;
  lRootNode.Properties.Add('Version', '1.0'); // This is the first implementation !

  // add column header and his property
  lDataNode := lRootNode.Items.Add('METADATA');
  lFieldsNode := lDataNode.Items.Add('FIELDS');
  for I := 0 to FColumnCount - 1 do
    with FRecordColumns[I] do
      if Visible and (Field <> nil) then
      begin
        with lFieldsNode.Items.Add('FIELD') do
        begin
          Properties.Add('ATTRNAME', ColumnName);
          Properties.Add('FIELDTYPE', ClassNameNoT(Field));
          Properties.Add('WIDTH', Column.Width);
        end;
      end;

  // now add all the record
  lRecordNode := lRootNode.Items.Add('ROWDATA');
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
          with lRecordNode.Items.Add('ROW') do
          begin
            for I := 0 to FColumnCount - 1 do
              if FRecordColumns[I].Exportable then
              try
                with FRecordColumns[I] do
                  Properties.Add(ColumnName, Field.AsString);
              except
                Result := False;
                HandleException;
              end;
          end;

          Next;
          Inc(ARecNo);
          if not DoProgress(0, lRecCount, ARecNo, Caption) then
            Last;
        end;
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

procedure TJvDBGridXMLExport.DoSave;
begin
  inherited DoSave;
  FXML.SaveToFile(FileName);
end;

procedure TJvDBGridXMLExport.DoClose;
begin
  // do nothing
end;

//============================================================================

type
  TGridValue = packed record
    Value: Integer;
    Name: PChar;
  end;

const
  GridFormats: array [$10..$17] of TGridValue =
   ((Value: $10; Name: 'wdTableFormatGrid1'),
    (Value: $11; Name: 'wdTableFormatGrid2'),
    (Value: $12; Name: 'wdTableFormatGrid3'),
    (Value: $13; Name: 'wdTableFormatGrid4'),
    (Value: $14; Name: 'wdTableFormatGrid5'),
    (Value: $15; Name: 'wdTableFormatGrid6'),
    (Value: $16; Name: 'wdTableFormatGrid7'),
    (Value: $17; Name: 'wdTableFormatGrid8'));

function WordGridFormatIdentToInt(const Ident: string; var Value: Longint): Boolean;
var
  I: Integer;
begin
  for I := Low(GridFormats) to High(GridFormats) do
    if SameText(GridFormats[I].Name, Ident) then
    begin
      Result := True;
      Value := GridFormats[I].Value;
      Exit;
    end;
  Result := False;
end;

function IntToWordGridFormatIdent(Value: Longint; var Ident: string): Boolean;
var
  I: Integer;
begin
  for I := Low(GridFormats) to High(GridFormats) do
    if GridFormats[I].Value = Value then
    begin
      Result := True;
      Ident := GridFormats[I].Name;
      Exit;
    end;
  Result := False;
end;

procedure GetWordGridFormatValues(Proc: TGetStrProc);
var
  I: Integer;
begin
  for I := Low(GridFormats) to High(GridFormats) do
    Proc(GridFormats[I].Name);
end;

initialization
  RegisterIntegerConsts(TypeInfo(TWordGridFormat), WordGridFormatIdentToInt, IntToWordGridFormatIdent);

end.

