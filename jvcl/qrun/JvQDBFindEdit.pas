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

The Original Code is: JvDBFindEdit.pas, released 2004-03-23

The Initial Developer of the Original Code is yul
Portions created by yul are Copyright (C) 2004 yul.
All Rights Reserved.

Contributor(s):

Current Version: 0.50

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvQDBFindEdit;

{$I jvcl.inc}

interface

uses
  QWindows, Classes, QExtCtrls, DB, QDBCtrls,
  JvQMaskEdit;

type
  TJvEditFindStyle = (fsNavigate, fsFilter);
  TJvEditFindMode = (fmFirstPos, fmAnyPos);

  TJvDBFindEdit = class(TJvMaskEdit)
  private
    FTimer: TTimer;
    FOldFiltered: Boolean;
    FOldFilterRecord: TFilterRecordEvent;
    FDataLink: TFieldDataLink;
    FIgnoreCase: Boolean;
    FFindMode: TJvEditFindMode;
    FFindStyle: TJvEditFindStyle;
    FSearchText: string;
    procedure ActiveChange(Sender: TObject);
    function GetDataField: string;
    function GetDataSource: TDataSource;
    procedure SetDataField(const Value: string);
    procedure SetDataSource(const Value: TDataSource);
    procedure SetFindMode(const Value: TJvEditFindMode);
    procedure SetFindStyle(const Value: TJvEditFindStyle);
    procedure SetIgnoreCase(const Value: Boolean);
    procedure FTimerTimer(Sender: TObject);
    procedure AFilterRecord(DataSet: TDataSet; var Accept: Boolean);
  protected
    procedure Change; override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function DateVal: Boolean;
    function IsDate(S1: string): Boolean;
    function GetDateDelimiter: string;
    function IsNumeric(S1: string): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Find(AText: string);
    procedure ResetFilter;
  published
    property DataField: string read GetDataField write SetDataField;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property FindStyle: TJvEditFindStyle read FFindStyle write SetFindStyle default fsNavigate;
    property FindMode: TJvEditFindMode read FFindMode write SetFindMode default fmFirstPos;
    property IgnoreCase: Boolean read FIgnoreCase write SetIgnoreCase default True;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BorderStyle;
    property CharCase;
    property Color;
    property Constraints; 
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    Property EditMask;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
//    property PasswordChar;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnChange;
    property OnClick;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDrag;
  end;

implementation

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  SysUtils;

constructor TJvDBFindEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FFindStyle := fsNavigate;
  FFindMode := fmFirstPos;
  FIgnoreCase := True;
  FTimer := TTimer.Create(Self);
  FTimer.Enabled := False;
  FTimer.Interval := 400;
  FTimer.OnTimer := FTimerTimer;
  FSearchText := '';
  FOldFiltered := False;
  FOldFilterRecord := nil;
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnActiveChange := ActiveChange;
end;

destructor TJvDBFindEdit.Destroy;
begin
  if FDataLink.Active and (FFindStyle = fsFilter) then
  begin
    FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
    FDataLink.DataSet.Filtered := FOldFiltered;
  end;
  FDataLink.Control := nil;
  DataSource := nil;
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBFindEdit.Change;
begin
  FTimer.Enabled := False;
  if Text = '' then
    FTimer.Interval := 400;
  FTimer.Enabled := True;
  FSearchText := Text;
  inherited Change;
end;

// (ahuser) Compiler gives hint for unused value. This function prevents it
function ToDoubleVal(const S: string; var Err: Integer): Double;
begin
  Val(S, Result, Err);
end;

function TJvDBFindEdit.IsNumeric(S1: string): Boolean;
var
  ier: Integer;
begin
  Result := True;
  ToDoubleVal(S1, ier);
  if ier > 0 then
    Result := False;
end;

function TJvDBFindEdit.GetDateDelimiter: string;
var
  S1: string;
  I: Integer;
begin
  S1 := DateTimeToStr(Now);
  for I := 1 to Length(S1) do
    if not (S1[I] in ['0'..'9']) then
    begin
      Result := S1[I];
      Break;
    end;
end;

function TJvDBFindEdit.IsDate(S1: string): Boolean;
var
  I, k, p1, p2: Integer;
  sm, sd, sj, ss: string;
begin
  Result := False;
  ss := GetDateDelimiter;
  k := Length(S1);
  if k > 0 then
  begin
    p1 := 0;
    p2 := 0;
    for I := 1 to k do
    begin
      if p1 = 0 then
      begin
        if S1[I] = ss then
          p1 := I;
      end
      else
      begin
        if S1[I] = ss then
          p2 := I;
      end;
    end;
    if (p1 > 0) and (p2 > 0) and (p2 > p1) then
    begin
      sm := Copy(S1, 1, p1 - 1);
      sd := Copy(S1, p1 + 1, p2 - p1 -1);
      sj := Copy(S1, p2 + 1, k - p2);
      if IsNumeric(sm) and IsNumeric(sd) and IsNumeric(sj) then
      begin
        p1 := StrToInt(sd);
        if (p1 > 0) and (p1 < 32) then
        begin
          p1 := StrToInt(sm);
          if (p1 > 0) and (p1 < 13) then
          begin
            p1 := StrToInt(sj);
            if p1 > 1969 then
              Result := True;
          end;
        end;
      end;
    end;
  end;
end;

function TJvDBFindEdit.DateVal: Boolean;
begin
  Result := True;
  if FDataLink.Field is TDateField then
    if not IsDate(FSearchText) then
      Result := False;

  if IsDate(FSearchText) then
  //begin
  //  DateSeparator :='/';
  //  ShortDateFormat := 'mm/dd/yyyy';
    FSearchText := (DateToStr(StrToDate(FSearchText)));
  //end;
end;

procedure TJvDBFindEdit.ResetFilter;
begin
  Text:='';
// FSearchText:='';
  FDataLink.DataSet.Filtered := False;

end;

procedure TJvDBFindEdit.FTimerTimer(Sender: TObject);
begin
  FTimer.Enabled := False;
  ActiveChange(Self);
  if FSearchText = '' then
  begin
    if FFindStyle = fsFilter then
    begin
      FDataLink.DataSet.OnFilterRecord := FOldFilterRecord;
      FDataLink.DataSet.Filtered := FOldFiltered;
    end;
  end
  else
  begin
    if not FDataLink.Active or (FDataLink.Field = nil) then
      Exit;
    if DateVal and not(FDataLink.Field is TBlobField) then
      if FFindStyle = fsNavigate then
        if IgnoreCase then
          FDataLink.DataSet.Locate(DataField, FSearchText, [loCaseInsensitive, loPartialKey])
        else
          FDataLink.DataSet.Locate(DataField, FSearchText, [loPartialKey])
      else
        FDataLink.DataSet.Filtered := True;
  end;
  FTimer.Interval := 100;
end;

procedure TJvDBFindEdit.Find(AText: string);
begin
  FSearchText := AText;
  FTimerTimer(FTimer);
end;

procedure TJvDBFindEdit.AFilterRecord(DataSet: TDataSet;
  var Accept: Boolean);
begin
  Accept := True;
  if FOldFiltered and Assigned(FOldFilterRecord) then
    FOldFilterRecord(DataSet, Accept);
  if not Accept then
    Exit;
  if FFindMode = fmFirstPos then
    if IgnoreCase then
      Accept := Pos(AnsiUpperCase(FSearchText),
        AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) = 1
    else
      Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) = 1
  else
  if IgnoreCase then
    Accept := Pos(AnsiUpperCase(FSearchText),
      AnsiUpperCase(DataSet.FieldByName(DataField).AsString)) > 0
  else
    Accept := Pos(FSearchText, DataSet.FieldByName(DataField).AsString) > 0
end;

procedure TJvDBFindEdit.ActiveChange(Sender: TObject);
var
  Func1, Func2: TFilterRecordEvent;
begin
  if (FFindStyle = fsNavigate) or (FDataLink.DataSet = nil) then
    Exit;
  Func1 := FDataLink.DataSet.OnFilterRecord;
  Func2 := AFilterRecord;
  if FDataLink.Active and (@Func1 <> @Func2) and (FSearchText > '') then
  begin
    FOldFilterRecord := FDataLink.DataSet.OnFilterRecord;
    FOldFiltered := FDataLink.DataSet.Filtered;
    FDataLink.DataSet.OnFilterRecord := AFilterRecord;
  end;
end;

function TJvDBFindEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBFindEdit.SetDataSource(const Value: TDataSource);
begin
  if not (FDataLink.DataSourceFixed and (csLoading in ComponentState)) then
    FDataLink.DataSource := Value;
  if Value <> nil then
    Value.FreeNotification(Self);
end;

function TJvDBFindEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBFindEdit.SetDataField(const Value: string);
begin
  if Value > '' then
    FDataLink.FieldName := Value;
end;

procedure TJvDBFindEdit.SetFindMode(const Value: TJvEditFindMode);
begin
  if FFindStyle = fsNavigate then
    FFindMode := fmFirstPos
  else
    FFindMode := Value;
end;

procedure TJvDBFindEdit.SetFindStyle(const Value: TJvEditFindStyle);
begin
  FFindStyle := Value;
  if FFindStyle = fsNavigate then
    FFindMode := fmFirstPos;
  ActiveChange(Self);
end;

procedure TJvDBFindEdit.SetIgnoreCase(const Value: Boolean);
begin
  FIgnoreCase := Value;
end;

procedure TJvDBFindEdit.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if (FDataLink <> nil) and (AComponent = DataSource) then
      DataSource := nil;
end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );

initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

