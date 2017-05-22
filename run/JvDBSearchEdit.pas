{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvDBSearchEdit.pas, released on 2004-02-28.

The Initial Developer of the Original Code is Lionel Reynaud
Portions created by S�bastien Buysse are Copyright (C) 2004 Lionel Reynaud.
All Rights Reserved.

Contributor(s):

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.delphi-jedi.org

Known Issues:

Description:
// DB Component to find record with Edit
// Free modified and corrected component TDBSearchEdit from Alexander Burlakov
-----------------------------------------------------------------------------}
// $Id$

unit JvDBSearchEdit;

{$I jvcl.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows,
  Messages,
  Classes, Controls, DB, DBCtrls,
  JvEdit;

type
  TJvDBCustomSearchEdit = class(TJvCustomEdit)
  private
    FDataLink: TFieldDataLink;
    FSearchOptions: TLocateOptions;
    FClearOnEnter: Boolean;
    FDataResult: string;
    FRaiseLocateException: Boolean;
    procedure DataChange(Sender: TObject);
    function GetDataSource: TDataSource;
    function GetDataField: string;
    procedure SetDataSource(Value: TDataSource);
    procedure SetDataField(const Value: string);
    procedure SetSearchOptions(const Value: TLocateOptions);
    procedure CMChanged(var Msg: TMessage); message CM_CHANGED;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure KeyPress(var Key: Char); override;
    procedure Notification(Component: TComponent; Operation: TOperation); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function GetResult: Variant;
    property SearchOptions: TLocateOptions read FSearchOptions
      write SetSearchOptions default [loCaseInsensitive, loPartialKey];
  published
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DataResult: string read FDataResult write FDataResult;
    property DataField: string read GetDataField write SetDataField;
    property TabStop default True;
    property ClearOnEnter: Boolean read FClearOnEnter write FClearOnEnter default True;
    //1 Property to raise/hide any exception inside the Dataset.Locate call
    property RaiseLocateException: Boolean read FRaiseLocateException write FRaiseLocateException default true;
  end;

  {$IFDEF RTL230_UP}
  [ComponentPlatformsAttribute(pidWin32 or pidWin64)]
  {$ENDIF RTL230_UP}
  TJvDBSearchEdit = class(TJvDBCustomSearchEdit)
  published
    property SearchOptions default [loCaseInsensitive, loPartialKey];
    property Align;
    property Alignment;
    property Anchors;
    property AutoSelect;
    property AutoSize;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BorderStyle;
    property CharCase;
    property Color;
    property Flat;
    property DragCursor;
    property ImeMode;
    property ImeName;
    property OEMConvert;
    property ParentFlat;
    property DragMode;
    property Enabled;
    property Font;
    property HideSelection;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnContextPopup;
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
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$URL$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
  );
{$ENDIF UNITVERSIONING}

implementation

uses
  Variants, Forms,
  JvConsts;

//=== { TJvDBCustomSearchEdit } ==============================================

constructor TJvDBCustomSearchEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDataLink := TFieldDataLink.Create;
  FDataLink.Control := Self;
  FDataLink.OnDataChange := DataChange;
  FSearchOptions := [loCaseInsensitive, loPartialKey];
  FClearOnEnter := True;
  Text := '';
  FRaiseLocateException := True;
end;

destructor TJvDBCustomSearchEdit.Destroy;
begin
  FDataLink.Free;
  FDataLink := nil;
  inherited Destroy;
end;

procedure TJvDBCustomSearchEdit.Notification(Component: TComponent; Operation: TOperation);
begin
  inherited Notification(Component, Operation);
  if (FDataLink <> nil) and (Component = DataSource) and (Operation = opRemove) then
    DataSource := nil;
end;

procedure TJvDBCustomSearchEdit.DataChange(Sender: TObject);
begin
  if FDataLink.Field <> nil then
  begin
    if Screen.ActiveControl <> Self then
    begin
      if FDataLink.CanModify then
        Text := FDataLink.Field.Text
      else
        Text := FDataLink.Field.DisplayText;
      SelectAll;
    end;
  end
  else
  begin
    if csDesigning in ComponentState then
      Text := Name
    else
      Text := '';
  end;
end;

procedure TJvDBCustomSearchEdit.CMChanged(var Msg: TMessage);
var
  LText: string;
begin
  if (not ((csDesigning in ComponentState) and
    (csLoading in ComponentState))) and
    Assigned(FDataLink.DataSet) then
    if (Screen.ActiveControl = Self) and FDataLink.Active then
      try
        if FDataLink.DataSet.Locate(FDataLink.FieldName, Text, FSearchOptions) then
        begin
          LText := Text;
          Text := FDataLink.DataSet.FieldByName(DataField).AsString;
          SelStart := Length(LText);
          SelLength := Length(Text) - SelStart;
        end;
      except
        if RaiseLocateException then
          raise;
      end;
end;

procedure TJvDBCustomSearchEdit.KeyPress(var Key: Char);
var
  LLength: Integer;
begin
  if Key = Backspace then
  begin
    LLength := SelLength;
    SelStart := SelStart - 1;
    SelLength := LLength + 1;
  end;
  inherited KeyPress(Key);
end;

function TJvDBCustomSearchEdit.GetDataSource: TDataSource;
begin
  Result := FDataLink.DataSource;
end;

procedure TJvDBCustomSearchEdit.SetDataSource(Value: TDataSource);
begin
  FDataLink.DataSource := Value;
end;

function TJvDBCustomSearchEdit.GetDataField: string;
begin
  Result := FDataLink.FieldName;
end;

procedure TJvDBCustomSearchEdit.SetDataField(const Value: string);
begin
  FDataLink.FieldName := Value;
end;

procedure TJvDBCustomSearchEdit.SetSearchOptions(const Value: TLocateOptions);
begin
  FSearchOptions := Value;
end;

function TJvDBCustomSearchEdit.GetResult: Variant;
begin
  Result := Null;
  if Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active and (DataResult <> '') then
    Result := FDataLink.DataSet.Lookup(DataField, Text, DataResult);
end;

procedure TJvDBCustomSearchEdit.DoEnter;
begin
  if FClearOnEnter then
    Text := '';
  inherited DoEnter;
end;

procedure TJvDBCustomSearchEdit.DoExit;
begin
  inherited DoExit;
  // On replace le texte sur l'enregistrement en cours
  if Assigned(FDataLink.DataSet) and FDataLink.DataSet.Active then
    Text := FDataLink.DataSet.FieldByName(DataField).AsString;
end;

{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.
