{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCheckListBox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

This is a merging of the code in the original JvCheckListBox.pas and JvFixedCheckListBox.pas
Merging done 2002-06-05 by Peter Thornqvist [peter3@peter3.com]

Contributor(s):
Michael Beck [mbeck@bigfoot.com].
Peter Below <100113.1101@compuserve.com>

Last Modified: 2002-06-05

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I JVCL.INC}

unit JvCheckListBox;

interface



uses
  Windows, Messages, SysUtils, Classes, Graphics, checklst, Controls, Forms,
  JvItemsSearchs, JVCLVer;

type
  TJvCheckListBox = class(TCheckListbox)
  private
    FEffect: Boolean;
    FOnMouseEnter: TNotifyEvent;
    FColor: TColor;
    FSaved: TColor;
    FOnMouseLeave: TNotifyEvent;
    FOnCtl3DChanged: TNotifyEvent;
    FOnParentColorChanged: TNotifyEvent;
    FOnSelectCancel: TNotifyEvent;
    FOver: Boolean;
    FMaxWidth: Integer;
    FScroll: Boolean;
    FOnHScroll: TNotifyEvent;
    FOnVScroll: TNotifyEvent;
    FItemSearchs: TJvItemsSearchs;
    FAboutJVCL: TJVCLAboutInfo;
    procedure SetHScroll(const Value: Boolean);
    procedure RefreshH;
    procedure SetEffect(const Value: Boolean);
  protected
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
    procedure CMCtl3DChanged(var Msg: TMessage); message CM_CTL3DCHANGED;
    procedure CMParentColorChanged(var Msg: TMessage); message CM_PARENTCOLORCHANGED;
    procedure SelectCancel(var Msg: TMessage); message LBN_SELCANCEL;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure WndProc(var Msg: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;

    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;

    procedure SelectAll;{$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure UnselectAll;
    procedure InvertSelection;
    procedure CheckAll;
    procedure UnCheckall;
    procedure InvertCheck;
    function GetChecked: TStringList;
    function GetUnChecked: TStringList;
    procedure DeleteSelected;{$IFDEF COMPILER6_UP}override;{$ENDIF}
    procedure SaveToFile(FileName: TFileName);
    procedure LoadFromFile(FileName: TFileName);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToStream(Stream: TStream);
  published
    property AboutJVCL: TJVCLAboutInfo read FAboutJVCL write FAboutJVCL stored False;
    property HotTrack: Boolean read FEffect write SetEffect default False;
    property HorScrollbar: Boolean read FScroll write SetHScroll default True;
    property HintColor: TColor read FColor write FColor default clInfoBk;

    property OnMouseEnter: TNotifyEvent read FOnMouseEnter write FOnMouseEnter;
    property OnMouseLeave: TNotifyEvent read FOnMouseLeave write FOnMouseLeave;
    property OnCtl3DChanged: TNotifyEvent read FOnCtl3DChanged write FOnCtl3DChanged;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
    property OnSelectCancel: TNotifyEvent read FOnSelectCancel write FOnSelectCancel;
    property OnVerticalScroll: TNotifyEvent read FOnVScroll write FOnVScroll;
    property OnHorizontalScroll: TNotifyEvent read FOnHScroll write FOnHScroll;
  end;

implementation

type
  //Used for the load/save methods
  TCheckListRecord = record
    Checked: Boolean;
    StringSize: Integer;
  end;

  {**************************************************}

constructor TJvCheckListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FMaxWidth := 0;
  FEffect := False;
  FColor := clInfoBk;
  FOver := False;
  FScroll := True;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FItemSearchs := TJvItemsSearchs.Create;
end;

{**************************************************}

destructor TJvCheckListBox.Destroy;
begin
  FItemSearchs.Free;
  inherited;
end;

{**************************************************}

procedure TJvCheckListBox.CreateParams(var Params: TCreateParams);
begin
  inherited;
  with Params do
    if FScroll then
      Style := Style or WS_HSCROLL
    else
      Style := Style xor WS_HSCROLL;
end;

{***********************************************}

procedure TJvCheckListBox.RefreshH;
var
  i: Integer;
  ItemWidth: Word;
begin
  FMaxWidth := 0;
  for i := 0 to Items.Count - 1 do
  begin
    ItemWidth := Canvas.TextWidth(Items[i] + ' ');
    if FMaxWidth < ItemWidth then
      FMaxWidth := ItemWidth;
  end;
  SetHScroll(FScroll);
end;

{***********************************************}

procedure TJvCheckListBox.WndProc(var Msg: TMessage);
var
  ItemWidth: Word;
begin
  case Msg.Msg of
    LB_ADDSTRING, LB_INSERTSTRING:
      begin
        ItemWidth := Canvas.TextWidth(StrPas(PChar(Msg.lParam)) + ' ');
        if FMaxWidth < ItemWidth then
          FMaxWidth := ItemWidth;
        SetHScroll(FScroll);
      end;
    LB_DELETESTRING:
      begin
        ItemWidth := Canvas.TextWidth(Items[Msg.wParam] + ' ');
        if ItemWidth = FMaxWidth then
        begin
          inherited;
          RefreshH;
          Exit;
        end;
      end;
    LB_RESETCONTENT:
      begin
        FMaxWidth := 0;
        SetHScroll(FScroll);
      end;
    WM_SETFONT:
      begin
        inherited;
        Canvas.Font.Assign(Font);
        RefreshH;
        Exit;
      end;
  end;
  inherited;
end;

{***********************************************}

procedure TJvCheckListBox.SetHScroll(const Value: Boolean);
begin
  FScroll := Value;
  if FScroll then
    SendMessage(Handle, LB_SETHORIZONTALEXTENT, FMaxWidth, 0);
end;

{***********************************************}

procedure TJvCheckListBox.CMCtl3DChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnCtl3DChanged) then
    FOnCtl3DChanged(Self);
end;

{**************************************************}

procedure TJvCheckListBox.CMParentColorChanged(var Msg: TMessage);
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

{**************************************************}

procedure TJvCheckListBox.CMMouseEnter(var Msg: TMessage);
begin
  if not FOver then
  begin
    FSaved := Application.HintColor;
    Application.HintColor := FColor;
    if FEffect then
      Ctl3d := True;
    FOver := True;
  end;
  if Assigned(FOnMouseEnter) then
    FOnMouseEnter(Self);
end;

{**************************************************}

procedure TJvCheckListBox.WMHScroll(var Msg: TWMHScroll);
var
  scrollpos: Integer;
  r: TRect;
begin
  inherited;
  // (p3) what does this code do, really?
  if msg.ScrollCode <> SB_ENDSCROLL then
  begin
    scrollpos := GetScrollPos(handle, SB_HORZ);
    if scrollpos < 20 then
    begin
      r := ClientRect;
      r.Right := r.left + 20;
      InvalidateRect(handle, @r, false);
    end;
  end;
  if Assigned(FOnHScroll) then
    FOnHScroll(Self);
end;
{**************************************************}

procedure TJvCheckListBox.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  if Assigned(FOnVScroll) then
    FOnVScroll(Self);
end;

{**************************************************}

procedure TJvCheckListBox.CMMouseLeave(var Msg: TMessage);
begin
  if FOver then
  begin
    Application.HintColor := FSaved;
    if FEffect then
      Ctl3d := False;
    FOver := False;
  end;
  if Assigned(FOnMouseLeave) then
    FOnMouseLeave(Self);
end;

{**************************************************}

function TJvCheckListBox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCheckListBox.SetEffect(const Value: Boolean);
begin
  FEffect := Value;
  if FEffect then
    Ctl3d := False;
end;

{**************************************************}

function TJvCheckListBox.SearchPrefix(Value: string; CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCheckListBox.SelectCancel(var Msg: TMessage);
begin
  if Assigned(FOnSelectCancel) then
    FOnSelectCancel(Self);
end;

{**************************************************}

function TJvCheckListBox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

{**************************************************}

function TJvCheckListBox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

{**************************************************}

procedure TJvCheckListBox.SelectAll;
var
  i: Integer;
begin
  // (rom) simplified
  if MultiSelect then
    for i := 0 to Items.Count - 1 do
      Selected[i] := True;
end;

{**************************************************}

procedure TJvCheckListBox.UnselectAll;
var
  i: Integer;
begin
  if MultiSelect then
    for i := 0 to Items.Count - 1 do
      Selected[i] := False;
end;

{**************************************************}

procedure TJvCheckListBox.InvertSelection;
var
  i: Integer;
begin
  if MultiSelect then
    for i := 0 to Items.Count - 1 do
      Selected[i] := not Selected[i];
end;

{**************************************************}

procedure TJvCheckListBox.CheckAll;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := True;
end;

{**************************************************}

procedure TJvCheckListBox.UnCheckall;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := False;
end;

{**************************************************}

procedure TJvCheckListBox.InvertCheck;
var
  i: Integer;
begin
  for i := 0 to Items.Count - 1 do
    Checked[i] := not Checked[i];
end;

{**************************************************}

function TJvCheckListBox.GetChecked: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to Items.Count - 1 do
    if Checked[i] then
      Result.AddObject(Items[i], Items.Objects[i]);
end;

{**************************************************}

function TJvCheckListBox.GetUnChecked: TStringList;
var
  i: Integer;
begin
  Result := TStringList.Create;
  for i := 0 to Items.Count - 1 do
    if not Checked[i] then
      Result.AddObject(Items[i], Items.Objects[i]);
end;

{**************************************************}

procedure TJvCheckListBox.LoadFromFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  LoadFromStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvCheckListBox.LoadFromStream(Stream: TStream);
var
  CheckLst: TCheckListRecord;
  st: string;
  buf: array[0..1024] of Char;
begin
  Items.Clear;
  while Stream.Position + SizeOf(TCheckListRecord) <= Stream.Size do
  begin
    Stream.Read(CheckLst, SizeOf(TCheckListRecord));
    if Stream.Position + CheckLst.StringSize <= Stream.Size then
    begin
      Stream.Read(buf, CheckLst.StringSize);
      buf[CheckLst.StringSize] := #0;
      st := buf;
      Checked[Items.Add(st)] := CheckLst.Checked;
    end;
  end;
end;

{**************************************************}

procedure TJvCheckListBox.SaveToFile(FileName: TFileName);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  SaveToStream(Stream);
  Stream.Free;
end;

{**************************************************}

procedure TJvCheckListBox.SaveToStream(Stream: TStream);
var
  CheckLst: TCheckListRecord;
  buf: array[1..1024] of Char;
  i, j: Integer;
begin
  for i := 0 to Items.Count - 1 do
  begin
    CheckLst.Checked := Checked[i];
    CheckLst.StringSize := Length(Items[i]);
    Stream.Write(CheckLst, SizeOf(TCheckListRecord));
    for j := 1 to Length(Items[i]) do
      buf[j] := Items[i][j];
    Stream.Write(buf, CheckLst.StringSize);
  end;
end;

{**************************************************}

procedure TJvCheckListBox.DeleteSelected;
var
  i: Integer;
begin
  if MultiSelect then
  begin
    for i := Items.Count - 1 downto 0 do
      if Selected[i] then
        Items.Delete(i);
  end
  else if ItemIndex <> -1 then
  begin
    i := ItemIndex;
    Items.Delete(i);
    if i > 0 then
      Dec(i);
    if Items.Count > 0 then
      ItemIndex := i;
  end;
end;

end.
