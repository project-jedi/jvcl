{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvCombobox.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse@buypin.com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck@bigfoot.com].
                André Snepvangers [asn@xs4all.nl]

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}

{$I jvcl.inc}

unit JvQCombobox;

interface

uses
  SysUtils, Classes, QGraphics, QControls, QForms, QStdCtrls, QMenus,
  Qt, QWindows,
  JvQCheckListBox, JvQExStdCtrls, JvQMaxPixel, JvQToolEdit, JvQConsts,
  JvQResources, JvQItemsSearchs;

type
  TJvCustomCombobox = class(TJvExCustomComboBox)
  private
    FKey: Word;
    FSearching: Boolean;
    FOnParentColorChanged: TNotifyEvent;
    FOnRestored: TNotifyEvent;
    FOver: Boolean;
    FMaxPixel: TJvMaxPixel;
    FItemSearchs: TJvItemsSearchs;
    procedure MaxPixelChanged(Sender: TObject);
  protected
    procedure Change; override;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure ParentColorChanged; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Loaded; override;
    function SearchExactString(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchPrefix(Value: string; CaseSensitive: Boolean = True): Integer;
    function SearchSubString(Value: string; CaseSensitive: Boolean = True): Integer;
    function DeleteExactString(Value: string; All: Boolean;
      CaseSensitive: Boolean = True): Integer;
  protected
    property AutoComplete default True;
    property SelStart;
    property SelText;
    property SelLength;
    property ItemIndex;
    property DragMode;
    property MaxPixel: TJvMaxPixel read FMaxPixel write FMaxPixel;
    property OnRestored: TNotifyEvent read FOnRestored write FOnRestored;
    property OnParentColorChange: TNotifyEvent read FOnParentColorChanged write FOnParentColorChanged;
  end;

  TJvComboBox = class(TJvCustomComboBox)
  published
    property SelStart;
    property SelText;
    property SelLength;
    property InsertMode;
    property MaxPixel;
    property AutoComplete default True;
    property Style; {Must be published before Items}
    property Anchors;
    property CharCase;
    property Color;
    property Constraints;
    property DragMode;
    property DropDownCount;
    property Duplicates;
    property Enabled;
    property Font;
    property ItemHeight;
    property ItemIndex default -1;
    property MaxLength;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property Text;
    property Visible;
    property OnChange;
    property OnClick;
    property OnCloseUp;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnDropDown;
    property OnHighLighted;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnSelect;
    property OnStartDrag;
    property Items; { Must be published after OnMeasureItem }
    property OnRestored;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;
  end;

  TJvCHBQuoteStyle = (qsNone, qsSingle, qsDouble);

  TJvCheckedComboBox = class(TJvCustomComboEdit)
  private
    FCapSelAll: string;
    FCapDeselAll: string;
    FMouseOverButton: Boolean;
    FItems: TStrings;
    FPrivForm: TForm;
    FListBox: TJvCheckListBox;
    FPopupMenu: TPopupMenu;
    FSelectAll: TMenuItem;
    FDeselectAll: TMenuItem;
    FNoFocusColor: TColor;
    FSorted: Boolean;
    FQuoteStyle: TJvCHBQuoteStyle; // added 2000/04/08
    FCheckedCount: Integer;
    FColumns: Integer;
    FDropDownLines: Integer;
    FDelimiter: Char;
    procedure SetItems(AItems: TStrings);
    procedure ToggleOnOff(Sender: TObject);
    procedure KeyListBox(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ShowCheckList;
    procedure CloseCheckList(Sender: TObject);
    procedure ItemsChange(Sender: TObject);
    procedure SetSorted(Value: Boolean);
    procedure AdjustHeight;
    procedure SetNoFocusColor(Value: TColor);
    procedure SetColumns(Value: Integer);
    procedure SetChecked(Index: Integer; Checked: Boolean);
    procedure SetDropDownLines(Value: Integer);
    function GetChecked(Index: Integer): Boolean;
    function GetItemEnabled(Index: Integer): Boolean;
    procedure SetItemEnabled(Index: Integer; const Value: Boolean);
    function GetState(Index: Integer): TCheckBoxState;
    procedure SetState(Index: Integer; const Value: TCheckBoxState);
    procedure SetDelimiter(const Value: Char);
    function IsStoredCapDeselAll: Boolean;
    function IsStoredCapSelAll: Boolean;
  protected
    procedure DoEnter; override;
    procedure DoExit; override;
    procedure ButtonClick; override;
    procedure AdjustSize; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear; override;
    procedure SetUnCheckedAll(Sender: TObject);
    procedure SetCheckedAll(Sender: TObject);
    function IsChecked(Index: Integer): Boolean;
    function GetText: TCaption; override;
    property Checked[Index: Integer]: Boolean read GetChecked write SetChecked;
    property CheckedCount: Integer read FCheckedCount;
    property ItemEnabled[Index: Integer]: Boolean read GetItemEnabled write SetItemEnabled; //dejoy added
    property State[Index: Integer]: TCheckBoxState read GetState write SetState;
  published
    property Items: TStrings read FItems write SetItems;
    property CapSelectAll: string read FCapSelAll write FCapSelAll stored IsStoredCapSelAll;
    property CapDeSelectAll: string read FCapDeselAll write FCapDeselAll stored IsStoredCapDeselAll;
    property NoFocusColor: TColor read FNoFocusColor write SetNoFocusColor;
    property Sorted: Boolean read FSorted write SetSorted default False;
    property QuoteStyle: TJvCHBQuoteStyle read FQuoteStyle write FQuoteStyle default qsNone; // added 2000/04/08
    property Columns: Integer read FColumns write SetColumns default 0;
    property DropDownLines: Integer read FDropDownLines write SetDropDownLines default 6;
    property Delimiter: Char read FDelimiter write SetDelimiter default ',';
    property Cursor;
    property Enabled;
    property Font;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnResize;
    property OnStartDrag;
  end;

implementation

constructor TJvCustomCombobox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSearching := False;
  ControlStyle := ControlStyle + [csAcceptsControls];
  FOver := False;
  FMaxPixel := TJvMaxPixel.Create(Self);
  FMaxPixel.OnChanged := MaxPixelChanged;
  FItemSearchs := TJvItemsSearchs.Create;
end;

destructor TJvCustomCombobox.Destroy;
begin
  FMaxPixel.Free;
  FItemSearchs.Free;
  inherited;
end;

procedure TJvCustomCombobox.Loaded;
begin
  inherited;
end;

procedure TJvCustomCombobox.Change;
begin
  inherited;
end;

function TJvCustomCombobox.SearchExactString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchExactString(Items, Value, CaseSensitive);
end;

function TJvCustomCombobox.SearchPrefix(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchPrefix(Items, Value, CaseSensitive);
end;

procedure TJvCustomCombobox.KeyDown(var Key: Word; Shift: TShiftState);
begin
  inherited KeyDown(Key, Shift);
  FKey := Key;
end;

procedure TJvCustomCombobox.ParentColorChanged;
begin
  inherited;
  if Assigned(FOnParentColorChanged) then
    FOnParentColorChanged(Self);
end;

function TJvCustomCombobox.SearchSubString(Value: string;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.SearchSubString(Items, Value, CaseSensitive);
end;

function TJvCustomCombobox.DeleteExactString(Value: string; All: Boolean;
  CaseSensitive: Boolean): Integer;
begin
  Result := FItemSearchs.DeleteExactString(Items, Value, CaseSensitive);
end;

procedure TJvCustomCombobox.MaxPixelChanged(Sender: TObject);
var
  st: string;
begin
  if Style <> csDropDownList then
  begin
    st := Text;
    FMaxPixel.Test(st, Font);
    if Text <> st then
      Text := st;
    SelStart := Length(Text);
  end;
end;

//=== { TJvPrivForm } ========================================================

type
  TJvPrivForm = class(TForm)
  protected
    function WidgetFlags: integer; override;
  public
    constructor Create(AOwner: TComponent); override;
  end;

constructor TJvPrivForm.Create(AOwner: TComponent);
begin
  inherited CreateNew(AOwner);
  Color := clWindow;
end;

function TJvPrivForm.WidgetFlags: integer;
begin
  Result := Inherited WidgetFlags or
    Integer(WidgetFlags_WType_Popup) or         // WS_POPUPWINDOW
    Integer(WidgetFlags_WStyle_NormalBorder) or // WS_BORDER
    Integer(WidgetFlags_WStyle_Tool);           // WS_EX_TOOLWINDOW
end;

//=== { TJvCheckedComboBox } =================================================

const
  MAXSELLENGTH = 256;
  MINDROPLINES = 6;
  MAXDROPLINES = 10;

constructor TJvCheckedComboBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FDropDownLines := MINDROPLINES;
  FDelimiter := ',';
  FColumns := 0;
  FQuoteStyle := qsNone;  // added 2000/04/08
  FCheckedCount := 0;
  FNoFocusColor := clWindow;
  Caption := '';
  FCapSelAll := RsCapSelAll;
  FCapDeselAll := RsCapDeselAll;
  Height := 24;
  Width := 121;

  FItems := TStringList.Create;
  TStringList(FItems).OnChange := ItemsChange;

  Color := clWindow;
  ReadOnly := True;

  ShowButton := True;
  ImageKind := ikDropDown;
  AlwaysEnableButton := True;
  AlwaysShowPopup := True;

  Text := '';

  // Create a form with its contents
  FPrivForm := TJvPrivForm.Create(Self);

  // Create CheckListBox
  FListBox := TJvCheckListBox.Create(FPrivForm);
  FListBox.Parent := FPrivForm;
  FListBox.BorderStyle := bsNone;
  FListBox.Columns := FColumns;
  FListBox.Align := alClient;
  FListBox.OnClickCheck := ToggleOnOff;
  FListBox.OnKeyDown := KeyListBox;
  // Create PopUp
  FPopupMenu := TPopupMenu.Create(FListBox);
  FSelectAll := TMenuItem.Create(FPopupMenu);
  FSelectAll.Caption := FCapSelAll;
  FDeselectAll := TMenuItem.Create(FPopupMenu);
  FDeselectAll.Caption := FCapDeselAll;
  FPopupMenu.Items.Insert(0, FSelectAll);
  FPopupMenu.Items.Insert(1, FDeselectAll);
  FSelectAll.OnClick := SetCheckedAll;
  FDeselectAll.OnClick := SetUnCheckedAll;
  FListBox.PopupMenu := FPopupMenu;
end;

destructor TJvCheckedComboBox.Destroy;
begin
  FSelectAll.Free;
  FDeselectAll.Free;
  FPopupMenu.Free;
  FListBox.Free;
  FItems.Free;
  FPrivForm.Free;
  inherited Destroy;
end;

procedure TJvCheckedComboBox.ShowCheckList;
var
  ScreenPoint: TPoint;
begin
  if FMouseOverButton then  // Jan Verhoeven
  begin
    FMouseOverButton := False;
    Exit;
  end;

  Click;
  if FColumns > 1 then
    FDropDownLines := FListBox.Items.Count div FColumns + 1;
  if FDropDownLines < MINDROPLINES then
    FDropDownLines := MINDROPLINES;
  if FDropDownLines > MAXDROPLINES then
    FDropDownLines := MAXDROPLINES;

  // Assign Form coordinate and show
  ScreenPoint := Parent.ClientToScreen(Point(Self.Left, Self.Top + Self.Height));
  FSelectAll.Caption := FCapSelAll;
  FDeselectAll.Caption := FCapDeselAll;
  with TJvPrivForm(FPrivForm) do
  begin
    Font := Self.Font;
    Left := ScreenPoint.X;
    Top := ScreenPoint.Y;
    Width := Self.Width;
    Height := (FDropDownLines * FListBox.ItemHeight + 4{ FEdit.Height });
    BorderStyle := fbsNone;
    OnDeactivate := CloseCheckList;
  end;
  if FPrivForm.Height + ScreenPoint.Y > Screen.Height - 20 then
    FPrivForm.Top := ScreenPoint.Y - FPrivForm.Height - Self.Height;
  FPrivForm.Show;
end;

procedure TJvCheckedComboBox.CloseCheckList(Sender: TObject);
var
  Pt: TPoint;
begin
  // code added by Jan Verhoeven
  // check if the mouse is over the combobox button
  GetCursorPos(Pt);
  Pt := Button.ScreenToClient(Pt);
  with Button do
    FMouseOverButton := (Pt.X > 0) and (Pt.X < Width) and (Pt.Y > 0) and (Pt.Y < Height);
  FPrivForm.Close;
end;

// exanines if string (part) exist in string (source)
// where source is in format part1[,part2]

function PartExist(const Part, Source: string; Delimiter: Char): Boolean;
var
  m: Integer;
  Temp1, Temp2: string;
begin
  Temp1 := Copy(Source, 1, MAXSELLENGTH);
  Result := Part = Temp1;
  while not Result do
  begin
    m := Pos(Delimiter, Temp1);
    if m > 0 then
      Temp2 := Copy(Temp1, 1, m - 1)
    else
      Temp2 := Temp1;
    Result := Part = Temp2;
    if Result or (m = 0) then
      Break;
    Delete(Temp1, 1, m);
  end;
end;

// removes a string (part) from another string (source)
// when source is in format part1[,part2]

function RemovePart(const Part, Source: string; Delimiter: Char): string;
var
  Len, P: Integer;
  S1, S2: string;
begin
  Result := Source;
  S1 := Delimiter + Part + Delimiter;
  S2 := Delimiter + Source + Delimiter;
  P := Pos(S1, S2);
  if P > 0 then
  begin
    Len := Length(Part);
    if P = 1 then
      Result := Copy(Source, P + Len + 1, MAXSELLENGTH)
    else
    begin
      Result := Copy(S2, 2, P - 1) + Copy(S2, P + Len + 2, MAXSELLENGTH);
      SetLength(Result, Length(Result) - 1);
    end;
  end;
end;

function Add(const Sub: string; var Str: string; Delimiter: Char): Boolean;
begin
  Result := False;
  if Length(Str) + Length(Sub) + 1 >= MAXSELLENGTH then
  begin
    raise Exception.CreateRes(@RsENoMoreLength);
    Exit;
  end;
  if Str = '' then
  begin
    Str := Sub;
    Result := True;
  end
  else
  if not PartExist(Sub, Str, Delimiter) then
  begin
    Str := Str + Delimiter + Sub;
    Result := True;
  end;
end;

function Remove(const Sub: string; var Str: string; Delimiter: Char): Boolean;
var
  Temp: string;
begin
  Result := False;
  if Str <> '' then
  begin
    Temp := RemovePart(Sub, Str, Delimiter);
    if Temp <> Str then
    begin
      Str := Temp;
      Result := True;
    end;
  end;
end;

procedure TJvCheckedComboBox.ToggleOnOff(Sender: TObject);
var
  S: string;
begin
  if FListBox.ItemIndex = -1 then
    Exit;
  S := Text;
  if FListBox.Checked[FListBox.ItemIndex] then
  begin
    if Add(FListBox.Items[FListBox.ItemIndex], S, Delimiter) then
      FCheckedCount := FCheckedCount + 1
  end
  else
  if Remove(FListBox.Items[FListBox.ItemIndex], S, Delimiter) then
    FCheckedCount := FCheckedCount - 1;
  Text := S;
  Change;
end;

procedure TJvCheckedComboBox.KeyListBox(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Key = VK_ESCAPE) and (Shift * KeyboardShiftStates = []) then
  begin
    FPrivForm.Close;
    FMouseOverButton := False;
  end;
end;

// added 2000/04/08
function GetFormatedText(Kind: TJvCHBQuoteStyle; const Str: string; Delimiter: Char): string;
var
  S: string;
begin
  Result := Str;
  if Str <> '' then
  begin
    S := Str;
    case Kind of
      qsSingle:
        Result := '''' + StringReplace(S, Delimiter, '''' + Delimiter + '''', [rfReplaceAll]) + '''';
      qsDouble:
        Result := '"' + StringReplace(S, Delimiter, '"' + Delimiter + '"', [rfReplaceAll]) + '"';
    end;
  end;
end;

function TJvCheckedComboBox.GetText: TCaption;
begin
  Result := inherited GetText;
  if FQuoteStyle <> qsNone then
    Result := GetFormatedText(FQuoteStyle, Result, Delimiter);
end;

procedure TJvCheckedComboBox.SetDropDownLines(Value: Integer);
begin
  if FDropDownLines <> Value then
    if (Value >= MINDROPLINES) and (Value <= MAXDROPLINES) then
      FDropDownLines := Value;
end;

procedure TJvCheckedComboBox.SetColumns(Value: Integer);
begin
  if FColumns <> Value then
  begin
    FColumns := Value;
    FListBox.Columns := FColumns;
  end;
end;

procedure TJvCheckedComboBox.SetCheckedAll(Sender: TObject);
var
  I: Integer;
  S: string;
begin
  S := '';
  for I := 0 to FListBox.Items.Count - 1 do
  begin
    if not FListBox.Checked[I] then
      FListBox.Checked[I] := True;

    if I = 0 then
      S := FListBox.Items[I]
    else
      S := S + Delimiter + FListBox.Items[I];
  end;
  Text := S;
  FCheckedCount := FListBox.Items.Count;
  Repaint;
  Change;
end;

procedure TJvCheckedComboBox.SetUnCheckedAll(Sender: TObject);
var
  I: Integer;
begin
  FCheckedCount := 0;
  with FListBox do
  begin
    for I := 0 to Items.Count - 1 do
      if Checked[I] then
        Checked[I] := False;
  end;
  Text := '';
  Change;
end;

function TJvCheckedComboBox.IsChecked(Index: Integer): Boolean;
begin
  Result := FListBox.Checked[Index];
end;

procedure TJvCheckedComboBox.SetChecked(Index: Integer; Checked: Boolean);
var
  S: string;
  ChangeData: Boolean;
begin
  if Index < FListBox.Items.Count then
  begin
    S := Text;
    ChangeData := False;
    if not FListBox.Checked[Index] and Checked then
    begin
      if Add(FListBox.Items[Index], S, Delimiter) then
      begin
        FCheckedCount := FCheckedCount + 1;
        ChangeData := True;
      end;
    end
    else
    if FListBox.Checked[Index] and not Checked then
      if Remove(FListBox.Items[Index], S, Delimiter) then
      begin
        FCheckedCount := FCheckedCount - 1;
        ChangeData := True;
      end;
    if ChangeData then
    begin
      FListBox.Checked[Index] := Checked;
      Text := S;
      Change;
    end;
  end;
end;

function TJvCheckedComboBox.GetChecked(Index: Integer): Boolean;
begin
  if Index < FListBox.Items.Count then
    Result := FListBox.Checked[Index]
  else
    Result := False;
end;

procedure TJvCheckedComboBox.SetItems(AItems: TStrings);
begin
  FItems.Assign(AItems);
end;

procedure TJvCheckedComboBox.ItemsChange(Sender: TObject);
begin
  FListBox.Clear;
  Text := '';
  FListBox.Items.Assign(FItems);
end;

procedure TJvCheckedComboBox.AdjustHeight;
var
  DC: HDC;
  SaveFont: HFont;
  I: Integer;
  SysMetrics, Metrics: TTextMetric;
begin
  DC := GetDC(0);
  GetTextMetrics(DC, SysMetrics);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  if NewStyleControls then
  begin
//    if Ctl3D then
//      I := 8
//    else
      I := 6;
    I := GetSystemMetrics(SM_CYBORDER) * I;
  end
  else
  begin
    I := SysMetrics.tmHeight;
    if I > Metrics.tmHeight then
      I := Metrics.tmHeight;
    I := I div 4 + GetSystemMetrics(SM_CYBORDER) * 4;
  end;
  Height := Metrics.tmHeight + I;
end;

procedure TJvCheckedComboBox.DoEnter;
begin
  Color := clWindow;
  inherited DoEnter;
end;

procedure TJvCheckedComboBox.DoExit;
begin
  Color := FNoFocusColor;
  inherited DoExit;
end;

procedure TJvCheckedComboBox.SetNoFocusColor(Value: TColor);
begin
  if FNoFocusColor <> Value then
  begin
    FNoFocusColor := Value;
    Color := Value;
  end;
end;

procedure TJvCheckedComboBox.Clear;
begin
  FItems.Clear;
  FListBox.Clear;
  inherited Clear;
end;

procedure TJvCheckedComboBox.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    FSorted := Value;
    TStringList(FItems).Sorted := FSorted;
  end;
end;

function TJvCheckedComboBox.GetItemEnabled(Index: Integer): Boolean;
begin
  Result := FListBox.ItemEnabled[Index];
end;

procedure TJvCheckedComboBox.SetItemEnabled(Index: Integer; const Value: Boolean);
begin
  FListBox.ItemEnabled[Index] := Value;
end;

function TJvCheckedComboBox.GetState(Index: Integer): TCheckBoxState;
begin
  Result := FListBox.State[Index];
end;

procedure TJvCheckedComboBox.SetState(Index: Integer;
  const Value: TCheckBoxState);
begin
  FListBox.State[Index] := Value;
end;

procedure TJvCheckedComboBox.ButtonClick;
begin
  ShowCheckList;
end;

procedure TJvCheckedComboBox.AdjustSize;
begin
  inherited AdjustSize;
  AdjustHeight;
end;

procedure TJvCheckedComboBox.SetDelimiter(const Value: Char);
var
  I: Integer;
  S: string;
begin
  if Value <> FDelimiter then
  begin
    FDelimiter := Value;
    Text := '';
    S := '';
    for I := 0 to FListBox.Items.Count - 1 do
    begin
      if FListBox.Checked[I] then
      begin
        if I = 0 then
          S := FListBox.Items[I]
        else
          S := S + Delimiter + FListBox.Items[I];
      end;
    end;
    Text := S;
  end;
end;

function TJvCheckedComboBox.IsStoredCapDeselAll: Boolean;
begin
  Result := FCapSelAll <> RsCapSelAll;
end;

function TJvCheckedComboBox.IsStoredCapSelAll: Boolean;
begin
  Result := FCapDeselAll <> RsCapDeselAll;
end;

end.


