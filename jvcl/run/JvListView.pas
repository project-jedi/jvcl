{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvListView.PAS, released on 2001-02-28.

The Initial Developer of the Original Code is Sébastien Buysse [sbuysse att buypin dott com]
Portions created by Sébastien Buysse are Copyright (C) 2001 Sébastien Buysse.
All Rights Reserved.

Contributor(s): Michael Beck [mbeck att bigfoot dott com].
                dejoy

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
// $Id$

unit JvListView;

{$I jvcl.inc}
{$I vclonly.inc}

interface

uses
  {$IFDEF UNITVERSIONING}
  JclUnitVersioning,
  {$ENDIF UNITVERSIONING}
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  ComCtrls, CommCtrl, Menus, ImgList, Clipbrd,
  JvTypes, JvExComCtrls, JvAppStorage;

const
  WM_AUTOSELECT = WM_USER + 1;

type
  EJvListViewError = EJVCLException;
  //  TJvSortMethod = (smAutomatic, smAlphabetic, smNonCaseSensitive, smNumeric, smDate, smTime, smDateTime, smCurrency);
  TJvOnProgress = procedure(Sender: TObject; Progression, Total: Integer) of object;

  TJvListItems = class(TListItems, IJvAppStorageHandler, IJvAppStoragePublishedProps)
  private
    FOwnerInterface: IInterface;
  protected
    { IInterface }
    function _AddRef: Integer; stdcall;
    function _Release: Integer; stdcall;
    { IJvAppStorageHandler }
    procedure ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
    procedure WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);

    { List item reader used in the call to ReadList. }
    procedure ReadListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { List item writer used in the call to WriteList. }
    procedure WriteListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const Index: Integer; const ItemName: string);
    { List item deleter usedin the call to WriteList. }
    procedure DeleteListItem(Sender: TJvCustomAppStorage; const Path: string;
      const List: TObject; const First, Last: Integer; const ItemName: string);
  public
    function QueryInterface(const IID: TGUID; out Obj): HRESULT; virtual; stdcall;
    procedure AfterConstruction; override;
  end;

  TJvListItem = class(TListItem)
  private
    FPopupMenu: TPopupMenu;
    FBold: Boolean;
    FFont: TFont;
    FBrush: TBrush;
    procedure SetBrush(const Value: TBrush);
  protected
    procedure SetPopupMenu(const Value: TPopupMenu);
    procedure SetFont(const Value: TFont);
  public
    constructor CreateEnh(AOwner: TListItems; const Popup: TPopupMenu);
    destructor Destroy; override;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
  published
    property Font: TFont read FFont write SetFont;
    property Brush: TBrush read FBrush write SetBrush;
    // Published now for the usage of AppStorage.Read/WritePersistent
    property Caption;
    property Checked;
    property Selected;
    property SubItems;
  end;

  TJvListExtendedColumn = class(TCollectionItem)
  private
    FSortMethod: TJvSortMethod;
    FUseParentSortMethod: Boolean;
    function GetSortMethod: TJvSortMethod;
    procedure SetSortMethod(const Value: TJvSortMethod);
  public
    constructor Create(Collection: TCollection); override;

    procedure Assign(AValue: TPersistent); override;
  published
    property SortMethod: TJvSortMethod read GetSortMethod write SetSortMethod default smAutomatic;
    property UseParentSortMethod : Boolean read FUseParentSortMethod write FUseParentSortMethod default True;
  end;

  TJvListExtendedColumns = class(TOwnedCollection)
  private
    function GetItem(Index: Integer): TJvListExtendedColumn;
    procedure SetItem(Index: Integer; const Value: TJvListExtendedColumn);

    function Owner : TPersistent;
  public
    constructor Create(AOwner: TPersistent);
    property Items[Index: Integer] : TJvListExtendedColumn read GetItem write SetItem; default;
  end;

  TViewStyles = set of TViewStyle;

const
  ALL_VIEW_STYLES = [vsIcon, vsSmallIcon, vsList, vsReport];

type
  TJvListView = class(TJvExListView)
  private
    FAutoClipboardCopy: Boolean;
    FSortOnClick: Boolean;
    FLast: Integer;
    FOnSaveProgress: TJvOnProgress;
    FOnLoadProgress: TJvOnProgress;
    FOnAutoSort: TJvListViewColumnSortEvent;
    FSortMethod: TJvSortMethod;
    FOnHorizontalScroll: TNotifyEvent;
    FOnVerticalScroll: TNotifyEvent;
    FImageChangeLink: TChangeLink;
    FHeaderImages: TCustomImageList;
    FAutoSelect: Boolean;
    FPicture: TPicture;
    FExtendedColumns: TJvListExtendedColumns;
    FSavedExtendedColumns: TJvListExtendedColumns;
    FViewStylesItemBrush: TViewStyles;  // use for Create/DestroyWnd process
    procedure DoPictureChange(Sender: TObject);
    procedure SetPicture(const Value: TPicture);
    procedure SetHeaderImages(const Value: TCustomImageList);
    procedure UpdateHeaderImages(HeaderHandle: Integer);
    procedure WMAutoSelect(var Msg: TMessage); message WM_AUTOSELECT;
    procedure SetExtendedColumns(const Value: TJvListExtendedColumns);
    {$IFDEF COMPILER5}
    function GetItemIndex: Integer;
    procedure SetItemIndex(const Value: Integer);
    {$ENDIF COMPILER5}
    procedure SetViewStylesItemBrush(const Value: TViewStyles);
  protected
    function CreateListItem: TListItem; override;
    function CreateListItems: TListItems; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    procedure WMHScroll(var Msg: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Msg: TWMVScroll); message WM_VSCROLL;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    function GetColumnsOrder: string;
    procedure SetColumnsOrder(const Order: string);
    procedure SetItemPopup(Node: TListItem; Value: TPopupMenu);
    function GetItemPopup(Node: TListItem): TPopupMenu;
    procedure DoHeaderImagesChange(Sender: TObject);
    procedure Loaded; override;

    procedure CreateWnd; override;
    procedure DestroyWnd; override;

    procedure WMNCCalcSize(var Msg: TWMNCCalcSize); message WM_NCCALCSIZE;
    procedure LVMDeleteColumn(var Msg: TMessage); message LVM_DELETECOLUMN;
    procedure LVMInsertColumn(var Msg: TMessage); message LVM_INSERTCOLUMN;

    procedure InsertItem(Item: TListItem); override;
    function IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean; {$IFDEF COMPILER6_UP} override; {$ENDIF}
    function CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawItem(Item: TListItem; State: TCustomDrawState;
      Stage: TCustomDrawStage): Boolean; override;
    function CustomDrawSubItem(Item: TListItem; SubItem: Integer;
      State: TCustomDrawState; Stage: TCustomDrawStage): Boolean; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ColClick(Column: TListColumn); override;
    procedure SaveToStrings(Strings: TStrings; Separator: Char);
    procedure LoadFromStrings(Strings: TStrings; Separator: Char);
    procedure SaveToFile(FileName: string; ForceOldStyle: Boolean = False);
    procedure LoadFromFile(FileName: string);
    procedure SaveToStream(Stream: TStream; ForceOldStyle: Boolean = False);
    procedure LoadFromStream(Stream: TStream);
    procedure SaveToCSV(FileName: string; Separator: Char = ';');
    procedure LoadFromCSV(FileName: string; Separator: Char = ';');
    procedure SetSmallImages(const Value: TCustomImageList);
    {$IFDEF COMPILER5}
    procedure SelectAll;
    procedure DeleteSelected;
    {$ENDIF COMPILER5}
    procedure UnselectAll;
    procedure InvertSelection;
    function MoveUp(Index: Integer; Focus: Boolean = True): Integer;
    function MoveDown(Index: Integer; Focus: Boolean = True): Integer;
    function SelectNextItem(Focus: Boolean = True): Integer;
    function SelectPrevItem(Focus: Boolean = True): Integer;

    property ItemPopup[Item: TListItem]: TPopupMenu read GetItemPopup write SetItemPopup;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer); override;
    procedure SetFocus; override;
    {$IFDEF COMPILER5}
    property ItemIndex: Integer read GetItemIndex write SetItemIndex;
    {$ENDIF COMPILER5}
  published
    property AutoSelect: Boolean read FAutoSelect write FAutoSelect default True;
    property ColumnsOrder: string read GetColumnsOrder write SetColumnsOrder;
    property HintColor;
    property Picture: TPicture read FPicture write SetPicture;
    property HeaderImages: TCustomImageList read FHeaderImages write SetHeaderImages;
    property SortMethod: TJvSortMethod read FSortMethod write FSortMethod default smAutomatic;
    property SortOnClick: Boolean read FSortOnClick write FSortOnClick default True;
    property SmallImages write SetSmallImages;
    property AutoClipboardCopy: Boolean read FAutoClipboardCopy write FAutoClipboardCopy default True;

    property ViewStylesItemBrush : TViewStyles read FViewStylesItemBrush write SetViewStylesItemBrush default ALL_VIEW_STYLES;

    property OnAutoSort: TJvListViewColumnSortEvent read FOnAutoSort write FOnAutoSort;
    property OnHorizontalScroll: TNotifyEvent read FOnHorizontalScroll write FOnHorizontalScroll;
    property OnLoadProgress: TJvOnProgress read FOnLoadProgress write FOnLoadProgress;
    property OnSaveProgress: TJvOnProgress read FOnSaveProgress write FOnSaveProgress;
    property OnVerticalScroll: TNotifyEvent read FOnVerticalScroll write FOnVerticalScroll;
    property OnMouseEnter;
    property OnMouseLeave;
    property OnParentColorChange;

    // This property contains a collection that allows to specify additional
    // properties for each columns (sort method for instance). It can not be
    // included in the Columns collection as the VCL does not offer a way
    // to specify which class to use for the items of the Columns collection.
    // Note that this one (ExtendedColumns) is populated automatically when
    // a column is added or deleted. But because the VCL code for add starts
    // by deleting all columns to reinsert them after, you should not change
    // the properties for any item of ExtendedColumns in a loop that contains
    // a call to the Add method of the Columns property.
    property ExtendedColumns : TJvListExtendedColumns read FExtendedColumns write SetExtendedColumns;
  end;

{$IFDEF UNITVERSIONING}
const
  UnitVersioning: TUnitVersionInfo = (
    RCSfile: '$RCSfile$';
    Revision: '$Revision$';
    Date: '$Date$';
    LogPath: 'JVCL\run'
    );
{$ENDIF UNITVERSIONING}

implementation

uses
  Math,
  JvJCLUtils, JvConsts, JvResources;

//=== { TJvListItem } ========================================================

const
  // (rom) increased from 100
  cColumnsHandled = 1024;

constructor TJvListItem.CreateEnh(AOwner: TListItems; const Popup: TPopupMenu);
begin
  inherited Create(AOwner);
  
  FBold := False;
  FPopupMenu := Popup; // (Salvatore) Get it from the JvListView
  FFont := TFont.Create;
  FBrush := TBrush.Create;
end;

destructor TJvListItem.Destroy;
begin
  FFont.Free;
  FBrush.Free;
  
  inherited Destroy;
end;

procedure TJvListItem.SetBrush(const Value: TBrush);
begin
  FBrush.Assign(Value);
end;

procedure TJvListItem.SetFont(const Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TJvListItem.SetPopupMenu(const Value: TPopupMenu);
begin
  FPopupMenu := Value;
end;

//=== { TJvListItems } =======================================================

procedure TJvListItems.AfterConstruction;
begin
  inherited AfterConstruction;
  if GetOwner <> nil then
    GetOwner.GetInterface(IInterface, FOwnerInterface);
end;

function TJvListItems._AddRef: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._AddRef
  else
    Result := -1;
end;

function TJvListItems._Release: Integer;
begin
  if FOwnerInterface <> nil then
    Result := FOwnerInterface._Release
  else
    Result := -1;
end;

function TJvListItems.QueryInterface(const IID: TGUID; out Obj): HRESULT;
const
  E_NOINTERFACE = HRESULT($80004002);
begin
  if GetInterface(IID, Obj) then
    Result := 0
  else
    Result := E_NOINTERFACE;
end;

procedure TJvListItems.ReadFromAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  BeginUpdate;
  try
    Clear;
    AppStorage.ReadList(BasePath, Self, ReadListItem, cItem);
  finally
    EndUpdate;
  end;
end;

procedure TJvListItems.WriteToAppStorage(AppStorage: TJvCustomAppStorage; const BasePath: string);
begin
  AppStorage.WriteList(BasePath, Self, Count, WriteListItem, DeleteListItem, cItem);
end;

procedure TJvListItems.ReadListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
var
  NewItem: TPersistent;
  NewPath: string;
begin
  if List is TJvListItems then
    try
      NewPath := Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]);
      NewItem := TJvListItems(List).Add;
      Sender.ReadPersistent(NewPath, NewItem);
    except
    end;
end;

procedure TJvListItems.WriteListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const Index: Integer; const ItemName: string);
begin
  if List is TJvListItems then
    if Assigned(TJvListItems(List)[Index]) then
      Sender.WritePersistent(Sender.ConcatPaths([Path, ItemName + IntToStr(Index)]), TPersistent(TJvListItems(List)[Index]));
end;

procedure TJvListItems.DeleteListItem(Sender: TJvCustomAppStorage;
  const Path: string; const List: TObject; const First, Last: Integer; const ItemName: string);
var
  I: Integer;
begin
  if List is TJvListItems then
    for I := First to Last do
      Sender.DeleteValue(Sender.ConcatPaths([Path, ItemName + IntToStr(I)]));
end;

{ TJvListExtendedColumn }

procedure TJvListExtendedColumn.Assign(AValue: TPersistent);
begin
  if AValue is TJvListExtendedColumn then
  begin
    FSortMethod := TJvListExtendedColumn(AValue). SortMethod;
    FUseParentSortMethod := TJvListExtendedColumn(AValue).UseParentSortMethod;
  end
  else
    inherited Assign(AValue);
end;

constructor TJvListExtendedColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);

  FSortMethod := smAutomatic;
  FUseParentSortMethod := True;
end;

function TJvListExtendedColumn.GetSortMethod: TJvSortMethod;
begin
  if (TJvListExtendedColumns(Collection).Owner is TJvListView) and UseParentSortMethod then
    Result := TJvListView(TJvListExtendedColumns(Collection).Owner).SortMethod
  else
    Result := FSortMethod;
end;

procedure TJvListExtendedColumn.SetSortMethod(
  const Value: TJvSortMethod);
begin
  FSortMethod := Value;
  UseParentSortMethod := False;
end;

{ TJvListExtendedColumns }

constructor TJvListExtendedColumns.Create(AOwner: TPersistent);
begin
  inherited Create(AOwner, TJvListExtendedColumn);
end;

function TJvListExtendedColumns.GetItem(
  Index: Integer): TJvListExtendedColumn;
begin
  Result := TJvListExtendedColumn(inherited Items[Index]);
end;

function TJvListExtendedColumns.Owner: TPersistent;
begin
  Result := GetOwner;
end;

procedure TJvListExtendedColumns.SetItem(Index: Integer;
  const Value: TJvListExtendedColumn);
begin
  inherited Items[Index] := Value;
end;

//=== { TJvListView } ========================================================

const
  cLISTVIEW01 = 'LISTVIEW01';

constructor TJvListView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSortOnClick := True;
  FSortMethod := smAutomatic;
  FLast := -1;
  FAutoClipboardCopy := True;
  FImageChangeLink := TChangeLink.Create;
  FImageChangeLink.OnChange := DoHeaderImagesChange;
  FAutoSelect := True;
  FPicture := TPicture.Create;
  FPicture.OnChange := DoPictureChange;

  FViewStylesItemBrush := ALL_VIEW_STYLES;
  FExtendedColumns := TJvListExtendedColumns.Create(Self);
  FSavedExtendedColumns := TJvListExtendedColumns.Create(Self);
end;

destructor TJvListView.Destroy;
begin
  FExtendedColumns.Free;
  FSavedExtendedColumns.Free;

  FImageChangeLink.Free;
  FPicture.Free;
  inherited Destroy;
end;

procedure TJvListView.WMHScroll(var Msg: TWMHScroll);
begin
  inherited;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if Assigned(FOnHorizontalScroll) then
    FOnHorizontalScroll(Self);
end;

procedure TJvListView.WMVScroll(var Msg: TWMVScroll);
begin
  inherited;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if Assigned(FOnVerticalScroll) then
    FOnVerticalScroll(Self);
end;

procedure TJvListView.ColClick(Column: TListColumn);
type
  TParamSort = record
    Index: Integer;
    Sender: TObject;
  end;
var
  Parm: TParamSort;

  function CustomCompare1(Item1, Item2, ParamSort: Integer): Integer stdcall;
  var
    Parm: TParamSort;
    i1, i2: TListItem;
    S1, S2: string;
    I: Integer;
    SortKind: TJvSortMethod;

    function IsBigger(First, Second: string; SortType: TJvSortMethod): Boolean;
    var
      I, J: Real;
      d, e: TDateTime;
      a, b: Currency;
      l, m: Int64;
      st, st2: string;
      int1, int2: Integer;

      function FirstNonAlpha(Value: string): Integer;
      var
        Len: Integer;
        I, J: Integer;
        Comma: Boolean;
      begin
        Len := Length(Value);
        I := 1;
        J := 0;
        Comma := False;

        while I <= Len do
        begin
          case Value[I] of
            '0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
              J := I;
            ',', '.':
              if not Comma then
                Comma := True
              else
              begin
                J := I - 1;
                I := Len;
              end;
          else
            begin
              J := I - 1;
              I := Len;
            end;
          end;
          Inc(I);
        end;

        Result := J;
      end;

    begin
      Result := False;
      if Trim(First) = '' then
        Result := False
      else
        if Trim(Second) = '' then
          Result := True
        else
        begin
          case SortType of
            smAlphabetic:
              Result := First > Second;
            smNonCaseSensitive:
              Result := UpperCase(First) > UpperCase(Second);
            smNumeric:
              begin
                try
                  I := StrToFloat(First);
                  J := StrToFloat(Second);
                  Result := I > J;
                except
                  try
                    l := StrToInt64(First);
                  except
                    l := 0;
                  end;
                  try
                    m := StrToInt64(Second);
                  except
                    m := 0;
                  end;
                  Result := l > m;
                end;
              end;
            smDate:
              begin
                d := StrToDate(First);
                e := StrToDate(Second);
                Result := d > e;
              end;
            smTime:
              begin
                d := StrToTime(First);
                e := StrToTime(Second);
                Result := d > e;
              end;
            smDateTime:
              begin
                d := StrToDateTime(First);
                e := StrToDateTime(Second);
                Result := d > e;
              end;
            smCurrency:
              begin
                a := StrToCurr(First);
                b := StrToCurr(Second);
                Result := a > b;
              end;
            smAutomatic:
              begin
                int1 := FirstNonAlpha(First);
                int2 := FirstNonAlpha(Second);
                if (int1 <> 0) and (int2 <> 0) then
                begin
                  st := Copy(First, 1, int1);
                  st2 := Copy(Second, 1, int2);
                  try
                    Result := StrToFloat(st) > StrToFloat(st2);
                  except
                    Result := First > Second;
                  end;
                end
                else
                  Result := First > Second;
              end;
          end;
        end;
    end;

  begin
    Parm := TParamSort(Pointer(ParamSort)^);
    i1 := TListItem(Item1);
    i2 := TListItem(Item2);
    I := Parm.Index;

    // (Salvatore)
    if Parm.Index < TJvListView(Parm.Sender).ExtendedColumns.Count  then
      SortKind := TJvListView(Parm.Sender).ExtendedColumns[Parm.Index].SortMethod
    else
      SortKind := TJvListView(Parm.Sender).SortMethod; 
      
    if Assigned(TJvListView(Parm.Sender).OnAutoSort) then
      TJvListView(Parm.Sender).OnAutoSort(Parm.Sender, Parm.Index, SortKind);

    case I of
      {sort by caption}
      0:
        begin
          S1 := i1.Caption;
          S2 := i2.Caption;

          if IsBigger(S1, S2, SortKind) then
            Result := 1
          else
            if IsBigger(S2, S1, SortKind) then
              Result := -1
            else
              Result := 0;
        end;
    else
      {sort by Column}
      begin
        if I > i1.SubItems.Count then
        begin
          if I > i2.SubItems.Count then
            Result := 0
          else
            Result := -1;
        end
        else
          if I > i2.SubItems.Count then
            Result := 1
          else
          begin
            S1 := i1.SubItems[I - 1];
            S2 := i2.SubItems[I - 1];
            if IsBigger(S1, S2, SortKind) then
              Result := 1
            else
              if IsBigger(S2, S1, SortKind) then
                Result := -1
              else
                Result := 0;
          end;
      end;
    end;
  end;

  function CustomCompare2(Item1, Item2, ParamSort: Integer): Integer; stdcall;
  begin
    Result := -CustomCompare1(Item1, Item2, ParamSort);
  end;

begin
  inherited ColClick(Column);
  if FSortOnClick then
  begin
    Parm.Index := Column.Index;
    Parm.Sender := Self;
    if FLast = Column.Index then
    begin
      FLast := -1;
      CustomSort(TLVCompare(@CustomCompare2), Integer(@Parm));
    end
    else
    begin
      FLast := Column.Index;
      CustomSort(TLVCompare(@CustomCompare1), Integer(@Parm));
    end;
  end;
end;

function TJvListView.CreateListItem: TListItem;
begin
  Result := TJvListItem.CreateEnh(Items, Self.PopupMenu);
end;

function TJvListView.CreateListItems: TListItems;
begin
  Result := TJvListItems.Create(Self);
end;

function TJvListView.GetItemPopup(Node: TListItem): TPopupMenu;
begin
  Result := TJvListItem(Node).PopupMenu;
end;

procedure TJvListView.SetItemPopup(Node: TListItem; Value: TPopupMenu);
begin
  TJvListItem(Node).PopupMenu := Value;
end;

procedure TJvListView.LoadFromFile(FileName: string);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmOpenRead or fmShareDenyWrite);
  try
    LoadFromStream(Stream);
  finally
    Stream.Free;
  end;
end;

// (rom) a 100 char buffer is silly

procedure TJvListView.LoadFromStream(Stream: TStream);
var
  Buf: array [0..100] of Char;
  Start: Integer;

  procedure LoadOldStyle(Stream: TStream);
  var
    I, J, K: Integer;
    Buf: array [0..100] of Byte;
    st: string;
    ch1, checks: Boolean;
    t: TListItem;
  begin
    I := Stream.Position;
    t := nil;
    st := '';
    Items.Clear;
    if Assigned(FOnLoadProgress) then
      FOnLoadProgress(Self, 0, Stream.Size - Start);
    checks := False;
    ch1 := CheckBoxes;
    while I < Stream.Size do
    begin
      J := Stream.Read(Buf, 100);
      if Assigned(FOnLoadProgress) then
        FOnLoadProgress(Self, J, Stream.Size - Start);
      I := I + J;
      K := 0;
      while K < J do
      begin
        while (K < J) and (Buf[K] <> 0) and (Buf[K] <> 1) do
        begin
          st := st + Char(Buf[K]);
          Inc(K);
        end;

        if K < J then
        begin
          if t <> nil then
            t.SubItems.Add(st)
          else
          begin
            t := Items.Add;
            checks := checks or (st[1] = 'T');
            t.Checked := st[1] = 'T';
            st := Copy(st, 2, Length(st));
            t.Caption := st;
          end;
          if Buf[K] = 1 then
            t := nil;
          st := '';
        end;
        Inc(K);
      end;
    end;
    if (not ch1) and (not checks) then
      CheckBoxes := False;
  end;

  procedure LoadNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    // hs-    LV_CHECKED = $8000;
  var
    Count, I, J: SmallInt;
    Options: Byte;
    st: string;
    t: TListItem;
    Buf: array [0..2048] of Char;
  begin
    try
      Self.Items.BeginUpdate;
      Self.Items.Clear;
      Self.Items.EndUpdate;

      Stream.Read(Options, SizeOf(Options));
      CheckBoxes := (Options and LV_HASCHECKBOXES) = LV_HASCHECKBOXES;

      //Read all lines
      while Stream.Position < Stream.Size do
      begin
        Stream.Read(Count, SizeOf(Count));

        //statistics
        if Assigned(FOnLoadProgress) then
          FOnLoadProgress(Self, Stream.Position, Stream.Size - Start);

        //Read all columns
        t := Self.Items.Add;
        for I := 1 to Count do
        begin
          // hs-
          if I = 1 then
          begin
            Stream.Read(Options, SizeOf(Options));
            if CheckBoxes then
              t.Checked := Boolean(Options and Ord(True));
          end;
          // -hs

          (* hs-
                    Stream.Read(J, SizeOf(I));
          -hs *)
          Stream.Read(J, SizeOf(J));

          //Read the string
          FillChar(Buf, SizeOf(Buf), #0);
          Stream.Read(Buf, J);
          st := Buf;

          if I = 1 then
          begin
            t.Caption := st;
            (* hs-
                        if CheckBoxes then
                          t.Checked := (I and LV_CHECKED) = LV_CHECKED;
            -hs *)
          end
          else
            t.SubItems.Add(st);
        end;
      end;
    except
    end;
  end;

begin
  Start := Stream.Position;
  Stream.Read(Buf, 10);
  Buf[10] := #0;
  if Buf <> cLISTVIEW01 then
  begin
    Stream.Position := Start;
    LoadOldStyle(Stream);
  end
  else
    LoadNewStyle(Stream);
end;

procedure TJvListView.SaveToFile(FileName: string; ForceOldStyle: Boolean);
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate or fmShareExclusive);
  try
    SaveToStream(Stream, ForceOldStyle);
  finally
    Stream.Free;
  end;
end;

procedure TJvListView.SaveToStream(Stream: TStream; ForceOldStyle: Boolean);

  procedure SaveOldStyle(Stream: TStream);
  var
    I, J, K: Integer;
    b, c, d, e: Byte;
    st: string;
    Buf: array [0..1000] of Byte;
  begin
    b := 0;
    c := 1;
    d := Ord('T'); //checked
    e := Ord('F'); //not checked
    if Assigned(FOnSaveProgress) then
      FOnSaveProgress(Self, 0, Self.Items.Count);
    for I := 0 to Self.Items.Count - 1 do
    begin
      if Assigned(FOnSaveProgress) then
        FOnSaveProgress(Self, I + 1, Self.Items.Count);
      st := Self.Items[I].Caption;
      for K := 1 to Length(st) do
        Buf[K - 1] := Byte(st[K]);
      K := Length(st);
      //write checked,not
      if Self.Items[I].Checked then
        Stream.Write(d, 1)
      else
        Stream.Write(e, 1);
      Stream.Write(Buf, K);
      if Self.Items[I].SubItems.Count = 0 then
        Stream.Write(c, 1)
      else
      begin
        Stream.Write(b, 1);
        for J := 0 to Self.Items[I].SubItems.Count - 2 do
        begin
          st := Self.Items[I].SubItems[J];
          for K := 1 to Length(st) do
            Buf[K - 1] := Byte(st[K]);
          K := Length(st);
          Stream.Write(Buf, K);
          Stream.Write(b, 1);
        end;
        J := Self.Items[I].SubItems.Count - 1;
        st := Self.Items[I].SubItems[J];
        for K := 1 to Length(st) do
          Buf[K - 1] := Byte(st[K]);
        K := Length(st);
        Stream.Write(Buf, K);
        Stream.Write(c, 1);
      end;
    end;
  end;

  procedure SaveNewStyle(Stream: TStream);
  const
    LV_HASCHECKBOXES = $80;
    // hs-    LV_CHECKED = $8000;
  var
    Buf: array [0..100] of Char;
    // hs-    I, J: Word;
    I: Integer;
    J: SmallInt;

    // hs    Options : Byte;
    Options, IsChecked: Byte;

    procedure WriteString(const Txt: string);
    var
      I: Word;
    begin
      I := Length(Txt);
      Stream.Write(I, SizeOf(I));
      if I > 0 then
        Stream.Write(Txt[1], I);
    end;

  begin
    Buf := cLISTVIEW01;
    Stream.Write(Buf, 10);
    if CheckBoxes then
      Options := LV_HASCHECKBOXES
    else
      Options := 0;
    Stream.Write(Options, SizeOf(Options));
    for I := 0 to Items.Count - 1 do
      with Items[I] do
      begin
        J := SubItems.Count + 1;
        Stream.Write(J, SizeOf(J));
        // hs-
        IsChecked := Options or (Byte(Ord(Checked)));
        Stream.Write(IsChecked, SizeOf(IsChecked));
        // -hs
        WriteString(Items[I].Caption);
        for J := 0 to Items[I].SubItems.Count - 1 do
          WriteString(SubItems[J]);
      end;
  end;

begin
  if ForceOldStyle then
    SaveOldStyle(Stream)
  else
    SaveNewStyle(Stream);
end;

procedure TJvListView.SaveToStrings(Strings: TStrings; Separator: Char);
var
  I, J: Integer;
  TmpStr: string;
begin
  if Assigned(FOnSaveProgress) then
    FOnSaveProgress(Self, 0, Items.Count);
  for I := 0 to Items.Count - 1 do
  begin
    if Assigned(FOnSaveProgress) then
      FOnSaveProgress(Self, I + 1, Items.Count);
    TmpStr := AnsiQuotedStr(Items[I].Caption, '"');
    for J := 0 to Items[I].SubItems.Count - 1 do
      TmpStr := TmpStr + Separator + AnsiQuotedStr(Items[I].SubItems[J], '"');
    Strings.Add(TmpStr);
  end;
end;

procedure TJvListView.LoadFromStrings(Strings: TStrings; Separator: Char);
var
  I: Integer;
  Start, Stop, TmpStart: PChar;
  TmpStr: string;
  Li: TListItem;
begin
  for I := 0 to Strings.Count - 1 do
  begin
    Li := nil;
    Start := PChar(Strings[I]);
    Stop := Start + Length(Strings[I]);
    if (Start <> Stop) and (Start <> nil) and (Start^ <> #0) then
    begin
      if Start^ = '"' then
      begin
        Li := Items.Add;
        TmpStr := AnsiExtractQuotedStr(Start, '"'); // this moves the PChar pointer
        Li.Caption := TmpStr;
      end
      else
      begin
        TmpStart := Start;
        while Start^ <> Separator do
        begin
          if Start = Stop then
            Break;
          Inc(Start);
        end;
        SetString(TmpStr, TmpStart, Start - TmpStart);
        Li := Items.Add;
        Li.Caption := TmpStr;
      end;
    end;
    if Li <> nil then
    begin
      while (Start <> Stop) and (Start <> nil) and (Start^ <> #0) do
      begin
        while Start^ = Separator do
          Inc(Start);
        if Start^ = '"' then
        begin
          TmpStr := AnsiExtractQuotedStr(Start, '"'); // this moves the PChar pointer
          Li.SubItems.Add(TmpStr);
        end
        else
        begin
          TmpStart := Start;
          while Start^ <> Separator do
          begin
            if Start = Stop then
              Break;
            Inc(Start);
          end;
          SetString(TmpStr, TmpStart, Start - TmpStart);
          Li.SubItems.Add(TmpStr);
        end;
      end;
    end;
  end;
end;

procedure TJvListView.LoadFromCSV(FileName: string; Separator: Char);
var
  S: TStringList;
begin
  S := TStringList.Create;
  Items.BeginUpdate;
  try
    Items.Clear;
    S.LoadFromFile(FileName);
    LoadFromStrings(S, Separator);
  finally
    Items.EndUpdate;
    S.Free;
  end;
end;

procedure TJvListView.SaveToCSV(FileName: string; Separator: Char);
var
  S: TStringList;
begin
  S := TStringList.Create;
  Items.BeginUpdate;
  try
    SaveToStrings(S, Separator);
    S.SaveToFile(FileName);
  finally
    Items.EndUpdate;
    S.Free;
  end;
end;

procedure TJvListView.InvertSelection;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := not Items[I].Selected;
  Items.EndUpdate;
end;

{$IFDEF COMPILER5}
procedure TJvListView.SelectAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := True;
  Items.EndUpdate;
end;
{$ENDIF COMPILER5}

procedure TJvListView.UnselectAll;
var
  I: Integer;
begin
  Items.BeginUpdate;
  for I := 0 to Items.Count - 1 do
    Items[I].Selected := False;
  Items.EndUpdate;
end;

procedure TJvListView.KeyUp(var Key: Word; Shift: TShiftState);
var
  st: string;
  I, J: Integer;
begin
  inherited KeyUp(Key, Shift);
  if AutoClipboardCopy then
    if (Key in [Ord('c'), Ord('C')]) and (ssCtrl in Shift) then
    begin
      for I := 0 to Columns.Count - 1 do
        st := st + Columns[I].Caption + Tab;
      if st <> '' then
        st := st + sLineBreak;
      for I := 0 to Items.Count - 1 do
        if (SelCount = 0) or Items[I].Selected then
        begin
          st := st + Items[I].Caption;
          for J := 0 to Items[I].SubItems.Count - 1 do
            st := st + Tab + Items[I].SubItems[J];
          st := st + sLineBreak;
        end;
      Clipboard.SetTextBuf(PChar(st));
    end;
end;

{$IFDEF COMPILER5}
procedure TJvListView.DeleteSelected;
var
  I: Integer;
begin
  Items.BeginUpdate;
  if SelCount = 1 then
  begin
    I := Selected.Index - 1;
    Selected.Delete;
    if I = -1 then
      I := 0;
    if Items.Count > 0 then
      Selected := Items[I];
  end
  else
    for I := Items.Count - 1 downto 0 do
      if Items[I].Selected then
        Items[I].Delete;
  Items.EndUpdate;
end;
{$ENDIF COMPILER5}

function TJvListView.GetColumnsOrder: string;
var
  Res: array [0..cColumnsHandled - 1] of Integer;
  I: Integer;
begin
  ListView_GetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
  Result := '';
  if Columns.Count > cColumnsHandled then
    raise EJvListViewError.CreateRes(@RsETooManyColumns);
  for I := 0 to Columns.Count - 1 do
  begin
    if Result <> '' then
      Result := Result + ',';
    Result := Result + IntToStr(Res[I]) + '=' + IntToStr(Columns[I].Width);
  end;
end;

procedure TJvListView.SetColumnsOrder(const Order: string);
var
  Res: array [0..cColumnsHandled - 1] of Integer;
  I, J: Integer;
  st: string;
begin
  FillChar(Res, SizeOf(Res), #0);
  with TStringList.Create do
  try
    CommaText := Order;
    I := 0;
    while Count > 0 do
    begin
      st := Strings[0];
      J := Pos('=', st);
      if (J <> 0) and (I < Columns.Count) then
      begin
        Columns[I].Width := StrToIntDef(Copy(st, J + 1, Length(st)), Columns[I].Width);
        st := Copy(st, 1, J - 1);
      end;
      Res[I] := StrToIntDef(st, 0);
      Delete(0);
      Inc(I);
    end;
    ListView_SetColumnOrderArray(Columns.Owner.Handle, Columns.Count, @Res[0]);
  finally
    Free;
  end;
end;

procedure TJvListView.SetHeaderImages(const Value: TCustomImageList);
begin
  if FHeaderImages <> Value then
  begin
    if FHeaderImages <> nil then
      FHeaderImages.UnRegisterChanges(FImageChangeLink);
    FHeaderImages := Value;
    if Assigned(FHeaderImages) then
    begin
      FHeaderImages.RegisterChanges(FImageChangeLink);
      FHeaderImages.FreeNotification(Self);
    end;
    UpdateHeaderImages(ListView_GetHeader(Handle));
  end;
end;

procedure TJvListView.SetExtendedColumns(
  const Value: TJvListExtendedColumns);
begin
  FExtendedColumns.Assign(Value);
end;

procedure TJvListView.Notification(AComponent: TComponent;
  Operation: TOperation);
var
  I: Integer;
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
    if AComponent = HeaderImages then
      HeaderImages := nil
    else
      if not (csDestroying in ComponentState) and (AComponent is TPopupMenu) then
        for I := 0 to Items.Count - 1 do
          if TJvListItem(Items[I]).PopupMenu = AComponent then
            TJvListItem(Items[I]).PopupMenu := nil;
end;

procedure TJvListView.CreateWnd;
begin
  inherited CreateWnd;
  UpdateHeaderImages(ListView_GetHeader(Handle));
  if FSavedExtendedColumns.Count > 0 then
    FExtendedColumns.Assign(FSavedExtendedColumns);
end;

procedure TJvListView.UpdateHeaderImages(HeaderHandle: Integer);
//var
//  WP: TWindowPlacement;
begin
  if (HeaderHandle <> 0) and (ViewStyle = vsReport) and ShowColumnHeaders then
  begin
//    WP.length := SizeOf(WP);
//    GetWindowPlacement(HeaderHandle, @WP);
    if HeaderImages <> nil then
    begin
      Header_SetImageList(HeaderHandle, HeaderImages.Handle);
//      WP.rcNormalPosition.Bottom := WP.rcNormalPosition.Top + HeaderImages.Height + 3;
    end
    else
      if ComponentState * [csLoading, csDestroying] = [] then
      begin
        Header_SetImageList(HeaderHandle, 0);
//      WP.rcNormalPosition.Bottom := WP.rcNormalPosition.Top + 17;
      end;
    // the problem with resizing the header is that there doesn't seem to be an easy way of telling the listview about it...
//    SetWindowPlacement(HeaderHandle, @WP);
    UpdateColumns;
    InvalidateRect(HeaderHandle, nil, True)
  end;
end;

procedure TJvListView.DoHeaderImagesChange(Sender: TObject);
begin
  UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.SetSmallImages(const Value: TCustomImageList);
begin
  inherited SmallImages := Value;
  UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.Loaded;
begin
  inherited Loaded;
  UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.WMNCCalcSize(var Msg: TWMNCCalcSize);
//var
//  R: TRect;
begin
  inherited;
//  if Msg.CalcValidRects and Assigned(HeaderImages) and (ViewStyle = vsReport) and ShowColumnHeaders then
//    with Msg.CalcSize_Params^.rgrc[0] do
//      Top := Top + HeaderImages.Height + 3;
end;

procedure TJvListView.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited SetBounds(ALeft, ATop, AWidth, AHeight);
  if HandleAllocated then
    UpdateHeaderImages(ListView_GetHeader(Handle));
end;

procedure TJvListView.InsertItem(Item: TListItem);
begin
  inherited InsertItem(Item);
  if AutoSelect and (Selected = nil) and (Items.Count < 2) then
    PostMessage(Handle, WM_AUTOSELECT, Integer(Item), 1);
end;

procedure TJvListView.WMAutoSelect(var Msg: TMessage);
var
  lv: TListItem;
begin
  with Msg do
  begin
    lv := TListItem(WParam);
    if Assigned(lv) and (Items.IndexOf(lv) >= 0) and (LParam = 1) then
    begin
      lv.Selected := True;
      lv.Focused := True;
    end;
  end;
end;

function TJvListView.MoveDown(Index: Integer; Focus: Boolean = True): Integer;
var
  lv, lv2: TListItem;
  FOnInsert, FOnDeletion: TLVDeletedEvent;
  FOnCompare: TLVCompareEvent;
begin
  Result := Index;
  if (Index >= 0) and (Index < Items.Count) then
  begin
    lv2 := Items[Index];
    FOnInsert := OnInsert;
    FOnDeletion := OnDeletion;
    FOnCompare := OnCompare;
    try
      OnInsert := nil;
      OnDeletion := nil;
      OnCompare := nil;
      lv := Items.Insert(Index + 2);
      lv.Assign(lv2);
      lv2.Delete;
    finally
      OnInsert := FOnInsert;
      OnDeletion := FOnDeletion;
      OnCompare := FOnCompare;
    end;
    if Focus then
    begin
      lv.Selected := True;
      lv.Focused := True;
    end;
    Result := lv.Index;
  end;
end;

function TJvListView.MoveUp(Index: Integer; Focus: Boolean = True): Integer;
var
  lv, lv2: TListItem;
  FOnInsert, FOnDeletion: TLVDeletedEvent;
  FOnCompare: TLVCompareEvent;
begin
  Result := Index;
  if (Index > 0) and (Index < Items.Count) then
  begin
    lv2 := Items[Index];
    FOnInsert := OnInsert;
    FOnDeletion := OnDeletion;
    FOnCompare := OnCompare;
    try
      OnInsert := nil;
      OnDeletion := nil;
      OnCompare := nil;
      lv := Items.Insert(Index - 1);
      lv.Assign(lv2);
      lv2.Delete;
    finally
      OnInsert := FOnInsert;
      OnDeletion := FOnDeletion;
      OnCompare := FOnCompare;
    end;
    if Focus then
    begin
      lv.Selected := True;
      lv.Focused := True;
    end;
    Result := lv.Index;
  end;
end;

function TJvListView.SelectNextItem(Focus: Boolean = True): Integer;
begin
  Result := ItemIndex + 1;
  if Result < Items.Count then
    ItemIndex := Result;
  Result := ItemIndex;
  if Focus and (Result >= 0) and (Result < Items.Count) then
  begin
    Items[Result].Selected := True;
    Items[Result].Focused := True;
  end;
end;

function TJvListView.SelectPrevItem(Focus: Boolean = True): Integer;
begin
  Result := ItemIndex - 1;
  if Result >= 0 then
    ItemIndex := Result;
  Result := ItemIndex;
  if Focus and (Result >= 0) and (Result < Items.Count) then
  begin
    Items[Result].Selected := True;
    Items[Result].Focused := True;
  end;
end;

procedure TJvListView.SetFocus;
var
  Index: Integer;
begin
  inherited SetFocus;

  Index := 0;
  if Assigned(ItemFocused) then
    Index := ItemIndex;
    
  if AutoSelect and (Selected = nil) and (Items.Count > 0) then
    PostMessage(Handle, WM_AUTOSELECT, Integer(Items[Index]), 1);
end;


function TJvListView.IsCustomDrawn(Target: TCustomDrawTarget; Stage: TCustomDrawStage): Boolean;
begin
  Result := inherited IsCustomDrawn(Target, Stage) or
    ((Stage = cdPrePaint) and (Picture.Graphic <> nil) and not Picture.Graphic.Empty) or
    ((Stage = cdPrePaint) and ((Target = dtItem) or (Target = dtSubItem)));
end;


function TJvListView.CustomDraw(const ARect: TRect; Stage: TCustomDrawStage): Boolean;
var
  BmpXPos, BmpYPos: Integer; // X and Y position for bitmap
  ItemRect: TRect; // List item bounds rectangle
  TopOffset: Integer; // Y pos where bmp drawing starts
  Bmp: TBitmap;

  function GetHeaderHeight: Integer;
  var
    Header: HWND; // header window handle
    Pl: TWindowPlacement; // header window placement
  begin
    // Get header window
    Header := SendMessage(Handle, LVM_GETHEADER, 0, 0);
    // Get header window placement
    FillChar(Pl, SizeOf(Pl), 0);
    Pl.length := SizeOf(Pl);
    GetWindowPlacement(Header, @Pl);
    // Calculate header window height
    Result := Pl.rcNormalPosition.Bottom - Pl.rcNormalPosition.Top;
  end;

begin
  Result := inherited CustomDraw(ARect, Stage);

  if Result and (Stage = cdPrePaint) and (FPicture <> nil) and (FPicture.Graphic <> nil) and not
    FPicture.Graphic.Empty and (FPicture.Graphic.Width > 0) and (FPicture.Graphic.Height > 0) then
  begin
    Bmp := TBitmap.Create;
    try
      Bmp.Width := ClientWidth;
      Bmp.Height := ClientHeight;
      Bmp.Canvas.Brush.Color := Self.Color;
      Bmp.Canvas.FillRect(ClientRect);

    // Get top offset where drawing starts
      if Items.Count > 0 then
      begin
        ListView_GetItemRect(Handle, 0, ItemRect, LVIR_BOUNDS);
        TopOffset := ListView_GetTopIndex(Handle) * (ItemRect.Bottom - ItemRect.Top);
      end
      else
        TopOffset := 0;
      if ViewStyle = vsReport then
        BmpYPos := ARect.Top - TopOffset + GetHeaderHeight
      else
        BmpYPos := 0;
      // Draw the image
      while BmpYPos < ARect.Bottom do
      begin
        // draw image across width of display
        BmpXPos := ARect.Left;
        while BmpXPos < ARect.Right do
        begin
//      DrawIconEx draws alpha-blended icons better (on XP) but gives problems with selecting in the listview
//      if Picture.Graphic is TIcon then
//        DrawIconEx(Canvas.Handle, BmpXPos, BmpYPos, Picture.Icon.Handle, 0, 0, 0, 0, DI_NORMAL)
//      else
          Bmp.Canvas.Draw(BmpXPos, BmpYPos, Picture.Graphic);
          Inc(BmpXPos, Picture.Graphic.Width);
        end;
        // move to next row
        Inc(BmpYPos, Picture.Graphic.Height);
      end;
      BitBlt(Canvas, 0, 0, ClientWidth, ClientHeight, Bmp.Canvas, 0, 0, SRCCOPY);
    // Ensure that the items are drawn transparently
      SetBkMode(Canvas.Handle, TRANSPARENT);
      ListView_SetTextBkColor(Handle, CLR_NONE);
      ListView_SetBKColor(Handle, CLR_NONE);
    finally
      Bmp.Free;
    end;
  end;
end;

function TJvListView.CustomDrawItem(Item: TListItem;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  if (Stage = cdPrePaint) and Assigned(Item) then
  begin
    Canvas.Font := TJvListItem(Item).Font;
    if ViewStyle in ViewStylesItemBrush then
      Canvas.Brush := TJvListItem(Item).Brush;
  end;

  Result := inherited CustomDrawItem(Item, State, Stage);
end;

function TJvListView.CustomDrawSubItem(Item: TListItem; SubItem: Integer;
  State: TCustomDrawState; Stage: TCustomDrawStage): Boolean;
begin
  if (Stage = cdPrePaint) and Assigned(Item) then
  begin
    Canvas.Font := TJvListItem(Item).Font;
    if ViewStyle in ViewStylesItemBrush then
      Canvas.Brush := TJvListItem(Item).Brush;
  end;

  Result := inherited CustomDrawSubItem(Item, SubItem, State, Stage);
end;

procedure TJvListView.SetPicture(const Value: TPicture);
begin
  FPicture.Assign(Value);
end;

procedure TJvListView.DoPictureChange(Sender: TObject);
begin
//  if (Picture.Graphic <> nil) and not Picture.Graphic.Empty then
//    Picture.Graphic.Transparent := true;
  Invalidate;
end;

procedure TJvListView.LVMDeleteColumn(var Msg: TMessage);
begin
  inherited;
  // This may happen at design time, especially when migrating
  // a project that uses an old version of TJvListView that did
  // not the ExtendedColumns
  if Msg.WParam < FExtendedColumns.Count then
    FExtendedColumns.Delete(Msg.WParam);
end;

procedure TJvListView.LVMInsertColumn(var Msg: TMessage);
begin
  inherited;
  FExtendedColumns.Insert(Msg.WParam);
end;

procedure TJvListView.DestroyWnd;
begin
  FSavedExtendedColumns.Assign(FExtendedColumns);
  inherited DestroyWnd;
end;

{$IFDEF COMPILER5}

function TJvListView.GetItemIndex: Integer;
begin
  if Selected <> nil then
    Result := Selected.Index
  else
    Result := -1;
end;

procedure TJvListView.SetItemIndex(const Value: Integer);
begin
  if (Value >= 0) and (Value < Items.Count) then
    Items[Value].Selected := True;
end;

{$ENDIF COMPILER5}

procedure TJvListView.SetViewStylesItemBrush(const Value: TViewStyles);
begin
  FViewStylesItemBrush := Value;
  Invalidate;
end;


{$IFDEF UNITVERSIONING}
initialization
  RegisterUnitVersion(HInstance, UnitVersioning);

finalization
  UnregisterUnitVersion(HInstance);
{$ENDIF UNITVERSIONING}

end.

