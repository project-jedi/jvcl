{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: QToolWin.pas, released on 2004-05-16

The Initial Developer of the Original Code is Andreas Hausladen
                                              [Andreas dott Hausladen att gmx dott de]
Copyright (C) 2004 Andreas Hausladen.
All Rights Reserved.

Contributor(s):

Known Issues:
----------------------------------------------------------------------------}
// $Id$

unit QNotebook;

interface

uses
  SysUtils, Classes, Contnrs, Types, Qt, QGraphics, QControls, QForms;

type
  TPage = class(TCustomControl)
  protected
    procedure ReadState(Reader: TReader); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
  published
    property Caption;
    property Height stored False;
    property TabOrder stored False;
    property Visible stored False;
    property Width stored False;
  end;

  TNotebook = class(TCustomControl)
  private
    FPages: TStrings;
    FPageIndex: Integer;
    FOnPageChanged: TNotifyEvent;
    procedure SetPages(Value: TStrings);
    procedure SetActivePage(const Value: string);
    function GetActivePage: string;
    procedure SetPageIndex(Value: Integer);
    procedure ShowPage(Index: Integer);
    function GetPageControls(Index: Integer): TPage;
    function GetPageCount: Integer;
  protected
    function GetChildOwner: TComponent; override;
    procedure GetChildren(Proc: TGetChildProc; Root: TComponent); override;
    procedure ReadState(Reader: TReader); override;
    procedure ShowControl(AControl: TControl); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property PageCount: Integer read GetPageCount;
    property PageControls[Index: Integer]: TPage read GetPageControls;
  published
    property ActivePage: string read GetActivePage write SetActivePage stored False;
    property Align;
    property Anchors;
    property Color;
    property DragMode;
    property Font;
    property Enabled;
    property Constraints;
    property PageIndex: Integer read FPageIndex write SetPageIndex default 0;
    property Pages: TStrings read FPages write SetPages stored False;
    property ParentColor;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnPageChanged: TNotifyEvent read FOnPageChanged write FOnPageChanged;
    property OnStartDrag;
  end;

implementation

uses
  QConsts;

type
  TPages = class(TStringList)
  private
    FPageList: TObjectList;
    FNotebook: TNotebook;
    FLoadingAdd: TPage;
  protected
    procedure Put(Index: Integer; const S: string); override;
    procedure InsertItem(Index: Integer; const S: string; AObject: TObject); override;
  public
    constructor Create(ANotebook: TNotebook);
    destructor Destroy; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Move(CurIndex, NewIndex: Integer); override;
  end;

{ TPage }

constructor TPage.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Visible := False;
  ControlStyle := ControlStyle + [csAcceptsControls, csNoDesignVisible];
  Align := alClient;
end;

procedure TPage.Paint;
begin
  if csDesigning in ComponentState then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDash;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TPage.ReadState(Reader: TReader);
begin
  if Reader.Parent is TNotebook then
  begin
    with TPages(TNotebook(Reader.Parent).FPages) do
    begin
      FLoadingAdd := Self;
      FPageList.Add(Self);
    end;
  end;
  inherited ReadState(Reader);
end;

{ TPages }

constructor TPages.Create(ANotebook: TNotebook);
begin
  inherited Create;
  FPageList := TObjectList.Create;
  FNotebook := ANotebook;
end;

destructor TPages.Destroy;
begin
  FPageList.Free;
  inherited Destroy;
end;

procedure TPages.Clear;
begin
  inherited Clear;
  FPageList.Clear;
end;

procedure TPages.Delete(Index: Integer);
begin
  inherited Delete(Index);
  FPageList.Delete(Index);
  if Index >= Count then
    Dec(Index);
  FNotebook.PageIndex := Index;
end;

procedure TPages.InsertItem(Index: Integer; const S: string;
  AObject: TObject);
begin
  inherited InsertItem(Index, S, AObject);
  if FLoadingAdd = nil then
    FLoadingAdd := TPage.Create(FNotebook);
  FLoadingAdd.Parent := FNotebook;
  FPageList.Insert(Index, FLoadingAdd);
  FLoadingAdd := nil;
  FNotebook.PageIndex := Index;
end;

procedure TPages.Move(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);
  FPageList.Move(CurIndex, NewIndex);
end;

procedure TPages.Put(Index: Integer; const S: string);
begin
  inherited Put(Index, S);
  TPage(FPageList[Index]).Caption := S;
end;

{ TNotebook }

var
  NotebookRegistered: Boolean = False;

constructor TNotebook.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Exclude(FComponentStyle, csInheritable);
  Width := 150;
  Height := 150;
  FPages := TPages.Create(Self);
  FPageIndex := -1;
  FPages.Add(SDefault);
  PageIndex := 0;
  if not NotebookRegistered then
  begin
    RegisterClasses([TPage]);
    NotebookRegistered := True;
  end;
end;

destructor TNotebook.Destroy;
begin
  FPages.Free;
  inherited Destroy;
end;

function TNotebook.GetActivePage: string;
begin
  if PageIndex <> -1 then
    Result := Pages[PageIndex]
  else
    Result := '';
end;

function TNotebook.GetChildOwner: TComponent;
begin
  Result := Self;
end;

procedure TNotebook.GetChildren(Proc: TGetChildProc; Root: TComponent);
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
    Proc(PageControls[i]);
end;

function TNotebook.GetPageControls(Index: Integer): TPage;
begin
  Result := TPage(TPages(Pages).FPageList[Index]);
end;

function TNotebook.GetPageCount: Integer;
begin
  Result := FPages.Count;
end;

procedure TNotebook.Paint;
begin
  if PageCount = 0 then
  begin
    Canvas.Brush.Style := bsClear;
    Canvas.Pen.Style := psDot;
    Canvas.Rectangle(0, 0, Width, Height);
  end;
end;

procedure TNotebook.ReadState(Reader: TReader);
begin
  Pages.Clear;
  inherited ReadState(Reader);
  if (PageIndex <> -1) and (Cardinal(PageIndex) < Cardinal(PageCount)) then
    ShowPage(PageIndex)
  else
    FPageIndex := -1;
end;

procedure TNotebook.SetActivePage(const Value: string);
begin
  PageIndex := FPages.IndexOf(Value);
end;

procedure TNotebook.SetPageIndex(Value: Integer);
var
  ParentForm: TCustomForm;
begin
  if csLoading in ComponentState then
  begin
    FPageIndex := Value;
    Exit;
  end;

  if (Value <> FPageIndex) and (Cardinal(Value) < Cardinal(PageCount)) then
  begin
    ParentForm := GetParentForm(Self);
    if ParentForm <> nil then
      if ContainsControl(ParentForm.ActiveControl) then
        ParentForm.ActiveControl := Self;
    ShowPage(Value);

    if (Cardinal(FPageIndex) < Cardinal(PageCount)) then
      PageControls[FPageIndex].Visible := False;
    FPageIndex := Value;
    if ParentForm <> nil then
      if ParentForm.ActiveControl = Self then
        SelectFirst;

    if Assigned(FOnPageChanged) then
      FOnPageChanged(Self);
  end;
end;

procedure TNotebook.SetPages(Value: TStrings);
begin
  FPages.Assign(Value);
end;

procedure TNotebook.ShowControl(AControl: TControl);
var
  i: Integer;
begin
  for i := 0 to PageCount - 1 do
    if PageControls[i] = AControl then
    begin
      PageIndex := i;
      Exit;
    end;
  inherited ShowControl(AControl);
end;

procedure TNotebook.ShowPage(Index: Integer);
begin
  with PageControls[Index] do
  begin
    Align := alClient;
    BringToFront;
    Visible := True;
  end
end;

end.
