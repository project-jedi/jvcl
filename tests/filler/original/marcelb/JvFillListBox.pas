unit JvFillListBox;
// peter3
interface
uses
  Windows, Classes, StdCtrls, Controls, Graphics, JvFillIntf_mbe2;

type
  TJvFillListBox = class(TCustomListBox, IFillerNotify)
  private
    FFiller: IFiller;
    FOptions: TJvFillerOptions;
    procedure FillerOptionsChanged(Sender: TObject);
    procedure FillerChanging(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure FillerChanged(const AFiller: IFiller; AReason: TJvFillerChangeReason);
    procedure setFiller(const Value: iFiller);
  protected
    // FillerOptions
    procedure DefineProperties(Filer: TFiler); override;
    procedure ReadFillerOptionClass(Reader: TReader);
    procedure WriteFillerOptions(Writer: TWriter);
    procedure WriteFillerOptionClass(Writer: TWriter);

    procedure DrawItem(Index: Integer; Rect: TRect;
      State: TOwnerDrawState); override;
    procedure MeasureItem(Index: Integer; var Height: Integer);override;

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetBounds(ALeft: Integer; ATop: Integer; AWidth: Integer;
      AHeight: Integer);override;
  published
    property Items: IFiller read FFiller write setFiller;
    property AutoComplete;
    property Align;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelKind default bkNone;
    property BevelOuter;
    property BiDiMode;
    property BorderStyle;
    property Color;
    property Columns;
    property Constraints;
    property Ctl3D;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property ExtendedSelect;
    property FillerOptions: TJvFillerOptions read FOptions write FOptions stored False;
    property Font;
    property ImeMode;
    property ImeName;
    property IntegralHeight;
    property ItemHeight;
    property MultiSelect;
    property ParentBiDiMode;
    property ParentColor;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ScrollWidth;
    property ShowHint;
    property Sorted;
    property TabOrder;
    property TabStop;
    property TabWidth;
    property Visible;
    property OnClick;
    property OnContextPopup;
    property OnData;
    property OnDataFind;
    property OnDataObject;
    property OnDblClick;
    property OnDragDrop;
    property OnDragOver;
    property OnDrawItem;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnMeasureItem;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
  end;

implementation

uses Dialogs, SysUtils, Math, RTLConsts, TypInfo;
const
  cStyle = lbVirtualOwnerDraw;

  { TJvFillListBox }

constructor TJvFillListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
end;

destructor TJvFillListBox.Destroy;
begin
  //  Style := lbStandard;
  Items := nil;
  inherited;
end;

procedure TJvFillListBox.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  Filer.DefineProperty('FillerOptionsClass', ReadFillerOptionClass, WriteFillerOptionClass, FillerOptions <> nil);
end;

type
  TOpenReader = class(TReader)
  end;
  {$M+}
  TOpenWriter = class(TWriter)
    function GetPropPath: string;
    function PropPathField: PString;
    procedure SetPropPath(const NewPath: string);
    property PropPath: string read GetPropPath write SetPropPath;
  published
    property RootAncestor;
  end;
  {$M-}

  function TOpenWriter.GetPropPath: string;
  begin
    Result := PropPathField^;
  end;

  function TOpenWriter.PropPathField: PString;
  var
    RAPI: PPropInfo;
  begin
    RAPI := GetPropInfo(TOpenWriter, 'RootAncestor');
    if RAPI = nil then
      raise Exception.Create('Internal error.');
    Result := Pointer(Cardinal(RAPI.GetProc) and $00FFFFFF + Cardinal(Self) + 4);
  end;

  procedure TOpenWriter.SetPropPath(const NewPath: string);
  begin
    if NewPath <> PropPath then
      PropPathField^ := NewPath;
  end;

procedure TJvFillListBox.ReadFillerOptionClass(Reader: TReader);
var
  S: string;
  OptionClass: TPersistentClass;
begin
  if FOptions <> nil then
    FreeAndNil(FOptions);
  S := Reader.ReadString;
  OptionClass := FindClass(S);
  if OptionClass.InheritsFrom(TJvFillerOptions) then
    FOptions := TJvFillerOptionsClass(OptionClass).Create(FillerOptionsChanged)
  else
    raise EClassNotFound.CreateFmt(SClassNotFound, [S]);
end;

procedure TJvFillListBox.WriteFillerOptions(Writer: TWriter);
var
  OldAncestor: TPersistent;
  OldPropPath: string;
begin
  { We're cheating a bit here: we store the current Ancestor and PropPath and the set Ancestor
    to nil and PropPath to 'FillerOptions.'. Note that the latter is not a property so it's
    redeclared in TOpenWriter and using some fiddling with RTTI and GetPropInfo we managed to
    get the info. This implementation is very dangerous as it assumes the FPropPath field follows
    directly after the FRootAncestor field. This hasn't changed since D3 so I think it's safe ;) }
  OldAncestor := Writer.Ancestor;
  Writer.Ancestor := nil;
  try
    OldPropPath := TOpenWriter(Writer).PropPath;
    try
      TOpenWriter(Writer).PropPath := 'FillerOptions.';
      TOpenWriter(Writer).WriteProperties(FOptions);
    finally
      TOpenWriter(Writer).PropPath := OldPropPath;
    end;
  finally
    Writer.Ancestor := OldAncestor;
  end;
end;

procedure TJvFillListBox.WriteFillerOptionClass(Writer: TWriter);
begin
  Writer.WriteString(FOptions.ClassName);
  // write the FillerOptions properties
  WriteFillerOptions(Writer);
end;

procedure TJvFillListBox.DrawItem(Index: Integer; Rect: TRect;
  State: TOwnerDrawState);
var
  FillerItems: IFillerItems;
begin
  if Items <> nil then
  begin
    if Supports(Items, IFillerItems, FillerItems) then
    begin
      Canvas.Font := Font;
      if odSelected in State then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color  := clHighlightText;
      end
      else
        Canvas.Brush.Color := Color;
      if fsCanRender in Items.getSupports then
        FillerItems.DrawItem(Canvas, Rect, Index, State, FOptions)
      else
        Canvas.TextRect(Rect, Rect.left, Rect.Top, (FillerItems.Items[Index] as IFillerItemText).Caption);
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TJvFillListBox.FillerChanged(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if Style = cStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cStyle then
      Count := (Items as IFillerItems).Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.FillerOptionsChanged(Sender: TObject);
begin
  Invalidate;
end;

procedure TJvFillListBox.FillerChanging(const AFiller: IFiller;
  AReason: TJvFillerChangeReason);
begin
  case AReason of
    frDestroy:
      begin
        if Style = cStyle then
          Count := 0;
        Items := nil;
      end;
  else
    if Style = cStyle then
      Count := (Items as IFillerItems).Count;
    Invalidate;
  end;
end;

procedure TJvFillListBox.MeasureItem(Index: Integer; var Height: Integer);
var
  aSize: TSize;
  FillerItems: IFillerItems;
begin
  if fsCanMeasure in Items.getSupports then
  begin
    if Supports(Self, IFillerItems, FillerItems) then
    begin
      aSize.cy := 0;
      if Items <> nil then
        aSize := FillerItems.MeasureItem(Canvas, Index, FOptions);
      if aSize.cy <> 0 then
        Height := aSize.cy;
    end
    else
      inherited;
  end
  else
    inherited;
end;

procedure TJvFillListBox.SetBounds(ALeft, ATop, AWidth, AHeight: Integer);
begin
  inherited;
  Invalidate;
end;

procedure TJvFillListBox.setFiller(const Value: iFiller);
var aSize:TSize;
begin
  if FFiller <> Value then
  begin
    if FFiller <> nil then
    begin
      FFiller.UnRegisterChangeNotify(self);
      if (Style = cStyle) and HasParent then
        Count := 0;
    end;
    if (FOptions <> nil) and not (csLoading in ComponentState) then
      FreeAndNil(FOptions);
    FFiller := Value;
    if FFiller <> nil then
    begin
      FFiller.RegisterChangeNotify(self);
      if HasParent then
      begin
        Style := cStyle;
        Count := (FFiller as IFillerItems).Count;
      end
      else
        Style := lbStandard;
      if fsCanMeasure in FFiller.getSupports then
      begin
        aSize := (FFiller as IFillerItems).MeasureItem(Canvas,-1, FOptions);
        if aSize.cx <> 0 then
          ClientWidth := aSize.cx;
        if aSize.cy <> 0 then
          ItemHeight := aSize.cy;
      end;
      if (FFiller.getOptionClass <> nil) and not (csLoading in ComponentState) then
        FOptions := FFiller.getOptionClass.Create(FillerOptionsChanged);
    end;
  end;
end;

end.

