{**************************************************************************************************}
{  WARNING:  JEDI preprocessor generated unit. Manual modifications will be lost on next release.  }
{**************************************************************************************************}

{-----------------------------------------------------------------------------
The contents of this file are subject to the Mozilla Public License
Version 1.1 (the "License"); you may not use this file except in compliance
with the License. You may obtain a copy of the License at
http://www.mozilla.org/MPL/MPL-1.1.html

Software distributed under the License is distributed on an "AS IS" basis,
WITHOUT WARRANTY OF ANY KIND, either expressed or implied. See the License for
the specific language governing rights and limitations under the License.

The Original Code is: JvImageListViewer.PAS, released on 2003-12-01.

The Initial Developer of the Original Code is: Peter Thörnqvist
All Rights Reserved.

Last Modified: 2003-12-27

You may retrieve the latest version of this file at the Project JEDI's JVCL home page,
located at http://jvcl.sourceforge.net

Known Issues:
-----------------------------------------------------------------------------}
unit JvQImageListViewer;

{$I jvcl.inc}
interface
uses
  SysUtils, Classes,
  QControls, QGraphics, QStdCtrls, QComCtrls, QImgList,
  JvQCustomItemViewer;

type
  TJvImageListViewerOptions = class(TJvCustomItemViewerOptions)
  private
    FDrawingStyle: TDrawingStyle;
    FSelectedStyle: TDrawingStyle;
    FFillCaption: boolean;
    FFrameSize: word;
    procedure SetDrawingStyle(const Value: TDrawingStyle);
    procedure SetSelectedStyle(const Value: TDrawingStyle);
    procedure SetFillCaption(const Value: boolean);
    procedure SetFrameSize(const Value: word);
  public
    constructor Create(AOwner: TJvCustomItemViewer); override;
  published
    property AutoCenter;
    property BrushPattern;
    property DragAutoScroll;
    property DrawingStyle: TDrawingStyle read FDrawingStyle write SetDrawingStyle default dsTransparent;
    property FillCaption: boolean read FFillCaption write SetFillCaption default true;
    property SelectedStyle: TDrawingStyle read FSelectedStyle write SetSelectedStyle default dsSelected;
    property FrameSize:word read FFrameSize write SetFrameSize default 1;
    property Height;
    property Layout;
    property RightClickSelect;
    property ScrollBar;
    property ShowCaptions;
    property Tracking;
    property Width;
  end;

  TJvImageListViewerCaptionEvent = procedure(Sender: TObject; ImageIndex: integer; var ACaption: string) of object;
  TJvImageListViewer = class(TJvCustomItemViewer)
  private
    FChangeLink: TChangeLink;
    FImages: TCustomImageList;
    FOnGetCaption: TJvImageListViewerCaptionEvent;
    procedure SetImages(const Value: TCustomImageList);
    function GetOptions: TJvImageListViewerOptions;
    procedure SetOptions(const Value: TJvImageListViewerOptions);
  protected
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure DoImageChange(Sender: TObject);
    procedure DrawItem(Index: Integer; State: TCustomDrawState; Canvas: TCanvas; ItemRect, TextRect: TRect); override;
    function GetOptionsClass: TJvItemViewerOptionsClass; override;
    function GetCaption(ImageIndex: integer): string; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  published
    property Images: TCustomImageList read FImages write SetImages;
    property Options: TJvImageListViewerOptions read GetOptions write SetOptions;
    property SelectedIndex;
    property OnGetCaption: TJvImageListViewerCaptionEvent read FOnGetCaption write FOnGetCaption;

    property Align;
    property Anchors;
    //    property BiDiMode;
    property Color;
    property Constraints;
    property DockSite;
    property DragCursor;
    property DragKind;
    property DragMode;
    property Enabled;
    property Font;
    //    property ParentBiDiMode;
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
    property OnDockDrop;
    property OnDockOver;
    property OnDragOver;
    property OnEndDock;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetSiteInfo;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnStartDock;
    property OnStartDrag;
    property OnUnDock;
  end;

implementation
uses
  CommCtrl, JvQJCLUtils, QMath;

{ TJvImageListViewerOptions }

constructor TJvImageListViewerOptions.Create(AOwner: TJvCustomItemViewer);
begin
  inherited;
  FDrawingStyle := dsTransparent;
  FSelectedStyle := dsSelected;
  FFillCaption := true;
  FFrameSize := 1;
end;

procedure TJvImageListViewerOptions.SetDrawingStyle(
  const Value: TDrawingStyle);
begin
  if FDrawingStyle <> Value then
  begin
    FDrawingStyle := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetFillCaption(const Value: boolean);
begin
  if FFillCaption <> Value then
  begin
    FFillCaption := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetFrameSize(const Value: word);
begin
  if FFrameSize <> Value then
  begin
    FFrameSize := Value;
    Change;
  end;
end;

procedure TJvImageListViewerOptions.SetSelectedStyle(
  const Value: TDrawingStyle);
begin
  if FSelectedStyle <> Value then
  begin
    FSelectedStyle := Value;
    Change;
  end;
end;

{ TJvImageListViewer }

constructor TJvImageListViewer.Create(AOwner: TComponent);
begin
  inherited;
  FChangeLink := TChangeLink.Create;
  FChangeLink.OnChange := DoImageChange;
  Color := clWindow;
end;

destructor TJvImageListViewer.Destroy;
begin
  Images := nil; 
  FChangeLink.Free;
  inherited;
end;

procedure TJvImageListViewer.DoImageChange(Sender: TObject);
begin
  if Images <> nil then
    Count := Images.Count
  else
    Count := 0;
  Repaint;
end;

procedure TJvImageListViewer.DrawItem(Index: Integer; State: TCustomDrawState; Canvas: TCanvas; ItemRect, TextRect:
  TRect);
const
  DrawingStyles: array[TDrawingStyle] of Cardinal = (ILD_FOCUS, ILD_SELECTED,
    ILD_NORMAL, ILD_TRANSPARENT);
  DrawMask: array[boolean] of Cardinal = (ILD_MASK, ILD_NORMAL);
var
  X, Y: integer;
  S: string;             
  DrawStyle, Flags: Cardinal;
begin
  Canvas.Brush.Color := Color;
  Canvas.Font := self.Font;
  if Images <> nil then
  begin
    Flags := DT_END_ELLIPSIS or DT_EDITCONTROL;
    S := GetCaption(Index);
    // determine where to draw image
    X := Max(ItemRect.Left, ItemRect.Left + (RectWidth(ItemRect) - Images.Width) div 2);
    Y := ItemRect.Top + (RectHeight(ItemRect) - Images.Height) div 2;
    if not Options.FillCaption then
      OffsetRect(TextRect,0,2);
    if cdsSelected in State then
    begin
      if Options.BrushPattern.Active then
      begin
        Canvas.Pen.Color := Options.BrushPattern.OddColor;
        Canvas.Brush.Bitmap := Options.BrushPattern.GetBitmap;
      end
      else
      begin
        Canvas.Pen.Color := Options.BrushPattern.OddColor;
        Canvas.Brush.Color := Options.BrushPattern.OddColor;
      end;
      if Options.FrameSize > 0 then
      begin
        Canvas.Pen.Width := Options.FrameSize;
        Canvas.Rectangle(ItemRect);
      end
      else
        Canvas.FillRect(ItemRect);
    end
    else
    begin
      Canvas.Brush.Color := Color;
      Canvas.FillRect(ItemRect);
    end;
    if cdsSelected in Items[Index].State then
      DrawStyle := DrawingStyles[Options.SelectedStyle]
    else
      DrawStyle := DrawingStyles[Options.DrawingStyle];
    ImageList_Draw(Images.Handle, Index, Canvas.Handle, X, Y, DrawStyle or
      DrawMask[Images.ImageType = itImage]);
    Images.
    if S <> '' then
    begin
      if cdsSelected in State then
      begin
        Canvas.Brush.Color := clHighlight; // Options.BrushPattern.OddColor;
        Canvas.Font.Color := clHighlightText; // Options.BrushPattern.EvenColor;
      end
      else
        SetBkMode(Canvas.Handle, Windows.TRANSPARENT);
      if (Options.Layout <> tlCenter) and Options.FillCaption then
        Canvas.FillRect(TextRect)
      else
        S := ' ' + S + ' ';
      ViewerDrawText(Canvas, PChar(S), Length(S), TextRect, Flags, taCenter, tlCenter, true);
    end;
//    if not Options.BrushPattern.Active and (cdsSelected in State) then
//    begin
//      Canvas.DrawFocusRect(ItemRect);
//    end;
  end;
end;

function TJvImageListViewer.GetCaption(ImageIndex: integer): string;
begin
  Result := '';
  if Assigned(FOnGetCaption) then
    FOnGetCaption(self, ImageIndex, Result);
end;

function TJvImageListViewer.GetOptions: TJvImageListViewerOptions;
begin
  Result := TJvImageListViewerOptions(inherited Options);
end;

function TJvImageListViewer.GetOptionsClass: TJvItemViewerOptionsClass;
begin
  Result := TJvImageListViewerOptions;
end;

procedure TJvImageListViewer.Notification(AComponent: TComponent;
  Operation: TOperation);
begin
  inherited;
  if (Operation = opRemove) and (AComponent = FImages) then
    Images := nil;
end;

procedure TJvImageListViewer.SetImages(const Value: TCustomImageList);
begin
  if FImages <> Value then
  begin
    if FImages <> nil then
      FImages.UnRegisterChanges(FChangeLink);
    Count := 0;
    FImages := Value;
    if FImages <> nil then
    begin
      Options.Width := Max(Options.Width, FImages.Width);
      Options.Height := Max(Options.Height, FImages.Height);
      FImages.RegisterChanges(FChangeLink);
    end;
    DoImageChange(Value);
  end;
end;

procedure TJvImageListViewer.SetOptions(const Value: TJvImageListViewerOptions);
begin
  inherited Options := Value;
end;

end.

 
